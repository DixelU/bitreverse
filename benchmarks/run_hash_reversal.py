#!/usr/bin/env python3
"""Run bounded digest-only inversion experiments with independent hash checks.

The C++ benchmark writes the forward DAG record before creating an equality.
This driver retains that record even if the later solve reaches a process
timeout. Python's hashlib/zlib provide targets and validate returned preimages;
their native implementations also supply a small-domain enumeration baseline.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import subprocess
import sys
import time
import zlib
from pathlib import Path


ALGORITHMS = ("crc32", "md5", "sha1", "sha256")
ENGINES = ("dpll", "affine", "cdcl")
FIELDS = (
    "algorithm", "message_hex", "unknown_bits", "engine", "budget", "target_hex",
    "output_bits", "nodes", "gates", "inputs", "constants", "edges", "depth",
    "width", "gate_width", "max_fanout", "xor_gates", "and_gates", "or_gates",
    "not_gates", "output_depth_min", "output_depth_mean", "output_depth_max",
    "build_ms", "metrics_ms", "equality_nodes", "equality_depth", "equality_width",
    "status", "solve_ms", "decisions", "conflicts", "propagations", "search_steps",
    "solutions", "affine_active", "recovered_hex", "recovered_verified",
    "process_ms", "process_timeout_s", "process_returncode", "forward_record",
    "native_domain", "native_trials", "native_matches", "native_total_ms",
    "native_first_trials", "native_first_ms", "native_first_hex",
    "native_exhaustive", "error", "extra_json",
)


def digest(algorithm: str, message: bytes) -> str:
    if algorithm == "crc32":
        return f"{zlib.crc32(message) & 0xffffffff:08x}"
    return hashlib.new(algorithm, message).hexdigest()


def candidate(message: bytes, unknown_bits: int, assignment: int) -> bytes:
    """Replace low bits of byte 0, then byte 1, preserving the fixed bits."""
    result = bytearray(message)
    remaining = unknown_bits
    for index in range((unknown_bits + 7) // 8):
        width = min(8, remaining)
        mask = (1 << width) - 1
        result[index] = (result[index] & ~mask) | (assignment & mask)
        assignment >>= width
        remaining -= width
    return bytes(result)


def native_baseline(algorithm: str, message: bytes, unknown_bits: int,
                    target: str) -> dict:
    """Full enumeration, including uniqueness, is intentionally capped at 16 bits.

    Timings include Python candidate construction, iteration, and API overhead,
    plus the native hash call. They are not pure native implementation timings.
    """
    domain = 1 << unknown_bits
    result = {"native_domain": domain, "native_exhaustive": False}
    if unknown_bits > 16:
        return result
    matches = 0
    started = time.perf_counter()
    for assignment in range(domain):
        probe = candidate(message, unknown_bits, assignment)
        if digest(algorithm, probe) == target:
            matches += 1
            if matches == 1:
                result.update(native_first_trials=assignment + 1,
                              native_first_ms=(time.perf_counter() - started) * 1000,
                              native_first_hex=probe.hex())
    result.update(native_trials=domain, native_matches=matches,
                  native_total_ms=(time.perf_counter() - started) * 1000,
                  native_exhaustive=True)
    if matches == 0:
        raise RuntimeError("native enumeration omitted the known target witness")
    return result


def as_text(value: str | bytes | None) -> str:
    if value is None:
        return ""
    return value.decode("utf-8", errors="replace") if isinstance(value, bytes) else value


def verify_preimage(algorithm: str, original: bytes, unknown_bits: int,
                    target: str, recovered_hex: str) -> None:
    recovered = bytes.fromhex(recovered_hex)
    if len(recovered) != len(original):
        raise ValueError("recovered message has the wrong length")
    for index, (expected, actual) in enumerate(zip(original, recovered)):
        variable_width = max(0, min(8, unknown_bits - index * 8))
        fixed_mask = 0xff ^ ((1 << variable_width) - 1)
        if ((actual ^ expected) & fixed_mask) != 0:
            raise ValueError("recovered message changes a fixed input bit")
    if digest(algorithm, recovered) != target:
        raise ValueError("recovered message fails independent hashlib/zlib verification")


def invoke(executable: Path, algorithm: str, message: bytes, unknown_bits: int,
           target: str, engine: str, max_steps: int, timeout: float) -> dict:
    row = dict(algorithm=algorithm, message_hex=message.hex(), unknown_bits=unknown_bits,
               engine=engine, budget=max_steps, target_hex=target,
               process_timeout_s=timeout, forward_record=False,
               recovered_verified=False)
    command = [str(executable), algorithm, message.hex(), str(unknown_bits),
               target, engine, str(max_steps)]
    started = time.perf_counter()
    timed_out = False
    try:
        process = subprocess.run(command, capture_output=True, text=True,
                                 encoding="utf-8", errors="replace", timeout=timeout,
                                 check=False)
        stdout, stderr = process.stdout, process.stderr
        row["process_returncode"] = process.returncode
    except subprocess.TimeoutExpired as error:
        timed_out = True
        stdout, stderr = as_text(error.stdout), as_text(error.stderr)
    except OSError as error:
        row.update(status="error", error=str(error),
                   process_ms=(time.perf_counter() - started) * 1000)
        return row
    row["process_ms"] = (time.perf_counter() - started) * 1000
    records = []
    errors = []
    for line in stdout.splitlines():
        if not line.strip():
            continue
        try:
            record = json.loads(line)
            if not isinstance(record, dict) or record.get("phase") not in ("forward", "solve"):
                raise ValueError("unexpected benchmark record")
            records.append(record)
        except (json.JSONDecodeError, ValueError) as error:
            # A killed child can leave a final incomplete JSON line. Earlier
            # complete records remain useful; report truncation in the CSV.
            errors.append(f"output parsing: {error}")
    forward = [record for record in records if record["phase"] == "forward"]
    solved = [record for record in records if record["phase"] == "solve"]
    if forward:
        row.update({key: value for key, value in forward[0].items() if key != "phase"})
        row["forward_record"] = True
        for key, expected in (("algorithm", algorithm), ("message_hex", message.hex()),
                              ("unknown_bits", unknown_bits), ("engine", engine),
                              ("budget", max_steps), ("target_hex", target)):
            if row.get(key) != expected:
                errors.append(f"forward record {key} does not match the requested experiment")
    if solved:
        row.update({key: value for key, value in solved[0].items() if key != "phase"})
        if solved[0].get("status") == "sat":
            try:
                verify_preimage(algorithm, message, unknown_bits, target,
                                solved[0].get("recovered_hex", ""))
                row["recovered_verified"] = True
            except (ValueError, TypeError) as error:
                errors.append(str(error))
        elif solved[0].get("status") == "unsat":
            errors.append("solver rejected a target with a known in-domain witness")
        elif solved[0].get("status") != "step_limit":
            errors.append("unknown solve status")
    if timed_out:
        row["status"] = "timeout"
    elif row.get("process_returncode") != 0:
        errors.append(f"benchmark exited with code {row.get('process_returncode')}")
    elif len(forward) != 1 or (engine == "shape" and solved) or (engine != "shape" and len(solved) != 1):
        errors.append("benchmark did not emit exactly the expected phase records")
    elif engine == "shape":
        row["status"] = "shape_only"
    if errors:
        # Timeouts are still distinguished from solver UNSAT and hard failures.
        row["status"] = "timeout" if timed_out else "error"
        row["error"] = "; ".join(errors)
    if stderr.strip():
        row["error"] = "; ".join(part for part in (row.get("error", ""), stderr.strip()) if part)
    extras = {key: value for key, value in row.items() if key not in FIELDS}
    if extras:
        row["extra_json"] = json.dumps(extras, sort_keys=True, separators=(",", ":"))
    return {key: value for key, value in row.items() if key in FIELDS}


def comma_list(value: str) -> list[str]:
    return [part.strip() for part in value.split(",") if part.strip()]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--exe", required=True, type=Path, help="compiled hash_reversal_benchmark executable")
    parser.add_argument("--output", default="-", help="CSV destination, or - for stdout")
    parser.add_argument("--max-steps", type=int, default=2_000_000, help="search work units per solve (default: 2000000)")
    parser.add_argument("--timeout", type=float, default=30.0, help="wall-clock seconds allowed per process (default: 30)")
    parser.add_argument("--quick", action="store_true", help="4/8-bit shapes and DPLL queries only")
    parser.add_argument("--algorithms", type=comma_list, default=list(ALGORITHMS), help="comma-separated crc32,md5,sha1,sha256")
    parser.add_argument("--bits", type=comma_list, help="comma-separated unknown widths; explicit widths also select solve widths")
    parser.add_argument("--engines", type=comma_list, help="comma-separated dpll,affine,cdcl; shape is always measured")
    parser.add_argument("--input-hex", default="61626364", help="known target witness (default: abcd); low prefix bits become unknown")
    args = parser.parse_args()
    try:
        args.message = bytes.fromhex(args.input_hex)
        args.bits = [int(part) for part in args.bits] if args.bits else ([4, 8] if args.quick else [0, 4, 8, 12, 16, 24, 32])
    except ValueError as error:
        parser.error(str(error))
    if not args.bits or any(width < 0 or width > len(args.message) * 8 for width in args.bits):
        parser.error("unknown bit widths must fit inside the input message")
    if not args.algorithms or any(name not in ALGORITHMS for name in args.algorithms):
        parser.error("unsupported or empty algorithm list")
    args.engines = args.engines if args.engines is not None else (["dpll"] if args.quick else ["dpll", "affine"])
    if any(engine not in ENGINES for engine in args.engines):
        parser.error("engines must be dpll, affine, or cdcl (shape is always included)")
    if args.max_steps <= 0 or args.timeout <= 0:
        parser.error("--max-steps and --timeout must be positive")
    args.exe = args.exe.resolve()
    if not args.exe.is_file():
        parser.error(f"benchmark executable does not exist: {args.exe}")
    # Preserve user order while avoiding accidental duplicate timing rows.
    args.bits = list(dict.fromkeys(args.bits))
    args.algorithms = list(dict.fromkeys(args.algorithms))
    args.engines = list(dict.fromkeys(args.engines))
    return args


def main() -> int:
    args = parse_args()
    explicit_bits = any(arg == "--bits" or arg.startswith("--bits=") for arg in sys.argv[1:])
    solve_bits = set(args.bits) if explicit_bits else {4, 8, 12, 16}
    destination = None
    if args.output != "-":
        output = Path(args.output)
        output.parent.mkdir(parents=True, exist_ok=True)
        destination = output.open("w", newline="", encoding="utf-8")
    stream = destination if destination is not None else sys.stdout
    failures = 0
    try:
        writer = csv.DictWriter(stream, fieldnames=FIELDS)
        writer.writeheader()
        stream.flush()
        print("Targets and recovered messages: Python hashlib/zlib; native baseline timings "
              "include Python enumeration and allocation overhead. Search limits are not UNSAT.", file=sys.stderr)
        for algorithm in args.algorithms:
            target = digest(algorithm, args.message)
            for unknown_bits in args.bits:
                baseline = native_baseline(algorithm, args.message, unknown_bits, target)
                engines = ["shape"] + (args.engines if unknown_bits in solve_bits else [])
                for engine in engines:
                    print(f"{algorithm}: {unknown_bits} unknown bits, {engine}", file=sys.stderr, flush=True)
                    row = invoke(args.exe, algorithm, args.message, unknown_bits, target,
                                 engine, args.max_steps, args.timeout)
                    row.update(baseline)
                    writer.writerow(row)
                    stream.flush()
                    print(f"  {row.get('status', 'error')}, process {row['process_ms']:.3f} ms" +
                          (f", {row['error']}" if row.get("error") else ""), file=sys.stderr, flush=True)
                    failures += row.get("status") == "error"
    finally:
        if destination is not None:
            destination.close()
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
