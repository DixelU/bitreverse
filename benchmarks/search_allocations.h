#pragma once

// Optional single-threaded benchmark instrumentation. Count requested C++ heap
// bytes, including temporary buffers, but not allocator overhead or process RSS.
// Compile a separate binary with -DBITREVERSE_TRACK_ALLOCATIONS; use the ordinary
// binary for timing so allocation instrumentation cannot skew the comparison.
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <limits>
#include <new>

namespace search_allocations
{
inline std::size_t live = 0, peak = 0, calls = 0;
struct header { void* allocation; std::size_t size; };

inline void* allocate(std::size_t size, std::size_t alignment)
{
    alignment = alignment < alignof(header) ? alignof(header) : alignment;
    const auto overhead = sizeof(header) + alignment - 1;
    if (size > std::numeric_limits<std::size_t>::max() - overhead)
        throw std::bad_alloc();
    void* raw = std::malloc(size + overhead);
    if (!raw) throw std::bad_alloc();
    const auto address = (reinterpret_cast<std::uintptr_t>(raw) + overhead)
        & ~(static_cast<std::uintptr_t>(alignment) - 1);
    auto* metadata = reinterpret_cast<header*>(address) - 1;
    *metadata = {raw, size};
    live += size;
    if (live > peak) peak = live;
    ++calls;
    return reinterpret_cast<void*>(address);
}

inline void release(void* pointer) noexcept
{
    if (!pointer) return;
    const auto* metadata = static_cast<header*>(pointer) - 1;
    live -= metadata->size;
    std::free(metadata->allocation);
}
}

void* operator new(std::size_t size) { return search_allocations::allocate(size, alignof(std::max_align_t)); }
void* operator new[](std::size_t size) { return ::operator new(size); }
void* operator new(std::size_t size, std::align_val_t alignment)
{ return search_allocations::allocate(size, static_cast<std::size_t>(alignment)); }
void* operator new[](std::size_t size, std::align_val_t alignment) { return ::operator new(size, alignment); }
void operator delete(void* pointer) noexcept { search_allocations::release(pointer); }
void operator delete[](void* pointer) noexcept { search_allocations::release(pointer); }
void operator delete(void* pointer, std::size_t) noexcept { search_allocations::release(pointer); }
void operator delete[](void* pointer, std::size_t) noexcept { search_allocations::release(pointer); }
void operator delete(void* pointer, std::align_val_t) noexcept { search_allocations::release(pointer); }
void operator delete[](void* pointer, std::align_val_t) noexcept { search_allocations::release(pointer); }
void operator delete(void* pointer, std::size_t, std::align_val_t) noexcept { search_allocations::release(pointer); }
void operator delete[](void* pointer, std::size_t, std::align_val_t) noexcept { search_allocations::release(pointer); }
