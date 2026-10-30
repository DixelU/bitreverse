#ifndef BITREVERSE_SUPPORT_TIMING_H
#define BITREVERSE_SUPPORT_TIMING_H

#include <chrono>

namespace support
{

using clock = std::chrono::steady_clock;

template<class Rep, class Period>
double milliseconds(std::chrono::duration<Rep, Period> duration)
{
	return std::chrono::duration<double, std::milli>(duration).count();
}

template<class Rep, class Period>
double seconds(std::chrono::duration<Rep, Period> duration)
{
	return std::chrono::duration<double>(duration).count();
}

inline double milliseconds_since(clock::time_point started)
{
	return milliseconds(clock::now() - started);
}

} // namespace support

#endif // BITREVERSE_SUPPORT_TIMING_H
