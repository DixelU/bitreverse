cmake_minimum_required(VERSION 3.25)
project(bitreverse)

set(CMAKE_CXX_STANDARD 23)

add_executable(bitreverse main.cpp)

set(BUILD_SHARED_LIBS OFF)
set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++ -static")
set_target_properties(bitreverse PROPERTIES LINK_SEARCH_END_STATIC 1)