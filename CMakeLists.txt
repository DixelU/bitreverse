cmake_minimum_required(VERSION 3.25)
project(bitreverse)

set(CMAKE_CXX_STANDARD 23)

add_executable(bitreverse main.cpp)

target_compile_options(bitreverse PUBLIC -Wall -Wextra -pedantic -O3 -ggdb)
#target_link_options(bitreverse PUBLIC -fsanitize=address)