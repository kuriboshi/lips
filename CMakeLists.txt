#
# Copyright 2020 Krister Joas
#
cmake_minimum_required(VERSION 3.13.0)

project(lips VERSION 0.2.0)
set(CMAKE_CXX_STANDARD 17)

add_subdirectory(libisp)
add_subdirectory(src)
