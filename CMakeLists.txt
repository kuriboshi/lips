#
# Lips -- lisp shell
# Copyright 2020-2025 Krister Joas
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# The minimum version of cmake which supports version 3 of
# CMakePresets.json.
cmake_minimum_required(VERSION 3.21.0)

# The top level CMakeLists.txt is used to configure and build the
# project in two phases. In the first phase a configuration is created
# which doesn't do the actual build. Instead it creates an environment
# which defines several targets which then builds the project. That's
# why the LANGUAGES is set to NONE so that phase one doesn't bother
# trying to find the compilers which are not needed during phase one.
project(lips VERSION 3.4.0 LANGUAGES NONE)

# This project requires C++23.
set(CMAKE_CXX_STANDARD 23)

# Visually organize parts of the project in folders in the IDE.
set_property(GLOBAL PROPERTY USE_FOLDERS YES)

# In cmake 3.24 and later this policy is set to OLD and will issue a
# warning. The policy how timestamps are handled when extracting
# downloaded artifacts.
if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()

option(LIPS_ENABLE_TRACE "Enable the ability to trace the interpreter" ON)
option(LIPS_ENABLE_CODE_COVERAGE "Enable code coverage support" OFF)
option(LIPS_ENABLE_TESTS "Enable compiling test code" OFF)
option(LIPS_ENABLE_BENCHMARK "Enable compiling benchmark program" OFF)
option(LIPS_ENABLE_CLANG_TIDY "Enable checking with clang-tidy" OFF)

# This controls the build phase. During phase one this variable is set
# to OFF. During phase two, when the actual build takes place, this
# variable is set to ON. The default is ON so that if you don't use
# the presets or the Makefile phase one is skipped. This is not a user
# editable option.
option(LIPS_BUILD "Used to enabled building from source - not user editable" ON)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# Include the build phase cmake code.
include(build)

# Formatting the code using clang-format can be done in phase one as
# long as the program clang-format is found.
include(clang-format)

# Phase two of the build.
if(LIPS_BUILD)
  enable_language(CXX)
  enable_testing()

  if(LIPS_ENABLE_CODE_COVERAGE AND NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
    include(coverage)
  endif()

  if(LIPS_ENABLE_CLANG_TIDY)
    include(clang-tidy)
  endif()

  # External packages are installed in this director and subsequent
  # calls to find_package look in this location to find the
  # dependencies.
  list(APPEND CMAKE_PREFIX_PATH "${PROJECT_BINARY_DIR}/install")
  add_subdirectory(external)

  find_package(Curses REQUIRED)
  find_package(Catch2 REQUIRED)
  find_package(fmt REQUIRED)

  add_subdirectory(src)

  if(LIPS_ENABLE_TRACE)
    # Enable ability to trace calls to peval.
    target_compile_definitions(liblisp PRIVATE LIPS_ENABLE_TRACE)
  endif()

  # The installation part of build phase two.
  include(install)
endif()
