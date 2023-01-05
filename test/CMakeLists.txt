#
# Lips -- lisp shell
# Copyright 2023 Krister Joas
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

cmake_minimum_required(VERSION 3.21.0)
project(package_test VERSION 0.0.0)
set(CMAKE_CXX_STANDARD 20)
enable_testing()

include(FetchContent)

if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()

function(make_available _NAME)
  FetchContent_GetProperties(${_NAME})
  string(TOLOWER ${_NAME} _name)
  if(NOT ${_name}_POPULATED)
    FetchContent_Populate(${_NAME})
    add_subdirectory(${${_name}_SOURCE_DIR} ${${_name}_BINARY_DIR} EXCLUDE_FROM_ALL)
  endif()
endfunction()

FetchContent_Declare(lips
  GIT_REPOSITORY file://${CMAKE_CURRENT_SOURCE_DIR}/..
  GIT_TAG HEAD
)

FetchContent_Declare(catch2
  URL https://github.com/catchorg/Catch2/archive/v3.2.1.tar.gz
  URL_HASH SHA256=4613d3e8142b672159fcae252a4860d72c8cf8e2df5043b1ae3541db9ef5d73c)

FetchContent_Declare(fmt
  URL https://github.com/fmtlib/fmt/archive/9.1.0.tar.gz
  URL_HASH SHA256=5dea48d1fcddc3ec571ce2058e13910a0d4a6bab4cc09a809d8b1dd1c88ae6f2)

make_available(catch2)
make_available(fmt)
make_available(lips)

add_executable(package_test)
target_sources(package_test PRIVATE package_test.cc)
target_link_libraries(package_test lips::lisp fmt::fmt Catch2::Catch2)

add_test(NAME package_lisp COMMAND package_test)
