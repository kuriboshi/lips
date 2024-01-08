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

FetchContent_Declare(
  lips
  GIT_REPOSITORY "file://${CMAKE_CURRENT_SOURCE_DIR}/.."
  GIT_TAG HEAD)

FetchContent_MakeAvailable(lips)
find_package(Catch2 REQUIRED)

add_executable(package_test)
target_sources(package_test PRIVATE package_test.cc)
target_link_libraries(package_test lips::lisp Catch2::Catch2)

add_test(NAME package_lisp COMMAND package_test)
