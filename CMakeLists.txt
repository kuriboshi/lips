#
# Lips -- lisp shell
# Copyright 2020-2023 Krister Joas
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

project(lips VERSION 1.0.3)
set(CMAKE_CXX_STANDARD 20)
enable_testing()
set_property(GLOBAL PROPERTY USE_FOLDERS YES)

if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()

option(LIPS_ENABLE_TRACE "Enable the ability to trace the interpreter" ON)
option(LIPS_ENABLE_CODE_COVERAGE "Enable code coverage support" OFF)
option(LIPS_ENABLE_TESTS "Enable compiling test code" OFF)
option(LIPS_ENABLE_BENCHMARK "Enable compiling benchmark program" OFF)
option(LIPS_ENABLE_CLANG_TIDY "Enable checking with clang-tidy" OFF)
option(LIPS_USE_SSHFS "Turn this ON if using sshfs on macOS (see README.md)"
       OFF)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

if(LIPS_ENABLE_CODE_COVERAGE AND NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
  include(coverage)
endif()

if(LIPS_ENABLE_CLANG_TIDY)
  include(clang-tidy)
endif()
include(clang-format)

include(docker)
include(cmake-format)
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(LIPS_SOURCE_LOCATION
    "${CMAKE_INSTALL_PREFIX}/src/lips"
    CACHE STRING "Where to install source code for cpprint")

list(APPEND CMAKE_PREFIX_PATH "${PROJECT_BINARY_DIR}/install")
add_subdirectory(external)

add_subdirectory(src)

if(LIPS_ENABLE_TRACE)
  # Enable ability to trace calls to peval.
  target_compile_definitions(lisp PRIVATE TRACE)
endif()

install(
  EXPORT ${PROJECT_NAME}-targets
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
  # FILE ${PROJECT_NAME}-config.cmake
  NAMESPACE lips::)

configure_package_config_file(cmake/config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  INSTALL_DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/cmake/${PROJECT_NAME})

write_basic_package_version_file(
  ${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})
