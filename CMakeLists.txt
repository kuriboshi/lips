#
# Copyright 2020-2022 Krister Joas
#
cmake_minimum_required(VERSION 3.18.0)

project(lips VERSION 0.3.0)
set(CMAKE_CXX_STANDARD 20)
enable_testing()
if(POLICY CMP0114)
  cmake_policy(SET CMP0114 NEW)
endif()

option(LIPS_ENABLE_TRACE "Enable the ability to trace the interpreter" ON)
option(LIPS_ENABLE_CODE_COVERAGE "Enable code coverage support" ON)
option(LIPS_ENABLE_TESTS "Enable compiling test code" ON)
option(LIPS_ENABLE_CLANG_TIDY "Enable checking with clang-tidy" OFF)
option(LIPS_ENABLE_OBJECT_SIZES "Enable printing object sizes with" OFF)
option(LIPS_USE_SSHFS "Turn this ON if using sshfs on macOS (see README.md)" OFF)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

if(LIPS_ENABLE_CODE_COVERAGE AND NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
  include(coverage)
endif()

if(LIPS_ENABLE_CLANG_TIDY)
  include(clang-tidy)
endif()

include(docker)

include(cmake-format)

set(LIPS_SOURCE_LOCATION
    "${CMAKE_INSTALL_PREFIX}/src/lips"
    CACHE STRING "Where to install source code for cpprint")

list(APPEND CMAKE_PREFIX_PATH "${PROJECT_BINARY_DIR}/install")
add_subdirectory(external)

find_package(fmt REQUIRED)
find_package(Catch2 REQUIRED)

add_subdirectory(src)

if(LIPS_ENABLE_TESTS)
  add_test(NAME lisp_test
    COMMAND lisp_test --load ${CMAKE_CURRENT_SOURCE_DIR}/lisp/test.lisp
                      --loadpath ${CMAKE_CURRENT_SOURCE_DIR})
endif()

if(LIPS_ENABLE_TRACE)
  # Enable ability to trace calls to peval.
  target_compile_definitions(lisp PRIVATE TRACE)
endif()

include(GNUInstallDirs)
install(TARGETS lisp EXPORT lisp)
install(
  EXPORT lisp
  NAMESPACE lisp::
  FILE lispConfig.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake)
