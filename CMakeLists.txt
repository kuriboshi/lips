#
# Copyright 2020-2022 Krister Joas
#
cmake_minimum_required(VERSION 3.18.0)

project(lips VERSION 0.3.0)
set(CMAKE_CXX_STANDARD 17)
enable_testing()
if(POLICY CMP0114)
  cmake_policy(SET CMP0114 NEW)
endif()

option(ENABLE_TRACE "Enable the ability to trace the interpreter" ON)
option(ENABLE_CODE_COVERAGE "Enable code coverage support" ON)
option(ENABLE_OBJECT_SIZES "Enable printing object sizes with --test" OFF)

if(ENABLE_CODE_COVERAGE AND NOT ${CMAKE_GENERATOR} STREQUAL "Xcode")
  if("${CMAKE_C_COMPILER_ID}" MATCHES "(Apple)?[Cc]lang"
     OR "${CMAKE_CXX_COMPILER_ID}" MATCHES "(Apple)?[Cc]lang")
    if(APPLE)
      list(APPEND CMAKE_PREFIX_PATH "/usr/local/opt/llvm")
    endif()
    find_program(LLVM_PROFDATA llvm-profdata REQUIRED)
    find_program(LLVM_COV llvm-cov REQUIRED)

    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -fprofile-instr-generate -fcoverage-mapping")

    set(TARGET_NAME main)
    add_custom_target(
      ccov-preprocessing
      COMMAND LLVM_PROFILE_FILE=${TARGET_NAME}.profraw
              $<TARGET_FILE:${TARGET_NAME}> --test
      COMMAND ${LLVM_PROFDATA} merge -sparse ${TARGET_NAME}.profraw -o
              ${TARGET_NAME}.profdata
      DEPENDS ${TARGET_NAME})
    add_custom_target(
      ccov-report
      COMMAND ${LLVM_COV} report $<TARGET_FILE:${TARGET_NAME}>
              -instr-profile=${TARGET_NAME}.profdata
      DEPENDS ccov-preprocessing)
    add_custom_target(
      ccov
      COMMAND
        ${LLVM_COV} show $<TARGET_FILE:${TARGET_NAME}>
        -instr-profile=${TARGET_NAME}.profdata -show-line-counts-or-regions
        -output-dir=${CMAKE_CURRENT_BINARY_DIR}/llvm-cov -format="html"
      DEPENDS ccov-preprocessing)
  elseif(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} --coverage -fprofile-arcs -ftest-coverage")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} --coverage -fprofile-arcs -ftest-coverage")
  else()
    message(FATAL_ERROR "Code coverage requires Clang or GCC")
  endif()
endif()

set(LIPS_SOURCE_LOCATION
    "${CMAKE_INSTALL_PREFIX}/src/lips"
    CACHE STRING "Where to install source code for cpprint")

list(APPEND CMAKE_PREFIX_PATH "${PROJECT_BINARY_DIR}/install")
add_subdirectory(external)

find_package(fmt REQUIRED)
find_package(doctest REQUIRED)

add_subdirectory(src)

if(ENABLE_TRACE)
  # Enable ability to trace calls to peval.
  target_compile_definitions(lisp PRIVATE TRACE)
endif()

if(ENABLE_OBJECT_SIZES)
  target_compile_definitions(main PRIVATE ENABLE_OBJECT_SIZES)
endif()

include(GNUInstallDirs)
install(TARGETS lisp EXPORT lisp)
install(
  EXPORT lisp
  NAMESPACE lisp::
  FILE lispConfig.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake)
