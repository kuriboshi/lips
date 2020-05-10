#
# Copyright 2020 Krister Joas
#
cmake_minimum_required(VERSION 3.13.0)

project(lips VERSION 0.2.0)
set(CMAKE_CXX_STANDARD 17)

include(FetchContent)

function(make_available _NAME)
  fetchcontent_getproperties(${_NAME})
  string(TOLOWER ${_NAME} _name)
  if(NOT ${_name}_POPULATED)
    fetchcontent_populate(${_NAME})
    if(EXISTS "${${_name}_SOURCE_DIR}/CMakeLists.txt")
      add_subdirectory(${${_name}_SOURCE_DIR} ${${_name}_BINARY_DIR} EXCLUDE_FROM_ALL)
    else()
      # If there is no CMakeLists.txt we assume a header only library
      add_library(${_name} INTERFACE IMPORTED)
      target_include_directories(${_name} INTERFACE "${${_name}_SOURCE_DIR}/include")
    endif()
  endif()
  # Force the SOURCE_DIR variable into parent scope
  set(${_name}_SOURCE_DIR "${${_name}_SOURCE_DIR}" PARENT_SCOPE)
endfunction()

fetchcontent_declare(doctest
  URL https://github.com/onqtam/doctest/archive/2.3.7.tar.gz)
make_available(doctest)

add_subdirectory(libisp)
add_subdirectory(src)

# Define TRACE if you want to be able to trace calls to peval.  Useful
# for debugging.
target_compile_definitions(libisp PRIVATE TRACE)

# If this isn't defined you can't read floating point numbers.
# target_compile_definitions(libisp PRIVATE FLOATING)

# Defines where the source is for cpprint.
#target_compile_definitions(libisp PRIVATE "LIPSLIB=\"/usr/local/src/lips\"")

# File that is used to find source code for primitives.
#target_compile_definitions(libisp PRIVATE "TAGSFILE=\"/usr/local/src/lips/src/Tags\"")

# Defining this will use the select call to handle default answer in
# getuser().
target_compile_definitions(lips PRIVATE SELECT)

# Define this to enable job control, requires SIGCHLD, sigblock...
target_compile_definitions(lips PRIVATE JOB_CONTROL)

# Defining this includes code to take care of signals and ask if the
# core should be dumped or not.
target_compile_definitions(lips PRIVATE FANCY_SIGNALS)

# Global init file.
#target_compile_definitions(lips PRIVATE "LIPSRC=\"/usr/local/lib/lipsrc\"")
