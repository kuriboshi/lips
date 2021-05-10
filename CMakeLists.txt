#
# Copyright 2020 Krister Joas
#
cmake_minimum_required(VERSION 3.13.0)

project(lips VERSION 0.2.0)
set(CMAKE_CXX_STANDARD 17)

option(ENABLE_TRACE "Enable the ability to trace the interpreter" ON)
option(ENABLE_FLOATING "Enable reading floating point values" OFF)
option(ENABLE_SELECT "Enable using select when asking about core dumps" ON)
option(ENABLE_JOB_CONTROL "Enable job control in the shell" ON)
option(ENABLE_FANCY_SIGNALS "Enable catching signals and offer to skip dumping core" ON)
option(ENABLE_CPPRINT "Enable ability to print source code of primitives" OFF)
option(ENABLE_LIPSRC "Enable loading a global lipsrc file" OFF)

set(LIPS_SOURCE_LOCATION "${CMAKE_INSTALL_PREFIX}/src/lips" CACHE
  STRING "Where to install source code for cpprint")

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(external)

list(APPEND CMAKE_PREFIX_PATH "${PROJECT_BINARY_DIR}/install")
add_subdirectory(external)
find_package(fmt REQUIRED)

add_subdirectory(libisp)
add_subdirectory(src)

if(ENABLE_TRACE)
  # Enable ability to trace calls to peval.
  target_compile_definitions(lisp PRIVATE TRACE)
endif()

if(ENABLE_FLOATING)
  target_compile_definitions(lisp PRIVATE FLOATING)
endif()

if(ENABLE_SELECT)
  target_compile_definitions(lips PRIVATE SELECT)
endif()

if(ENABLE_JOB_CONTROL)
  target_compile_definitions(lips PRIVATE JOB_CONTROL)
endif()

if(ENABLE_FANCY_SIGNALS)
  target_compile_definitions(lips PRIVATE FANCY_SIGNALS)
endif()

if(ENABLE_CPPRINT)
  # Defines where the source is for cpprint.
  target_compile_definitions(lisp PRIVATE "LIPSLIB=\"${LIPS_SOURCE_LOCATION}\"")
  # File that is used to find source code for primitives.
  target_compile_definitions(lisp PRIVATE "TAGSFILE=\"${LIPS_SOURCE_LOCATION}/Tags\"")
endif()

if(ENABLE_LIPSRC)
  target_compile_definitions(lips PRIVATE "LIPSRC=\"${CMAKE_INSTALL_PREFIX}/lib/lipsrc\"")
endif()

include(GNUInstallDirs)
install(TARGETS lisp
  EXPORT lisp)
install(EXPORT lisp
  NAMESPACE lisp::
  FILE lispConfig.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake)
