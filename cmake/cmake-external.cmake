#
# Lips, lisp shell.
#
# Copyright 2022 Krister Joas
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

function(cmake_external)
  set(oneValueArgs NAME URL SHA256 BUILD_COMMAND INSTALL_COMMAND)
  set(multiValueArgs DEFINE)
  cmake_parse_arguments(EXTERNAL "" "${oneValueArgs}" "${multiValueArgs}"
                        ${ARGN})

  if(NOT EXTERNAL_NAME)
    message(FATAL_ERROR "cmake_external: Missing NAME")
  endif()

  if(NOT EXTERNAL_URL)
    message(FATAL_ERROR "cmake_external: Missing URL for ${EXTERNAL_NAME}")
  endif()

  if(EXTERNAL_SHA256)
    set(SHA256 "URL_HASH SHA256=${EXTERNAL_SHA256}")
  else()
    message(WARNING "cmake_external: Missing SHA256 for ${EXTERNAL_NAME}")
    set(SHA256 "")
  endif()

  set(DEFS "")
  set(prefix "")
  foreach(D ${EXTERNAL_DEFINE})
    string(APPEND DEFS "${prefix}-D ${D}")
    set(prefix "\n    ")
  endforeach()
  if(CMAKE_OSX_DEPLOYMENT_TARGET)
    string(
      APPEND DEFS
      "${prefix}-D CMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}")
  endif()

  if(EXTERNAL_BUILD_COMMAND)
    set(BUILD_COMMAND "${EXTERNAL_BUILD_COMMAND}")
  else()
    set(BUILD_COMMAND "${CMAKE_COMMAND} --build . --config Release")
  endif()

  if(EXTERNAL_INSTALL_COMMAND)
    set(INSTALL_COMMAND "${EXTERNAL_INSTALL_COMMAND}")
  else()
    set(INSTALL_COMMAND "${CMAKE_COMMAND} --install . --config Release")
  endif()

  file(
    WRITE "${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt.in"
    [[
cmake_minimum_required(VERSION ${CMAKE_MINIMUM_REQUIRED_VERSION})

project(${EXTERNAL_NAME}.download NONE)

include(ExternalProject)

ExternalProject_Add(${EXTERNAL_NAME}.external
  URL ${EXTERNAL_URL}
  ${SHA256}
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/src
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/build
  INSTALL_DIR ${PROJECT_BINARY_DIR}/install
  CONFIGURE_COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}"
    ${DEFS}
    -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
    <SOURCE_DIR>
  BUILD_COMMAND ${BUILD_COMMAND}
  INSTALL_COMMAND ${INSTALL_COMMAND}
  TEST_COMMAND ""
  USES_TERMINAL_BUILD TRUE
  )
]])

  configure_file("${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt")

  execute_process(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
                  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/download")
  execute_process(COMMAND "${CMAKE_COMMAND}" --build .
                  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/download")

endfunction()
