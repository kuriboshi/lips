#
# Copyright 2022-2023 Krister Joas
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

# Configure and build an external library.
#
# Options:
#   NAME                Name of the external library
#   URL                 URL from where to download the source code
#   SHA256              The SHA256 checksum of the library (optional
#                       but a warning will be issued)
#   BUILD_COMMAND       The build command if different from the default
#   INSTALL_COMMAND     The install command if different from the default
#
# For the last two the default build and install command uses cmake
# with the Release configuration.
#
function(cmake_external)
  set(oneValueArgs NAME URL SHA256 BUILD_COMMAND INSTALL_COMMAND)
  set(multiValueArgs DEFINE)
  cmake_parse_arguments(EXTERNAL "" "${oneValueArgs}" "${multiValueArgs}"
                        ${ARGN})

  # NAME is mandatory.
  if(NOT EXTERNAL_NAME)
    message(FATAL_ERROR "cmake_external: Missing NAME")
  endif()

  # URL is mandatory.
  if(NOT EXTERNAL_URL)
    message(FATAL_ERROR "cmake_external: Missing URL for ${EXTERNAL_NAME}")
  endif()

  # SHA256 is optional but recommended.
  if(EXTERNAL_SHA256)
    set(SHA256 "URL_HASH SHA256=${EXTERNAL_SHA256}")
  else()
    message(WARNING "cmake_external: Missing SHA256 for ${EXTERNAL_NAME}")
    set(SHA256 "")
  endif()

  # Build the list of cmake definitions passed on to the configuration
  # of the external library. Anything specified in the DEFINE option
  # is included as well as some predefined definitions passed on from
  # the parent project.
  set(DEFS "")                  # List of definitions
  set(prefix "")                # Prefix to keep the output tidy
  # Add user specified definitions.
  foreach(D ${EXTERNAL_DEFINE})
    string(APPEND DEFS "${prefix}-D ${D}")
    set(prefix "\n    ")
  endforeach()
  # Add list of definitions propagated from the parent project.
  foreach(I CMAKE_OSX_DEPLOYMENT_TARGET BUILD_SHARED_LIBS)
    if(${I})
      string(
        APPEND DEFS
        "${prefix}-D ${I}=${${I}}")
    endif()
  endforeach()

  if(EXTERNAL_BUILD_COMMAND)
    set(BUILD_COMMAND "${EXTERNAL_BUILD_COMMAND}")
  else()
    set(BUILD_COMMAND "${CMAKE_COMMAND} --build .")
  endif()

  if(EXTERNAL_INSTALL_COMMAND)
    set(INSTALL_COMMAND "${EXTERNAL_INSTALL_COMMAND}")
  else()
    set(INSTALL_COMMAND "${CMAKE_COMMAND} --install .")
  endif()

  # Generate the cmake configuration file, configure, and install the
  # external library to a location within the build hierarchy.
  file(
    WRITE "${CMAKE_CURRENT_BINARY_DIR}/${EXTERNAL_NAME}/download/CMakeLists.txt.in"
    [[
cmake_minimum_required(VERSION ${CMAKE_MINIMUM_REQUIRED_VERSION})

project(${EXTERNAL_NAME}.download NONE)

include(ExternalProject)

ExternalProject_Add(${EXTERNAL_NAME}.external
  URL ${EXTERNAL_URL}
  ${SHA256}
  DOWNLOAD_EXTRACT_TIMESTAMP ON
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${EXTERNAL_NAME}/src
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/${EXTERNAL_NAME}/build
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

  configure_file("${CMAKE_CURRENT_BINARY_DIR}/${EXTERNAL_NAME}/download/CMakeLists.txt.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/${EXTERNAL_NAME}/download/CMakeLists.txt")

  execute_process(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
                  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${EXTERNAL_NAME}/download")
  execute_process(COMMAND "${CMAKE_COMMAND}" --build .
                  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${EXTERNAL_NAME}/download")

endfunction()
