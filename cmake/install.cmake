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

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set(LIPS_SOURCE_LOCATION "${CMAKE_INSTALL_PREFIX}/src/lips"
    CACHE STRING "Where to install source code for cpprint")

list(APPEND CMAKE_PREFIX_PATH "${PROJECT_BINARY_DIR}/install")

install(EXPORT ${PROJECT_NAME}-targets DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}
        NAMESPACE lips::)

configure_package_config_file(
  cmake/config.cmake.in "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  INSTALL_DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/cmake/${PROJECT_NAME})

write_basic_package_version_file(
  ${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})
