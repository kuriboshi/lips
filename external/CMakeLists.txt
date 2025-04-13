#
# Lips -- lisp shell
# Copyright 2020-2024 Krister Joas
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

file(
  DOWNLOAD https://raw.githubusercontent.com/kuriboshi/cmake-external/v1.1.0/cmake-external.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/cmake/cmake-external.cmake
  EXPECTED_HASH SHA256=3d616b2d2fc702e7caef237ef86777b6e679140d31e9f2037b4a45b3ce97b81e)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_BINARY_DIR}/cmake)

include(cmake-external)

cmake_external(catch2 URL https://github.com/catchorg/Catch2/archive/v3.8.1.tar.gz
  SHA256 18b3f70ac80fccc340d8c6ff0f339b2ae64944782f8d2fca2bd705cf47cadb79)

cmake_external(fmt URL https://github.com/fmtlib/fmt/archive/11.1.4.tar.gz
  SHA256 ac366b7b4c2e9f0dde63a59b3feb5ee59b67974b14ee5dc9ea8ad78aa2c1ee1e
  DEFINE FMT_DOC=OFF FMT_TEST=OFF)
