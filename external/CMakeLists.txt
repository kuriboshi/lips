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

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/cmake-external.cmake)
  file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/cmake)
  file(COPY_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake-external.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/cmake/cmake-external.cmake
  )
else()
  file(DOWNLOAD
    https://raw.githubusercontent.com/kuriboshi/cmake-external/v1.1.0/cmake-external.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/cmake/cmake-external.cmake
    EXPECTED_HASH SHA256=3d616b2d2fc702e7caef237ef86777b6e679140d31e9f2037b4a45b3ce97b81e
  )
endif()
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_BINARY_DIR}/cmake)

include(cmake-external)

cmake_external(catch2
  URL https://github.com/catchorg/Catch2/archive/v3.5.0.tar.gz
  SHA256 f6d4f8d78a9b59ec72a81d49f58d18eb317372ac07f8d9432710a079e69fd66a
)

cmake_external(fmt
  URL https://github.com/fmtlib/fmt/archive/10.1.1.tar.gz
  SHA256 78b8c0a72b1c35e4443a7e308df52498252d1cefc2b08c9a97bc9ee6cfe61f8b
  DEFINE FMT_DOC=OFF FMT_TEST=OFF
)
