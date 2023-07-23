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

file(DOWNLOAD
  https://raw.githubusercontent.com/kuriboshi/cmake-external/v1.0.1/cmake-external.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/cmake/cmake-external.cmake
  EXPECTED_HASH SHA256=eb85f6bfd601ad472160a1d9d2880ac57a96ce635132a4162c1a5a13d9ab9152
)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_BINARY_DIR}/cmake)

include(cmake-external)

cmake_external(catch2
  URL https://github.com/catchorg/Catch2/archive/v3.3.2.tar.gz
  SHA256 8361907f4d9bff3ae7c1edb027f813659f793053c99b67837a0c0375f065bae2
)

cmake_external(fmt
  URL https://github.com/fmtlib/fmt/archive/9.1.0.tar.gz
  SHA256 5dea48d1fcddc3ec571ce2058e13910a0d4a6bab4cc09a809d8b1dd1c88ae6f2
  DEFINE FMT_DOC=OFF FMT_TEST=OFF
)
