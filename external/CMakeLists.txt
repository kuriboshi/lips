#
# Lips -- lisp shell
# Copyright 2020-2022 Krister Joas
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

include(FetchContent)

function(make_available _NAME)
  FetchContent_GetProperties(${_NAME})
  string(TOLOWER ${_NAME} _name)
  if(NOT ${_name}_POPULATED)
    FetchContent_Populate(${_NAME})
    add_subdirectory(${${_name}_SOURCE_DIR} ${${_name}_BINARY_DIR} EXCLUDE_FROM_ALL)
  endif()
endfunction()

FetchContent_Declare(catch2
  URL https://github.com/catchorg/Catch2/archive/v3.2.1.tar.gz
  URL_HASH SHA256=4613d3e8142b672159fcae252a4860d72c8cf8e2df5043b1ae3541db9ef5d73c)

FetchContent_Declare(fmt
  URL https://github.com/fmtlib/fmt/archive/8.1.1.tar.gz
  URL_HASH SHA256=3d794d3cf67633b34b2771eb9f073bde87e846e0d395d254df7b211ef1ec7346)

make_available(catch2)
make_available(fmt)
