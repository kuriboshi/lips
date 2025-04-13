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

find_program(LLVM_CLANG_FORMAT clang-format)
message(STATUS "LLVM_CLANG_FORMAT: ${LLVM_CLANG_FORMAT}")
if(LLVM_CLANG_FORMAT)
  file(GLOB_RECURSE LIPS_FILES "src/*.cc" "src/*.hh")
  add_custom_target(format COMMAND ${LLVM_CLANG_FORMAT} -i ${LIPS_FILES})
  add_custom_target(format-check COMMAND ${LLVM_CLANG_FORMAT} -n ${LIPS_FILES})
endif()
