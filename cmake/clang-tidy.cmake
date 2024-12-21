#
# Lips, lisp shell.
#
# Copyright 2022-2024 Krister Joas
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

if(APPLE)
  list(APPEND CMAKE_PREFIX_PATH "/usr/local/opt/llvm/bin")
  list(APPEND CMAKE_PREFIX_PATH "/opt/homebrew/opt/llvm/bin")
endif()
find_program(LLVM_CLANG_TIDY NAMES clang-tidy-19 clang-tidy-18 clang-tidy REQUIRED)
