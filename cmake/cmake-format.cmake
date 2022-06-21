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

find_program(CMAKE_FORMAT cmake-format)
if(CMAKE_FORMAT)
  file(GLOB_RECURSE LIPS_CMAKE_FILES "src/CMake*.txt" "cmake/*.cmake")
  add_custom_target(cmake-format COMMAND ${CMAKE_FORMAT} -i ${LIPS_CMAKE_FILES})
endif()
