#
# Lips -- lisp shell
# Copyright 2025 Krister Joas
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

# Adds a custom target which runs the cmake configure stage. It
# creates a file called ${target}_configured to indicate that the
# configuration is done.
function(lips_configure target)
  add_custom_target(
    "config_${target}"
    USES_TERMINAL
    COMMAND ${CMAKE_COMMAND} --preset "${target}" -S "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMAND ${CMAKE_COMMAND} -E touch "${CMAKE_CURRENT_BINARY_DIR}/${target}_configured"
    BYPRODUCTS "${CMAKE_CURRENT_BINARY_DIR}/${target}_configured"
  )
endfunction()

# Adds a custom target which builds the project.
function(lips_build target)
  lips_configure("${target}")
  add_custom_target(
    "${target}"
    USES_TERMINAL
    COMMENT "Build target ${target}"
    DEPENDS "config_${target}"
    COMMAND ${CMAKE_COMMAND} --build "${CMAKE_CURRENT_BINARY_DIR}/${target}"
  )
endfunction()

#  Create a collection of targets we can build during phase two.
lips_build(debug)               # Standard debug target.
lips_build(release)             # Release target.
lips_build(clang)               # Use clang (only relevant on Linux)
lips_build(llvm)                # Use llvm clang (only relevant on macos)
lips_build(tidy)                # Run clang-tidy on the project.
