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

# Choose either `podman` or `docker` depending on the platform.
if(APPLE)
  set(LIPS_CONTAINER_APP "podman")
else()
  set(LIPS_CONTAINER_APP "docker")
endif()

#
# @brief Add a custom target building on Linux according to parameters.
#
# @details Adds a custom target which builds a docker image based on
# *dockerfile*. The custom target is tagged with *container_tag*. The
# lips source code is mounted read-only under `/project/lips` and the
# ephemeral result of the build is in `/project/build`. The
# *build_type* is the CMake preset used to configure and build the
# project.
#
# @param dockerfile The dockerfile used to build the image.
# @param container_tag The tag used to tag the docker image.
# @param build_type The CMake preset used to configure and build the
#   project.
#
function(lips_build_and_test dockerfile container_tag)
  execute_process(COMMAND id -u OUTPUT_VARIABLE USER OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND id -g OUTPUT_VARIABLE GROUP OUTPUT_STRIP_TRAILING_WHITESPACE)
  if(NOT ARGV2)
    set(build_type "container")
    set(target "${container_tag}")
  else()
    set(build_type "${ARGV2}")
    set(target "${container_tag}-${build_type}")
  endif()
  message(STATUS "${dockerfile} ${container_tag} ${build_type} ${target}")
  add_custom_target(
    "${target}"
    USES_TERMINAL
    COMMENT "Build for ${dockerfile}/${target}"
    COMMAND "${LIPS_CONTAINER_APP}" build -t "${container_tag}" -f
            "${CMAKE_CURRENT_SOURCE_DIR}/${dockerfile}" .
    COMMAND mkdir -p "${CMAKE_CURRENT_BINARY_DIR}/${target}"
    COMMAND "${LIPS_CONTAINER_APP}" run --rm --user "${USER}:${GROUP}"
            -v "${CMAKE_CURRENT_SOURCE_DIR}/..:/project/lips:ro"
            -v "${CMAKE_CURRENT_BINARY_DIR}/${target}:/project/build:rw"
            "${container_tag}"
            /project/lips/test/build.sh "${build_type}")
  set_target_properties("${target}" PROPERTIES FOLDER "Test")
  add_dependencies(test-linux "${target}")
endfunction()

add_custom_target(test-linux)
set_target_properties(test-linux PROPERTIES FOLDER "Test")

lips_build_and_test(Ubuntu-22.04 ubuntu22)
lips_build_and_test(Ubuntu-24.04 ubuntu24)
lips_build_and_test(Ubuntu-24.04 ubuntu24 clang)

lips_build_and_test(Fedora-40 fedora40)
lips_build_and_test(Fedora-41 fedora41)
lips_build_and_test(Fedora-41 fedora41 tidy)
lips_build_and_test(Fedora-41 fedora41 clang)

lips_build_and_test(Alpine alpine)
