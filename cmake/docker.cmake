#
# Lips, lisp shell.
#
# Copyright 2022-2023 Krister Joas
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
function(lips_build_and_test dockerfile container_tag build_type)
  add_custom_target(
    ${container_tag}
    USES_TERMINAL
    COMMENT "Build for ${dockerfile}/${container_tag}/${build_type}"
    COMMAND
      "${LIPS_CONTAINER_APP}" build -t "${container_tag}"
      -f "${CMAKE_CURRENT_SOURCE_DIR}/test/${dockerfile}" .
    COMMAND
      "${LIPS_CONTAINER_APP}" run --rm
      -v "${CMAKE_CURRENT_SOURCE_DIR}:/project/lips:ro"
      "${container_tag}"
      bash -c "/project/lips/test/build.sh ${build_type}")
  set_target_properties("${container_tag}" PROPERTIES FOLDER "Test")
  add_dependencies(test-linux "${container_tag}")
endfunction()

add_custom_target(test-linux)
set_target_properties(test-linux PROPERTIES FOLDER "Test")
lips_build_and_test(Ubuntu-20.04 ubuntu20 docker)
lips_build_and_test(Ubuntu-22.04 ubuntu22 docker)
lips_build_and_test(Ubuntu-22.04 ubuntu22-tidy tidy)
lips_build_and_test(Ubuntu-22.04 ubuntu22-clang clang)
