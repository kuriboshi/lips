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

if(APPLE)
  set(LIPS_CONTAINER_APP "podman")
  set(LIPS_CONTAINER_USER_ID "0")
  set(LIPS_CONTAINER_GROUP_ID "0")
else()
  set(LIPS_CONTAINER_APP "docker")
  execute_process(COMMAND id -u OUTPUT_VARIABLE LIPS_CONTAINER_USER_ID)
  string(STRIP ${LIPS_CONTAINER_USER_ID} LIPS_CONTAINER_USER_ID)
  execute_process(COMMAND id -g OUTPUT_VARIABLE LIPS_CONTAINER_GROUP_ID)
  string(STRIP ${LIPS_CONTAINER_GROUP_ID} LIPS_CONTAINER_GROUP_ID)
endif()

macro(lips_build_and_test dockerfile container_tag)
  add_custom_target(
    ubuntu${container_tag}
    USES_TERMINAL
    COMMAND
      "${LIPS_CONTAINER_APP}" build --build-arg
      USER_ID="${LIPS_CONTAINER_USER_ID}" --build-arg
      GROUP_ID="${LIPS_CONTAINER_GROUP_ID}" -t ${container_tag} -f
      "${CMAKE_CURRENT_SOURCE_DIR}/test/${dockerfile}" .
    COMMAND
      "${LIPS_CONTAINER_APP}" run --rm
      -u "${LIPS_CONTAINER_USER_ID}:${LIPS_CONTAINER_GROUP_ID}"
      -v "${CMAKE_CURRENT_SOURCE_DIR}:/project"
      ${container_tag} bash -c
      "test/build.sh build/${container_tag}")
  set_target_properties(ubuntu${container_tag} PROPERTIES FOLDER "Test")
  add_dependencies(test-linux ubuntu${container_tag})
  if("${ARGV2}" STREQUAL "tidy")
    add_custom_target(
      ubuntu${container_tag}-tidy
      USES_TERMINAL
      COMMAND
        "${LIPS_CONTAINER_APP}" build --build-arg
        USER_ID="${LIPS_CONTAINER_USER_ID}" --build-arg
        GROUP_ID="${LIPS_CONTAINER_GROUP_ID}" -t ${container_tag} -f
        "${CMAKE_CURRENT_SOURCE_DIR}/test/${dockerfile}" .
      COMMAND
        "${LIPS_CONTAINER_APP}" run --rm
        -u "${LIPS_CONTAINER_USER_ID}:${LIPS_CONTAINER_GROUP_ID}"
        -v "${CMAKE_CURRENT_SOURCE_DIR}:/project"
        "${container_tag}" bash -c "test/tidy.sh build/${container_tag}")
    set_target_properties(ubuntu${container_tag}-tidy PROPERTIES FOLDER "Test")
    add_dependencies(test-linux ubuntu${container_tag}-tidy)
  endif()
endmacro()

add_custom_target(test-linux)
set_target_properties(test-linux PROPERTIES FOLDER "Test")
lips_build_and_test(Ubuntu-20.04 20)
lips_build_and_test(Ubuntu-22.04 22 tidy)
lips_build_and_test(Ubuntu-22.04-clang 22-clang)
