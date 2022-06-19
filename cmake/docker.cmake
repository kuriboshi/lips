#
# Lips, lisp shell.
#
# Copyright 2022 Krister Joas
#
if(APPLE)
  set(LIPS_CONTAINER_APP "podman")
  set(LIPS_CONTAINER_USER_ID "0")
  set(LIPS_CONTAINER_GROUP_ID "0")
  if(LIPS_USE_SSHFS)
    set(LIPS_CONTAINER_TEST "-t")
    set(LIPS_CONTAINER_PREFIX "/mnt/")
  else()
    set(LIPS_CONTAINER_TEST "")
    set(LIPS_CONTAINER_PREFIX "")
  endif()
else()
  set(LIPS_CONTAINER_APP "docker")
  execute_process(COMMAND id -u OUTPUT_VARIABLE LIPS_CONTAINER_USER_ID)
  string(STRIP ${LIPS_CONTAINER_USER_ID} LIPS_CONTAINER_USER_ID)
  execute_process(COMMAND id -g OUTPUT_VARIABLE LIPS_CONTAINER_GROUP_ID)
  string(STRIP ${LIPS_CONTAINER_GROUP_ID} LIPS_CONTAINER_GROUP_ID)
  set(LIPS_CONTAINER_TEST "-t")
  set(LIPS_CONTAINER_PREFIX "")
endif()

macro(lips_build_and_test dockerfile container_tag)
  add_custom_target(
    ubuntu${container_tag}
    USES_TERMINAL
    COMMAND "${LIPS_CONTAINER_APP}" build
      --build-arg USER_ID="${LIPS_CONTAINER_USER_ID}"
      --build-arg GROUP_ID="${LIPS_CONTAINER_GROUP_ID}"
      -t ${container_tag}
      -f "${CMAKE_CURRENT_SOURCE_DIR}/test/${dockerfile}" .
    COMMAND "${LIPS_CONTAINER_APP}" run --rm -u 0:0
      -v "${LIPS_CONTAINER_PREFIX}${CMAKE_CURRENT_SOURCE_DIR}:/project" ${container_tag}
      bash -c "test/build.sh ${LIPS_CONTAINER_TEST} build/${container_tag}"
  )
  add_dependencies(test-linux ubuntu${container_tag})
  if("${ARGV2}" STREQUAL "tidy")
    add_custom_target(
      ubuntu${container_tag}-tidy
      USES_TERMINAL
      COMMAND "${LIPS_CONTAINER_APP}" build
        --build-arg USER_ID="${LIPS_CONTAINER_USER_ID}"
        --build-arg GROUP_ID="${LIPS_CONTAINER_GROUP_ID}"
        -t ${container_tag}
        -f "${CMAKE_CURRENT_SOURCE_DIR}/test/${dockerfile}" .
      COMMAND "${LIPS_CONTAINER_APP}" run --rm -u 0:0
        -v "${LIPS_CONTAINER_PREFIX}${CMAKE_CURRENT_SOURCE_DIR}:/project" "${container_tag}"
        bash -c "test/tidy.sh build/${container_tag}"
    )
    add_dependencies(test-linux ubuntu${container_tag}-tidy)
  endif()
endmacro()

add_custom_target(test-linux)
lips_build_and_test(Ubuntu-18.04 18)
lips_build_and_test(Ubuntu-20.04 20)
lips_build_and_test(Ubuntu-22.04 22 tidy)
lips_build_and_test(Ubuntu-22.04-clang 22-clang)
