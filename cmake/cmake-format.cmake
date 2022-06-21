#
# Lips, lisp shell.
#
# Copyright 2022 Krister Joas
#
find_program(CMAKE_FORMAT cmake-format)
if(CMAKE_FORMAT)
  file(GLOB_RECURSE LIPS_CMAKE_FILES "src/CMake*.txt" "cmake/*.cmake")
  add_custom_target(cmake-format COMMAND ${CMAKE_FORMAT} -i ${LIPS_CMAKE_FILES})
endif()
