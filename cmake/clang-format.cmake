#
# Lips, lisp shell.
#
# Copyright 2022 Krister Joas
#
#if(APPLE)
#  list(APPEND CMAKE_PREFIX_PATH "/usr/local/opt/llvm/bin")
#endif()
find_program(LLVM_CLANG_FORMAT clang-format)
message(STATUS "LLVM_CLANG_FORMAT: ${LLVM_CLANG_FORMAT}")
if(LLVM_CLANG_FORMAT)
  file(GLOB_RECURSE LIPS_FILES "src/*.cc" "src/*.hh")
  add_custom_target(format COMMAND ${LLVM_CLANG_FORMAT} -i ${LIPS_FILES})
  add_custom_target(format-check COMMAND ${LLVM_CLANG_FORMAT} -n ${LIPS_FILES})
endif()
