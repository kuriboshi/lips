#
# Lips, lisp shell.
#
# Copyright 2022 Krister Joas
#
if(APPLE)
  list(APPEND CMAKE_PREFIX_PATH "/usr/local/opt/llvm/bin")
endif()
find_program(LLVM_CLANG_TIDY clang-tidy REQUIRED)
set(CMAKE_CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
