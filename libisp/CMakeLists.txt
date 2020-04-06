#
# Copyright 2020 Krister Joas
#
add_library(libisp)
target_sources(libisp PRIVATE
  alloc.c
  arith.c
  config.h
  constants.h
  defs.h
  error.h
  eval.c
  file.c
  func.h
  garb.h
  init.c
  io.h
  libisp.h
  lisp.h
  logic.c
  low.c
  map.c
  misc.c
  os.c
  pred.c
  prim.c
  prop.c
  read.c
  string.c
  unix.c
  user.c
  version.c
)
target_include_directories(libisp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(libisp PUBLIC -Werror)
