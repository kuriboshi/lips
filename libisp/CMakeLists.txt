#
# Copyright 2020 Krister Joas
#
add_library(libisp)
target_sources(libisp PRIVATE
  alloc.c
  arith.c
  eval.c
  file.c
  init.c
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
target_sources(libisp PRIVATE
  config.h
  constants.h
  defs.h
  error.h
  func.h
  garb.h
  io.h
  libisp.h
  lisp.h
 )
target_include_directories(libisp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(libisp PUBLIC -Wall -Werror)
