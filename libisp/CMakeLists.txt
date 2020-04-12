#
# Copyright 2020 Krister Joas
#
add_library(libisp)
target_sources(libisp PRIVATE
  alloc.cc
  arith.cc
  eval.cc
  file.cc
  init.cc
  logic.cc
  low.cc
  map.cc
  misc.cc
  os.cc
  pred.cc
  prim.cc
  prop.cc
  read.cc
  string.cc
  unix.cc
  user.cc
  version.cc
)
target_sources(libisp PRIVATE
  config.hh
  constants.hh
  defs.hh
  error.hh
  except.hh
  func.hh
  garb.hh
  io.hh
  libisp.hh
  lisp.hh
)
target_include_directories(libisp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(libisp PUBLIC -Wall -Werror)
