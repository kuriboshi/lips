#
# Copyright 2020 Krister Joas
#
add_library(libisp)
target_sources(libisp PRIVATE
  alloc.cc
  arith.cc
  eval.cc
  file.cc
  io.cc
  lisp.cc
  logic.cc
  low.cc
  map.cc
  debug.cc
  posix.cc
  pred.cc
  prim.cc
  prop.cc
  string.cc
  user.cc
  version.cc
)
target_sources(libisp PRIVATE
  alloc.hh
  arith.hh
  base.hh
  config.hh
  defs.hh
  error.hh
  eval.hh
  except.hh
  file.hh
  io.hh
  iter.hh
  libisp.hh
  lisp.hh
  logic.hh
  low.hh
  map.hh
  debug.hh
  posix.hh
  pred.hh
  prim.hh
  prop.hh
  string.hh
  user.hh
  version.hh
)
target_include_directories(libisp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(libisp PUBLIC -Wall -Werror)
target_compile_definitions(libisp PUBLIC LISP_T_VARIANT)
target_link_libraries(libisp doctest)
add_executable(testlibisp test.cc)
target_link_libraries(testlibisp libisp)
add_executable(main main.cc)
target_link_libraries(main libisp)
