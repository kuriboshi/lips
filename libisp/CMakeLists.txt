#
# Copyright 2020 Krister Joas
#
add_library(lisp)
add_library(lips::lisp ALIAS lisp)
target_sources(lisp PRIVATE
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
target_sources(lisp PRIVATE
  alloc.hh
  arith.hh
  base.hh
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
target_include_directories(lisp PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:include/libisp>
  )
target_compile_options(lisp PRIVATE -Wall -Werror)
target_link_libraries(lisp doctest)
add_executable(test test.cc)
target_link_libraries(test lisp)
add_executable(main main.cc)
target_link_libraries(main lisp)
