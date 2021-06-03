#
# Copyright 2020-2021 Krister Joas
#
add_library(lisp)
add_library(lips::lisp ALIAS lisp)
target_sources(lisp PRIVATE
  src/lisp/alloc.cc
  src/lisp/arith.cc
  src/lisp/eval.cc
  src/lisp/file.cc
  src/lisp/io.cc
  src/lisp/lisp.cc
  src/lisp/logic.cc
  src/lisp/low.cc
  src/lisp/map.cc
  src/lisp/debug.cc
  src/lisp/posix.cc
  src/lisp/pred.cc
  src/lisp/prim.cc
  src/lisp/prop.cc
  src/lisp/string.cc
  src/lisp/user.cc
  src/lisp/version.cc
)
target_sources(lisp PRIVATE
  src/lisp/alloc.hh
  src/lisp/arith.hh
  src/lisp/base.hh
  src/lisp/error.hh
  src/lisp/eval.hh
  src/lisp/except.hh
  src/lisp/file.hh
  src/lisp/io.hh
  src/lisp/iter.hh
  src/lisp/libisp.hh
  src/lisp/lisp.hh
  src/lisp/logic.hh
  src/lisp/low.hh
  src/lisp/map.hh
  src/lisp/debug.hh
  src/lisp/posix.hh
  src/lisp/pred.hh
  src/lisp/prim.hh
  src/lisp/prop.hh
  src/lisp/string.hh
  src/lisp/user.hh
  src/lisp/version.hh
)
target_include_directories(lisp PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
  $<INSTALL_INTERFACE:include/lisp>
  )
target_link_libraries(lisp PUBLIC fmt::fmt)

add_executable(test src/lisp/test.cc)
target_link_libraries(test PUBLIC lisp doctest::doctest)

add_executable(main src/lisp/main.cc)
target_link_libraries(main PUBLIC lisp)
