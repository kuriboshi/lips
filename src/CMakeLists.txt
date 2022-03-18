#
# Copyright 2020-2022 Krister Joas
#
add_library(lisp)
add_library(lips::lisp ALIAS lisp)
target_sources(
  lisp
  PRIVATE lisp/alloc.cc
          lisp/arith.cc
          lisp/eval.cc
          lisp/file.cc
          lisp/io.cc
          lisp/lisp.cc
          lisp/logic.cc
          lisp/low.cc
          lisp/map.cc
          lisp/debug.cc
          lisp/parser.cc
          lisp/pred.cc
          lisp/prim.cc
          lisp/prop.cc
          lisp/reader.cc
          lisp/string.cc
          lisp/user.cc
          lisp/version.cc)
target_sources(
  lisp
  PRIVATE lisp/alloc.hh
          lisp/arith.hh
          lisp/error.hh
          lisp/eval.hh
          lisp/except.hh
          lisp/file.hh
          lisp/io.hh
          lisp/iter.hh
          lisp/libisp.hh
          lisp/lisp.hh
          lisp/logic.hh
          lisp/low.hh
          lisp/map.hh
          lisp/debug.hh
          lisp/parser.hh
          lisp/pred.hh
          lisp/prim.hh
          lisp/prop.hh
          lisp/reader.hh
          lisp/ref_ptr.hh
          lisp/repl.hh
          lisp/string.hh
          lisp/user.hh
          lisp/version.hh)
target_include_directories(
  lisp PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
              $<INSTALL_INTERFACE:include/lisp>)
target_link_libraries(lisp PUBLIC fmt::fmt)

add_executable(main lisp/main.cc)
target_sources(
  main
  PRIVATE lisp/alloc.test.cc
          lisp/arith.test.cc
          lisp/debug.test.cc
          lisp/eval.test.cc
          lisp/file.test.cc
          lisp/io.test.cc
          lisp/lisp.test.cc
          lisp/logic.test.cc
          lisp/low.test.cc
          lisp/map.test.cc
          lisp/main.test.cc
          lisp/parser.test.cc
          lisp/pred.test.cc
          lisp/prim.test.cc
          lisp/prop.test.cc
          lisp/reader.test.cc
          lisp/repl.test.cc
          lisp/string.test.cc
          lisp/symbol.test.cc
          lisp/user.test.cc
          lisp/main.hh)
target_link_libraries(main PUBLIC lisp Catch2::Catch2)
add_test(NAME lisp_test COMMAND main --test)

add_subdirectory(lips)
