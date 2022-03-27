#
# Copyright 2020-2022 Krister Joas
#
add_library(liblisp)
add_library(lips::lisp ALIAS liblisp)
target_sources(
  liblisp
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
  liblisp
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
  liblisp PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                 $<INSTALL_INTERFACE:include/lisp>)
target_link_libraries(liblisp PUBLIC fmt::fmt)

add_executable(lisp lisp/main.cc)
target_sources(
  lisp
  PRIVATE lisp/main.hh)
target_link_libraries(lisp PUBLIC liblisp Catch2::Catch2)

add_subdirectory(lips)
if(LIPS_ENABLE_TESTS)
  add_subdirectory(test)
endif()
