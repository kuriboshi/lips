#
# Copyright 2020-2022 Krister Joas
#
add_executable(lisp_test lisp_test.cc)
target_sources(
  lisp_test
  PRIVATE alloc.test.cc
          arith.test.cc
          debug.test.cc
          eval.test.cc
          file.test.cc
          io.test.cc
          lisp.test.cc
          logic.test.cc
          low.test.cc
          map.test.cc
          main.test.cc
          parser.test.cc
          pred.test.cc
          prim.test.cc
          prop.test.cc
          reader.test.cc
          repl.test.cc
          string.test.cc
          symbol.test.cc
          user.test.cc)
target_link_libraries(lisp_test PUBLIC liblisp Catch2::Catch2)
add_test(NAME lisp_test COMMAND lisp_test)
