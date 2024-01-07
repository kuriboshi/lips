#
# Lips, lisp shell.
# Copyright 2020-2024 Krister Joas
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/version.cc.in ${CMAKE_CURRENT_BINARY_DIR}/version.cc)

target_sources(
  liblisp
  PRIVATE alloc.cc
          arith.cc
          context.cc
          vm.cc
          file.cc
          io.cc
          lexer.cc
          lisp.cc
          list.cc
          logic.cc
          low.cc
          map.cc
          debug.cc
          parser.cc
          predicate.cc
          property.cc
          repl.cc
          rtable.cc
          string.cc
          symbol.cc
          syntax.cc
          user.cc
          ${CMAKE_CURRENT_BINARY_DIR}/version.cc)

set(_liblisp_source
          alloc.hh
          arith.hh
          check.hh
          context.hh
          error.hh
          vm.hh
          except.hh
          exit.hh
          file.hh
          io.hh
          iter.hh
          lexer.hh
          lisp.hh
          list.hh
          logic.hh
          low.hh
          map.hh
          debug.hh
          parser.hh
          pool.hh
          predicate.hh
          property.hh
          ref_ptr.hh
          repl.hh
          rtable.hh
          run.hh
          string.hh
          syntax.hh
          types.hh
          user.hh
          version.hh)
set(_liblisp_details
          details/alloc.hh
          details/arith.hh
          details/debug.hh
          details/file.hh
          details/list.hh
          details/logic.hh
          details/low.hh
          details/map.hh
          details/predicate.hh
          details/property.hh
          details/string.hh
          details/user.hh
          details/vm.hh)
# The reason for defining two variables with the same contents is that
# adding the PARENT_SCOPE keyword will set the value ONLY in the
# parent scope and not in the current scope. This is documented
# behavior but I find it counterintuitive.
set(_liblisp_details_parent ${_liblisp_details} PARENT_SCOPE)
target_sources(liblisp PRIVATE ${_liblisp_source} ${_liblisp_details})

add_executable(lisp)
target_sources(lisp PRIVATE main.cc)
target_link_libraries(lisp lips::lisp)

if(LIPS_ENABLE_CLANG_TIDY)
  set_target_properties(lisp PROPERTIES CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
endif()

if(LIPS_ENABLE_TESTS)
  add_executable(lisp_test)
  target_sources(
    lisp_test
    PRIVATE lisp_test.cc
            alloc.test.cc
            arith.test.cc
            check.test.cc
            context.test.cc
            debug.test.cc
            error.test.cc
            vm.test.cc
            file.test.cc
            io.test.cc
            lexer.test.cc
            lisp.test.cc
            list.test.cc
            logic.test.cc
            low.test.cc
            map.test.cc
            main.test.cc
            parser.test.cc
            pool.test.cc
            predicate.test.cc
            property.test.cc
            repl.test.cc
            rtable.test.cc
            string.test.cc
            symbol.test.cc
            syntax.test.cc
            user.test.cc)
  target_link_libraries(lisp_test PUBLIC lips::lisp Catch2::Catch2)
endif()

if(LIPS_ENABLE_TESTS)
  add_test(NAME test_lisp
           COMMAND lisp_test --load ${CMAKE_CURRENT_SOURCE_DIR}/../../lisp/test.lisp
                   --loadpath ${CMAKE_CURRENT_SOURCE_DIR}
                   --loadpath ${CMAKE_CURRENT_BINARY_DIR} "$ENV{LIPS_TEST_TAGS}")
endif()

if(LIPS_ENABLE_BENCHMARK)
  add_executable(lisp_benchmark)
  target_sources(
    lisp_benchmark
    PRIVATE lisp_benchmark.cc
            alloc.bench.cc
            pool.bench.cc)
  target_link_libraries(lisp_benchmark PUBLIC lips::lisp Catch2::Catch2)

  add_test(NAME benchmark
           COMMAND lisp_benchmark)
endif()
