#
# Lips, lisp shell.
# Copyright 2020-2022 Krister Joas
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

add_executable(lisp_test lisp_test.cc)
target_sources(
  lisp_test
  PRIVATE alloc.test.cc
          arith.test.cc
          debug.test.cc
          eval.test.cc
          file.test.cc
          io.test.cc
          lexer.test.cc
          lisp.test.cc
          logic.test.cc
          low.test.cc
          map.test.cc
          main.test.cc
          parser.test.cc
          pool.test.cc
          pred.test.cc
          prim.test.cc
          prop.test.cc
          repl.test.cc
          string.test.cc
          symbol.test.cc
          user.test.cc)
target_link_libraries(lisp_test PUBLIC liblisp Catch2::Catch2)
if(LIPS_ENABLE_OBJECT_SIZES)
  target_compile_definitions(lisp_test PRIVATE ENABLE_OBJECT_SIZES)
endif()
