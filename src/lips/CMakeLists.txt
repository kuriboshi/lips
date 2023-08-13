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

add_executable(lips)
target_sources(
  lips
  PRIVATE env.cc
          exec.cc
          glob.cc
          main.cc
          os.cc
          term.cc
          top.cc)
target_sources(
  lips
  PRIVATE env.hh
          exec.hh
          glob.hh
          job.hh
          main.hh
          lips_error.hh
          os.hh
          term.hh
          top.hh)
target_link_libraries(lips PRIVATE liblisp)
target_link_libraries(lips PRIVATE Catch2::Catch2)
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION
                                            VERSION_LESS 9.0)
  target_link_libraries(lips PRIVATE stdc++fs)
endif()
target_compile_options(lips PRIVATE -Wall -Werror)
target_compile_definitions(
  lips PRIVATE "LIPSRC=\"${CMAKE_INSTALL_PREFIX}/lib/lipsrc\"")
if(LIPS_ENABLE_CLANG_TIDY)
  set_target_properties(lips PROPERTIES CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
endif()

find_package(Curses REQUIRED)
target_link_libraries(lips PRIVATE ${CURSES_LIBRARIES})

if(LIPS_ENABLE_TESTS)
  add_test(NAME test_lips COMMAND lips -T)
endif()
