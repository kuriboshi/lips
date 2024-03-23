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

add_library(liblips)
target_sources(liblips PRIVATE
  env.cc
  exec.cc
  glob.cc
  job.cc
  os.cc
  term.cc
  top.cc
  transform.cc)
target_sources(liblips PRIVATE
  env.hh
  exec.hh
  glob.hh
  job.hh
  lips_error.hh
  os.hh
  term.hh
  top.hh
  transform.hh)
target_link_libraries(liblips PUBLIC fmt::fmt liblisp Catch2::Catch2)
set_target_properties(liblips PROPERTIES OUTPUT_NAME lips)

find_package(Curses REQUIRED)
target_link_libraries(liblips PUBLIC ${CURSES_LIBRARIES})

add_executable(lips)
target_sources(lips PRIVATE main.cc)
target_sources(lips PRIVATE main.hh)
target_link_libraries(lips PRIVATE liblips)
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.0)
  target_link_libraries(lips PRIVATE stdc++fs)
endif()
target_compile_options(lips PRIVATE -Wall -Werror)
target_compile_definitions(lips PRIVATE "LIPSRC=\"${CMAKE_INSTALL_PREFIX}/lib/lipsrc\"")
if(LIPS_ENABLE_CLANG_TIDY)
  set_target_properties(lips PROPERTIES CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
endif()

add_executable(lips_test)
target_sources(lips_test PRIVATE exec.test.cc lips_test.cc transform.test.cc glob.test.cc)
target_link_libraries(lips_test PUBLIC liblips)

if(LIPS_ENABLE_TESTS)
  add_test(NAME test_lips COMMAND lips_test)
endif()
