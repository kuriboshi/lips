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

add_library(liblisp)
add_library(lips::lisp ALIAS liblisp)
set_target_properties(liblisp PROPERTIES EXPORT_NAME lisp)
target_sources(
  liblisp
  PRIVATE lisp/alloc.cc
          lisp/arith.cc
          lisp/eval.cc
          lisp/file.cc
          lisp/io.cc
          lisp/lexer.cc
          lisp/lisp.cc
          lisp/logic.cc
          lisp/low.cc
          lisp/map.cc
          lisp/debug.cc
          lisp/parser.cc
          lisp/pred.cc
          lisp/prim.cc
          lisp/prop.cc
          lisp/repl.cc
          lisp/rtable.cc
          lisp/string.cc
          lisp/user.cc
          lisp/version.cc)

set(TARGET_HEADERS
    lisp/alloc.hh
    lisp/arith.hh
    lisp/error.hh
    lisp/eval.hh
    lisp/except.hh
    lisp/file.hh
    lisp/io.hh
    lisp/iter.hh
    lisp/lexer.hh
    lisp/lisp.hh
    lisp/logic.hh
    lisp/low.hh
    lisp/map.hh
    lisp/debug.hh
    lisp/parser.hh
    lisp/pool.hh
    lisp/pred.hh
    lisp/prim.hh
    lisp/prop.hh
    lisp/ref_ptr.hh
    lisp/repl.hh
    lisp/rtable.hh
    lisp/string.hh
    lisp/user.hh
    lisp/version.hh
    lisp/details/arith.hh
    lisp/details/debug.hh
    lisp/details/file.hh
    lisp/details/logic.hh
    lisp/details/low.hh
    lisp/details/map.hh
    lisp/details/pred.hh
    lisp/details/prim.hh
    lisp/details/prop.hh
    lisp/details/string.hh
    lisp/details/user.hh)
# Add the header files as private so they will show up in the IDE. The
# header files also need to be installed separately. Using
# PUBLIC_HEADER doesn't work because it doesn't preserve the directory
# hierarchy.
target_sources(liblisp PRIVATE ${TARGET_HEADERS})
target_include_directories(
  liblisp PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                 $<INSTALL_INTERFACE:include>)

# We only need to know where the header files for fmt are to be able
# to build the library. Any executable need to add fmt::fmt as a
# dependency themselves.
get_target_property(_fmt_include_directories fmt::fmt INTERFACE_INCLUDE_DIRECTORIES)
target_include_directories(liblisp PRIVATE ${_fmt_include_directories})

# Since there is both an executable and a library called 'lisp' we use
# the target 'liblisp' for the library. In order for the name of the
# library to be 'liblisp.a' we override the OUTPUT_NAME of the
# library.
set_target_properties(liblisp PROPERTIES OUTPUT_NAME lisp)

if(LIPS_ENABLE_CLANG_TIDY)
  set_target_properties(liblisp PROPERTIES CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
endif()

add_executable(lisp)
target_sources(lisp PRIVATE lisp/main.cc)
target_link_libraries(lisp lips::lisp fmt::fmt)

if(LIPS_ENABLE_CLANG_TIDY)
  set_target_properties(lisp PROPERTIES CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
endif()

add_subdirectory(lips)

if(LIPS_ENABLE_TESTS)
  add_executable(lisp_test lisp/lisp_test.cc)
  target_sources(
    lisp_test
    PRIVATE lisp/alloc.test.cc
            lisp/arith.test.cc
            lisp/debug.test.cc
            lisp/eval.test.cc
            lisp/file.test.cc
            lisp/io.test.cc
            lisp/lexer.test.cc
            lisp/lisp.test.cc
            lisp/logic.test.cc
            lisp/low.test.cc
            lisp/map.test.cc
            lisp/main.test.cc
            lisp/parser.test.cc
            lisp/pool.test.cc
            lisp/pred.test.cc
            lisp/prim.test.cc
            lisp/prop.test.cc
            lisp/repl.test.cc
            lisp/string.test.cc
            lisp/symbol.test.cc
            lisp/user.test.cc)
  target_link_libraries(lisp_test PUBLIC lips::lisp fmt::fmt Catch2::Catch2)
  if(LIPS_ENABLE_OBJECT_SIZES)
    target_compile_definitions(lisp_test PRIVATE ENABLE_OBJECT_SIZES)
  endif()
endif()

install(DIRECTORY lisp/
  DESTINATION include/lisp
  FILES_MATCHING PATTERN "*.hh")

install(TARGETS liblisp
  EXPORT ${PROJECT_NAME}-targets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  INCLUDES DESTINATION include)
