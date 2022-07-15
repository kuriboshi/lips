#
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
    lisp/version.hh)
# Add the header files as private so they will show up in the IDE. The
# header files also need to be added to the PUBLIC_HEADER property so
# they are installed.
target_sources(liblisp PRIVATE ${TARGET_HEADERS})
set_target_properties(liblisp PROPERTIES PUBLIC_HEADER "${TARGET_HEADERS}")
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

add_executable(lisp lisp/main.cc)
target_sources(lisp PRIVATE lisp/main.hh)
target_link_libraries(lisp PUBLIC liblisp fmt::fmt Catch2::Catch2)

if(LIPS_ENABLE_CLANG_TIDY)
  set_target_properties(lisp PROPERTIES CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
endif()

add_subdirectory(lips)

if(LIPS_ENABLE_TESTS)
  add_subdirectory(test)
endif()

install(TARGETS lisp RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(
  TARGETS liblisp
  EXPORT lisp
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/lisp")
