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

add_subdirectory(lisp)
add_subdirectory(lips)

install(DIRECTORY lisp/
  DESTINATION include/lisp
  FILES_MATCHING PATTERN "*.hh")

install(TARGETS liblisp
  EXPORT ${PROJECT_NAME}-targets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  INCLUDES DESTINATION include)
