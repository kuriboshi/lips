#
# Lips, lisp shell.
# Copyright 2020-2023 Krister Joas
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
target_include_directories(liblisp PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:include>)

target_link_libraries(liblisp PUBLIC fmt::fmt)

# Since there is both an executable and a library called 'lisp' we use
# the target 'liblisp' for the library. In order for the name of the
# library to be 'liblisp.a' we override the OUTPUT_NAME of the
# library.
set_target_properties(liblisp PROPERTIES OUTPUT_NAME lisp)

if(LIPS_ENABLE_CLANG_TIDY)
  set_target_properties(liblisp PROPERTIES CXX_CLANG_TIDY "${LLVM_CLANG_TIDY}")
endif()

add_subdirectory(lisp)

# Create a source group for the details header files. Adding the
# header files as private will flatten the hierarchy so the group
# "Header Files" ends up with multiple header files with the same
# name. The header files need to be installed separately anyway. Using
# PUBLIC_HEADER doesn't work because it also doesn't preserve the
# directory hierarchy.
#
# The source group for the details header files has to be added on the
# same level as the library to which they belong. At least it seems
# that way. Creating the source group one level down doesn't work.
list(TRANSFORM _liblisp_details_parent PREPEND "lisp/" OUTPUT_VARIABLE _liblisp_details)
source_group("Header Files/Details" FILES ${_liblisp_details})

add_subdirectory(lips)

install(DIRECTORY lisp/ DESTINATION include/lisp FILES_MATCHING PATTERN "*.hh")

install(TARGETS liblisp EXPORT ${PROJECT_NAME}-targets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  INCLUDES DESTINATION include)
