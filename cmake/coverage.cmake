#
# Lips, lisp shell.
#
# Copyright 2021-2022 Krister Joas
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

if("${CMAKE_C_COMPILER_ID}" MATCHES "(Apple)?[Cc]lang"
   OR "${CMAKE_CXX_COMPILER_ID}" MATCHES "(Apple)?[Cc]lang")
  if(APPLE)
    list(APPEND CMAKE_PREFIX_PATH "/Library/Developer/CommandLineTools/usr/bin")
  endif()
  find_program(LLVM_PROFDATA llvm-profdata REQUIRED)
  find_program(LLVM_COV llvm-cov REQUIRED)

  set(CMAKE_C_FLAGS
      "${CMAKE_C_FLAGS} -fprofile-instr-generate -fcoverage-mapping")
  set(CMAKE_CXX_FLAGS
      "${CMAKE_CXX_FLAGS} -fprofile-instr-generate -fcoverage-mapping")

  set(TARGET_NAME lisp_test)
  add_custom_target(
    ccov-preprocessing
    COMMAND
      LLVM_PROFILE_FILE=${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profraw
      $<TARGET_FILE:${TARGET_NAME}> --load
      ${CMAKE_CURRENT_SOURCE_DIR}/lisp/test.lisp --loadpath
      ${CMAKE_CURRENT_SOURCE_DIR}
    COMMAND
      ${LLVM_PROFDATA} merge -sparse
      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profraw -o
      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profdata
    DEPENDS ${TARGET_NAME})
  add_custom_target(
    ccov-report
    COMMAND ${LLVM_COV} report $<TARGET_FILE:${TARGET_NAME}>
            -instr-profile=${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profdata
    DEPENDS ccov-preprocessing)
  add_custom_target(
    ccov
    COMMAND
      ${LLVM_COV} show $<TARGET_FILE:${TARGET_NAME}>
      -instr-profile=${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profdata
      -show-line-counts-or-regions -output-dir=${CMAKE_CURRENT_BINARY_DIR}/html
      -format="html"
    DEPENDS ccov-preprocessing)
elseif(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_C_FLAGS
      "${CMAKE_C_FLAGS} --coverage -fprofile-arcs -ftest-coverage")
  set(CMAKE_CXX_FLAGS
      "${CMAKE_CXX_FLAGS} --coverage -fprofile-arcs -ftest-coverage")
else()
  message(FATAL_ERROR "Code coverage requires Clang or GCC")
endif()
