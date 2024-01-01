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

set(TARGET_NAME lisp_test)
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

  add_custom_target(
    ccov-preprocessing
    COMMAND
      LLVM_PROFILE_FILE=${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profraw
        $<TARGET_FILE:${TARGET_NAME}>
        --load ${CMAKE_CURRENT_SOURCE_DIR}/lisp/test.lisp
        --loadpath ${CMAKE_CURRENT_SOURCE_DIR}
        --loadpath ${CMAKE_CURRENT_BINARY_DIR} "$ENV{LIPS_TEST_TAGS}"
    COMMAND
      ${LLVM_PROFDATA} merge -sparse
      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profraw -o
      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profdata
    DEPENDS ${TARGET_NAME})
  add_custom_target(
    ccov-report
    COMMAND
      ${LLVM_COV} report $<TARGET_FILE:${TARGET_NAME}>
        --instr-profile=${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profdata
        --format="text"
        --ignore-filename-regex=exit.hh
        --ignore-filename-regex='.*.test.cc'
        ${CMAKE_CURRENT_SOURCE_DIR}/src > ${CMAKE_CURRENT_BINARY_DIR}/coverage.txt
    DEPENDS ccov-preprocessing)
  add_custom_target(
    ccov-show
    COMMAND
      ${LLVM_COV} show $<TARGET_FILE:${TARGET_NAME}>
        --instr-profile=${CMAKE_CURRENT_BINARY_DIR}/${TARGET_NAME}.profdata
        --show-line-counts-or-regions
        --format="html"
        --output-dir=${CMAKE_CURRENT_BINARY_DIR}/html
        --ignore-filename-regex=exit.hh
        --ignore-filename-regex='.*.test.cc'
        ${CMAKE_CURRENT_SOURCE_DIR}/src
    DEPENDS ccov-report)
  add_custom_target(coverage DEPENDS ccov-report ccov-show)
elseif(CMAKE_COMPILER_IS_GNUCXX)
  find_program(COVERAGE_GCOV gcov)
  find_program(COVERAGE_LCOV lcov)
  find_program(COVERAGE_GENHTML genhtml)
  set(CMAKE_C_FLAGS
      "${CMAKE_C_FLAGS} --coverage -fprofile-arcs -ftest-coverage")
  set(CMAKE_CXX_FLAGS
      "${CMAKE_CXX_FLAGS} --coverage -fprofile-arcs -ftest-coverage")
  add_custom_target(
    ccov-preprocessing
    COMMAND
      GCOV_PROFILE_DIR=${CMAKE_CURRENT_BINARY_DIR}/
        $<TARGET_FILE:${TARGET_NAME}>
        --load ${CMAKE_CURRENT_SOURCE_DIR}/lisp/test.lisp
        --loadpath ${CMAKE_CURRENT_SOURCE_DIR}
        --loadpath ${CMAKE_CURRENT_BINARY_DIR} "$ENV{LIPS_TEST_TAGS}"
    DEPENDS ${TARGET_NAME})
  add_custom_target(
    lcov-capture
    COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/output.info
    COMMAND
      lcov --capture
           --exclude='*test.cc'
           --directory ${CMAKE_CURRENT_BINARY_DIR}/src/lisp
           -o ${CMAKE_CURRENT_BINARY_DIR}/output.info
    DEPENDS ccov-preprocessing)
  add_custom_target(
    lcov-remove
    COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/coverage.info
    COMMAND
      lcov --remove
           ${CMAKE_CURRENT_BINARY_DIR}/output.info
           '/usr/include/*'
           '${CMAKE_CURRENT_BINARY_DIR}/_deps/*'
           -o ${CMAKE_CURRENT_BINARY_DIR}/coverage.info
    DEPENDS lcov-capture)
  add_custom_target(
    lcov-genhtml
    COMMAND rm -rf ${CMAKE_CURRENT_BINARY_DIR}/html
    COMMAND
      genhtml ${CMAKE_CURRENT_BINARY_DIR}/coverage.info
      -o ${CMAKE_CURRENT_BINARY_DIR}/html
    DEPENDS lcov-remove)
  add_custom_target(coverage DEPENDS lcov-genhtml)
else()
  message(FATAL_ERROR "Code coverage requires Clang or GCC")
endif()
