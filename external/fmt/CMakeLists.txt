file(
  WRITE "${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt.in"
  [[
cmake_minimum_required(VERSION 3.18.0)

project(fmt.download NONE)

include(ExternalProject)

ExternalProject_Add(fmt.external
  URL https://github.com/fmtlib/fmt/archive/8.1.1.tar.gz
  URL_HASH SHA256=3d794d3cf67633b34b2771eb9f073bde87e846e0d395d254df7b211ef1ec7346
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/src
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/build
  INSTALL_DIR ${PROJECT_BINARY_DIR}/install
  CONFIGURE_COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}"
                                       $<$<PLATFORM_ID:Darwin>:-DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}>
                                       -D CMAKE_BUILD_TYPE=Release
                                       -D FMT_TEST=OFF
                                       -D CMAKE_INSTALL_PREFIX=<INSTALL_DIR>
                                       <SOURCE_DIR>
  BUILD_COMMAND "${CMAKE_COMMAND}" --build . --config Release
  INSTALL_COMMAND "${CMAKE_COMMAND}" --install . --config Release
  TEST_COMMAND ""
  USES_TERMINAL_BUILD TRUE
  )
]])
configure_file("${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt.in"
               "${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt")

execute_process(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
                WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/download")
execute_process(COMMAND "${CMAKE_COMMAND}" --build .
                WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/download")
