file(
  WRITE "${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt.in"
  [[
cmake_minimum_required(VERSION 3.18.0)

project(doctest.download NONE)

include(ExternalProject)

ExternalProject_Add(doctest.external
  URL https://github.com/doctest/doctest/archive/v2.4.8.tar.gz
  URL_HASH SHA256=f52763630aa17bd9772b54e14b6cdd632c87adf0169455a86a49bd94abf2cd83
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/src
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/build
  INSTALL_DIR ${PROJECT_BINARY_DIR}/install
  CONFIGURE_COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}"
                                       $<$<PLATFORM_ID:Darwin>:-DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}>
                                       -D CMAKE_BUILD_TYPE=Release
                                       -D DOCTEST_WITH_TESTS=OFF
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
