file(
  WRITE "${CMAKE_CURRENT_BINARY_DIR}/download/CMakeLists.txt.in"
  [[
cmake_minimum_required(VERSION 3.18.0)

project(catch2.download NONE)

include(ExternalProject)

ExternalProject_Add(catch2.external
  URL https://github.com/catchorg/Catch2/archive/v2.13.8.tar.gz
  URL_HASH SHA256=b9b592bd743c09f13ee4bf35fc30eeee2748963184f6bea836b146e6cc2a585a
  SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/src
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/build
  INSTALL_DIR ${PROJECT_BINARY_DIR}/install
  CONFIGURE_COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}"
                                       $<$<PLATFORM_ID:Darwin>:-DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}>
                                       -D CMAKE_BUILD_TYPE=Release
                                       -D CATCH_BUILD_TESTING=OFF
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
