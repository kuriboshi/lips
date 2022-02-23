#
# Copyright 2020 Krister Joas
#
add_executable(lips)
target_sources(
  lips
  PRIVATE env.cc
          exec.cc
          glob.cc
          main.cc
          os.cc
          term.cc
          top.cc)
target_sources(
  lips
  PRIVATE env.hh
          exec.hh
          glob.hh
          main.hh
          os.hh
          term.hh
          top.hh)
target_link_libraries(lips PRIVATE lisp)
target_link_libraries(lips PRIVATE doctest::doctest)
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION
                                            VERSION_LESS 9.0)
  target_link_libraries(lips PRIVATE stdc++fs)
endif()
target_compile_options(lips PRIVATE -Wall -Werror)
target_compile_definitions(lips PRIVATE "LIPSRC=\"${CMAKE_INSTALL_PREFIX}/lib/lipsrc\"")

find_package(Curses REQUIRED)
target_link_libraries(lips PRIVATE ${CURSES_LIBRARIES})
