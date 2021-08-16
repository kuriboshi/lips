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
if(ENABLE_SELECT)
  target_compile_definitions(lips PRIVATE SELECT)
endif()

if(ENABLE_JOB_CONTROL)
  target_compile_definitions(lips PRIVATE JOB_CONTROL)
endif()

if(ENABLE_FANCY_SIGNALS)
  target_compile_definitions(lips PRIVATE FANCY_SIGNALS)
endif()

if(ENABLE_LIPSRC)
  target_compile_definitions(
    lips PRIVATE "LIPSRC=\"${CMAKE_INSTALL_PREFIX}/lib/lipsrc\"")
endif()

find_package(Curses)

if(CURSES_FOUND)
  target_compile_definitions(lips PRIVATE HAVE_CURSES)
  target_link_libraries(lips PRIVATE ${CURSES_LIBRARIES})
endif()
