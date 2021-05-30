#
# Copyright 2020 Krister Joas
#
add_executable(lips)
target_sources(lips PRIVATE
  exec.cc
  glob.cc
  main.cc
  os.cc
  term.cc
  top.cc
)
target_sources(lips PRIVATE
  exec.hh
  glob.hh
  main.hh
  os.hh
  term.hh
  top.hh
)
target_link_libraries(lips PRIVATE lisp)
target_link_libraries(lips PRIVATE doctest::doctest)
target_compile_options(lips PRIVATE -Wall -Werror)

find_package(Curses)

if(CURSES_FOUND)
  target_compile_definitions(lips PRIVATE TERMCAP)
  target_link_libraries(lips PRIVATE ${CURSES_LIBRARIES})
endif()
