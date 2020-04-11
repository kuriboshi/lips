#
# Copyright 2020 Krister Joas
#
add_executable(lips)
target_sources(lips PRIVATE
  exec.c
  glob.c
  main.c
  term.c
  top.c
)
target_sources(lips PRIVATE
  exec.h
  glob.h
  main.h
  top.h
)
target_link_libraries(lips PRIVATE libisp)

find_package(Curses)

if(CURSES_FOUND)
  target_compile_definitions(lips PRIVATE TERMCAP)
  target_link_libraries(lips PRIVATE ${CURSES_LIBRARIES})
endif()
