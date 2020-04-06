#
# Copyright 2020 Krister Joas
#
add_executable(lips)
target_sources(lips PRIVATE
  exec.c
  glob.c
  lips.h
  main.c
  term.c
  top.c
)
target_link_libraries(lips libisp termcap)
