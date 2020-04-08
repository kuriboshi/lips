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
target_link_libraries(lips libisp termcap)
