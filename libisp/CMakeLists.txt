#
# Copyright 2020 Krister Joas
#
add_library(libisp)
target_sources(libisp PRIVATE
  alloc.cc
  #arith.cc
  eval.cc
  file.cc
  lisp.cc
  #logic.cc
  low.cc
  #map.cc
  misc.cc
  #os.cc
  #pred.cc
  #prim.cc
  #prop.cc
  #read.cc
  #string.cc
  #unix.cc
  #user.cc
  version.cc
)
target_sources(libisp PRIVATE
  alloc.hh
  #arith.hh
  #config.hh
  #constants.hh
  #defs.hh
  #error.hh
  eval.hh
  #except.hh
  file.hh
  #iter.hh
  #libisp.hh
  lisp.hh
  #logic.hh
  low.hh
  #map.hh
  #misc.hh
  #os.hh
  #pred.hh
  #prim.hh
  #prop.hh
  #read.hh
  #string.hh
  #user.hh
  #unix.hh
  version.hh
)
target_include_directories(libisp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_options(libisp PUBLIC -Wall -Werror)
target_link_libraries(libisp doctest)
add_executable(testlibisp test.cc)
target_link_libraries(testlibisp libisp)
