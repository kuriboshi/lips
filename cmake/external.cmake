#
# Copyright 2020 Krister Joas
#
include(FetchContent)

function(make_available _NAME)
  fetchcontent_getproperties(${_NAME})
  string(TOLOWER ${_NAME} _name)
  if(NOT ${_name}_POPULATED)
    fetchcontent_populate(${_NAME})
    if(EXISTS "${${_name}_SOURCE_DIR}/CMakeLists.txt")
      add_subdirectory(${${_name}_SOURCE_DIR} ${${_name}_BINARY_DIR} EXCLUDE_FROM_ALL)
    else()
      # If there is no CMakeLists.txt we assume a header only library
      add_library(${_name} INTERFACE IMPORTED)
      target_include_directories(${_name} INTERFACE "${${_name}_SOURCE_DIR}/include")
    endif()
  endif()
  # Force the SOURCE_DIR variable into parent scope
  set(${_name}_SOURCE_DIR "${${_name}_SOURCE_DIR}" PARENT_SCOPE)
endfunction()

fetchcontent_declare(doctest
  URL https://github.com/onqtam/doctest/archive/2.3.7.tar.gz)
make_available(doctest)
