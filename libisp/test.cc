#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include <doctest/doctest.h>

#include "lisp.hh"

TEST_CASE("Create lisp object")
{
  lisp::lisp lisp;
}
