//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//
#include <doctest/doctest.h>
#include <iostream>
#include "libisp.hh"
#include "repl.hh"

namespace lisp
{

TEST_CASE("Interactive tests")
{
  lisp l;
  current c(l);

  std::ostringstream cout;
  auto out = std::make_unique<file_t>(cout);
  l.primout(std::move(out));

  std::ostringstream cerr;
  auto err = std::make_unique<file_t>(cerr);
  l.primerr(std::move(err));

  SUBCASE("Simple repl")
  {
    std::string is = R"((print "hello"))";
    l.primin(std::move(std::make_unique<file_t>(is)));
    repl repl(l);
    repl(NIL);
    std::string expected = R"(> "hello"
"hello"
> )";
    CHECK(cout.str() == expected);
  }
  
  SUBCASE("Break repl (reset)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(reset)
)";
    l.primin(std::move(std::make_unique<file_t>(is)));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    CHECK_THROWS(repl(NIL));
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
  }

  SUBCASE("Break repl (bt)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(bt)
(reset)
)";
    l.primin(std::move(std::make_unique<file_t>(is)));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    CHECK_THROWS(repl(NIL));
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
(xyzzy)
((lambda nil &))
)";
    std::string expected_out = R"(> : : )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
  }
}

}
