//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <catch2/catch.hpp>
#include <iostream>
#include <lisp/liblisp.hh>
#include <lisp/repl.hh>

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

  SECTION("Simple repl")
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
  
  SECTION("Example interaction")
  {
    std::string is = R"(
()
(setq a 100)
a
)";
    l.primin(std::move(std::make_unique<file_t>(is)));
    repl repl(l);
    repl(NIL);
    std::string expected = R"(> nil
> 100
> 100
> )";
    CHECK(cout.str() == expected);
  }

  SECTION("Break repl (reset)")
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

  SECTION("Break repl (bt)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(bt)
(reset)
)";
    l.primin(std::move(std::make_unique<file_t>(is)));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    CHECK_THROWS(repl(NIL));
    // Backtrace sets the print level to 2 which is why the '&' is printed
    // representing a deeper level.
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
(xyzzy)
((lambda nil &))
)";
    std::string expected_out = R"(> : : )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
  }

  SECTION("Break repl (return)")
  {
    std::string is = R"(((lambda () (xyzzy)))
(return "hello")
)";
    l.primin(std::move(std::make_unique<file_t>(is)));
    repl repl(l);
    l.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
    repl(NIL);
    std::string expected_err = R"(Undefined function xyzzy
(xyzzy broken)
)";
    std::string expected_out = R"(> : "hello"
> )";
    CHECK(cout.str() == expected_out);
    CHECK(cerr.str() == expected_err);
  }
}

}
