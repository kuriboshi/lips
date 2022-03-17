//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//

#include <catch2/catch.hpp>
#include <sstream>
#include <string>
#include "main.hh"

namespace lisp
{

TEST_CASE("incomplete input")
{
  // An incomplete input expression is treated as ending with a super
  // parenthesis so there should be no error message in this case.
  lisp l;
  current c(l);
  std::ostringstream cout;
  auto out = std::make_unique<file_t>(cout);
  l.primout(std::move(out));

  std::string is = R"((print "hello")";
  l.primin(std::move(std::make_unique<file_t>(is)));
  std::ostringstream os;
  run(l, os);
  CHECK(os.str() == "");
  CHECK(cout.str() == R"(> "hello"
"hello"
> )");
}

TEST_CASE("exit")
{
  lisp l;
  current c(l);
  std::ostringstream cout;
  auto out = std::make_unique<file_t>(cout);
  l.primout(std::move(out));

  {
    std::string is = R"((exit))";
    l.primin(std::move(std::make_unique<file_t>(is)));
    std::ostringstream os;
    CHECK(run(l, os) == 0);
  }
  {
    std::string is = R"((exit 99))";
    l.primin(std::move(std::make_unique<file_t>(is)));
    std::ostringstream os;
    CHECK(run(l, os) == 99);
  }
}

}
