//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//

#include <doctest/doctest.h>
#include <sstream>
#include <string>
#include "main.hh"

namespace lisp
{

TEST_CASE("run")
{
  lisp l;
  current c(l);
  std::ostringstream cout;
  auto out = std::make_unique<file_t>(cout);
  l.primout(std::move(out));

  std::string is = R"((print "hello")";
  l.primin(std::move(std::make_unique<file_t>(is)));
  std::ostringstream os;
  run(l, os);
  CHECK(os.str() == "Unexpected end of file\n");
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
