//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
#include <sstream>
#include <string>
#include <catch2/catch.hpp>
#include <lisp/main.hh>

namespace lisp
{

TEST_CASE("incomplete input")
{
  // An incomplete input expression is treated as ending with a super
  // parenthesis so there should be no error message in this case.
  lisp l;
  current c(l);
  std::ostringstream cout;
  auto out = ref_file_t::create(cout);
  l.primout(std::move(out));

  l.primin(ref_file_t::create(R"((print "hello")"));
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
  auto out = ref_file_t::create(cout);
  l.primout(std::move(out));

  {
    l.primin(ref_file_t::create(R"((exit))"));
    std::ostringstream os;
    CHECK(run(l, os) == 0);
  }
  {
    l.primin(ref_file_t::create(R"((exit 99))"));
    std::ostringstream os;
    CHECK(run(l, os) == 99);
  }
}

}
