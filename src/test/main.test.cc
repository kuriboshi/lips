//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
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
