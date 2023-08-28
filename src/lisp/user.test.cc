//
// Lips, lisp shell.
// Copyright 2021-2023 Krister Joas
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

#include <catch2/catch_test_macros.hpp>

#include "alloc.hh"
#include "low.hh"
#include "pred.hh"
#include "prim.hh"
#include "prop.hh"
#include "user.hh"

namespace lisp
{

TEST_CASE("user: User defined functions")
{
  SECTION("defineq")
  {
    auto f0 = mklist(mklist(mkatom("f0"), lambda(mklist(mkatom("a")), mklist(mkatom("a")))),
      mklist(mkatom("f1"), lambda(mklist(mkatom("b")), mklist(mkatom("b")))));
    auto r0 = defineq(f0);
    REQUIRE(type_of(r0) == object::type::Cons);
    CHECK(equal(r0, mklist(mkatom("f0"), mkatom("f1"))));
    auto r1 = defineq(f0);
    REQUIRE(type_of(r1) == object::type::Cons);
    CHECK(equal(r1, mklist(mkatom("f0"), mkatom("f1"))));
    auto r2 = defineq(nil);
    CHECK(is_nil(r2));
  }

  SECTION("getrep")
  {
    auto nil0 = getrep(nil);
    CHECK(is_nil(nil0));
    auto f0 = lambda("(a)"_l, "((cons a nil))"_l);
    auto rep0 = getrep(f0);
    std::string s("(lambda (a) (cons a nil))\n");
    auto expected = lispread(s);
    CHECK(!is_nil(equal(rep0, expected)));
    auto rep1 = getrep(f0);
    CHECK(!is_nil(equal(rep0, rep1)));
    auto r0 = apply(f0, mklist(0_l));
    CHECK(!is_nil(equal(r0, cons(0_l, nil))));
    auto f2 = nlambda("a"_a, "(a)"_l);
    auto rep2 = getrep(f2);
    CHECK(!is_nil(equal(rep2, "(nlambda a a)"_l)));
    auto f3 = lambda("(a b . c)"_l, "((cons a b))"_l);
    auto rep3 = getrep(f3);
    CHECK(!is_nil(equal(rep3, "(lambda (a b . c) (cons a b))"_l)));
  }

  SECTION("Verbose flag")
  {
    CHECK(is_nil(vm::verbose()));
    eval("(setq verbose t)");
    CHECK(is_T(vm::verbose()));
  }

  SECTION("Redefine function")
  {
    remprop(mkatom("f"), C_OLDDEF);
    set(mkatom("f"), C_UNBOUND);
    auto f0 = define(mkatom("f"), lambda(mklist(mkatom("a")), mklist(mkatom("a"))));
    auto redef0 = getprop(mkatom("f"), mkatom("olddef"));
    CHECK(is_nil(redef0));
    std::ostringstream cout;
    auto old = vm::primout(ref_file_t::create(cout));
    eval("(setq verbose t)");
    auto f1 = define(mkatom("f"), lambda(mklist(mkatom("b")), mklist(mkatom("b"))));
    auto redef1 = getprop(mkatom("f"), mkatom("olddef"));
    CHECK(!is_nil(redef1));
    CHECK(cout.str() == "(f redefined)\n");
    vm::primout(old);
  }
}

} // namespace lisp
