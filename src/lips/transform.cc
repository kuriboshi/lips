//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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

#include "transform.hh"

#include <lisp/lisp.hh>
#include <catch2/catch_test_macros.hpp>

using lisp::lisp_t;
using lisp::nil;
using lisp::object;
using namespace lisp::literals;

lisp_t put_end(lisp_t list, lisp_t obj, bool conc)
{
  if(is_nil(list))
  {
    if(conc)
      return obj;
    return cons(obj, nil);
  }
  lisp_t t;
  for(t = list; type_of(t->cdr()) == object::type::Cons; t = t->cdr())
    ;
  if(conc)
    rplacd(t, obj);
  else
    rplacd(t, cons(obj, nil));
  return list;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
lisp_t transform(lisp_t list)
{
  lisp_t tl = nil;
  lisp_t res = nil;
  bool conc = false;
  for(auto ll = list; type_of(ll) == object::type::Cons; ll = ll->cdr())
  {
    if(type_of(ll->car()) == object::type::Cons)
      tl = put_end(tl, transform(ll->car()), conc);
    else if(ll->car() == C_BAR)
    {
      if(is_nil(res))
        res = cons(C_PIPE, cons(tl, nil));
      else
        res = cons(C_PIPE, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = false;
    }
    else if(ll->car() == C_SEMI)
    {
      // Semicolon is considered a comment character. If progn transformation
      // is to be effective ';' cannot be a comment character.
      if(is_nil(res))
        res = cons(C_PROGN, cons(tl, nil));
      else
        res = cons(C_PROGN, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = false;
    }
    else if(ll->car() == C_GT)
    {
      if(is_nil(res))
        res = cons(C_REDIR_TO, cons(tl, nil));
      else
        res = cons(C_REDIR_TO, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else if(ll->car() == C_GGT)
    {
      if(is_nil(res))
        res = cons(C_REDIR_APPEND, cons(tl, nil));
      else
        res = cons(C_REDIR_APPEND, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else if(ll->car() == C_LT)
    {
      if(is_nil(res))
        res = cons(C_REDIR_FROM, cons(tl, nil));
      else
        res = cons(C_REDIR_FROM, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else if(ll->car() == C_AMPER)
    {
      if(is_nil(res))
        res = cons(C_BACK, cons(tl, nil));
      else
        res = cons(C_BACK, cons(put_end(res, tl, conc), nil));
      tl = nil;
      conc = true;
    }
    else
      tl = put_end(tl, ll->car(), false);
  }
  if(is_nil(res))
    return tl;
  if(!is_nil(tl))
    res = put_end(res, tl, conc);
  return res;
}

TEST_CASE("transform")
{
  SECTION("pipe |")
  {
    auto result = transform("(ls | wc -l)"_l);
    CHECK(equal(result, "(pipe-cmd (ls) (wc -l))"_l));
  }

  SECTION("redirect >")
  {
    auto result = transform("(ls > foo)"_l);
    CHECK(equal(result, "(redir-to (ls) foo)"_l));
  }

  SECTION("redirect >")
  {
    auto result = transform("(ls > foo 1)"_l);
    CHECK(equal(result, "(redir-to (ls) foo 1)"_l));
  }

  SECTION("redirect >>")
  {
    auto result = transform("(ls >> foo)"_l);
    CHECK(equal(result, "(append-to (ls) foo)"_l));
  }

  SECTION("redirect <")
  {
    auto result = transform("(ls < foo)"_l);
    CHECK(equal(result, "(redir-from (ls) foo)"_l));
  }

  SECTION("background &")
  {
    auto result = transform("(ls &)"_l);
    CHECK(equal(result, "(back (ls))"_l));
  }
}
