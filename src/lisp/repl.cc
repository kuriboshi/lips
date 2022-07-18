//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
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

#include "lisp/repl.hh"

namespace lisp
{

repl::repl(lisp& lisp)
  : l(lisp)
{
  _prompt = "> "_s;
  _break_prompt = ": "_s;
  lisp.e().interactive(true);
}

LISPT repl::operator()(LISPT exp)
{
  level begin(*this);
  if(_level == 1)
  {
    while(true)
    {
      prin0(l, _prompt);
      auto expr = lispread(l, l.primin());
      if(expr == C_EMPTY)
        break;
      print(l, eval(l, expr));
    }
    return NIL;
  }
  while(true)
  {
    prin0(l, _break_prompt);
    auto com = lispread(l, l.primin());
    if(com == C_EMPTY)
      return C_EOF;
    /* OK, EVAL, ^, ... */
    if(type_of(com) != type::CONS)
    {
      prin0(l, "(go) continue"_s);
      terpri(l);
      prin0(l, "(reset) back to top loop"_s);
      terpri(l);
      prin0(l, "(bt) print backtrace"_s);
      terpri(l);
      prin0(l, "(return exp) return expression"_s);
      terpri(l);
      continue;
    }
    if(com->car() == C_GO)
      return print(l, eval(l, exp), false);
    if(com->car() == C_RESET)
    {
      l.e().unwind();
      throw lisp_reset();
    }
    if(com->car() == C_BT)
    {
      l.e().bt();
      continue;
    }
    if(com->car() == C_RETURN)
      return is_NIL(com->cdr()) ? NIL : com->cdr()->car();
  }
}

} // namespace lisp
