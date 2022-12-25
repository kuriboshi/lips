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

#include "alloc.hh"
#include "eval.hh"
#include "repl.hh"

namespace lisp
{

repl::repl(context& ctx)
  : _ctx(ctx)
{
  _prompt = "> "_s;
  _break_prompt = ": "_s;
  ctx.vm().interactive(true);
}

lisp_t repl::operator()(lisp_t exp)
{
  level begin(*this);
  if(_level == 1)
  {
    while(true)
    {
      prin0(_prompt);
      auto expr = lispread(_ctx.primin());
      if(expr == C_EOF)
        break;
      auto result = eval(expr);
      if(result != C_ERROR)
        print(result);
    }
    return nil;
  }
  while(true)
  {
    prin0(_break_prompt);
    auto com = lispread(_ctx.primin());
    if(com == C_EOF)
      return C_EOF;
    /* OK, EVAL, ^, ... */
    if(type_of(com) != type::Cons)
    {
      prin0("(go) continue"_s);
      terpri();
      prin0("(reset) back to top loop"_s);
      terpri();
      prin0("(bt) print backtrace"_s);
      terpri();
      prin0("(return exp) return expression"_s);
      terpri();
      continue;
    }
    if(com->car() == C_GO)
      return print(eval(exp), false);
    if(com->car() == C_RESET)
    {
      _ctx.vm().unwind();
      throw lisp_reset();
    }
    if(com->car() == C_BT)
    {
      _ctx.vm().bt();
      continue;
    }
    if(com->car() == C_RETURN)
      return is_nil(com->cdr()) ? nil : com->cdr()->car();
  }
}

} // namespace lisp
