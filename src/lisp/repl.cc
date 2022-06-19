//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#include "repl.hh"

namespace lisp
{

repl::repl(lisp& lisp) : l(lisp)
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
