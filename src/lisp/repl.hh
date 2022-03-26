//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
//

#ifndef LISP_REPL_HH
#define LISP_REPL_HH

#include "lisp.hh"
#include "alloc.hh"
#include "io.hh"
#include "eval.hh"

namespace lisp
{

class repl
{
public:
  repl(lisp& lisp) : l(lisp)
  {
    _prompt = "> "_s;
    _break_prompt = ": "_s;
    lisp.e().interactive(true);
  }
  ~repl() = default;

  class level
  {
  public:
    level(repl& repl) : _repl(repl) { ++_repl._level; }
    ~level() { --_repl._level; }
  private:
    repl& _repl;
  };

  LISPT operator()(LISPT exp)
  {
    level begin(*this);
    if(_level == 1)
    {
      while(true)
      {
        prin0(l, _prompt, l.primout());
        auto expr = lispread(l, l.primin());
        if(expr == C_EMPTY)
          break;
        print(l, eval(l, expr), l.primout());
      }
      return NIL;
    }
    while(true)
    {
      prin0(l, _break_prompt, l.primout());
      auto com = lispread(l, l.primin());
      if(com == C_EMPTY)
        return C_EOF;
      /* OK, EVAL, ^, ... */
      if(type_of(com) != type::CONS)
        continue;
      else if(EQ(com->car(), C_GO))
        return print(l, eval(l, exp), false);
      else if(EQ(com->car(), C_RESET))
      {
        l.e().unwind();
        throw lisp_reset();
      }
      else if(EQ(com->car(), C_BT))
      {
        l.e().bt();
        continue;
      }
      else if(EQ(com->car(), C_RETURN))
        return is_NIL(com->cdr()) ? NIL : com->cdr()->car();
    }
  }

private:
  lisp& l;
  int _level = 0;
  LISPT _prompt;
  LISPT _break_prompt;
};

}

#endif
