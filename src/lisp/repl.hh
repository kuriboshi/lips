//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LISP_REPL_HH
#define LISP_REPL_HH

#include "lisp.hh"

namespace lisp
{

class repl
{
public:
  repl(lisp&);
  ~repl() = default;

  class level
  {
  public:
    level(repl& repl) : _repl(repl) { ++_repl._level; }
    ~level() { --_repl._level; }
  private:
    repl& _repl;
  };

  LISPT operator()(LISPT);

private:
  lisp& l;
  int _level = 0;
  LISPT _prompt;
  LISPT _break_prompt;
};

} // namespace lisp

#endif
