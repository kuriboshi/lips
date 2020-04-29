//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

namespace lisp
{
class debug
{
public:
  debug(lisp&);
  ~debug() = default;
private:
  lisp& _lisp;
};

/* variables */
extern int trace;

/* functions */
extern LISPT perror(int, LISPT);
extern LISPT error(int, LISPT);
extern LISPT syserr(LISPT);
extern LISPT break0(LISPT);

} // namespace lisp
