//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
/* variables */
extern LISPT verboseflg;

class low: public base
{
public:
  low(lisp&);
  ~low() = default;
  LISPT set(LISPT, LISPT);
  LISPT setq(LISPT, LISPT);
  LISPT progn(LISPT);
  LISPT cond(LISPT);
  LISPT xwhile(LISPT, LISPT);
  LISPT prog1(LISPT, LISPT);
  LISPT prog2(LISPT, LISPT, LISPT);
  LISPT topofstack();
  LISPT envget(LISPT, LISPT);
};

inline LISPT set(lisp& l, LISPT a, LISPT b) { return low(l).set(a, b); }
inline LISPT setq(lisp& l, LISPT a, LISPT b) { return low(l).setq(a, b); }
inline LISPT setqq(lisp& l, LISPT a, LISPT b) { return low(l).set(a, b); }
inline LISPT cond(lisp& l, LISPT a) { return low(l).cond(a); }
inline LISPT xwhile(lisp& l, LISPT a, LISPT b) { return low(l).xwhile(a, b); }
inline LISPT progn(lisp& l, LISPT a) { return low(l).progn(a); }
inline LISPT prog1(lisp& l, LISPT a, LISPT b) { return low(l).prog1(a, b); }
inline LISPT prog2(lisp& l, LISPT a, LISPT b, LISPT c) { return low(l).prog2(a, b, c); }
#if 0
inline LISPT topofstack(lisp& l) { return low(l).topofstack(); }
#endif
inline LISPT envget(lisp& l, LISPT a, LISPT b) { return low(l).envget(a, b); }

} // namespace lisp
