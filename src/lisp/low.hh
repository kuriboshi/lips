//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LISP_LOW_HH
#define LISP_LOW_HH

#include "lisp.hh"

namespace lisp::low
{
void init();

LISPT cond(lisp&, LISPT);
LISPT prog1(lisp&, LISPT, LISPT);
LISPT progn(lisp&, LISPT);
LISPT set(lisp&, LISPT, LISPT);
LISPT setq(lisp&, LISPT, LISPT);
LISPT xwhile(lisp&, LISPT, LISPT);
} // namespace lisp::low

namespace lisp
{
inline LISPT cond(lisp& l, LISPT a) { return low::cond(l, a); }
inline LISPT cond(LISPT a) { return low::cond(lisp::current(), a); }
inline LISPT prog1(lisp& l, LISPT a, LISPT b) { return low::prog1(l, a, b); }
inline LISPT prog1(LISPT a, LISPT b) { return low::prog1(lisp::current(), a, b); }
inline LISPT progn(lisp& l, LISPT a) { return low::progn(l, a); }
inline LISPT progn(LISPT a) { return low::progn(lisp::current(), a); }
inline LISPT set(lisp& l, LISPT a, LISPT b) { return low::set(l, a, b); }
inline LISPT set(LISPT a, LISPT b) { return low::set(lisp::current(), a, b); }
inline LISPT setq(lisp& l, LISPT a, LISPT b) { return low::setq(l, a, b); }
inline LISPT setq(LISPT a, LISPT b) { return low::setq(lisp::current(), a, b); }
inline LISPT setqq(lisp& l, LISPT a, LISPT b) { return low::set(l, a, b); }
inline LISPT setqq(LISPT a, LISPT b) { return low::set(lisp::current(), a, b); }
inline LISPT xwhile(lisp& l, LISPT a, LISPT b) { return low::xwhile(l, a, b); }
inline LISPT xwhile(LISPT a, LISPT b) { return low::xwhile(lisp::current(), a, b); }
} // namespace lisp

#endif
