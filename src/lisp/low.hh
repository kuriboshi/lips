//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
namespace low
{
void init();

LISPT set(lisp&, LISPT, LISPT);
LISPT setq(lisp&, LISPT, LISPT);
LISPT progn(lisp&, LISPT);
LISPT cond(lisp&, LISPT);
LISPT xwhile(lisp&, LISPT, LISPT);
LISPT prog1(lisp&, LISPT, LISPT);
} // namespace low

inline LISPT set(lisp& l, LISPT a, LISPT b) { return low::set(l, a, b); }
inline LISPT set(LISPT a, LISPT b) { return low::set(lisp::current(), a, b); }
inline LISPT setq(lisp& l, LISPT a, LISPT b) { return low::setq(l, a, b); }
inline LISPT setq(LISPT a, LISPT b) { return low::setq(lisp::current(), a, b); }
inline LISPT setqq(lisp& l, LISPT a, LISPT b) { return low::set(l, a, b); }
inline LISPT setqq(LISPT a, LISPT b) { return low::set(lisp::current(), a, b); }
inline LISPT cond(lisp& l, LISPT a) { return low::cond(l, a); }
inline LISPT cond(LISPT a) { return low::cond(lisp::current(), a); }
inline LISPT xwhile(lisp& l, LISPT a, LISPT b) { return low::xwhile(l, a, b); }
inline LISPT xwhile(LISPT a, LISPT b) { return low::xwhile(lisp::current(), a, b); }
inline LISPT progn(lisp& l, LISPT a) { return low::progn(l, a); }
inline LISPT progn(LISPT a) { return low::progn(lisp::current(), a); }
inline LISPT prog1(lisp& l, LISPT a, LISPT b) { return low::prog1(l, a, b); }
inline LISPT prog1(LISPT a, LISPT b) { return low::prog1(lisp::current(), a, b); }

} // namespace lisp
