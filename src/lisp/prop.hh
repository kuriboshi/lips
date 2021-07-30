//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
namespace prop
{
void init();

LISPT setplist(lisp&, LISPT, LISPT);
LISPT getplist(lisp&, LISPT);
LISPT putprop(lisp&, LISPT, LISPT, LISPT);
LISPT getprop(lisp&, LISPT, LISPT);
LISPT remprop(lisp&, LISPT, LISPT);
} // namespace prop

inline LISPT setplist(lisp& l, LISPT a, LISPT b) { return prop::setplist(l, a, b); }
inline LISPT setplist(LISPT a, LISPT b) { return prop::setplist(lisp::current(), a, b); }
inline LISPT getplist(lisp& l, LISPT a) { return prop::getplist(l, a); }
inline LISPT getplist(LISPT a) { return prop::getplist(lisp::current(), a); }
inline LISPT putprop(lisp& l, LISPT a, LISPT b, LISPT c) { return prop::putprop(l, a, b, c); }
inline LISPT putprop(LISPT a, LISPT b, LISPT c) { return prop::putprop(lisp::current(), a, b, c); }
inline LISPT getprop(lisp& l, LISPT a, LISPT b) { return prop::getprop(l, a, b); }
inline LISPT getprop(LISPT a, LISPT b) { return prop::getprop(lisp::current(), a, b); }
inline LISPT remprop(lisp& l, LISPT a, LISPT b) { return prop::remprop(l, a, b); }
inline LISPT remprop(LISPT a, LISPT b) { return prop::remprop(lisp::current(), a, b); }

} // namespace lisp
