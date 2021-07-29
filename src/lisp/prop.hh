//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
class prop
{
public:
  static void init();

  static LISPT setplist(lisp&, LISPT, LISPT);
  static LISPT getplist(lisp&, LISPT);
  static LISPT putprop(lisp&, LISPT, LISPT, LISPT);
  static LISPT getprop(lisp&, LISPT, LISPT);
  static LISPT remprop(lisp&, LISPT, LISPT);
};

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
