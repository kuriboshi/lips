//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class prop: public base
{
public:
  prop();
  prop(lisp&);
  ~prop() = default;
  static void init();

  LISPT setplist(LISPT, LISPT);
  LISPT getplist(LISPT);
  LISPT putprop(LISPT, LISPT, LISPT);
  LISPT getprop(LISPT, LISPT);
  LISPT remprop(LISPT, LISPT);
};

inline LISPT setplist(lisp& l, LISPT a, LISPT b) { return prop(l).setplist(a, b); }
inline LISPT setplist(LISPT a, LISPT b) { return prop().setplist(a, b); }
inline LISPT getplist(lisp& l, LISPT a) { return prop(l).getplist(a); }
inline LISPT getplist(LISPT a) { return prop().getplist(a); }
inline LISPT putprop(lisp& l, LISPT a, LISPT b, LISPT c) { return prop(l).putprop(a, b, c); }
inline LISPT putprop(LISPT a, LISPT b, LISPT c) { return prop().putprop(a, b, c); }
inline LISPT getprop(lisp& l, LISPT a, LISPT b) { return prop(l).getprop(a, b); }
inline LISPT getprop(LISPT a, LISPT b) { return prop().getprop(a, b); }
inline LISPT remprop(lisp& l, LISPT a, LISPT b) { return prop(l).remprop(a, b); }
inline LISPT remprop(LISPT a, LISPT b) { return prop().remprop(a, b); }

} // namespace lisp
