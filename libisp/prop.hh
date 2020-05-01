//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class prop : public base
{
public:
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
inline LISPT getplist(lisp& l, LISPT a) { return prop(l).getplist(a); }
inline LISPT putprop(lisp& l, LISPT a, LISPT b, LISPT c) { return prop(l).putprop(a, b, c); }
inline LISPT getprop(lisp& l, LISPT a, LISPT b) { return prop(l).getprop(a, b); }
inline LISPT remprop(lisp& l, LISPT a, LISPT b) { return prop(l).remprop(a, b); }

} // namespace lisp
