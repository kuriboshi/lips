//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
class debug
{
public:
  static LISPT evaltrace(lisp&, LISPT);

  static void init();
};

inline LISPT evaltrace(lisp& l, LISPT x) { return debug::evaltrace(l, x); }
inline LISPT evaltrace(LISPT x) { return debug::evaltrace(lisp::current(), x); }

} // namespace lisp
