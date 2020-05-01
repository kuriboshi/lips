//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class logic : public base
{
public:
  logic(lisp&);
  ~logic() = default;
  static void init();

  LISPT p_and(LISPT);
  LISPT p_or(LISPT);
  LISPT p_not(LISPT);
  LISPT xif(LISPT, LISPT, LISPT);
};

inline LISPT p_and(lisp& l, LISPT x) { return logic(l).p_and(x); }
inline LISPT p_or(lisp& l, LISPT x) { return logic(l).p_or(x); }
inline LISPT p_not(lisp& l, LISPT x) { return logic(l).p_not(x); }
inline LISPT xif(lisp& l, LISPT pred, LISPT true_expr, LISPT false_expr) { return logic(l).xif(pred, true_expr, false_expr); }

} // namespace lisp
