//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_AND = "and"; // and
inline constexpr auto PN_OR = "or";   // or
inline constexpr auto PN_NOT = "not"; // not
inline constexpr auto PN_IF = "if";   // if a then b else c

class logic: public base
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
inline LISPT p_and(LISPT x) { return logic(lisp::current()).p_and(x); }
inline LISPT p_or(lisp& l, LISPT x) { return logic(l).p_or(x); }
inline LISPT p_or(LISPT x) { return logic(lisp::current()).p_or(x); }
inline LISPT p_not(lisp& l, LISPT x) { return logic(l).p_not(x); }
inline LISPT p_not(LISPT x) { return logic(lisp::current()).p_not(x); }
inline LISPT xif(lisp& l, LISPT pred, LISPT true_expr, LISPT false_expr)
{
  return logic(l).xif(pred, true_expr, false_expr);
}
inline LISPT xif(LISPT pred, LISPT true_expr, LISPT false_expr)
{
  return xif(lisp::current(), pred, true_expr, false_expr);
}

} // namespace lisp