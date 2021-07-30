//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
namespace logic
{
void init();

LISPT p_and(lisp&, LISPT);
LISPT p_or(lisp&, LISPT);
LISPT p_not(lisp&, LISPT);
LISPT xif(lisp&, LISPT, LISPT, LISPT);
} // namespace logic

inline LISPT p_and(lisp& l, LISPT x) { return logic::p_and(l, x); }
inline LISPT p_and(LISPT x) { return logic::p_and(lisp::current(), x); }
inline LISPT p_or(lisp& l, LISPT x) { return logic::p_or(l, x); }
inline LISPT p_or(LISPT x) { return logic::p_or(lisp::current(), x); }
inline LISPT p_not(lisp& l, LISPT x) { return logic::p_not(l, x); }
inline LISPT p_not(LISPT x) { return logic::p_not(lisp::current(), x); }
inline LISPT xif(lisp& l, LISPT pred, LISPT true_expr, LISPT false_expr)
{
  return logic::xif(l, pred, true_expr, false_expr);
}
inline LISPT xif(LISPT pred, LISPT true_expr, LISPT false_expr)
{
  return logic::xif(lisp::current(), pred, true_expr, false_expr);
}

} // namespace lisp
