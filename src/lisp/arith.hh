//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
class arith
{
public:
  static void init();

  /// @brief Calculate sum of all parameters.
  static LISPT plus(lisp&, LISPT);
  /// @brief Calculate integer sum of all paramerers.
  static LISPT iplus(lisp&, LISPT);
  /// @brief Calculate floating point sum of all paramerers.
  static LISPT fplus(lisp&, LISPT);
  static LISPT difference(lisp&, LISPT, LISPT);
  static LISPT idifference(lisp&, LISPT, LISPT);
  static LISPT fdifference(lisp&, LISPT, LISPT);
  static LISPT ltimes(lisp&, LISPT);
  static LISPT itimes(lisp&, LISPT);
  static LISPT ftimes(lisp&, LISPT);
  static LISPT divide(lisp&, LISPT, LISPT);
  static LISPT iquotient(lisp&, LISPT, LISPT);
  static LISPT iremainder(lisp&, LISPT, LISPT);
  static LISPT fdivide(lisp&, LISPT, LISPT);
  static LISPT minus(lisp&, LISPT);
  static LISPT iminus(lisp&, LISPT);
  static LISPT absval(lisp&, LISPT);
  static LISPT itof(lisp&, LISPT);
  static LISPT add1(lisp&, LISPT);
  static LISPT sub1(lisp&, LISPT);
  static LISPT greaterp(lisp&, LISPT, LISPT);
  static LISPT lessp(lisp&, LISPT, LISPT);
  static LISPT eqp(lisp&, LISPT, LISPT);
  static LISPT geq(lisp&, LISPT, LISPT);
  static LISPT leq(lisp&, LISPT, LISPT);
  static LISPT neqp(lisp&, LISPT, LISPT);
  static LISPT zerop(lisp&, LISPT);
  static LISPT minusp(lisp&, LISPT);
};

inline LISPT plus(lisp& l, LISPT a) { return arith::plus(l, a); }
inline LISPT plus(LISPT a) { return arith::plus(lisp::current(), a); }
inline LISPT iplus(lisp& l, LISPT a) { return arith::iplus(l, a); }
inline LISPT iplus(LISPT a) { return arith::iplus(lisp::current(), a); }
inline LISPT fplus(lisp& l, LISPT a) { return arith::fplus(l, a); }
inline LISPT fplus(LISPT a) { return arith::fplus(lisp::current(), a); }
inline LISPT difference(lisp& l, LISPT a, LISPT b) { return arith::difference(l, a, b); }
inline LISPT difference(LISPT a, LISPT b) { return arith::difference(lisp::current(), a, b); }
inline LISPT idifference(lisp& l, LISPT a, LISPT b) { return arith::idifference(l, a, b); }
inline LISPT idifference(LISPT a, LISPT b) { return arith::idifference(lisp::current(), a, b); }
inline LISPT fdifference(lisp& l, LISPT a, LISPT b) { return arith::fdifference(l, a, b); }
inline LISPT fdifference(LISPT a, LISPT b) { return arith::fdifference(lisp::current(), a, b); }
inline LISPT ltimes(lisp& l, LISPT a) { return arith::ltimes(l, a); }
inline LISPT ltimes(LISPT a) { return arith::ltimes(lisp::current(), a); }
inline LISPT itimes(lisp& l, LISPT a) { return arith::itimes(l, a); }
inline LISPT itimes(LISPT a) { return arith::itimes(lisp::current(), a); }
inline LISPT ftimes(lisp& l, LISPT a) { return arith::ftimes(l, a); }
inline LISPT ftimes(LISPT a) { return arith::ftimes(lisp::current(), a); }
inline LISPT divide(lisp& l, LISPT a, LISPT b) { return arith::divide(l, a, b); }
inline LISPT divide(LISPT a, LISPT b) { return arith::divide(lisp::current(), a, b); }
inline LISPT iquotient(lisp& l, LISPT a, LISPT b) { return arith::iquotient(l, a, b); }
inline LISPT iquotient(LISPT a, LISPT b) { return arith::iquotient(lisp::current(), a, b); }
inline LISPT iremainder(lisp& l, LISPT a, LISPT b) { return arith::iremainder(l, a, b); }
inline LISPT iremainder(LISPT a, LISPT b) { return arith::iremainder(lisp::current(), a, b); }
inline LISPT fdivide(lisp& l, LISPT a, LISPT b) { return arith::fdivide(l, a, b); }
inline LISPT fdivide(LISPT a, LISPT b) { return arith::fdivide(lisp::current(), a, b); }
inline LISPT minus(lisp& l, LISPT a) { return arith::minus(l, a); }
inline LISPT minus(LISPT a) { return arith::minus(lisp::current(), a); }
inline LISPT iminus(lisp& l, LISPT a) { return arith::iminus(l, a); }
inline LISPT iminus(LISPT a) { return arith::iminus(lisp::current(), a); }
inline LISPT absval(lisp& l, LISPT i) { return arith::absval(l, i); }
inline LISPT absval(LISPT i) { return arith::absval(lisp::current(), i); }
inline LISPT itof(lisp& l, LISPT i) { return arith::itof(l, i); }
inline LISPT itof(LISPT i) { return arith::itof(lisp::current(), i); }
inline LISPT add1(lisp& l, LISPT a) { return arith::add1(l, a); }
inline LISPT add1(LISPT a) { return arith::add1(lisp::current(), a); }
inline LISPT sub1(lisp& l, LISPT a) { return arith::sub1(l, a); }
inline LISPT sub1(LISPT a) { return arith::sub1(lisp::current(), a); }
inline LISPT greaterp(lisp& l, LISPT x, LISPT y) { return arith::greaterp(l, x, y); }
inline LISPT greaterp(LISPT x, LISPT y) { return arith::greaterp(lisp::current(), x, y); }
inline LISPT lessp(lisp& l, LISPT x, LISPT y) { return arith::lessp(l, x, y); }
inline LISPT lessp(LISPT x, LISPT y) { return arith::lessp(lisp::current(), x, y); }
inline LISPT eqp(lisp& l, LISPT x, LISPT y) { return arith::eqp(l, x, y); }
inline LISPT eqp(LISPT x, LISPT y) { return arith::eqp(lisp::current(), x, y); }
inline LISPT geq(lisp& l, LISPT x, LISPT y) { return arith::geq(l, x, y); }
inline LISPT geq(LISPT x, LISPT y) { return arith::geq(lisp::current(), x, y); }
inline LISPT leq(lisp& l, LISPT x, LISPT y) { return arith::leq(l, x, y); }
inline LISPT leq(LISPT x, LISPT y) { return arith::leq(lisp::current(), x, y); }
inline LISPT neqp(lisp& l, LISPT x, LISPT y) { return arith::neqp(l, x, y); }
inline LISPT neqp(LISPT x, LISPT y) { return arith::neqp(lisp::current(), x, y); }
inline LISPT zerop(lisp& l, LISPT x) { return arith::zerop(l, x); }
inline LISPT zerop(LISPT x) { return arith::zerop(lisp::current(), x); }
inline LISPT minusp(lisp& l, LISPT x) { return arith::minusp(l, x); }
inline LISPT minusp(LISPT x) { return arith::minusp(lisp::current(), x); }

} // namespace lisp
