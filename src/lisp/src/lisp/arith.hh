//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class arith: public base
{
public:
  arith();
  arith(lisp&);
  static void init();

  /// @brief Calculate sum of all parameters.
  LISPT plus(LISPT);
  /// @brief Calculate integer sum of all paramerers.
  LISPT iplus(LISPT);
  /// @brief Calculate floating point sum of all paramerers.
  LISPT fplus(LISPT);
  LISPT difference(LISPT, LISPT);
  LISPT idifference(LISPT, LISPT);
  LISPT fdifference(LISPT, LISPT);
  LISPT ltimes(LISPT);
  LISPT itimes(LISPT);
  LISPT ftimes(LISPT);
  LISPT divide(LISPT, LISPT);
  LISPT iquotient(LISPT, LISPT);
  LISPT iremainder(LISPT, LISPT);
  LISPT fdivide(LISPT, LISPT);
  LISPT minus(LISPT);
  LISPT iminus(LISPT);
  LISPT absval(LISPT);
  LISPT itof(LISPT);
  LISPT add1(LISPT);
  LISPT sub1(LISPT);
  LISPT greaterp(LISPT, LISPT);
  LISPT lessp(LISPT, LISPT);
  LISPT eqp(LISPT, LISPT);
  LISPT geq(LISPT, LISPT);
  LISPT leq(LISPT, LISPT);
  LISPT neqp(LISPT, LISPT);
  LISPT zerop(LISPT);
  LISPT minusp(LISPT);
};

inline LISPT plus(lisp& l, LISPT a) { return arith(l).plus(a); }
inline LISPT plus(LISPT a) { return arith().plus(a); }
inline LISPT iplus(lisp& l, LISPT a) { return arith(l).iplus(a); }
inline LISPT iplus(LISPT a) { return arith().iplus(a); }
inline LISPT fplus(lisp& l, LISPT a) { return arith(l).fplus(a); }
inline LISPT fplus(LISPT a) { return arith().fplus(a); }
inline LISPT difference(lisp& l, LISPT a, LISPT b) { return arith(l).difference(a, b); }
inline LISPT difference(LISPT a, LISPT b) { return arith().difference(a, b); }
inline LISPT idifference(lisp& l, LISPT a, LISPT b) { return arith(l).idifference(a, b); }
inline LISPT idifference(LISPT a, LISPT b) { return arith().idifference(a, b); }
inline LISPT fdifference(lisp& l, LISPT a, LISPT b) { return arith(l).fdifference(a, b); }
inline LISPT fdifference(LISPT a, LISPT b) { return arith().fdifference(a, b); }
inline LISPT ltimes(lisp& l, LISPT a) { return arith(l).ltimes(a); }
inline LISPT ltimes(LISPT a) { return arith().ltimes(a); }
inline LISPT itimes(lisp& l, LISPT a) { return arith(l).itimes(a); }
inline LISPT itimes(LISPT a) { return arith().itimes(a); }
inline LISPT ftimes(lisp& l, LISPT a) { return arith(l).ftimes(a); }
inline LISPT ftimes(LISPT a) { return arith().ftimes(a); }
inline LISPT divide(lisp& l, LISPT a, LISPT b) { return arith(l).divide(a, b); }
inline LISPT divide(LISPT a, LISPT b) { return arith().divide(a, b); }
inline LISPT iquotient(lisp& l, LISPT a, LISPT b) { return arith(l).iquotient(a, b); }
inline LISPT iquotient(LISPT a, LISPT b) { return arith().iquotient(a, b); }
inline LISPT iremainder(lisp& l, LISPT a, LISPT b) { return arith(l).iremainder(a, b); }
inline LISPT iremainder(LISPT a, LISPT b) { return arith().iremainder(a, b); }
inline LISPT fdivide(lisp& l, LISPT a, LISPT b) { return arith(l).fdivide(a, b); }
inline LISPT fdivide(LISPT a, LISPT b) { return arith().fdivide(a, b); }
inline LISPT minus(lisp& l, LISPT a) { return arith(l).minus(a); }
inline LISPT minus(LISPT a) { return arith().minus(a); }
inline LISPT iminus(lisp& l, LISPT a) { return arith(l).iminus(a); }
inline LISPT iminus(LISPT a) { return arith().iminus(a); }
inline LISPT absval(lisp& l, LISPT i) { return arith(l).absval(i); }
inline LISPT absval(LISPT i) { return arith().absval(i); }
inline LISPT itof(lisp& l, LISPT i) { return arith(l).itof(i); }
inline LISPT itof(LISPT i) { return arith().itof(i); }
inline LISPT add1(lisp& l, LISPT a) { return arith(l).add1(a); }
inline LISPT add1(LISPT a) { return arith().add1(a); }
inline LISPT sub1(lisp& l, LISPT a) { return arith(l).sub1(a); }
inline LISPT sub1(LISPT a) { return arith().sub1(a); }
inline LISPT greaterp(lisp& l, LISPT x, LISPT y) { return arith(l).greaterp(x, y); }
inline LISPT greaterp(LISPT x, LISPT y) { return arith().greaterp(x, y); }
inline LISPT lessp(lisp& l, LISPT x, LISPT y) { return arith(l).lessp(x, y); }
inline LISPT lessp(LISPT x, LISPT y) { return arith().lessp(x, y); }
inline LISPT eqp(lisp& l, LISPT x, LISPT y) { return arith(l).eqp(x, y); }
inline LISPT eqp(LISPT x, LISPT y) { return arith().eqp(x, y); }
inline LISPT geq(lisp& l, LISPT x, LISPT y) { return arith(l).geq(x, y); }
inline LISPT geq(LISPT x, LISPT y) { return arith().geq(x, y); }
inline LISPT leq(lisp& l, LISPT x, LISPT y) { return arith(l).leq(x, y); }
inline LISPT leq(LISPT x, LISPT y) { return arith().leq(x, y); }
inline LISPT neqp(lisp& l, LISPT x, LISPT y) { return arith(l).neqp(x, y); }
inline LISPT neqp(LISPT x, LISPT y) { return arith().neqp(x, y); }
inline LISPT zerop(lisp& l, LISPT x) { return arith(l).zerop(x); }
inline LISPT zerop(LISPT x) { return arith().zerop(x); }
inline LISPT minusp(lisp& l, LISPT x) { return arith(l).minusp(x); }
inline LISPT minusp(LISPT x) { return arith().minusp(x); }

} // namespace lisp
