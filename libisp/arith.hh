//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_PLUS = "+";            // add
inline constexpr auto PN_DIFFERENCE = "-";      // subtract
inline constexpr auto PN_TIMES = "*";           // multiply
inline constexpr auto PN_DIVIDE = "/";          // divide
inline constexpr auto PN_IPLUS = "i+";          // integer add
inline constexpr auto PN_IDIFFERENCE = "i-";    // integer subtract
inline constexpr auto PN_ITIMES = "i*";         // integer multiply
inline constexpr auto PN_IQUOTIENT = "i/";      // integer divide
inline constexpr auto PN_IREMAINDER = "i%";     // integer mod
inline constexpr auto PN_IMINUS = "iminus";     // integer change sign
inline constexpr auto PN_MINUS = "minus";       // change sign generic
inline constexpr auto PN_ADD1 = "add1";         // add one
inline constexpr auto PN_SUB1 = "sub1";         // subtract one
inline constexpr auto PN_ABS = "abs";           // absolute value
inline constexpr auto PN_FPLUS = "f+";          // float add
inline constexpr auto PN_FDIFFERENCE = "f-";    // float subtract
inline constexpr auto PN_FTIMES = "f*";         // float multiply
inline constexpr auto PN_FDIVIDE = "f/";        // float divide
inline constexpr auto PN_ITOF = "itof";         // integer to float
inline constexpr auto PN_GREATERP = "greaterp"; // t if greater than
inline constexpr auto PN_GEQ = "geq";           // t if greater or eq
inline constexpr auto PN_LESSP = "lessp";       // less than
inline constexpr auto PN_LEQ = "leq";           // less or eq
inline constexpr auto PN_ZEROP = "zerop";       // t if eq to 0
inline constexpr auto PN_EQP = "eqp";           // number eq
inline constexpr auto PN_NEQP = "neqp";         // not eqp
inline constexpr auto PN_MINUSP = "minusp";     // t if negative

class arith: public base
{
public:
  arith(lisp&);
  static void init();

  LISPT plus(LISPT);
  LISPT iplus(LISPT);
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
inline LISPT iplus(lisp& l, LISPT a) { return arith(l).iplus(a); }
inline LISPT fplus(lisp& l, LISPT a) { return arith(l).fplus(a); }
inline LISPT difference(lisp& l, LISPT a, LISPT b) { return arith(l).difference(a, b); }
inline LISPT idifference(lisp& l, LISPT a, LISPT b) { return arith(l).idifference(a, b); }
inline LISPT fdifference(lisp& l, LISPT a, LISPT b) { return arith(l).fdifference(a, b); }
inline LISPT ltimes(lisp& l, LISPT a) { return arith(l).ltimes(a); }
inline LISPT itimes(lisp& l, LISPT a) { return arith(l).itimes(a); }
inline LISPT ftimes(lisp& l, LISPT a) { return arith(l).ftimes(a); }
inline LISPT divide(lisp& l, LISPT a, LISPT b) { return arith(l).divide(a, b); }
inline LISPT iquotient(lisp& l, LISPT a, LISPT b) { return arith(l).iquotient(a, b); }
inline LISPT iremainder(lisp& l, LISPT a, LISPT b) { return arith(l).iremainder(a, b); }
inline LISPT fdivide(lisp& l, LISPT a, LISPT b) { return arith(l).fdivide(a, b); }
inline LISPT minus(lisp& l, LISPT a) { return arith(l).minus(a); }
inline LISPT iminus(lisp& l, LISPT a) { return arith(l).iminus(a); }
inline LISPT absval(lisp& l, LISPT i) { return arith(l).absval(i); }
inline LISPT itof(lisp& l, LISPT i) { return arith(l).itof(i); }
inline LISPT add1(lisp& l, LISPT a) { return arith(l).add1(a); }
inline LISPT sub1(lisp& l, LISPT a) { return arith(l).sub1(a); }
inline LISPT greaterp(lisp& l, LISPT x, LISPT y) { return arith(l).greaterp(x, y); }
inline LISPT lessp(lisp& l, LISPT x, LISPT y) { return arith(l).lessp(x, y); }
inline LISPT eqp(lisp& l, LISPT x, LISPT y) { return arith(l).eqp(x, y); }
inline LISPT geq(lisp& l, LISPT x, LISPT y) { return arith(l).geq(x, y); }
inline LISPT leq(lisp& l, LISPT x, LISPT y) { return arith(l).leq(x, y); }
inline LISPT neqp(lisp& l, LISPT x, LISPT y) { return arith(l).neqp(x, y); }
inline LISPT zerop(lisp& l, LISPT x) { return arith(l).zerop(x); }
inline LISPT minusp(lisp& l, LISPT x) { return arith(l).minusp(x); }

} // namespace lisp
