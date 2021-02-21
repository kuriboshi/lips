//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_SET = "set";               // set variable
inline constexpr auto PN_SETQ = "setq";             // set quoted variable
inline constexpr auto PN_SETQQ = "setqq";           // noeval set
inline constexpr auto PN_COND = "cond";             // cond
inline constexpr auto PN_WHILE = "while";           // while t
inline constexpr auto PN_PROGN = "progn";           // return last expression
inline constexpr auto PN_PROG1 = "prog1";           // return first expression
inline constexpr auto PN_PROG2 = "prog2";           // return second expression
inline constexpr auto PN_TOPOFSTACK = "topofstack"; // return top of value stack
inline constexpr auto PN_ENVGET = "envget";         // examine environment

class low: public base
{
public:
  low(lisp&);
  ~low() = default;
  static void init();

  LISPT set(LISPT, LISPT);
  LISPT setq(LISPT, LISPT);
  LISPT progn(LISPT);
  LISPT cond(LISPT);
  LISPT xwhile(LISPT, LISPT);
  LISPT prog1(LISPT, LISPT);
  LISPT prog2(LISPT, LISPT, LISPT);
};

inline LISPT set(lisp& l, LISPT a, LISPT b) { return low(l).set(a, b); }
inline LISPT set(LISPT a, LISPT b) { return low(lisp::current()).set(a, b); }
inline LISPT setq(lisp& l, LISPT a, LISPT b) { return low(l).setq(a, b); }
inline LISPT setq(LISPT a, LISPT b) { return low(lisp::current()).setq(a, b); }
inline LISPT setqq(lisp& l, LISPT a, LISPT b) { return low(l).set(a, b); }
inline LISPT setqq(LISPT a, LISPT b) { return low(lisp::current()).set(a, b); }
inline LISPT cond(lisp& l, LISPT a) { return low(l).cond(a); }
inline LISPT cond(LISPT a) { return low(lisp::current()).cond(a); }
inline LISPT xwhile(lisp& l, LISPT a, LISPT b) { return low(l).xwhile(a, b); }
inline LISPT xwhile(LISPT a, LISPT b) { return low(lisp::current()).xwhile(a, b); }
inline LISPT progn(lisp& l, LISPT a) { return low(l).progn(a); }
inline LISPT progn(LISPT a) { return low(lisp::current()).progn(a); }
inline LISPT prog1(lisp& l, LISPT a, LISPT b) { return low(l).prog1(a, b); }
inline LISPT prog1(LISPT a, LISPT b) { return low(lisp::current()).prog1(a, b); }
inline LISPT prog2(lisp& l, LISPT a, LISPT b, LISPT c) { return low(l).prog2(a, b, c); }
inline LISPT prog2(LISPT a, LISPT b, LISPT c) { return low(lisp::current()).prog2(a, b, c); }

} // namespace lisp
