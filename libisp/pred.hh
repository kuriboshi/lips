//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_LISTP = "listp";           // t if cons
inline constexpr auto PN_NLISTP = "nlistp";         // not listp
inline constexpr auto PN_NEQ = "neq";               // not eq
inline constexpr auto PN_NUMBERP = "numberp";       // integer of float
inline constexpr auto PN_MEMB = "memb";             // t if a in l
inline constexpr auto PN_EQUAL = "equal";           // equal
inline constexpr auto PN_BOUNDP = "boundp";         // t if var bound
inline constexpr auto PN_LITATOM = "litatom";       // t if literal atom
inline constexpr auto PN_TYPEOF = "typeof";         // return type as an atom

class pred : public base
{
public:
  pred(lisp&);
  ~pred() = default;
  static void init();

  LISPT numberp(LISPT);
  LISPT listp(LISPT);
  LISPT memb(LISPT, LISPT);
  LISPT equal(LISPT, LISPT);
  LISPT nlistp(LISPT);
  LISPT neq(LISPT, LISPT);
  LISPT boundp(LISPT);
  LISPT litatom(LISPT);
  LISPT xtypeof(LISPT);
};

inline LISPT numberp(lisp& l, LISPT a) { return pred(l).numberp(a); }
inline LISPT listp(lisp& l, LISPT a) { return pred(l).listp(a); }
inline LISPT memb(lisp& l, LISPT x, LISPT y) { return pred(l).memb(x, y); }
inline LISPT equal(lisp& l, LISPT l1, LISPT l2) { return pred(l).equal(l1, l2); }
inline LISPT nlistp(lisp& l, LISPT a) { return pred(l).nlistp(a); }
inline LISPT neq(lisp& l, LISPT a, LISPT b) { return pred(l).neq(a, b); }
inline LISPT boundp(lisp& l, LISPT a) { return pred(l).boundp(a); }
inline LISPT litatom(lisp& l, LISPT a) { return pred(l).litatom(a); }
inline LISPT xtypeof(lisp& l, LISPT a) { return pred(l).xtypeof(a); }

} // namespace lisp
