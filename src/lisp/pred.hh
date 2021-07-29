//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class pred
{
public:
  static void init();

  static LISPT numberp(lisp&, LISPT);
  static LISPT listp(lisp&, LISPT);
  static LISPT memb(lisp&, LISPT, LISPT);
  static LISPT equal(lisp&, LISPT, LISPT);
  static LISPT nlistp(lisp&, LISPT);
  static LISPT neq(lisp&, LISPT, LISPT);
  static LISPT boundp(lisp&, LISPT);
  static LISPT litatom(lisp&, LISPT);
  static LISPT xtypeof(lisp&, LISPT);
};

inline LISPT numberp(lisp& l, LISPT a) { return pred::numberp(l, a); }
inline LISPT numberp(LISPT a) { return pred::numberp(lisp::current(), a); }
inline LISPT listp(lisp& l, LISPT a) { return pred::listp(l, a); }
inline LISPT listp(LISPT a) { return pred::listp(lisp::current(), a); }
inline LISPT memb(lisp& l, LISPT x, LISPT y) { return pred::memb(l, x, y); }
inline LISPT memb(LISPT x, LISPT y) { return pred::memb(lisp::current(), x, y); }
inline LISPT equal(lisp& l, LISPT l1, LISPT l2) { return pred::equal(l, l1, l2); }
inline LISPT equal(LISPT l1, LISPT l2) { return pred::equal(lisp::current(), l1, l2); }
inline LISPT nlistp(lisp& l, LISPT a) { return pred::nlistp(l, a); }
inline LISPT nlistp(LISPT a) { return pred::nlistp(lisp::current(), a); }
inline LISPT neq(lisp& l, LISPT a, LISPT b) { return pred::neq(l, a, b); }
inline LISPT neq(LISPT a, LISPT b) { return pred::neq(lisp::current(), a, b); }
inline LISPT boundp(lisp& l, LISPT a) { return pred::boundp(l, a); }
inline LISPT boundp(LISPT a) { return pred::boundp(lisp::current(), a); }
inline LISPT litatom(lisp& l, LISPT a) { return pred::litatom(l, a); }
inline LISPT litatom(LISPT a) { return pred::litatom(lisp::current(), a); }
inline LISPT xtypeof(lisp& l, LISPT a) { return pred::xtypeof(l, a); }
inline LISPT xtypeof(LISPT a) { return pred::xtypeof(lisp::current(), a); }

} // namespace lisp
