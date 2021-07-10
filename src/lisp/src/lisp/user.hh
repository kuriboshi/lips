//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class user: public base
{
public:
  user();
  user(lisp&);
  ~user() = default;
  static void init();

  LISPT getrep(LISPT);
  LISPT define(LISPT, LISPT);
  LISPT de(LISPT, LISPT, LISPT);
  LISPT df(LISPT, LISPT, LISPT);
  LISPT funeq(LISPT, LISPT);

private:
  LISPT getargs(LISPT);
  LISPT def(LISPT, LISPT, LISPT, type);
  LISPT checkfn(LISPT, LISPT);
};

inline LISPT funeq(lisp& l, LISPT a, LISPT b) { return user(l).funeq(a, b); }
inline LISPT funeq(LISPT a, LISPT b) { return user().funeq(a, b); }
inline LISPT getrep(lisp& l, LISPT a) { return user(l).getrep(a); }
inline LISPT getrep(LISPT a) { return user().getrep(a); }
inline LISPT define(lisp& l, LISPT a, LISPT b) { return user(l).define(a, b); }
inline LISPT define(LISPT a, LISPT b) { return user().define(a, b); }
inline LISPT de(lisp& l, LISPT a, LISPT b, LISPT c) { return user(l).de(a, b, c); }
inline LISPT de(LISPT a, LISPT b, LISPT c) { return user().de(a, b, c); }
inline LISPT df(lisp& l, LISPT a, LISPT b, LISPT c) { return user(l).df(a, b, c); }
inline LISPT df(LISPT a, LISPT b, LISPT c) { return user().df(a, b, c); }

} // namespace lisp
