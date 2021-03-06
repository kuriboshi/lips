//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class map: public base
{
public:
  map();
  map(lisp&);
  ~map() = default;
  static void init();

  LISPT xmap(LISPT, LISPT, LISPT);
  LISPT mapc(LISPT, LISPT, LISPT);
  LISPT maplist(LISPT, LISPT, LISPT);
  LISPT mapcar(LISPT, LISPT, LISPT);
};

inline LISPT xmap(lisp& l, LISPT a, LISPT b, LISPT c) { return map(l).xmap(a, b, c); }
inline LISPT xmap(LISPT a, LISPT b, LISPT c) { return map().xmap(a, b, c); }
inline LISPT mapc(lisp& l, LISPT a, LISPT b, LISPT c) { return map(l).mapc(a, b, c); }
inline LISPT mapc(LISPT a, LISPT b, LISPT c) { return map().mapc(a, b, c); }
inline LISPT maplist(lisp& l, LISPT a, LISPT b, LISPT c) { return map(l).maplist(a, b, c); }
inline LISPT maplist(LISPT a, LISPT b, LISPT c) { return map().maplist(a, b, c); }
inline LISPT mapcar(lisp& l, LISPT a, LISPT b, LISPT c) { return map(l).mapcar(a, b, c); }
inline LISPT mapcar(LISPT a, LISPT b, LISPT c) { return map().mapcar(a, b, c); }

} // namespace lisp
