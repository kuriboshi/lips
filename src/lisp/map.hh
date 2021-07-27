//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class Map: public base
{
public:
  Map();
  Map(lisp&);
  ~Map() = default;
  static void init();

  LISPT map(LISPT, LISPT, LISPT);
  LISPT mapc(LISPT, LISPT, LISPT);
  LISPT maplist(LISPT, LISPT, LISPT);
  LISPT mapcar(LISPT, LISPT, LISPT);
};

inline LISPT map(lisp& l, LISPT a, LISPT b, LISPT c) { return Map(l).map(a, b, c); }
inline LISPT map(LISPT a, LISPT b, LISPT c) { return Map().map(a, b, c); }
inline LISPT mapc(lisp& l, LISPT a, LISPT b, LISPT c) { return Map(l).mapc(a, b, c); }
inline LISPT mapc(LISPT a, LISPT b, LISPT c) { return Map().mapc(a, b, c); }
inline LISPT maplist(lisp& l, LISPT a, LISPT b, LISPT c) { return Map(l).maplist(a, b, c); }
inline LISPT maplist(LISPT a, LISPT b, LISPT c) { return Map().maplist(a, b, c); }
inline LISPT mapcar(lisp& l, LISPT a, LISPT b, LISPT c) { return Map(l).mapcar(a, b, c); }
inline LISPT mapcar(LISPT a, LISPT b, LISPT c) { return Map().mapcar(a, b, c); }

} // namespace lisp
