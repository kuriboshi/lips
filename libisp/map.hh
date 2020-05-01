//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class map : public base
{
public:
  map(lisp&);
  ~map() = default;

  LISPT xmap(LISPT, LISPT, LISPT);
  LISPT mapc(LISPT, LISPT, LISPT);
  LISPT maplist(LISPT, LISPT, LISPT);
  LISPT mapcar(LISPT, LISPT, LISPT);
};

inline LISPT xmap(lisp& l, LISPT a, LISPT b, LISPT c) { return map(l).xmap(a, b, c); }
inline LISPT mapc(lisp& l, LISPT a, LISPT b, LISPT c) { return map(l).mapc(a, b, c); }
inline LISPT maplist(lisp& l, LISPT a, LISPT b, LISPT c) { return map(l).maplist(a, b, c); }
inline LISPT mapcar(lisp& l, LISPT a, LISPT b, LISPT c) {return map(l).mapcar(a, b, c); }

} // namespace lisp
