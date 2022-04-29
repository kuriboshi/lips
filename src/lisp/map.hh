//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LISP_MAP_HH
#define LISP_MAP_HH

#include "lisp.hh"

namespace lisp::Map
{
void init();

LISPT map(lisp&, LISPT, LISPT, LISPT);
LISPT mapc(lisp&, LISPT, LISPT, LISPT);
LISPT maplist(lisp&, LISPT, LISPT, LISPT);
LISPT mapcar(lisp&, LISPT, LISPT, LISPT);
} // namespace lisp::Map

namespace lisp
{
inline LISPT map(lisp& l, LISPT a, LISPT b, LISPT c) { return Map::map(l, a, b, c); }
inline LISPT map(LISPT a, LISPT b, LISPT c) { return Map::map(lisp::current(), a, b, c); }
inline LISPT mapc(lisp& l, LISPT a, LISPT b, LISPT c) { return Map::mapc(l, a, b, c); }
inline LISPT mapc(LISPT a, LISPT b, LISPT c) { return Map::mapc(lisp::current(), a, b, c); }
inline LISPT maplist(lisp& l, LISPT a, LISPT b, LISPT c) { return Map::maplist(l, a, b, c); }
inline LISPT maplist(LISPT a, LISPT b, LISPT c) { return Map::maplist(lisp::current(), a, b, c); }
inline LISPT mapcar(lisp& l, LISPT a, LISPT b, LISPT c) { return Map::mapcar(l, a, b, c); }
inline LISPT mapcar(LISPT a, LISPT b, LISPT c) { return Map::mapcar(lisp::current(), a, b, c); }
} // namespace lisp

#endif
