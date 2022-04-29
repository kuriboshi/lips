//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
//

#ifndef LISP_DEBUG_HH
#define LISP_DEBUG_HH

#include "lisp.hh"

namespace lisp::debug
{
LISPT evaltrace(lisp&, LISPT);
void init();
} // namespace lisp::debug

namespace lisp
{
inline LISPT evaltrace(lisp& l, LISPT x) { return debug::evaltrace(l, x); }
inline LISPT evaltrace(LISPT x) { return debug::evaltrace(lisp::current(), x); }
} // namespace lisp

#endif
