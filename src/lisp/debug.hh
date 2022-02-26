//
// Lips, lisp shell.
// Copyright 2020-2021 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
namespace debug
{
LISPT evaltrace(lisp&, LISPT);
void init();
}

inline LISPT evaltrace(lisp& l, LISPT x) { return debug::evaltrace(l, x); }
inline LISPT evaltrace(LISPT x) { return debug::evaltrace(lisp::current(), x); }

} // namespace lisp
