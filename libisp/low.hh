//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
/* variables */
extern LISPT verboseflg;

/* functions */
extern void init_low();

// Primitives
extern LISPT set(LISPT, LISPT);
extern LISPT setq(LISPT, LISPT);
extern LISPT set(LISPT, LISPT);
extern LISPT cond(LISPT);
extern LISPT xwhile(LISPT, LISPT);
extern LISPT progn(LISPT);
extern LISPT prog1(LISPT, LISPT);
extern LISPT prog2(LISPT, LISPT, LISPT);

} // namespace lisp
