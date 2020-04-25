//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp {

/* variables */
extern LISPT currentbase;
extern LISPT topprompt;
extern LISPT promptform;
extern LISPT brkprompt;
extern LISPT interactive;
extern LISPT version;

/* functions */
extern void init_lisp(void);
extern void initcvar(LISPT*, const char*, LISPT);

}
