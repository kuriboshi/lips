//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

/* variables */
extern long trace;
/* functions */
extern LISPT error(int, LISPT);
extern LISPT syserr(LISPT);
extern LISPT break0(LISPT);
extern void init_debug(void);
