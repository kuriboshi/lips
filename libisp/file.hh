//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <cstdio>

/* variables */
extern FILE* primin;
extern FILE* primout;
extern FILE* primerr;

namespace lisp
{
extern LISPT xratom(LISPT);
extern LISPT readc(LISPT);
extern LISPT xread(LISPT);
extern LISPT xprint(LISPT, LISPT);
extern LISPT load(LISPT);
extern LISPT xterpri(LISPT);
extern LISPT prin1(LISPT, LISPT);
extern LISPT prin2(LISPT, LISPT);
extern LISPT plevel(LISPT);
extern LISPT spaces(LISPT, LISPT);
extern LISPT xreadline(LISPT);
extern LISPT cpprint(LISPT, LISPT);

extern bool loadfile(const char*);

} // namespace lisp
