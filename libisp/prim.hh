//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
/* functions */
extern void mkprim(const char*, LISPT (*fname)(void), short, lisp_type);
extern void mkprim(const char*, LISPT (*fname)(LISPT), short, lisp_type);
extern void mkprim(const char*, LISPT (*fname)(LISPT, LISPT), short, lisp_type);
extern void mkprim(const char*, LISPT (*fname)(LISPT, LISPT, LISPT), short, lisp_type);
extern LISPT nth(LISPT list, int n);
extern LISPT closobj(LISPT vars);
extern LISPT mklambda(LISPT args, LISPT def, lisp_type);
extern void init_prim(void);

extern LISPT car(LISPT);
extern LISPT cdr(LISPT);
extern LISPT cadr(LISPT);
extern LISPT cdar(LISPT);
extern LISPT caar(LISPT);
extern LISPT cddr(LISPT);
extern LISPT cdddr(LISPT);
extern LISPT caddr(LISPT);
extern LISPT cdadr(LISPT);
extern LISPT caadr(LISPT);
extern LISPT cddar(LISPT);
extern LISPT cadar(LISPT);
extern LISPT cdaar(LISPT);
extern LISPT caaar(LISPT);
extern LISPT rplaca(LISPT, LISPT);
extern LISPT rplacd(LISPT, LISPT);
extern LISPT eq(LISPT, LISPT);
extern LISPT atom(LISPT);
extern LISPT nconc(LISPT);
extern LISPT tconc(LISPT, LISPT);
extern LISPT attach(LISPT, LISPT);
extern LISPT append(LISPT);
extern LISPT null(LISPT);
extern LISPT quote(LISPT);
extern LISPT lambda(LISPT, LISPT);
extern LISPT nlambda(LISPT, LISPT);
extern LISPT list(LISPT);
extern LISPT length(LISPT);
extern LISPT closure(LISPT, LISPT);
extern LISPT xnth(LISPT, LISPT);
extern LISPT nthd(LISPT, LISPT);
extern LISPT xerror(LISPT);
extern LISPT uxexit(LISPT);

} // namespace lisp
