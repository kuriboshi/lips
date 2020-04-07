/*
 * Lips, lisp shell.
 * Copyright 1992, 2020 Krister Joas.
 *
 * $Id$
 */
#pragma once

typedef struct lispt *LISPT;

#define PP0(name) extern LISPT name(void)
#define PP1(name) extern LISPT name(LISPT)
#define PP2(name) extern LISPT name(LISPT, LISPT)
#define PP3(name) extern LISPT name(LISPT, LISPT, LISPT)

/* Lisp primitives */
PP1(reclaim);
PP2(cons);
PP1(plus);
PP1(iplus);
PP1(fplus);
PP2(difference);
PP2(idifference);
PP2(fdifference);
PP1(ltimes);
PP1(itimes);
PP1(ftimes);
PP2(divide);
PP2(iquotient);
PP2(iremainder);
PP2(fdivide);
PP1(minus);
PP1(iminus);
PP1(absval);
PP1(itof);
PP1(add1);
PP1(sub1);
PP2(greaterp);
PP2(lessp);
PP2(eqp);
PP2(geq);
PP2(leq);
PP2(neqp);
PP1(zerop);
PP1(minusp);
PP1(eval);
PP2(apply);
PP2(apply);
PP0(baktrace);
PP1(xratom);
PP1(readc);
PP1(xread);
PP2(xprint);
PP1(load);
PP1(xterpri);
PP2(prin1);
PP2(prin2);
PP1(plevel);
PP2(spaces);
PP1(xreadline);
PP2(cpprint);
PP1(and);
PP1(or);
PP1(not);
PP3(xif);
PP2(set);
PP2(set);
PP2(setq);
PP1(progn);
PP1(cond);
PP2(xwhile);
PP2(prog1);
PP3(prog2);
PP0(topofstack);
PP2(envget);
PP3(map);
PP3(mapc);
PP3(maplist);
PP3(mapcar);
PP0(xobarray);
PP1(evaltrace);
PP0(freecount);
PP1(numberp);
PP1(listp);
PP2(memb);
PP2(equal);
PP1(nlistp);
PP2(neq);
PP1(boundp);
PP1(litatom);
PP1(xtypeof);
PP1(car);
PP1(cdr);
PP1(cadr);
PP1(cdar);
PP1(caar);
PP1(cddr);
PP1(cdddr);
PP1(caddr);
PP1(cdadr);
PP1(caadr);
PP1(cddar);
PP1(cadar);
PP1(cdaar);
PP1(caaar);
PP2(rplaca);
PP2(rplacd);
PP2(eq);
PP1(atom);
PP1(nconc);
PP2(tconc);
PP2(attach);
PP1(append);
PP1(null);
PP1(quote);
PP2(lambda);
PP2(nlambda);
PP1(list);
PP1(length);
PP2(closure);
PP2(xnth);
PP2(nthd);
PP1(xerror);
PP2(setplist);
PP1(getplist);
PP3(putprop);
PP2(getprop);
PP2(remprop);
PP1(symstr);
PP1(stringp);
PP2(streq);
PP2(strcomp);
PP1(concat);
PP1(xstrlen);
PP3(substr);
PP1(getrep);
PP2(define);
PP3(de);
PP3(df);

/* Other functions */
extern LISPT mkstring(char *);
extern LISPT mknumber(long);
extern LISPT mkatom(char *);
extern LISPT mkfloat(double);

extern int loadfile(char *);

extern void initcvar(LISPT *, char *, LISPT);

extern int (*undefhook)(LISPT, LISPT*);
extern void (*breakhook)(void);
