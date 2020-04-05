/*
 * Lips, lisp shell.
 * Copyright 1992, Krister Joas.
 *
 * $Id$
 */

typedef struct lispt *LISPT;

#if __STDC__ == 1
#define PP(args)	args
#define PV		(void)
#else
#define PP(args)	()
#define PV		()
#endif /* __STDC__ == 1 */

/* Lisp primitives */
extern LISPT reclaim PP((LISPT));
extern LISPT cons PP((LISPT, LISPT));
extern LISPT plus PP((LISPT));
extern LISPT iplus PP((LISPT));
extern LISPT fplus PP((LISPT));
extern LISPT difference PP((LISPT, LISPT));
extern LISPT idifference PP((LISPT, LISPT));
extern LISPT fdifference PP((LISPT, LISPT));
extern LISPT ltimes PP((LISPT));
extern LISPT itimes PP((LISPT));
extern LISPT ftimes PP((LISPT));
extern LISPT divide PP((LISPT, LISPT));
extern LISPT iquotient PP((LISPT, LISPT));
extern LISPT iremainder PP((LISPT, LISPT));
extern LISPT fdivide PP((LISPT, LISPT));
extern LISPT minus PP((LISPT));
extern LISPT iminus PP((LISPT));
extern LISPT absval PP((LISPT));
extern LISPT itof PP((LISPT));
extern LISPT add1 PP((LISPT));
extern LISPT sub1 PP((LISPT));
extern LISPT greaterp PP((LISPT, LISPT));
extern LISPT lessp PP((LISPT, LISPT));
extern LISPT eqp PP((LISPT, LISPT));
extern LISPT geq PP((LISPT, LISPT));
extern LISPT leq PP((LISPT, LISPT));
extern LISPT neqp PP((LISPT, LISPT));
extern LISPT zerop PP((LISPT));
extern LISPT minusp PP((LISPT));
extern LISPT eval PP((LISPT));
extern LISPT eval PP((LISPT));
extern LISPT apply PP((LISPT, LISPT));
extern LISPT apply PP((LISPT, LISPT));
extern LISPT baktrace PV;
extern LISPT xratom PP((LISPT));
extern LISPT readc PP((LISPT));
extern LISPT xread PP((LISPT));
extern LISPT xprint PP((LISPT, LISPT));
extern LISPT load PP((LISPT));
extern LISPT xterpri PP((LISPT));
extern LISPT prin1 PP((LISPT, LISPT));
extern LISPT prin2 PP((LISPT, LISPT));
extern LISPT plevel PP((LISPT));
extern LISPT spaces PP((LISPT, LISPT));
extern LISPT xreadline PP((LISPT));
extern LISPT cpprint PP((LISPT, LISPT));
extern LISPT and PP((LISPT));
extern LISPT or PP((LISPT));
extern LISPT not PP((LISPT));
extern LISPT xif PP((LISPT, LISPT, LISPT));
extern LISPT set PP((LISPT, LISPT));
extern LISPT set PP((LISPT, LISPT));
extern LISPT setq PP((LISPT, LISPT));
extern LISPT progn PP((LISPT));
extern LISPT cond PP((LISPT));
extern LISPT xwhile PP((LISPT, LISPT));
extern LISPT prog1 PP((LISPT, LISPT));
extern LISPT prog2 PP((LISPT, LISPT, LISPT));
extern LISPT topofstack PV;
extern LISPT envget PP((LISPT, LISPT));
extern LISPT map PP((LISPT, LISPT, LISPT));
extern LISPT mapc PP((LISPT, LISPT, LISPT));
extern LISPT maplist PP((LISPT, LISPT, LISPT));
extern LISPT mapcar PP((LISPT, LISPT, LISPT));
extern LISPT xobarray PV;
extern LISPT evaltrace PP((LISPT));
extern LISPT freecount PV;
extern LISPT numberp PP((LISPT));
extern LISPT listp PP((LISPT));
extern LISPT memb PP((LISPT, LISPT));
extern LISPT equal PP((LISPT, LISPT));
extern LISPT nlistp PP((LISPT));
extern LISPT neq PP((LISPT, LISPT));
extern LISPT boundp PP((LISPT));
extern LISPT litatom PP((LISPT));
extern LISPT xtypeof PP((LISPT));
extern LISPT car PP((LISPT));
extern LISPT cdr PP((LISPT));
extern LISPT cadr PP((LISPT));
extern LISPT cdar PP((LISPT));
extern LISPT caar PP((LISPT));
extern LISPT cddr PP((LISPT));
extern LISPT cdddr PP((LISPT));
extern LISPT caddr PP((LISPT));
extern LISPT cdadr PP((LISPT));
extern LISPT caadr PP((LISPT));
extern LISPT cddar PP((LISPT));
extern LISPT cadar PP((LISPT));
extern LISPT cdaar PP((LISPT));
extern LISPT caaar PP((LISPT));
extern LISPT rplaca PP((LISPT, LISPT));
extern LISPT rplacd PP((LISPT, LISPT));
extern LISPT eq PP((LISPT, LISPT));
extern LISPT atom PP((LISPT));
extern LISPT nconc PP((LISPT));
extern LISPT tconc PP((LISPT, LISPT));
extern LISPT attach PP((LISPT, LISPT));
extern LISPT append PP((LISPT));
extern LISPT null PP((LISPT));
extern LISPT quote PP((LISPT));
extern LISPT lambda PP((LISPT, LISPT));
extern LISPT nlambda PP((LISPT, LISPT));
extern LISPT list PP((LISPT));
extern LISPT length PP((LISPT));
extern LISPT closure PP((LISPT, LISPT));
extern LISPT xnth PP((LISPT, LISPT));
extern LISPT nthd PP((LISPT, LISPT));
extern LISPT xerror PP((LISPT));
extern LISPT setplist PP((LISPT, LISPT));
extern LISPT getplist PP((LISPT));
extern LISPT putprop PP((LISPT, LISPT, LISPT));
extern LISPT getprop PP((LISPT, LISPT));
extern LISPT remprop PP((LISPT, LISPT));
extern LISPT symstr PP((LISPT));
extern LISPT stringp PP((LISPT));
extern LISPT streq PP((LISPT, LISPT));
extern LISPT strcomp PP((LISPT, LISPT));
extern LISPT concat PP((LISPT));
extern LISPT xstrlen PP((LISPT));
extern LISPT substr PP((LISPT, LISPT, LISPT));
extern LISPT getrep PP((LISPT));
extern LISPT define PP((LISPT, LISPT));
extern LISPT de PP((LISPT, LISPT, LISPT));
extern LISPT df PP((LISPT, LISPT, LISPT));

/* Other functions */
extern LISPT mkstring PP((char *));
extern LISPT mknumber PP((long));
extern LISPT mkatom PP((char *));
extern LISPT mkfloat PP((double));

extern int loadfile PP((char *));

extern void initcvar PP((LISPT *, char *, LISPT));

extern int (*undefhook)();
extern void (*breakhook)();
