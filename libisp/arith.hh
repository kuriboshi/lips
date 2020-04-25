namespace lisp {

extern LISPT plus(LISPT);
extern LISPT iplus(LISPT);
extern LISPT fplus(LISPT);
extern LISPT difference(LISPT, LISPT);
extern LISPT idifference(LISPT, LISPT);
extern LISPT fdifference(LISPT, LISPT);
extern LISPT ltimes(LISPT);
extern LISPT itimes(LISPT);
extern LISPT ftimes(LISPT);
extern LISPT divide(LISPT, LISPT);
extern LISPT iquotient(LISPT, LISPT);
extern LISPT iremainder(LISPT, LISPT);
extern LISPT fdivide(LISPT, LISPT);
extern LISPT minus(LISPT);
extern LISPT iminus(LISPT);
extern LISPT absval(LISPT);
extern LISPT itof(LISPT);
extern LISPT add1(LISPT);
extern LISPT sub1(LISPT);
extern LISPT greaterp(LISPT, LISPT);
extern LISPT lessp(LISPT, LISPT);
extern LISPT eqp(LISPT, LISPT);
extern LISPT geq(LISPT, LISPT);
extern LISPT leq(LISPT, LISPT);
extern LISPT neqp(LISPT, LISPT);
extern LISPT zerop(LISPT);
extern LISPT minusp(LISPT);
extern void init_arith();

}
