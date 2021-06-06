//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class prim: public base
{
public:
  prim();
  prim(lisp&);
  ~prim() = default;
  static void init();

  LISPT car(LISPT);
  LISPT cdr(LISPT);
  LISPT cadr(LISPT);
  LISPT cdar(LISPT);
  LISPT caar(LISPT);
  LISPT cddr(LISPT);
  LISPT cdddr(LISPT);
  LISPT caddr(LISPT);
  LISPT cdadr(LISPT);
  LISPT caadr(LISPT);
  LISPT cddar(LISPT);
  LISPT cadar(LISPT);
  LISPT cdaar(LISPT);
  LISPT caaar(LISPT);
  LISPT rplaca(LISPT, LISPT);
  LISPT rplacd(LISPT, LISPT);
  LISPT eq(LISPT, LISPT);
  LISPT atom(LISPT);
  LISPT nconc(LISPT);
  LISPT tconc(LISPT, LISPT);
  LISPT attach(LISPT, LISPT);
  LISPT append(LISPT);
  LISPT null(LISPT);
  LISPT quote(LISPT);
  LISPT lambda(LISPT, LISPT);
  LISPT nlambda(LISPT, LISPT);
  LISPT list(LISPT);
  LISPT length(LISPT);
  LISPT closure(LISPT, LISPT);
  LISPT xnth(LISPT, LISPT);
  LISPT nthd(LISPT, LISPT);
  LISPT xerror(LISPT);
  LISPT uxexit(LISPT);

private:
  LISPT closobj(LISPT);
};

extern LISPT C_APPEND;
extern LISPT C_ERROR;
extern LISPT C_LAMBDA;
extern LISPT C_NLAMBDA;
extern LISPT C_QUOTE;

inline LISPT car(lisp& l, LISPT a) { return prim(l).car(a); }
inline LISPT car(LISPT a) { return prim().car(a); }
inline LISPT cdr(lisp& l, LISPT a) { return prim(l).cdr(a); }
inline LISPT cdr(LISPT a) { return prim().cdr(a); }
inline LISPT cadr(lisp& l, LISPT a) { return prim(l).cadr(a); }
inline LISPT cadr(LISPT a) { return prim().cadr(a); }
inline LISPT cdar(lisp& l, LISPT a) { return prim(l).cdar(a); }
inline LISPT cdar(LISPT a) { return prim().cdar(a); }
inline LISPT caar(lisp& l, LISPT a) { return prim(l).caar(a); }
inline LISPT caar(LISPT a) { return prim().caar(a); }
inline LISPT cddr(lisp& l, LISPT a) { return prim(l).cddr(a); }
inline LISPT cddr(LISPT a) { return prim().cddr(a); }
inline LISPT cdddr(lisp& l, LISPT a) { return prim(l).cdddr(a); }
inline LISPT cdddr(LISPT a) { return prim().cdddr(a); }
inline LISPT caddr(lisp& l, LISPT a) { return prim(l).caddr(a); }
inline LISPT caddr(LISPT a) { return prim().caddr(a); }
inline LISPT cdadr(lisp& l, LISPT a) { return prim(l).cdadr(a); }
inline LISPT cdadr(LISPT a) { return prim().cdadr(a); }
inline LISPT caadr(lisp& l, LISPT a) { return prim(l).caadr(a); }
inline LISPT caadr(LISPT a) { return prim().caadr(a); }
inline LISPT cddar(lisp& l, LISPT a) { return prim(l).cddar(a); }
inline LISPT cddar(LISPT a) { return prim().cddar(a); }
inline LISPT cadar(lisp& l, LISPT a) { return prim(l).cadar(a); }
inline LISPT cadar(LISPT a) { return prim().cadar(a); }
inline LISPT cdaar(lisp& l, LISPT a) { return prim(l).cdaar(a); }
inline LISPT cdaar(LISPT a) { return prim().cdaar(a); }
inline LISPT caaar(lisp& l, LISPT a) { return prim(l).caaar(a); }
inline LISPT caaar(LISPT a) { return prim().caaar(a); }
inline LISPT eq(lisp& l, LISPT a, LISPT b) { return prim(l).eq(a, b); }
inline LISPT eq(LISPT a, LISPT b) { return prim().eq(a, b); }
inline LISPT atom(lisp& l, LISPT a) { return prim(l).atom(a); }
inline LISPT atom(LISPT a) { return prim().atom(a); }
inline LISPT nconc(lisp& l, LISPT a) { return prim(l).nconc(a); }
inline LISPT nconc(LISPT a) { return prim().nconc(a); }
inline LISPT attach(lisp& l, LISPT a, LISPT b) { return prim(l).attach(a, b); }
inline LISPT attach(LISPT a, LISPT b) { return prim().attach(a, b); }
inline LISPT null(lisp& l, LISPT a) { return prim(l).null(a); }
inline LISPT null(LISPT a) { return prim().null(a); }
inline LISPT quote(lisp& l, LISPT a) { return prim(l).quote(a); }
inline LISPT quote(LISPT a) { return prim().quote(a); }
inline LISPT lambda(lisp& l, LISPT a, LISPT b) { return prim(l).lambda(a, b); }
inline LISPT lambda(LISPT a, LISPT b) { return prim().lambda(a, b); }
inline LISPT nlambda(lisp& l, LISPT a, LISPT b) { return prim(l).nlambda(a, b); }
inline LISPT nlambda(LISPT a, LISPT b) { return prim().nlambda(a, b); }
inline LISPT list(lisp& l, LISPT a) { return prim(l).list(a); }
inline LISPT list(LISPT a) { return prim().list(a); }
inline LISPT length(lisp& l, LISPT a) { return prim(l).length(a); }
inline LISPT length(LISPT a) { return prim().length(a); }
inline LISPT closure(lisp& l, LISPT a, LISPT b) { return prim(l).closure(a, b); }
inline LISPT closure(LISPT a, LISPT b) { return prim().closure(a, b); }
inline LISPT xnth(lisp& l, LISPT a, LISPT b) { return prim(l).xnth(a, b); }
inline LISPT xnth(LISPT a, LISPT b) { return prim().xnth(a, b); }
inline LISPT nthd(lisp& l, LISPT a, LISPT b) { return prim(l).nthd(a, b); }
inline LISPT nthd(LISPT a, LISPT b) { return prim().nthd(a, b); }
inline LISPT xerror(lisp& l, LISPT a) { return prim(l).xerror(a); }
inline LISPT xerror(LISPT a) { return prim().xerror(a); }
inline LISPT uxexit(lisp& l, LISPT a) { return prim(l).uxexit(a); }
inline LISPT uxexit(LISPT a) { return prim().uxexit(a); }

inline LISPT rplaca(lisp& l, LISPT a, LISPT b) { return prim(l).rplaca(a, b); }
inline LISPT rplaca(LISPT a, LISPT b) { return prim().rplaca(a, b); }
inline LISPT rplacd(lisp& l, LISPT a, LISPT b) { return prim(l).rplacd(a, b); }
inline LISPT rplacd(LISPT a, LISPT b) { return prim().rplacd(a, b); }
inline LISPT append(lisp& l, LISPT a) { return prim(l).append(a); }
inline LISPT append(LISPT a) { return prim().append(a); }
inline LISPT tconc(lisp& l, LISPT a, LISPT b) { return prim(l).tconc(a, b); }
inline LISPT tconc(LISPT a, LISPT b) { return prim().tconc(a, b); }

} // namespace lisp
