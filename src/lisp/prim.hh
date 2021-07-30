//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
class prim
{
public:
  static void init();

  static LISPT car(lisp&, LISPT);
  static LISPT cdr(lisp&, LISPT);
  static LISPT cadr(lisp&, LISPT);
  static LISPT cdar(lisp&, LISPT);
  static LISPT caar(lisp&, LISPT);
  static LISPT cddr(lisp&, LISPT);
  static LISPT cdddr(lisp&, LISPT);
  static LISPT caddr(lisp&, LISPT);
  static LISPT cdadr(lisp&, LISPT);
  static LISPT caadr(lisp&, LISPT);
  static LISPT cddar(lisp&, LISPT);
  static LISPT cadar(lisp&, LISPT);
  static LISPT cdaar(lisp&, LISPT);
  static LISPT caaar(lisp&, LISPT);
  static LISPT rplaca(lisp&, LISPT, LISPT);
  static LISPT rplacd(lisp&, LISPT, LISPT);
  static LISPT eq(lisp&, LISPT, LISPT);
  static LISPT atom(lisp&, LISPT);
  static LISPT nconc(lisp&, LISPT);
  static LISPT tconc(lisp&, LISPT, LISPT);
  static LISPT attach(lisp&, LISPT, LISPT);
  static LISPT append(lisp&, LISPT);
  static LISPT null(lisp&, LISPT);
  static LISPT quote(lisp&, LISPT);
  static LISPT lambda(lisp&, LISPT, LISPT);
  static LISPT nlambda(lisp&, LISPT, LISPT);
  static LISPT list(lisp&, LISPT);
  static LISPT length(lisp&, LISPT);
  static LISPT closure(lisp&, LISPT, LISPT);

  /// @brief Return the N'th element in the list LIST.  If N is greater than
  /// the length of LIST, return NIL.
  ///
  static LISPT nth(lisp&, LISPT, LISPT);
  static LISPT nthd(lisp&, LISPT, LISPT);
  static LISPT error(lisp&, LISPT);
  static LISPT uxexit(lisp&, LISPT);

private:
  static LISPT closobj(lisp&, LISPT);
};

extern LISPT C_ERROR;
extern LISPT C_LAMBDA;
extern LISPT C_NLAMBDA;
extern LISPT C_QUOTE;

inline LISPT car(lisp& l, LISPT a) { return prim::car(l, a); }
inline LISPT car(LISPT a) { return prim::car(lisp::current(), a); }
inline LISPT cdr(lisp& l, LISPT a) { return prim::cdr(l, a); }
inline LISPT cdr(LISPT a) { return prim::cdr(lisp::current(), a); }
inline LISPT cadr(lisp& l, LISPT a) { return prim::cadr(l, a); }
inline LISPT cadr(LISPT a) { return prim::cadr(lisp::current(), a); }
inline LISPT cdar(lisp& l, LISPT a) { return prim::cdar(l, a); }
inline LISPT cdar(LISPT a) { return prim::cdar(lisp::current(), a); }
inline LISPT caar(lisp& l, LISPT a) { return prim::caar(l, a); }
inline LISPT caar(LISPT a) { return prim::caar(lisp::current(), a); }
inline LISPT cddr(lisp& l, LISPT a) { return prim::cddr(l, a); }
inline LISPT cddr(LISPT a) { return prim::cddr(lisp::current(), a); }
inline LISPT cdddr(lisp& l, LISPT a) { return prim::cdddr(l, a); }
inline LISPT cdddr(LISPT a) { return prim::cdddr(lisp::current(), a); }
inline LISPT caddr(lisp& l, LISPT a) { return prim::caddr(l, a); }
inline LISPT caddr(LISPT a) { return prim::caddr(lisp::current(), a); }
inline LISPT cdadr(lisp& l, LISPT a) { return prim::cdadr(l, a); }
inline LISPT cdadr(LISPT a) { return prim::cdadr(lisp::current(), a); }
inline LISPT caadr(lisp& l, LISPT a) { return prim::caadr(l, a); }
inline LISPT caadr(LISPT a) { return prim::caadr(lisp::current(), a); }
inline LISPT cddar(lisp& l, LISPT a) { return prim::cddar(l, a); }
inline LISPT cddar(LISPT a) { return prim::cddar(lisp::current(), a); }
inline LISPT cadar(lisp& l, LISPT a) { return prim::cadar(l, a); }
inline LISPT cadar(LISPT a) { return prim::cadar(lisp::current(), a); }
inline LISPT cdaar(lisp& l, LISPT a) { return prim::cdaar(l, a); }
inline LISPT cdaar(LISPT a) { return prim::cdaar(lisp::current(), a); }
inline LISPT caaar(lisp& l, LISPT a) { return prim::caaar(l, a); }
inline LISPT caaar(LISPT a) { return prim::caaar(lisp::current(), a); }
inline LISPT eq(lisp& l, LISPT a, LISPT b) { return prim::eq(l, a, b); }
inline LISPT eq(LISPT a, LISPT b) { return prim::eq(lisp::current(), a, b); }
inline LISPT atom(lisp& l, LISPT a) { return prim::atom(l, a); }
inline LISPT atom(LISPT a) { return prim::atom(lisp::current(), a); }
inline LISPT nconc(lisp& l, LISPT a) { return prim::nconc(l, a); }
inline LISPT nconc(LISPT a) { return prim::nconc(lisp::current(), a); }
inline LISPT attach(lisp& l, LISPT a, LISPT b) { return prim::attach(l, a, b); }
inline LISPT attach(LISPT a, LISPT b) { return prim::attach(lisp::current(), a, b); }
inline LISPT null(lisp& l, LISPT a) { return prim::null(l, a); }
inline LISPT null(LISPT a) { return prim::null(lisp::current(), a); }
inline LISPT quote(lisp& l, LISPT a) { return prim::quote(l, a); }
inline LISPT quote(LISPT a) { return prim::quote(lisp::current(), a); }
inline LISPT lambda(lisp& l, LISPT a, LISPT b) { return prim::lambda(l, a, b); }
inline LISPT lambda(LISPT a, LISPT b) { return prim::lambda(lisp::current(), a, b); }
inline LISPT nlambda(lisp& l, LISPT a, LISPT b) { return prim::nlambda(l, a, b); }
inline LISPT nlambda(LISPT a, LISPT b) { return prim::nlambda(lisp::current(), a, b); }
inline LISPT list(lisp& l, LISPT a) { return prim::list(l, a); }
inline LISPT list(LISPT a) { return prim::list(lisp::current(), a); }
inline LISPT length(lisp& l, LISPT a) { return prim::length(l, a); }
inline LISPT length(LISPT a) { return prim::length(lisp::current(), a); }
inline LISPT closure(lisp& l, LISPT a, LISPT b) { return prim::closure(l, a, b); }
inline LISPT closure(LISPT a, LISPT b) { return prim::closure(lisp::current(), a, b); }
inline LISPT nth(lisp& l, LISPT a, LISPT b) { return prim::nth(l, a, b); }
inline LISPT nth(LISPT a, LISPT b) { return prim::nth(lisp::current(), a, b); }
inline LISPT nthd(lisp& l, LISPT a, LISPT b) { return prim::nthd(l, a, b); }
inline LISPT nthd(LISPT a, LISPT b) { return prim::nthd(lisp::current(), a, b); }
inline LISPT error(lisp& l, LISPT a) { return prim::error(l, a); }
inline LISPT error(LISPT a) { return prim::error(lisp::current(), a); }
inline LISPT uxexit(lisp& l, LISPT a) { return prim::uxexit(l, a); }
inline LISPT uxexit(LISPT a) { return prim::uxexit(lisp::current(), a); }

inline LISPT rplaca(lisp& l, LISPT a, LISPT b) { return prim::rplaca(l, a, b); }
inline LISPT rplaca(LISPT a, LISPT b) { return prim::rplaca(lisp::current(), a, b); }
inline LISPT rplacd(lisp& l, LISPT a, LISPT b) { return prim::rplacd(l, a, b); }
inline LISPT rplacd(LISPT a, LISPT b) { return prim::rplacd(lisp::current(), a, b); }
inline LISPT append(lisp& l, LISPT a) { return prim::append(l, a); }
inline LISPT append(LISPT a) { return prim::append(lisp::current(), a); }
inline LISPT tconc(lisp& l, LISPT a, LISPT b) { return prim::tconc(l, a, b); }
inline LISPT tconc(LISPT a, LISPT b) { return prim::tconc(lisp::current(), a, b); }

} // namespace lisp
