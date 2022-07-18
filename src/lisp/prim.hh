//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#ifndef LISP_PRIM_HH
#define LISP_PRIM_HH

#include "lisp.hh"
#include "details/prim.hh"

namespace lisp
{
extern LISPT C_ERROR;
extern LISPT C_LAMBDA;
extern LISPT C_NLAMBDA;
extern LISPT C_QUOTE;

inline LISPT car(lisp& l, LISPT a) { return details::prim::car(l, a); }
inline LISPT car(LISPT a) { return details::prim::car(lisp::current(), a); }
inline LISPT cdr(lisp& l, LISPT a) { return details::prim::cdr(l, a); }
inline LISPT cdr(LISPT a) { return details::prim::cdr(lisp::current(), a); }
inline LISPT cadr(lisp& l, LISPT a) { return details::prim::cadr(l, a); }
inline LISPT cadr(LISPT a) { return details::prim::cadr(lisp::current(), a); }
inline LISPT cdar(lisp& l, LISPT a) { return details::prim::cdar(l, a); }
inline LISPT cdar(LISPT a) { return details::prim::cdar(lisp::current(), a); }
inline LISPT caar(lisp& l, LISPT a) { return details::prim::caar(l, a); }
inline LISPT caar(LISPT a) { return details::prim::caar(lisp::current(), a); }
inline LISPT cddr(lisp& l, LISPT a) { return details::prim::cddr(l, a); }
inline LISPT cddr(LISPT a) { return details::prim::cddr(lisp::current(), a); }
inline LISPT cdddr(lisp& l, LISPT a) { return details::prim::cdddr(l, a); }
inline LISPT cdddr(LISPT a) { return details::prim::cdddr(lisp::current(), a); }
inline LISPT caddr(lisp& l, LISPT a) { return details::prim::caddr(l, a); }
inline LISPT caddr(LISPT a) { return details::prim::caddr(lisp::current(), a); }
inline LISPT cdadr(lisp& l, LISPT a) { return details::prim::cdadr(l, a); }
inline LISPT cdadr(LISPT a) { return details::prim::cdadr(lisp::current(), a); }
inline LISPT caadr(lisp& l, LISPT a) { return details::prim::caadr(l, a); }
inline LISPT caadr(LISPT a) { return details::prim::caadr(lisp::current(), a); }
inline LISPT cddar(lisp& l, LISPT a) { return details::prim::cddar(l, a); }
inline LISPT cddar(LISPT a) { return details::prim::cddar(lisp::current(), a); }
inline LISPT cadar(lisp& l, LISPT a) { return details::prim::cadar(l, a); }
inline LISPT cadar(LISPT a) { return details::prim::cadar(lisp::current(), a); }
inline LISPT cdaar(lisp& l, LISPT a) { return details::prim::cdaar(l, a); }
inline LISPT cdaar(LISPT a) { return details::prim::cdaar(lisp::current(), a); }
inline LISPT caaar(lisp& l, LISPT a) { return details::prim::caaar(l, a); }
inline LISPT caaar(LISPT a) { return details::prim::caaar(lisp::current(), a); }
inline LISPT eq(lisp& l, LISPT a, LISPT b) { return details::prim::eq(l, a, b); }
inline LISPT eq(LISPT a, LISPT b) { return details::prim::eq(lisp::current(), a, b); }
inline LISPT atom(lisp& l, LISPT a) { return details::prim::atom(l, a); }
inline LISPT atom(LISPT a) { return details::prim::atom(lisp::current(), a); }
inline LISPT nconc(lisp& l, LISPT a) { return details::prim::nconc(l, a); }
inline LISPT nconc(LISPT a) { return details::prim::nconc(lisp::current(), a); }
inline LISPT attach(lisp& l, LISPT a, LISPT b) { return details::prim::attach(l, a, b); }
inline LISPT attach(LISPT a, LISPT b) { return details::prim::attach(lisp::current(), a, b); }
inline LISPT null(lisp& l, LISPT a) { return details::prim::null(l, a); }
inline LISPT null(LISPT a) { return details::prim::null(lisp::current(), a); }
inline LISPT quote(lisp& l, LISPT a) { return details::prim::quote(l, a); }
inline LISPT quote(LISPT a) { return details::prim::quote(lisp::current(), a); }
inline LISPT lambda(lisp& l, LISPT a, LISPT b) { return details::prim::lambda(l, a, b); }
inline LISPT lambda(LISPT a, LISPT b) { return details::prim::lambda(lisp::current(), a, b); }
inline LISPT nlambda(lisp& l, LISPT a, LISPT b) { return details::prim::nlambda(l, a, b); }
inline LISPT nlambda(LISPT a, LISPT b) { return details::prim::nlambda(lisp::current(), a, b); }
inline LISPT list(lisp& l, LISPT a) { return details::prim::list(l, a); }
inline LISPT list(LISPT a) { return details::prim::list(lisp::current(), a); }
inline LISPT length(lisp& l, LISPT a) { return details::prim::length(l, a); }
inline LISPT length(LISPT a) { return details::prim::length(lisp::current(), a); }
inline LISPT closure(lisp& l, LISPT a, LISPT b) { return details::prim::closure(l, a, b); }
inline LISPT closure(LISPT a, LISPT b) { return details::prim::closure(lisp::current(), a, b); }
inline LISPT nth(lisp& l, LISPT a, LISPT b) { return details::prim::nth(l, a, b); }
inline LISPT nth(LISPT a, LISPT b) { return details::prim::nth(lisp::current(), a, b); }
inline LISPT error(lisp& l, LISPT a) { return details::prim::error(l, a); }
inline LISPT error(LISPT a) { return details::prim::error(lisp::current(), a); }
inline LISPT uxexit(lisp& l, LISPT a) { return details::prim::uxexit(l, a); }
inline LISPT uxexit(LISPT a) { return details::prim::uxexit(lisp::current(), a); }

inline LISPT rplaca(lisp& l, LISPT a, LISPT b) { return details::prim::rplaca(l, a, b); }
inline LISPT rplaca(LISPT a, LISPT b) { return details::prim::rplaca(lisp::current(), a, b); }
inline LISPT rplacd(lisp& l, LISPT a, LISPT b) { return details::prim::rplacd(l, a, b); }
inline LISPT rplacd(LISPT a, LISPT b) { return details::prim::rplacd(lisp::current(), a, b); }
inline LISPT append(lisp& l, LISPT a) { return details::prim::append(l, a); }
inline LISPT append(LISPT a) { return details::prim::append(lisp::current(), a); }
inline LISPT tconc(lisp& l, LISPT a, LISPT b) { return details::prim::tconc(l, a, b); }
inline LISPT tconc(LISPT a, LISPT b) { return details::prim::tconc(lisp::current(), a, b); }
} // namespace lisp

#endif
