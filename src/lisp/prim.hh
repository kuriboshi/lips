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

inline LISPT car(LISPT a) { return details::prim::car(context::current(), a); }
inline LISPT cdr(LISPT a) { return details::prim::cdr(context::current(), a); }
inline LISPT cadr(LISPT a) { return details::prim::cadr(context::current(), a); }
inline LISPT cdar(LISPT a) { return details::prim::cdar(context::current(), a); }
inline LISPT caar(LISPT a) { return details::prim::caar(context::current(), a); }
inline LISPT cddr(LISPT a) { return details::prim::cddr(context::current(), a); }
inline LISPT cdddr(LISPT a) { return details::prim::cdddr(context::current(), a); }
inline LISPT caddr(LISPT a) { return details::prim::caddr(context::current(), a); }
inline LISPT cdadr(LISPT a) { return details::prim::cdadr(context::current(), a); }
inline LISPT caadr(LISPT a) { return details::prim::caadr(context::current(), a); }
inline LISPT cddar(LISPT a) { return details::prim::cddar(context::current(), a); }
inline LISPT cadar(LISPT a) { return details::prim::cadar(context::current(), a); }
inline LISPT cdaar(LISPT a) { return details::prim::cdaar(context::current(), a); }
inline LISPT caaar(LISPT a) { return details::prim::caaar(context::current(), a); }
inline LISPT eq(LISPT a, LISPT b) { return details::prim::eq(context::current(), a, b); }
inline LISPT atom(LISPT a) { return details::prim::atom(context::current(), a); }
inline LISPT nconc(LISPT a) { return details::prim::nconc(context::current(), a); }
inline LISPT attach(LISPT a, LISPT b) { return details::prim::attach(context::current(), a, b); }
inline LISPT null(LISPT a) { return details::prim::null(context::current(), a); }
inline LISPT quote(LISPT a) { return details::prim::quote(context::current(), a); }
inline LISPT lambda(LISPT a, LISPT b) { return details::prim::lambda(context::current(), a, b); }
inline LISPT nlambda(LISPT a, LISPT b) { return details::prim::nlambda(context::current(), a, b); }
inline LISPT list(LISPT a) { return details::prim::list(context::current(), a); }
inline LISPT length(LISPT a) { return details::prim::length(context::current(), a); }
inline LISPT closure(LISPT a, LISPT b) { return details::prim::closure(context::current(), a, b); }
inline LISPT nth(LISPT a, LISPT b) { return details::prim::nth(context::current(), a, b); }
inline LISPT error(LISPT a) { return details::prim::error(context::current(), a); }
inline LISPT uxexit(LISPT a) { return details::prim::uxexit(context::current(), a); }

inline LISPT rplaca(LISPT a, LISPT b) { return details::prim::rplaca(context::current(), a, b); }
inline LISPT rplacd(LISPT a, LISPT b) { return details::prim::rplacd(context::current(), a, b); }
inline LISPT append(LISPT a) { return details::prim::append(context::current(), a); }
inline LISPT tconc(LISPT a, LISPT b) { return details::prim::tconc(context::current(), a, b); }
} // namespace lisp

#endif
