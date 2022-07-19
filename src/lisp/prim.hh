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

inline LISPT car(LISPT a) { return details::prim::car(lisp::current(), a); }
inline LISPT cdr(LISPT a) { return details::prim::cdr(lisp::current(), a); }
inline LISPT cadr(LISPT a) { return details::prim::cadr(lisp::current(), a); }
inline LISPT cdar(LISPT a) { return details::prim::cdar(lisp::current(), a); }
inline LISPT caar(LISPT a) { return details::prim::caar(lisp::current(), a); }
inline LISPT cddr(LISPT a) { return details::prim::cddr(lisp::current(), a); }
inline LISPT cdddr(LISPT a) { return details::prim::cdddr(lisp::current(), a); }
inline LISPT caddr(LISPT a) { return details::prim::caddr(lisp::current(), a); }
inline LISPT cdadr(LISPT a) { return details::prim::cdadr(lisp::current(), a); }
inline LISPT caadr(LISPT a) { return details::prim::caadr(lisp::current(), a); }
inline LISPT cddar(LISPT a) { return details::prim::cddar(lisp::current(), a); }
inline LISPT cadar(LISPT a) { return details::prim::cadar(lisp::current(), a); }
inline LISPT cdaar(LISPT a) { return details::prim::cdaar(lisp::current(), a); }
inline LISPT caaar(LISPT a) { return details::prim::caaar(lisp::current(), a); }
inline LISPT eq(LISPT a, LISPT b) { return details::prim::eq(lisp::current(), a, b); }
inline LISPT atom(LISPT a) { return details::prim::atom(lisp::current(), a); }
inline LISPT nconc(LISPT a) { return details::prim::nconc(lisp::current(), a); }
inline LISPT attach(LISPT a, LISPT b) { return details::prim::attach(lisp::current(), a, b); }
inline LISPT null(LISPT a) { return details::prim::null(lisp::current(), a); }
inline LISPT quote(LISPT a) { return details::prim::quote(lisp::current(), a); }
inline LISPT lambda(LISPT a, LISPT b) { return details::prim::lambda(lisp::current(), a, b); }
inline LISPT nlambda(LISPT a, LISPT b) { return details::prim::nlambda(lisp::current(), a, b); }
inline LISPT list(LISPT a) { return details::prim::list(lisp::current(), a); }
inline LISPT length(LISPT a) { return details::prim::length(lisp::current(), a); }
inline LISPT closure(LISPT a, LISPT b) { return details::prim::closure(lisp::current(), a, b); }
inline LISPT nth(LISPT a, LISPT b) { return details::prim::nth(lisp::current(), a, b); }
inline LISPT error(LISPT a) { return details::prim::error(lisp::current(), a); }
inline LISPT uxexit(LISPT a) { return details::prim::uxexit(lisp::current(), a); }

inline LISPT rplaca(LISPT a, LISPT b) { return details::prim::rplaca(lisp::current(), a, b); }
inline LISPT rplacd(LISPT a, LISPT b) { return details::prim::rplacd(lisp::current(), a, b); }
inline LISPT append(LISPT a) { return details::prim::append(lisp::current(), a); }
inline LISPT tconc(LISPT a, LISPT b) { return details::prim::tconc(lisp::current(), a, b); }
} // namespace lisp

#endif
