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

#include "types.hh"
#include "details/prim.hh"

namespace lisp
{
extern lisp_t C_ERROR;
extern lisp_t C_LAMBDA;
extern lisp_t C_NLAMBDA;
extern lisp_t C_QUOTE;

inline lisp_t car(lisp_t a) { return details::prim::car(context::current(), a); }
inline lisp_t cdr(lisp_t a) { return details::prim::cdr(context::current(), a); }
inline lisp_t cadr(lisp_t a) { return details::prim::cadr(context::current(), a); }
inline lisp_t cdar(lisp_t a) { return details::prim::cdar(context::current(), a); }
inline lisp_t caar(lisp_t a) { return details::prim::caar(context::current(), a); }
inline lisp_t cddr(lisp_t a) { return details::prim::cddr(context::current(), a); }
inline lisp_t cdddr(lisp_t a) { return details::prim::cdddr(context::current(), a); }
inline lisp_t caddr(lisp_t a) { return details::prim::caddr(context::current(), a); }
inline lisp_t cdadr(lisp_t a) { return details::prim::cdadr(context::current(), a); }
inline lisp_t caadr(lisp_t a) { return details::prim::caadr(context::current(), a); }
inline lisp_t cddar(lisp_t a) { return details::prim::cddar(context::current(), a); }
inline lisp_t cadar(lisp_t a) { return details::prim::cadar(context::current(), a); }
inline lisp_t cdaar(lisp_t a) { return details::prim::cdaar(context::current(), a); }
inline lisp_t caaar(lisp_t a) { return details::prim::caaar(context::current(), a); }
inline lisp_t eq(lisp_t a, lisp_t b) { return details::prim::eq(context::current(), a, b); }
inline lisp_t atom(lisp_t a) { return details::prim::atom(context::current(), a); }
inline lisp_t nconc(lisp_t a) { return details::prim::nconc(context::current(), a); }
inline lisp_t attach(lisp_t a, lisp_t b) { return details::prim::attach(context::current(), a, b); }
inline lisp_t null(lisp_t a) { return details::prim::null(context::current(), a); }
inline lisp_t quote(lisp_t a) { return details::prim::quote(context::current(), a); }
inline lisp_t lambda(lisp_t a, lisp_t b) { return details::prim::lambda(context::current(), a, b); }
inline lisp_t nlambda(lisp_t a, lisp_t b) { return details::prim::nlambda(context::current(), a, b); }
inline lisp_t list(lisp_t a) { return details::prim::list(context::current(), a); }
inline lisp_t length(lisp_t a) { return details::prim::length(context::current(), a); }
inline lisp_t closure(lisp_t a, lisp_t b) { return details::prim::closure(context::current(), a, b); }
inline lisp_t nth(lisp_t a, lisp_t b) { return details::prim::nth(context::current(), a, b); }
inline lisp_t error(lisp_t a) { return details::prim::error(context::current(), a); }

inline lisp_t rplaca(lisp_t a, lisp_t b) { return details::prim::rplaca(context::current(), a, b); }
inline lisp_t rplacd(lisp_t a, lisp_t b) { return details::prim::rplacd(context::current(), a, b); }
inline lisp_t append(lisp_t a) { return details::prim::append(context::current(), a); }
inline lisp_t tconc(lisp_t a, lisp_t b) { return details::prim::tconc(context::current(), a, b); }
} // namespace lisp

#endif
