//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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
/// @brief The car of the cons cell.
inline lisp_t car(lisp_t a) { return details::prim::car(a); }
/// @brief The cdr of the cons cell.
inline lisp_t cdr(lisp_t a) { return details::prim::cdr(a); }
/// @brief Same as (car (cdr a))
inline lisp_t cadr(lisp_t a) { return details::prim::cadr(a); }
/// @brief Same as (cdr (car a))
inline lisp_t cdar(lisp_t a) { return details::prim::cdar(a); }
/// @brief Same as (car (car a))
inline lisp_t caar(lisp_t a) { return details::prim::caar(a); }
/// @brief Same as (cdr (cdr a))
inline lisp_t cddr(lisp_t a) { return details::prim::cddr(a); }
/// @brief Same as (cdr (cdr (cdr a)))
inline lisp_t cdddr(lisp_t a) { return details::prim::cdddr(a); }
/// @brief Same as (car (cdr (cdr a)))
inline lisp_t caddr(lisp_t a) { return details::prim::caddr(a); }
/// @brief Same as (cdr (car (cdr a)))
inline lisp_t cdadr(lisp_t a) { return details::prim::cdadr(a); }
/// @brief Same as (car (car (cdr a)))
inline lisp_t caadr(lisp_t a) { return details::prim::caadr(a); }
/// @brief Same as (cdr (cdr (car a)))
inline lisp_t cddar(lisp_t a) { return details::prim::cddar(a); }
/// @brief Same as (car (cdr (car a)))
inline lisp_t cadar(lisp_t a) { return details::prim::cadar(a); }
/// @brief Same as (cdr (car (car a)))
inline lisp_t cdaar(lisp_t a) { return details::prim::cdaar(a); }
/// @brief Same as (car (car (car a)))
inline lisp_t caaar(lisp_t a) { return details::prim::caaar(a); }
/// @brief T if the objects a and b are the same object. Integers are
/// considered 'eq' if their values are the same.
inline lisp_t eq(lisp_t a, lisp_t b) { return details::prim::eq(a, b); }
/// @brief T if a is nil, t, a symbol, an integer, or a floating point object.
inline lisp_t atom(lisp_t a) { return details::prim::atom(a); }
inline lisp_t nconc(lisp_t a) { return details::prim::nconc(a); }
inline lisp_t attach(lisp_t a, lisp_t b) { return details::prim::attach(a, b); }
inline lisp_t null(lisp_t a) { return details::prim::null(a); }
inline lisp_t quote(lisp_t a) { return details::prim::quote(a); }
inline lisp_t lambda(lisp_t a, lisp_t b) { return details::prim::lambda(a, b); }
inline lisp_t nlambda(lisp_t a, lisp_t b) { return details::prim::nlambda(a, b); }
inline lisp_t list(lisp_t a) { return details::prim::list(a); }
inline lisp_t length(lisp_t a) { return details::prim::length(a); }
inline lisp_t closure(lisp_t a, lisp_t b) { return details::prim::closure(a, b); }
inline lisp_t nth(lisp_t a, lisp_t b) { return details::prim::nth(a, b); }
inline lisp_t error(lisp_t a) { return details::prim::error(a); }

inline lisp_t rplaca(lisp_t a, lisp_t b) { return details::prim::rplaca(a, b); }
inline lisp_t rplacd(lisp_t a, lisp_t b) { return details::prim::rplacd(a, b); }
inline lisp_t append(lisp_t a) { return details::prim::append(a); }
inline lisp_t tconc(lisp_t a, lisp_t b) { return details::prim::tconc(a, b); }
} // namespace lisp

#endif
