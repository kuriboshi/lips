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

#ifndef LISP_PRED_HH
#define LISP_PRED_HH

/// @file pred.hh
///
/// ### Predicates
///

#include "context.hh"
#include "types.hh"
#include "details/pred.hh"

namespace lisp
{
/// @brief Returns T if argument is a number (either an integer or a floating
/// point value).
inline lisp_t numberp(lisp_t a) { return details::pred::numberp(a); }
/// @brief Returns T if the argument is a list, i.e. a cons cell.
inline lisp_t listp(lisp_t a) { return details::pred::listp(a); }
/// @brief Compares each CAR of the @a list and if @a eq returns the list at
/// that point.
///
/// (memb 'b '(a b c)) => (b c)
inline lisp_t memb(lisp_t atom, lisp_t list) { return details::pred::memb(atom, list); }
/// @brief Returns T if the two lisp expressions are equal.
inline lisp_t equal(lisp_t l1, lisp_t l2) { return details::pred::equal(l1, l2); }
/// @brief Returns T if the expression is not a cons cell.
inline lisp_t nlistp(lisp_t a) { return details::pred::nlistp(a); }
/// @brief Returns T of a and b are not the same object.
inline lisp_t neq(lisp_t a, lisp_t b) { return details::pred::neq(a, b); }
/// @brief Returns T if the symbol is unbound.
inline lisp_t boundp(lisp_t a) { return details::pred::boundp(a); }
/// @brief Returns T if the value is a symbol or nil.
inline lisp_t litatom(lisp_t a) { return details::pred::litatom(a); }
/// @brief Returns the type of an expression as a symbol.
inline lisp_t xtypeof(lisp_t a) { return details::pred::xtypeof(a); }
} // namespace lisp

#endif
