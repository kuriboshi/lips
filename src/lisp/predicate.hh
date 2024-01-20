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

/// @file predicate.hh
///
/// # Predicates

#include "context.hh"
#include "types.hh"
#include "details/predicate.hh"

namespace lisp
{
/// @brief `t` if the objects _a_ and _b_ are the same object. Integers are
/// considered 'eq' if their values are the same.
/// @lisp{(eq a b),Function}
inline lisp_t eq(const lisp_t& a, const lisp_t& b) { return details::predicate::eq(a, b); }
/// @brief `t` if _a_ is `nil`, `t`, a symbol, an integer, or a floating point
/// object.
/// @lisp{(atom a),Function}
inline lisp_t atom(const lisp_t& a) { return details::predicate::atom(a); }
/// @brief Returns T if argument is a number (either an integer or a floating
/// point value).
/// @lisp{(numberp n),Function}
inline lisp_t numberp(const lisp_t& n) { return details::predicate::numberp(n); }
/// @brief Returns T if the argument is a list, i.e. a cons cell.
/// @lisp{(listp l),Function}
inline lisp_t listp(const lisp_t& l) { return details::predicate::listp(l); }
/// @brief Compares each CAR of the _list_ and if _eq_ returns the list at that
/// point.
/// @lisp{(memb atom list),Function}
///
/// Looks for an element _atom_ in _list_ using `eq`, returning the tail with
/// that element at the head. Returns `nil` if not found.
///
/// ```lisp
/// (memb 'a '(a b c))
///   => (a b c)
/// (memb 'b '(a b c))
///   => (b c)
/// (memb 'd '(a b c))
///   => nil
/// ```
inline lisp_t memb(const lisp_t& atom, const lisp_t& list) { return details::predicate::memb(atom, list); }
/// @brief Returns T if the two lisp expressions are equal.
/// @lisp{(equal l1 lf2),Function}
///
/// Returns `t` if _x_ and _y_ are `eq`, or if _x_ and _y_ are `eqp`, or if _x_
/// and _y_ are `strequal`, or if _x_ and _y_ are lists `(and (equal (car x)
/// (car x)) (equal (cdr x) (cdr y)))`.
inline lisp_t equal(const lisp_t& x, const lisp_t& y) { return details::predicate::equal(x, y); }
/// @brief Returns T if the expression is not a cons cell.
/// @lisp{(nlistp a),Function}
///
/// `t` if _x_ is not a list, otherwise `nil`. Same as `(not (listp x))`.
inline lisp_t nlistp(const lisp_t& a) { return details::predicate::nlistp(a); }
/// @brief Returns T of a and b are not the same object.
/// @lisp{(neq a b),Function}
inline lisp_t neq(const lisp_t& a, const lisp_t& b) { return details::predicate::neq(a, b); }
/// @brief Returns T if the symbol is unbound.
/// @lisp{(boundp a),Function}
inline lisp_t boundp(const lisp_t& a) { return details::predicate::boundp(a); }
/// @brief Returns T if the value is a symbol or nil.
/// @lisp{(litatom a),Function}
/// @lisp{(symbolp a),Function}
///
/// The function `symbolp` is an alias for `litatom`.
inline lisp_t litatom(const lisp_t& a) { return details::predicate::litatom(a); }
/// @brief Returns the type of an expression as a symbol.
/// @lisp{(typeof a),Function}
///
/// Returns a symbol depending on the type of the argument.
///
/// | Symbol    | Type                     |
/// |-----------|--------------------------|
/// | symbol    | Symbol                   |
/// | integer   | Integer                  |
/// | float     | Floating point           |
/// | indirect  | Indirect value           |
/// | cons      | Cons cell / list         |
/// | string    | String                   |
/// | subr      | Compiled function        |
/// | fsubr     | Compiled noeval function |
/// | lambda    | Lambda function          |
/// | nlambda   | NLambda function         |
/// | closure   | Closure                  |
/// | environ   | Environment              |
/// | file      | File                     |
/// | cvariable | C/++ Variable            |
inline lisp_t xtypeof(const lisp_t& a) { return details::predicate::xtypeof(a); }
} // namespace lisp

#endif
