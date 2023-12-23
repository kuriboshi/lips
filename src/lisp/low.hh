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

#ifndef LISP_LOW_HH
#define LISP_LOW_HH

/// @file low.hh
///
/// ### Low Level Functions

#include "types.hh"
#include "details/low.hh"

namespace lisp
{
/// @brief `(cond ...)` (_NoSpread NLambda_)
///
/// The cond special form.
///
/// The generalized conditional special form. The function takes zero or more
/// clauses. Each clause has one test followed by zero or more expressions
/// called consequents. The function evaluates each test in sequence until one
/// of them is evaluated to true (not `nil`). It then evaluates each consequent
/// in order and returns the value of the last consequent. If there are no
/// consequents the result is the value of the test expression. The degenerate
/// `cond` expression with no clauses at all evaluates to `nil`.
///
/// In the following example the return value is the value of the expression
/// `r0` if `e0` evaluates to non-`nil`, `r2` if `e1` is evaluated to
/// non-`nil`, `e2` if `e2` evaluates to non-`nil`. Finally, if none of the
/// expressions `e0`, `e1`, or `e2` is non-`nil` the final `t` provides a
/// default value. If none of the test expressions evaluate to non-`nil` then
/// the result of the entire `cond` expression is `nil`.
///
/// Note that the expressions after the conditional expressions are evaluated
/// in an implicit `progn` which is why the result of `e1` being non-nil is the
/// value of `r2`.
///
/// ```lisp
/// (cond (e0 r0)
///       (e1 r1 r2)
///       (e2)
///       (t r3))
/// ```
inline lisp_t cond(lisp_t clauses) { return details::low::cond(clauses); }
/// @brief `(prog1 args...)` (_NoSpread NLambda_)
///
/// Evaluates all arguments and returns the result of the first expression.
inline lisp_t prog1(lisp_t a, lisp_t b) { return details::low::prog1(a, b); }
/// @brief `(progn args...)` (_NoSpread NLambda_)
///
/// Evaluates all arguments and retuns the result of the last expression.
inline lisp_t progn(lisp_t a) { return details::low::progn(a); }
/// @brief `(set var expr)` (_Function_)
///
/// Sets the value of the symbol to the value. Both `var` and `expr` are
/// evaluated. Returns `val`.
///
/// @param var An expression which evaluates to a symbol.
/// @param expr An expression.
///
/// @returns The result of evaluating the expression `expr`.
inline lisp_t set(lisp_t var, lisp_t expr) { return details::low::set(var, expr); }
/// @brief `(setq var expr)` (_NLambda_)
///
/// Same as 'set' but the first argument is not evaluated.
///
/// @param var A literal symbol.
/// @param expr An expression.
///
/// @returns The result of evaluating the expression `expr`.
inline lisp_t setq(lisp_t a, lisp_t b) { return details::low::setq(a, b); }
/// @brief `(setqq var val)` (_NLambda_)
///
/// Same as 'set' but no argument is evaluated.
///
/// @param var A literal symbol.
/// @param val A constant expression which is not evaluated.
///
/// @returns The unevaluated `val` expression.
inline lisp_t setqq(lisp_t a, lisp_t b) { return details::low::set(a, b); }
/// @brief `(while first args...)` (_NoSpread NLambda_)
///
/// While the first argument is true evaluate the rest of the arguments in an
/// implicit `progn`. Returns `nil`.
inline lisp_t xwhile(lisp_t first, lisp_t second) { return details::low::xwhile(first, second); }
} // namespace lisp

#endif
