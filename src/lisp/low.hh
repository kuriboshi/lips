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

#include "types.hh"
#include "details/low.hh"

namespace lisp
{
/// @brief The cond special form.
///
/// The generalized conditional special form. The function takes zero or more
/// clauses. Each clause has one test followed by zero or more expressions
/// called consequents. The function evaluates each test in sequence until one
/// of them is evaluated to true (not @c nil). It then evaluates each
/// consequent in order and returns the value of the last consequent. If there
/// are no consequents the result is the value of the test expression. The
/// degenerate @c cond expression with no clauses at all evaluates to @c nil.
///
/// In the following example the return value is the value of the expression
/// @c r0 if @c e0 evaluates to non-nil, @c r2 if @c e1 is evaluated to non-nil,
/// @c e2 if @c e2 evaluates to non-nil. Finally, if none of the expressions
/// @c e0, @c e1, or @c e2 is non-nil the final @c t provides a default
/// value. If none of the test expressions evaluate to non-nil then the result
/// of the entire @c cond expression is @c nil.
///
/// Note that the expressions after the conditional expressions are evaluated
/// in an implicit @c progn which is why the result of @c e1 being non-nil is
/// the value of @c r2.
///
/// @code{.lisp}
/// (cond (e0 r0)
///       (e1 r1 r2)
///       (e2)
///       (t r3))
/// @endcode
///
inline lisp_t cond(lisp_t a) { return details::low::cond(a); }
/// @brief Evaluates all arguments and returns the result of the first
/// expression.
inline lisp_t prog1(lisp_t a, lisp_t b) { return details::low::prog1(a, b); }
/// @brief Evaluates all arguments and retuns the result of the last
/// expression.
inline lisp_t progn(lisp_t a) { return details::low::progn(a); }
/// @brief Sets the value of the symbol to the value.
inline lisp_t set(lisp_t a, lisp_t b) { return details::low::set(a, b); }
/// @brief Same as 'set' but the first argument is not evaluated.
inline lisp_t setq(lisp_t a, lisp_t b) { return details::low::setq(a, b); }
/// @brief Same as 'set' but no argument is evaluated.
inline lisp_t setqq(lisp_t a, lisp_t b) { return details::low::set(a, b); }
/// @brief While the first argument is true evaluate the second.
inline lisp_t xwhile(lisp_t first, lisp_t second) { return details::low::xwhile(first, second); }
} // namespace lisp

#endif
