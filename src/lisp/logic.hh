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

#ifndef LISP_LOGIC_HH
#define LISP_LOGIC_HH

/// @file logic.hh
///
/// ### Logical Functions

#include "types.hh"
#include "details/logic.hh"

namespace lisp
{

/// @brief `(and args...)` (_NoSpread NLambda_)
///
/// If any expression evaluates to nil return `nil` otherwise return the result
/// of the last expression.
inline lisp_t p_and(lisp_t args) { return details::logic::p_and(args); }
/// @brief `(or args...)` (_NoSpread NLambda_)
///
/// Returns the first expression evaluating to non-`nil`, otherwise return
/// `nil`.
inline lisp_t p_or(lisp_t args) { return details::logic::p_or(args); }
/// @brief `(not expr)` (_Function_)
///
/// Returns `t` if argument is `nil`, `nil` otherwise.
inline lisp_t p_not(lisp_t expr) { return details::logic::p_not(expr); }
/// @brief `(if p t . f)` (_NLambda_)
///
/// If the predicate _p_ evaluates to a non-`nil`\ value the expression
/// _t_ is evaluated and returned from the function. If _p_ evaluates to
/// `nil` then the value of the expression _f_ is returned.
} // namespace lisp

#endif
