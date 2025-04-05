//
// Lips, lisp shell.
// Copyright 2020-2025 Krister Joas
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

#pragma once

/// @file logic.hh
///
/// # Logic Functions

#include "types.hh"
#include "details/logic.hh"

namespace lisp
{

/// @brief If any expression evaluates to nil return `nil` otherwise return the
/// result of the last expression.
/// @lisp{(and args...),NoSpread NLambda Function}
inline lisp_t p_and(const lisp_t& args) { return details::logic::p_and(args); }
/// @brief Returns the first expression evaluating to non-`nil`, otherwise
/// return `nil`.
/// @lisp{(or args...),NoSpread NLambda}
inline lisp_t p_or(const lisp_t& args) { return details::logic::p_or(args); }
/// @brief Returns `t` if argument is `nil`, `nil` otherwise.
/// @lisp{(not expr),Function}
inline lisp_t p_not(const lisp_t& expr) { return details::logic::p_not(expr); }
/// @brief Evaluates and returns _t_ if _p_ is non-`nil`, evaluates and returns
/// _f_ otherwise.
/// @lisp{(if p t . f),NLambda}
///
/// If the predicate _p_ evaluates to a non-`nil`\ value the expression
/// _t_ is evaluated and returned from the function. If _p_ evaluates to
/// `nil` then the value of the expression _f_ is returned.
} // namespace lisp
