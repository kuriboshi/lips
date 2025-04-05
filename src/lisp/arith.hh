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

/// @file arith.hh
///
/// # Arithmetic Functions
///
/// `lips` supports both integer (currently only 64 bit values are supported)
/// and floating point numbers (double). There are functions specific for
/// either integers or floating points as well as generic functions which can
/// take either type.

#include "types.hh"
#include "details/arith.hh"

namespace lisp
{
/// @brief Returns the absolute value of _n_.
/// @lisp{(abs n),Function}
inline lisp_t abs(const lisp_t& n) { return details::arith::abs(n); }
/// @brief Returns the argument _n_ plus 1.
/// @lisp{(add1 n),Function}
///
/// Same as `(plus n 1)`.
inline lisp_t add1(const lisp_t& n) { return details::arith::add1(n); }
/// @brief Calculates the difference between _x_ and _y_.
/// @lisp{(difference x y),Function}
///
/// ```lisp
/// (difference 8 3)
///   => 5
/// ```
///
/// Returns the difference.
inline lisp_t difference(const lisp_t& x, const lisp_t& y) { return details::arith::difference(x, y); }
/// @brief Divides _x_ by _y_.
/// @lisp{(divide x y),Function}
///
/// The result may be an integer or a floating point depending on the types of
/// _x_ and _y_. If both are integers the result will be an integer and if
/// either _x_ or _y_, ot both, is a floating point number the result will be a
/// floating point number.
///
/// Returns the result of the division.
inline lisp_t divide(const lisp_t& x, const lisp_t& y) { return details::arith::divide(x, y); }
/// @brief Compares two integer or floating point values for equality.
/// @lisp{(eqp x y),Function}
///
/// (Note: This differs from Interlisp which returns `t` if _x_ and _y_ are
/// `eq` or if they are numbers and are equal in value.)
inline lisp_t eqp(const lisp_t& x, const lisp_t& y) { return details::arith::eqp(x, y); }
/// @brief Returns the floating point difference between _x_ and _y_.
/// @lisp{(fdifference x y),Function}
inline lisp_t fdifference(const lisp_t& x, const lisp_t& y) { return details::arith::fdifference(x, y); }
/// @brief Floating point division of _x_ by _y_.
/// @lisp{(fdivide a b),Function}
///
/// Returns the result of the division.
inline lisp_t fdivide(const lisp_t& x, const lisp_t& y) { return details::arith::fdivide(x, y); }
/// @brief Floating point addition of _args_.
/// @lisp{(fplus args...),NoSpread Function}
///
/// Returns the sum of all the arguments.
inline lisp_t fplus(const lisp_t& args) { return details::arith::fplus(args); }
/// @brief Floating point multiplication.
/// @lisp{(ftimes args),NoSpread Function}
///
/// Returns the floating point multiplication of _args_.
inline lisp_t ftimes(const lisp_t& args) { return details::arith::ftimes(args); }
/// @brief Compares if one numeric values are greater or equal to another.
/// @lisp{(geq x y),Function}
///
/// Returns 't' if $x \ge y$, `nil` otherwise.
inline lisp_t geq(const lisp_t& x, const lisp_t& y) { return details::arith::geq(x, y); }
/// @brief Compares of one numeric value is greater than another.
/// @lisp{(greaterp x y),Function}
///
/// Returns `t` if $x > y$, `nil` otherwise.
inline lisp_t greaterp(const lisp_t& x, const lisp_t& y) { return details::arith::greaterp(x, y); }
/// @brief Integer subtraction.
/// @lisp{(idifference a b),Function}
inline lisp_t idifference(const lisp_t& a, const lisp_t& b) { return details::arith::idifference(a, b); }
/// @brief Returns the _a_ with the opposite sign.
/// @lisp{(iminus a),Function}
///
/// Same as `(idifference 0 a)`.
inline lisp_t iminus(const lisp_t& a) { return details::arith::iminus(a); }
/// @brief Returns the sum of the arguments.
/// @lisp{(iplus args...),NoSpread Function}
inline lisp_t iplus(const lisp_t& args) { return details::arith::iplus(args); }
/// @brief Returns the truncated result of $a / b$.
/// @lisp{(iquotient a b),Function}
///
/// ```lisp
/// (iquotient 3 2) => 1
/// (iquotient -3 2) => -1
/// ```
inline lisp_t iquotient(const lisp_t& a, const lisp_t& b) { return details::arith::iquotient(a, b); }
/// @brief Returns the remainder of $a / b$.
/// @lisp{(iremainder a b),Function}
///
/// ```lisp
/// (iremainder 3 2) => 1
/// ```
inline lisp_t iremainder(const lisp_t& a, const lisp_t& b) { return details::arith::iremainder(a, b); }
/// @brief Returns the result of multiplying the arguments.
/// @lisp{(itimes args...),NoSpread Function}
///
/// ```lisp
/// (itimes 1 2 3 4) => 24
/// ```
inline lisp_t itimes(const lisp_t& args) { return details::arith::itimes(args); }
/// @brief Returns the integer _i_ converted to a floating point value.
/// @lisp{(itof i),Function}
inline lisp_t itof(const lisp_t& i) { return details::arith::itof(i); }
/// @brief Returns `t` if $a \le b$, `nil` otherwise.
/// @lisp{(leq x y),Function}
inline lisp_t leq(const lisp_t& x, const lisp_t& y) { return details::arith::leq(x, y); }
/// @brief Returns `t` if $a < b$, `nil` otherwise.
/// @lisp{(lessp x y),Function}
inline lisp_t lessp(const lisp_t& x, const lisp_t& y) { return details::arith::lessp(x, y); }
/// @brief Returns the result of multiplying the arguments.
/// @lisp{(times args...),NoSpread Function}
inline lisp_t times(const lisp_t& args) { return details::arith::times(args); }
/// @brief Returns _a_ with the opposite sign.
/// @lisp{(minus a),Function}
inline lisp_t minus(const lisp_t& a) { return details::arith::minus(a); }
/// @brief Returns `t` if _x_ is a negative number, `nil` otherwise.
/// @lisp{(minusp x),Function}
inline lisp_t minusp(const lisp_t& x) { return details::arith::minusp(x); }
/// @brief Returns `t` if $x \neq y$, `nil` otherwise.
/// @lisp{(neqp x y),Function}
inline lisp_t neqp(const lisp_t& x, const lisp_t& y) { return details::arith::neqp(x, y); }
/// @brief Returns the sum of all _args_.
/// @lisp{(plus args...),NoSpread Function}
inline lisp_t plus(const lisp_t& args) { return details::arith::plus(args); }
/// @brief Returns $a - 1$.
/// @lisp{(sub1 a),Function}
///
/// Same as `(difference a 1)`.
inline lisp_t sub1(const lisp_t& a) { return details::arith::sub1(a); }
/// @brief Returns `t` if $x = 0$, `nil` otherwise.
/// @lisp{(zerop x),Function}
inline lisp_t zerop(const lisp_t& x) { return details::arith::zerop(x); }
} // namespace lisp
