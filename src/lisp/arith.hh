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

#ifndef LISP_ARITH_HH
#define LISP_ARITH_HH

/// @file arith.hh
///
/// ### Arithmetic Functions
///
/// `lips` supports both integer and floating point numbers. There are
/// functions specific for either integers or floating points as well as
/// generic functions which can take either type.

#include "types.hh"
#include "details/arith.hh"

namespace lisp
{
/// @brief `(abs n)` (_Function_)
///
/// Returns the absolute value of _n_.
inline lisp_t abs(lisp_t n) { return details::arith::abs(n); }
/// @brief `(add1 n)` (_Function_)
///
/// `(plus n 1)`. Returns the argument _n_ plus 1.
inline lisp_t add1(lisp_t n) { return details::arith::add1(n); }
/// @brief `(difference x y)` (_Function_)
///
/// Calculates the difference between _x_ and _y_.
///
/// ```lisp
/// (difference 8 3)
///   => 5
/// ```
///
/// Returns the difference.
inline lisp_t difference(lisp_t x, lisp_t y) { return details::arith::difference(x, y); }
/// @brief `(divide x y)` (_Function_)
///
/// Divides _x_ by _y_.  The result may be an integer or a floating point
/// depending on the types of _x_ and _y_. If both are integers the result will
/// be an integer and if either _x_ or _y_, ot both, is a floating point number
/// the result will be a floating point number.
///
/// Returns the result of the division.
inline lisp_t divide(lisp_t x, lisp_t y) { return details::arith::divide(x, y); }
/// @brief `(eqp x y)` (_Function_)
inline lisp_t eqp(lisp_t x, lisp_t y) { return details::arith::eqp(x, y); }
/// @brief `(fdifference x y)` (_Function_)
///
/// Returns the floating point difference between _x_ and _y_.
inline lisp_t fdifference(lisp_t x, lisp_t y) { return details::arith::fdifference(x, y); }
/// @brief `(fdivide a b)` (_Function_)
///
/// Floating point division of _x_ by _y_.
///
/// Returns the result of the division.
inline lisp_t fdivide(lisp_t x, lisp_t y) { return details::arith::fdivide(x, y); }
/// @brief `(fplus args...)` (_NoSpread Function_)
///
/// Floating point addition of _args_.
///
/// Returns the sum of all the arguments.
inline lisp_t fplus(lisp_t args) { return details::arith::fplus(args); }
/// @brief `(ftimes args)` (_NoSpread Function_)
///
/// Returns the floating point multiplication of _args_.
inline lisp_t ftimes(lisp_t args) { return details::arith::ftimes(args); }
/// @brief `(geq x y)` (_Function_)
///
/// Returns 't' if $x \ge y$, `nil` otherwise.
inline lisp_t geq(lisp_t x, lisp_t y) { return details::arith::geq(x, y); }
/// @brief `(greaterp x y)` (_Function_)
///
/// Returns `t` if $x > y$, `nil` otherwise.
inline lisp_t greaterp(lisp_t x, lisp_t y) { return details::arith::greaterp(x, y); }
/// @brief `(idifference a b)` (_Function_)
///
/// Integer subtraction.
inline lisp_t idifference(lisp_t a, lisp_t b) { return details::arith::idifference(a, b); }
/// @brief `(iminus a)` (_Function_)
///
/// `(idifference 0 a)'. Returns the _a_ with the opposite sign.
inline lisp_t iminus(lisp_t a) { return details::arith::iminus(a); }
/// @brief `(iplus args...)` (_NoSpread Function_)
///
/// Returns the sum of the arguments.
inline lisp_t iplus(lisp_t args) { return details::arith::iplus(args); }
/// @brief `(iquotient a b)` (_Function_)
///
/// ```lisp
/// (iquotient 3 2) => 1
/// (iquotient -3 2) => -1
/// ```
///
/// Returns the truncated result of $a / b$.
inline lisp_t iquotient(lisp_t a, lisp_t b) { return details::arith::iquotient(a, b); }
/// @brief `(iremainder a b)` (_Function_)
///
/// ```lisp
/// (iremainder 3 2) => 1
/// ```
///
/// Returns the remainder of $a / b$.
inline lisp_t iremainder(lisp_t a, lisp_t b) { return details::arith::iremainder(a, b); }
/// @brief `(itimes args...)` (_NoSpread Function_)
///
/// ```lisp
/// (itimes 1 2 3 4) => 24
/// ```
///
/// Returns the result of multiplying the arguments.
inline lisp_t itimes(lisp_t args) { return details::arith::itimes(args); }
/// @brief `(itof i)` (_Function_)
///
/// Returns the integer _i_ converted to a floating point value.
inline lisp_t itof(lisp_t i) { return details::arith::itof(i); }
/// @brief `(leq x y)` (_Function_)
///
/// Returns `t` if $a \le b$, `nil` otherwise.
inline lisp_t leq(lisp_t x, lisp_t y) { return details::arith::leq(x, y); }
/// @brief `(lessp x y)` (_Function_)
///
/// Returns `t` if $a < b$, `nil` otherwise.
inline lisp_t lessp(lisp_t x, lisp_t y) { return details::arith::lessp(x, y); }
/// @brief `(times args...)` (_NoSpread Function_)
///
/// Returns the result of multiplying the arguments.
inline lisp_t times(lisp_t args) { return details::arith::times(args); }
/// @brief `(minus a)` (_Function_)
///
/// Returns _a_ with the opposite sign.
inline lisp_t minus(lisp_t a) { return details::arith::minus(a); }
/// @brief `(minusp x)` (_Function_)
///
/// Returns `t` if _x_ is a negative number, `nil` otherwise.
inline lisp_t minusp(lisp_t x) { return details::arith::minusp(x); }
/// @brief `(neqp x y)` (_Function_)
///
/// Returns `t` if $x \neq y$, `nil` otherwise.
inline lisp_t neqp(lisp_t x, lisp_t y) { return details::arith::neqp(x, y); }
/// @brief `(plus args...)` (_NoSpread Function_)
///
/// Returns the sum of all _args_.
inline lisp_t plus(lisp_t args) { return details::arith::plus(args); }
/// @brief `(sub1 a)` (_Function_)
///
/// `(difference a 1)`. Returns $a - 1$.
inline lisp_t sub1(lisp_t a) { return details::arith::sub1(a); }
/// @brief `(zerop x)` (_Function_)
///
/// Returns `t` if $x = 0$, `nil` otherwise.
inline lisp_t zerop(lisp_t x) { return details::arith::zerop(x); }
} // namespace lisp

#endif
