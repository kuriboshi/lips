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

#ifndef LISP_ARITH_HH
#define LISP_ARITH_HH

/// @file arith.hh
///
/// Lips supports both integer and floating point numbers. There are functions
/// specific for either integers or floating points as well as generic
/// functions which can take either type.

#include "types.hh"
#include "details/arith.hh"

namespace lisp
{
/// @brief Absolute value of n.
inline lisp_t abs(lisp_t n) { return details::arith::abs(context::current(), n); }
/// @brief Add 1 to n and return the value.
inline lisp_t add1(lisp_t n) { return details::arith::add1(context::current(), n); }
/// @brief Calculates the difference between x and y.
inline lisp_t difference(lisp_t x, lisp_t y) { return details::arith::difference(context::current(), x, y); }
/// @brief Divides x by y.
///
/// @details The result may be an integer or a floating point depending on the
/// types of x and y. If both are integers the result will be an integer and if
/// either x or y, ot both, is a floating point number the result will be a
/// floating point number.
inline lisp_t divide(lisp_t x, lisp_t y) { return details::arith::divide(context::current(), x, y); }
inline lisp_t eqp(lisp_t x, lisp_t y) { return details::arith::eqp(context::current(), x, y); }
/// @brief Floating point difference between x and y.
inline lisp_t fdifference(lisp_t x, lisp_t y) { return details::arith::fdifference(context::current(), x, y); }
inline lisp_t fdivide(lisp_t a, lisp_t b) { return details::arith::fdivide(context::current(), a, b); }
inline lisp_t fplus(lisp_t a) { return details::arith::fplus(context::current(), a); }
inline lisp_t ftimes(lisp_t a) { return details::arith::ftimes(context::current(), a); }
inline lisp_t geq(lisp_t x, lisp_t y) { return details::arith::geq(context::current(), x, y); }
inline lisp_t greaterp(lisp_t x, lisp_t y) { return details::arith::greaterp(context::current(), x, y); }
inline lisp_t idifference(lisp_t a, lisp_t b) { return details::arith::idifference(context::current(), a, b); }
inline lisp_t iminus(lisp_t a) { return details::arith::iminus(context::current(), a); }
inline lisp_t iplus(lisp_t a) { return details::arith::iplus(context::current(), a); }
inline lisp_t iquotient(lisp_t a, lisp_t b) { return details::arith::iquotient(context::current(), a, b); }
inline lisp_t iremainder(lisp_t a, lisp_t b) { return details::arith::iremainder(context::current(), a, b); }
inline lisp_t itimes(lisp_t a) { return details::arith::itimes(context::current(), a); }
inline lisp_t itof(lisp_t i) { return details::arith::itof(context::current(), i); }
inline lisp_t leq(lisp_t x, lisp_t y) { return details::arith::leq(context::current(), x, y); }
inline lisp_t lessp(lisp_t x, lisp_t y) { return details::arith::lessp(context::current(), x, y); }
inline lisp_t ltimes(lisp_t a) { return details::arith::ltimes(context::current(), a); }
inline lisp_t minus(lisp_t a) { return details::arith::minus(context::current(), a); }
inline lisp_t minusp(lisp_t x) { return details::arith::minusp(context::current(), x); }
inline lisp_t neqp(lisp_t x, lisp_t y) { return details::arith::neqp(context::current(), x, y); }
inline lisp_t plus(lisp_t a) { return details::arith::plus(context::current(), a); }
inline lisp_t sub1(lisp_t a) { return details::arith::sub1(context::current(), a); }
inline lisp_t zerop(lisp_t x) { return details::arith::zerop(context::current(), x); }
} // namespace lisp

#endif
