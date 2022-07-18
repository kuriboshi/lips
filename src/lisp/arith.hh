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
/// @lips supports both integer and floating point numbers. There are
/// functions specific for either integers or floating points as well as
/// generic functions which can take either type.

#include "lisp.hh"
#include "details/arith.hh"

namespace lisp
{
inline LISPT abs(LISPT i) { return details::arith::abs(lisp::current(), i); }
inline LISPT abs(lisp& l, LISPT i) { return details::arith::abs(l, i); }
inline LISPT add1(LISPT a) { return details::arith::add1(lisp::current(), a); }
inline LISPT add1(lisp& l, LISPT a) { return details::arith::add1(l, a); }
inline LISPT difference(LISPT a, LISPT b) { return details::arith::difference(lisp::current(), a, b); }
inline LISPT difference(lisp& l, LISPT a, LISPT b) { return details::arith::difference(l, a, b); }
inline LISPT divide(LISPT a, LISPT b) { return details::arith::divide(lisp::current(), a, b); }
inline LISPT divide(lisp& l, LISPT a, LISPT b) { return details::arith::divide(l, a, b); }
inline LISPT eqp(LISPT x, LISPT y) { return details::arith::eqp(lisp::current(), x, y); }
inline LISPT eqp(lisp& l, LISPT x, LISPT y) { return details::arith::eqp(l, x, y); }
inline LISPT fdifference(LISPT a, LISPT b) { return details::arith::fdifference(lisp::current(), a, b); }
inline LISPT fdifference(lisp& l, LISPT a, LISPT b) { return details::arith::fdifference(l, a, b); }
inline LISPT fdivide(LISPT a, LISPT b) { return details::arith::fdivide(lisp::current(), a, b); }
inline LISPT fdivide(lisp& l, LISPT a, LISPT b) { return details::arith::fdivide(l, a, b); }
inline LISPT fplus(LISPT a) { return details::arith::fplus(lisp::current(), a); }
inline LISPT fplus(lisp& l, LISPT a) { return details::arith::fplus(l, a); }
inline LISPT ftimes(LISPT a) { return details::arith::ftimes(lisp::current(), a); }
inline LISPT ftimes(lisp& l, LISPT a) { return details::arith::ftimes(l, a); }
inline LISPT geq(LISPT x, LISPT y) { return details::arith::geq(lisp::current(), x, y); }
inline LISPT geq(lisp& l, LISPT x, LISPT y) { return details::arith::geq(l, x, y); }
inline LISPT greaterp(LISPT x, LISPT y) { return details::arith::greaterp(lisp::current(), x, y); }
inline LISPT greaterp(lisp& l, LISPT x, LISPT y) { return details::arith::greaterp(l, x, y); }
inline LISPT idifference(LISPT a, LISPT b) { return details::arith::idifference(lisp::current(), a, b); }
inline LISPT idifference(lisp& l, LISPT a, LISPT b) { return details::arith::idifference(l, a, b); }
inline LISPT iminus(LISPT a) { return details::arith::iminus(lisp::current(), a); }
inline LISPT iminus(lisp& l, LISPT a) { return details::arith::iminus(l, a); }
inline LISPT iplus(LISPT a) { return details::arith::iplus(lisp::current(), a); }
inline LISPT iplus(lisp& l, LISPT a) { return details::arith::iplus(l, a); }
inline LISPT iquotient(LISPT a, LISPT b) { return details::arith::iquotient(lisp::current(), a, b); }
inline LISPT iquotient(lisp& l, LISPT a, LISPT b) { return details::arith::iquotient(l, a, b); }
inline LISPT iremainder(LISPT a, LISPT b) { return details::arith::iremainder(lisp::current(), a, b); }
inline LISPT iremainder(lisp& l, LISPT a, LISPT b) { return details::arith::iremainder(l, a, b); }
inline LISPT itimes(LISPT a) { return details::arith::itimes(lisp::current(), a); }
inline LISPT itimes(lisp& l, LISPT a) { return details::arith::itimes(l, a); }
inline LISPT itof(LISPT i) { return details::arith::itof(lisp::current(), i); }
inline LISPT itof(lisp& l, LISPT i) { return details::arith::itof(l, i); }
inline LISPT leq(LISPT x, LISPT y) { return details::arith::leq(lisp::current(), x, y); }
inline LISPT leq(lisp& l, LISPT x, LISPT y) { return details::arith::leq(l, x, y); }
inline LISPT lessp(LISPT x, LISPT y) { return details::arith::lessp(lisp::current(), x, y); }
inline LISPT lessp(lisp& l, LISPT x, LISPT y) { return details::arith::lessp(l, x, y); }
inline LISPT ltimes(LISPT a) { return details::arith::ltimes(lisp::current(), a); }
inline LISPT ltimes(lisp& l, LISPT a) { return details::arith::ltimes(l, a); }
inline LISPT minus(LISPT a) { return details::arith::minus(lisp::current(), a); }
inline LISPT minus(lisp& l, LISPT a) { return details::arith::minus(l, a); }
inline LISPT minusp(LISPT x) { return details::arith::minusp(lisp::current(), x); }
inline LISPT minusp(lisp& l, LISPT x) { return details::arith::minusp(l, x); }
inline LISPT neqp(LISPT x, LISPT y) { return details::arith::neqp(lisp::current(), x, y); }
inline LISPT neqp(lisp& l, LISPT x, LISPT y) { return details::arith::neqp(l, x, y); }
inline LISPT plus(LISPT a) { return details::arith::plus(lisp::current(), a); }
inline LISPT plus(lisp& l, LISPT a) { return details::arith::plus(l, a); }
inline LISPT sub1(LISPT a) { return details::arith::sub1(lisp::current(), a); }
inline LISPT sub1(lisp& l, LISPT a) { return details::arith::sub1(l, a); }
inline LISPT zerop(LISPT x) { return details::arith::zerop(lisp::current(), x); }
inline LISPT zerop(lisp& l, LISPT x) { return details::arith::zerop(l, x); }
} // namespace lisp

#endif
