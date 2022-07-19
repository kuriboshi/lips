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
inline LISPT add1(LISPT a) { return details::arith::add1(lisp::current(), a); }
inline LISPT difference(LISPT a, LISPT b) { return details::arith::difference(lisp::current(), a, b); }
inline LISPT divide(LISPT a, LISPT b) { return details::arith::divide(lisp::current(), a, b); }
inline LISPT eqp(LISPT x, LISPT y) { return details::arith::eqp(lisp::current(), x, y); }
inline LISPT fdifference(LISPT a, LISPT b) { return details::arith::fdifference(lisp::current(), a, b); }
inline LISPT fdivide(LISPT a, LISPT b) { return details::arith::fdivide(lisp::current(), a, b); }
inline LISPT fplus(LISPT a) { return details::arith::fplus(lisp::current(), a); }
inline LISPT ftimes(LISPT a) { return details::arith::ftimes(lisp::current(), a); }
inline LISPT geq(LISPT x, LISPT y) { return details::arith::geq(lisp::current(), x, y); }
inline LISPT greaterp(LISPT x, LISPT y) { return details::arith::greaterp(lisp::current(), x, y); }
inline LISPT idifference(LISPT a, LISPT b) { return details::arith::idifference(lisp::current(), a, b); }
inline LISPT iminus(LISPT a) { return details::arith::iminus(lisp::current(), a); }
inline LISPT iplus(LISPT a) { return details::arith::iplus(lisp::current(), a); }
inline LISPT iquotient(LISPT a, LISPT b) { return details::arith::iquotient(lisp::current(), a, b); }
inline LISPT iremainder(LISPT a, LISPT b) { return details::arith::iremainder(lisp::current(), a, b); }
inline LISPT itimes(LISPT a) { return details::arith::itimes(lisp::current(), a); }
inline LISPT itof(LISPT i) { return details::arith::itof(lisp::current(), i); }
inline LISPT leq(LISPT x, LISPT y) { return details::arith::leq(lisp::current(), x, y); }
inline LISPT lessp(LISPT x, LISPT y) { return details::arith::lessp(lisp::current(), x, y); }
inline LISPT ltimes(LISPT a) { return details::arith::ltimes(lisp::current(), a); }
inline LISPT minus(LISPT a) { return details::arith::minus(lisp::current(), a); }
inline LISPT minusp(LISPT x) { return details::arith::minusp(lisp::current(), x); }
inline LISPT neqp(LISPT x, LISPT y) { return details::arith::neqp(lisp::current(), x, y); }
inline LISPT plus(LISPT a) { return details::arith::plus(lisp::current(), a); }
inline LISPT sub1(LISPT a) { return details::arith::sub1(lisp::current(), a); }
inline LISPT zerop(LISPT x) { return details::arith::zerop(lisp::current(), x); }
} // namespace lisp

#endif
