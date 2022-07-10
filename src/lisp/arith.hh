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

#include "lisp.hh"

namespace lisp::arith
{
void init();

/// @brief Calculate sum of all parameters.
LISPT plus(lisp&, LISPT);
/// @brief Calculate integer sum of all paramerers.
LISPT iplus(lisp&, LISPT);
/// @brief Calculate floating point sum of all paramerers.
LISPT fplus(lisp&, LISPT);
LISPT difference(lisp&, LISPT, LISPT);
LISPT idifference(lisp&, LISPT, LISPT);
LISPT fdifference(lisp&, LISPT, LISPT);
LISPT ltimes(lisp&, LISPT);
LISPT itimes(lisp&, LISPT);
LISPT ftimes(lisp&, LISPT);
LISPT divide(lisp&, LISPT, LISPT);
LISPT iquotient(lisp&, LISPT, LISPT);
LISPT iremainder(lisp&, LISPT, LISPT);
LISPT fdivide(lisp&, LISPT, LISPT);
LISPT minus(lisp&, LISPT);
LISPT iminus(lisp&, LISPT);
LISPT abs(lisp&, LISPT);
LISPT itof(lisp&, LISPT);
LISPT add1(lisp&, LISPT);
LISPT sub1(lisp&, LISPT);
LISPT greaterp(lisp&, LISPT, LISPT);
LISPT lessp(lisp&, LISPT, LISPT);
LISPT eqp(lisp&, LISPT, LISPT);
LISPT geq(lisp&, LISPT, LISPT);
LISPT leq(lisp&, LISPT, LISPT);
LISPT neqp(lisp&, LISPT, LISPT);
LISPT zerop(lisp&, LISPT);
LISPT minusp(lisp&, LISPT);
} // namespace lisp::arith

namespace lisp
{
inline LISPT plus(lisp& l, LISPT a) { return arith::plus(l, a); }
inline LISPT plus(LISPT a) { return arith::plus(lisp::current(), a); }
inline LISPT iplus(lisp& l, LISPT a) { return arith::iplus(l, a); }
inline LISPT iplus(LISPT a) { return arith::iplus(lisp::current(), a); }
inline LISPT fplus(lisp& l, LISPT a) { return arith::fplus(l, a); }
inline LISPT fplus(LISPT a) { return arith::fplus(lisp::current(), a); }
inline LISPT difference(lisp& l, LISPT a, LISPT b) { return arith::difference(l, a, b); }
inline LISPT difference(LISPT a, LISPT b) { return arith::difference(lisp::current(), a, b); }
inline LISPT idifference(lisp& l, LISPT a, LISPT b) { return arith::idifference(l, a, b); }
inline LISPT idifference(LISPT a, LISPT b) { return arith::idifference(lisp::current(), a, b); }
inline LISPT fdifference(lisp& l, LISPT a, LISPT b) { return arith::fdifference(l, a, b); }
inline LISPT fdifference(LISPT a, LISPT b) { return arith::fdifference(lisp::current(), a, b); }
inline LISPT ltimes(lisp& l, LISPT a) { return arith::ltimes(l, a); }
inline LISPT ltimes(LISPT a) { return arith::ltimes(lisp::current(), a); }
inline LISPT itimes(lisp& l, LISPT a) { return arith::itimes(l, a); }
inline LISPT itimes(LISPT a) { return arith::itimes(lisp::current(), a); }
inline LISPT ftimes(lisp& l, LISPT a) { return arith::ftimes(l, a); }
inline LISPT ftimes(LISPT a) { return arith::ftimes(lisp::current(), a); }
inline LISPT divide(lisp& l, LISPT a, LISPT b) { return arith::divide(l, a, b); }
inline LISPT divide(LISPT a, LISPT b) { return arith::divide(lisp::current(), a, b); }
inline LISPT iquotient(lisp& l, LISPT a, LISPT b) { return arith::iquotient(l, a, b); }
inline LISPT iquotient(LISPT a, LISPT b) { return arith::iquotient(lisp::current(), a, b); }
inline LISPT iremainder(lisp& l, LISPT a, LISPT b) { return arith::iremainder(l, a, b); }
inline LISPT iremainder(LISPT a, LISPT b) { return arith::iremainder(lisp::current(), a, b); }
inline LISPT fdivide(lisp& l, LISPT a, LISPT b) { return arith::fdivide(l, a, b); }
inline LISPT fdivide(LISPT a, LISPT b) { return arith::fdivide(lisp::current(), a, b); }
inline LISPT minus(lisp& l, LISPT a) { return arith::minus(l, a); }
inline LISPT minus(LISPT a) { return arith::minus(lisp::current(), a); }
inline LISPT iminus(lisp& l, LISPT a) { return arith::iminus(l, a); }
inline LISPT iminus(LISPT a) { return arith::iminus(lisp::current(), a); }
inline LISPT abs(lisp& l, LISPT i) { return arith::abs(l, i); }
inline LISPT abs(LISPT i) { return arith::abs(lisp::current(), i); }
inline LISPT itof(lisp& l, LISPT i) { return arith::itof(l, i); }
inline LISPT itof(LISPT i) { return arith::itof(lisp::current(), i); }
inline LISPT add1(lisp& l, LISPT a) { return arith::add1(l, a); }
inline LISPT add1(LISPT a) { return arith::add1(lisp::current(), a); }
inline LISPT sub1(lisp& l, LISPT a) { return arith::sub1(l, a); }
inline LISPT sub1(LISPT a) { return arith::sub1(lisp::current(), a); }
inline LISPT greaterp(lisp& l, LISPT x, LISPT y) { return arith::greaterp(l, x, y); }
inline LISPT greaterp(LISPT x, LISPT y) { return arith::greaterp(lisp::current(), x, y); }
inline LISPT lessp(lisp& l, LISPT x, LISPT y) { return arith::lessp(l, x, y); }
inline LISPT lessp(LISPT x, LISPT y) { return arith::lessp(lisp::current(), x, y); }
inline LISPT eqp(lisp& l, LISPT x, LISPT y) { return arith::eqp(l, x, y); }
inline LISPT eqp(LISPT x, LISPT y) { return arith::eqp(lisp::current(), x, y); }
inline LISPT geq(lisp& l, LISPT x, LISPT y) { return arith::geq(l, x, y); }
inline LISPT geq(LISPT x, LISPT y) { return arith::geq(lisp::current(), x, y); }
inline LISPT leq(lisp& l, LISPT x, LISPT y) { return arith::leq(l, x, y); }
inline LISPT leq(LISPT x, LISPT y) { return arith::leq(lisp::current(), x, y); }
inline LISPT neqp(lisp& l, LISPT x, LISPT y) { return arith::neqp(l, x, y); }
inline LISPT neqp(LISPT x, LISPT y) { return arith::neqp(lisp::current(), x, y); }
inline LISPT zerop(lisp& l, LISPT x) { return arith::zerop(l, x); }
inline LISPT zerop(LISPT x) { return arith::zerop(lisp::current(), x); }
inline LISPT minusp(lisp& l, LISPT x) { return arith::minusp(l, x); }
inline LISPT minusp(LISPT x) { return arith::minusp(lisp::current(), x); }
} // namespace lisp

#endif
