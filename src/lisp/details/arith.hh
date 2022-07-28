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

#ifndef LISP_DETAILS_ARITH_HH
#define LISP_DETAILS_ARITH_HH

#include <lisp/lisp.hh>

namespace lisp::details::arith
{

void init();

LISPT abs(context&, LISPT);
LISPT add1(context&, LISPT);
LISPT difference(context&, LISPT, LISPT);
LISPT divide(context&, LISPT, LISPT);
LISPT eqp(context&, LISPT, LISPT);
LISPT fdifference(context&, LISPT, LISPT);
LISPT fdivide(context&, LISPT, LISPT);
LISPT fplus(context&, LISPT);
LISPT ftimes(context&, LISPT);
LISPT geq(context&, LISPT, LISPT);
LISPT greaterp(context&, LISPT, LISPT);
LISPT idifference(context&, LISPT, LISPT);
LISPT iminus(context&, LISPT);
LISPT iplus(context&, LISPT);
LISPT iquotient(context&, LISPT, LISPT);
LISPT iremainder(context&, LISPT, LISPT);
LISPT itimes(context&, LISPT);
LISPT itof(context&, LISPT);
LISPT leq(context&, LISPT, LISPT);
LISPT lessp(context&, LISPT, LISPT);
LISPT ltimes(context&, LISPT);
LISPT minus(context&, LISPT);
LISPT minusp(context&, LISPT);
LISPT neqp(context&, LISPT, LISPT);
LISPT plus(context&, LISPT);
LISPT sub1(context&, LISPT);
LISPT zerop(context&, LISPT);

} // namespace lisp::details::arith

#endif
