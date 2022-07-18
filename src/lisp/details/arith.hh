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

LISPT abs(lisp&, LISPT);
LISPT add1(lisp&, LISPT);
LISPT difference(lisp&, LISPT, LISPT);
LISPT divide(lisp&, LISPT, LISPT);
LISPT eqp(lisp&, LISPT, LISPT);
LISPT fdifference(lisp&, LISPT, LISPT);
LISPT fdivide(lisp&, LISPT, LISPT);
LISPT fplus(lisp&, LISPT);
LISPT ftimes(lisp&, LISPT);
LISPT geq(lisp&, LISPT, LISPT);
LISPT greaterp(lisp&, LISPT, LISPT);
LISPT idifference(lisp&, LISPT, LISPT);
LISPT iminus(lisp&, LISPT);
LISPT iplus(lisp&, LISPT);
LISPT iquotient(lisp&, LISPT, LISPT);
LISPT iremainder(lisp&, LISPT, LISPT);
LISPT itimes(lisp&, LISPT);
LISPT itof(lisp&, LISPT);
LISPT leq(lisp&, LISPT, LISPT);
LISPT lessp(lisp&, LISPT, LISPT);
LISPT ltimes(lisp&, LISPT);
LISPT minus(lisp&, LISPT);
LISPT minusp(lisp&, LISPT);
LISPT neqp(lisp&, LISPT, LISPT);
LISPT plus(lisp&, LISPT);
LISPT sub1(lisp&, LISPT);
LISPT zerop(lisp&, LISPT);

} // namespace lisp::details::arith

#endif
