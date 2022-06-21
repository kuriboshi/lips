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

#ifndef LISP_DEBUG_HH
#define LISP_DEBUG_HH

#include "lisp.hh"

namespace lisp::debug
{
LISPT evaltrace(lisp&, LISPT);
void init();
} // namespace lisp::debug

namespace lisp
{
inline LISPT evaltrace(lisp& l, LISPT x) { return debug::evaltrace(l, x); }
inline LISPT evaltrace(LISPT x) { return debug::evaltrace(lisp::current(), x); }
} // namespace lisp

#endif
