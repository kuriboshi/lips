//
// Lips, lisp shell.
// Copyright 1988-1989, 2022 Krister Joas
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

#ifndef LISP_RTABLE_HH
#define LISP_RTABLE_HH

#include <array>
#include <vector>
#include "lisp.hh"

namespace lisp
{
class lisp_t;
using LISPT = ref_ptr<lisp_t>;
class file_t;
using ref_file_t = ref_ptr<file_t>;
class lisp;
} // namespace lisp

namespace lisp::rtable
{
void init();

LISPT dquote(lisp&, LISPT);
LISPT squote(lisp&, LISPT);
LISPT getenv(lisp&, LISPT);
} // namespace lisp::rtable

#endif
