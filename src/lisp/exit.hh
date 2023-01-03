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

// @file This file exists only to exclude calling the 'exit' function from the
// coverage report.

#ifndef LISP_EXIT_HH
#define LISP_EXIT_HH

#include "types.hh"
#include "details/prim.hh"

namespace lisp
{
inline lisp_t exit(lisp_t a) { return details::prim::exit(a); }
} // namespace lisp

#endif
