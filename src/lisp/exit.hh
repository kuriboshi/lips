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

// This file exists only to exclude calling the 'exit' function from the
// coverage report.

#pragma once

#include "types.hh"
#include "details/vm.hh"

namespace lisp
{
/// @brief Exit the lisp interpreter.
///
/// @param code An exit code. Must be of type integer.
/// @returns Does not return.
inline lisp_t exit(lisp_t code) { return details::vm::exit(code); }
} // namespace lisp
