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

#include <iostream>
#include <memory>
#include <system_error>

#include "lisp.hh"
#include "syntax.hh"

namespace lisp
{
// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
std::unordered_map<std::string, subr_t::subr_index> subr_t::subr_map;
subr_t::subr_vector subr_t::subr_store;
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

} // namespace lisp
