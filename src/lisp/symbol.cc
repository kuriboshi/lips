//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

#include "symbol.hh"
#include "atoms.hh"

#include <string_view>

namespace lisp::symbol
{
symbol_t* symbol_t::intern(std::string_view pname)
{
  auto p = store().find(pname);
  if(p != store().end())
    return p->second;
  auto* sym = new symbol_t;
  sym->pname = pname;
  sym->value = atoms::UNBOUND;
  auto i = store().insert(std::make_pair(pname, sym));
  return i.first->second;
}
} // namespace lisp::symbol
