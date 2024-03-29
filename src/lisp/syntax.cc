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

#include "alloc.hh"
#include "syntax.hh"

namespace lisp
{
lisp_t syntax::macro(ref_file_t source, std::uint8_t index)
{
  auto fn = _macro.at(index);
  if(fn != nil)
  {
    const lisp_t f = getobject(std::move(source));
    return apply(fn, cons(f, nil));
  }
  return nil;
}
} // namespace lisp
