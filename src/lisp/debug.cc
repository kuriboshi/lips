//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
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

#include "lisp/debug.hh"

namespace lisp::details::debug
{
LISPT evaltrace(lisp& l, LISPT state)
{
  auto i = l.e().trace();

  if(!is_NIL(state))
  {
    check(state, type::INTEGER);
    l.e().trace(state->intval());
  }
  return mknumber(l, i);
}

namespace pn
{
inline constexpr auto EVALTRACE = "evaltrace"; // evaltrace
}

void init()
{
  // clang-format off
  mkprim(pn::EVALTRACE, evaltrace, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format in
}

}
