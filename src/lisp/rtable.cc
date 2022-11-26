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

#include <cstdlib>
#include <string>

#include "alloc.hh"
#include "check.hh"
#include "io.hh"
#include "rtable.hh"

namespace lisp::rtable
{
//
// Read macros.
//
LISPT rmdquote(context&, LISPT stream)
{
  check(stream, type::File);
  std::string buffer;
  auto c = stream->file()->getch();
  while(c != '"')
  {
    if(c == '\\')
      c = stream->file()->getch();
    buffer.push_back(static_cast<char>(c));
    c = stream->file()->getch();
  }
  return mkstring(buffer);
}

LISPT rmsquote(context&, LISPT stream)
{
  check(stream, type::File);
  int c = 0;
  if((c = stream->file()->getch()) == ')' /*|| is_sepr(l, c)*/)
  {
    stream->file()->ungetch(c);
    return "'"_a;
  }
  stream->file()->ungetch(c);
  return cons(C_QUOTE, cons(io::lispread(stream->file()), NIL));
}

LISPT rmgetenv(context&, LISPT stream)
{
  check(stream, type::File);
  auto sym = ratom(stream->file());
  check(sym, type::Symbol, type::String);
  auto* val = std::getenv(sym->getstr().c_str());
  if(val == nullptr)
    return NIL;
  return mkstring(val);
}

namespace pn
{
inline constexpr auto RMDQUOTE = "rmdquote";
inline constexpr auto RMGETENV = "rmgetenv";
inline constexpr auto RMSQUOTE = "rmsquote";
} // namespace pn

void init()
{
  mkprim(pn::RMDQUOTE, rmdquote, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::RMGETENV, rmgetenv, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::RMSQUOTE, rmsquote, subr_t::subr::EVAL, subr_t::spread::SPREAD);
}
} // namespace lisp::rtable
