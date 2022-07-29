//
// Lips, lisp shell.
// Copyright 1989, 2020-2022 Krister Joas
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

#ifndef LISP_CHECK_HH
#define LISP_CHECK_HH

#include "types.hh"
#include "error.hh"

namespace lisp
{

template<typename T>
void check(LISPT arg, T type)
{
  if(type_of(arg) != type)
  {
    switch(type)
    {
      case type::Nil:
        error(type_errc::not_nil, arg);
        break;
      case type::T:
        error(type_errc::not_t, arg);
        break;
      case type::Empty:
        error(type_errc::not_empty, arg);
        break;
      case type::Symbol:
        error(type_errc::not_symbol, arg);
        break;
      case type::Integer:
        error(type_errc::not_integer, arg);
        break;
      case type::Float:
        error(type_errc::not_float, arg);
        break;
      case type::Indirect:
        error(type_errc::not_indirect, arg);
        break;
      case type::Cons:
        error(type_errc::not_cons, arg);
        break;
      case type::String:
        error(type_errc::not_string, arg);
        break;
      case type::Subr:
        error(type_errc::not_subr, arg);
        break;
      case type::Fsubr:
        error(type_errc::not_fsubr, arg);
        break;
      case type::Lambda:
        error(type_errc::not_lambda, arg);
        break;
      case type::Nlambda:
        error(type_errc::not_nlambda, arg);
        break;
      case type::Closure:
        error(type_errc::not_closure, arg);
        break;
      case type::Unbound:
        error(type_errc::not_unbound, arg);
        break;
      case type::Environ:
        error(type_errc::not_environ, arg);
        break;
      case type::File:
        error(type_errc::not_filet, arg);
        break;
      case type::Free:
        error(type_errc::not_free, arg);
        break;
      case type::Eof:
        error(type_errc::not_endoffile, arg);
        break;
      case type::Error:
        error(type_errc::not_error, arg);
        break;
      case type::Cvariable:
        error(type_errc::not_cvariable, arg);
        break;
    }
    error(error_errc::illegal_arg, arg);
  }
}

template<typename T, typename... Ts>
void check(LISPT arg, T type, Ts... types)
{
  if(type_of(arg) == type)
    return;
  check(arg, types...);
}

} // namespace lisp

#endif
