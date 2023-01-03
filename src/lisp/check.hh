//
// Lips, lisp shell.
// Copyright 1989, 2020-2023 Krister Joas
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

#include "context.hh"
#include "error.hh"
#include "types.hh"

namespace lisp
{

template<typename T>
void check(const lisp_t& arg, T type)
{
  if(type_of(arg) != type)
  {
    switch(type)
    {
      case object::type::Nil:
        error(type_errc::not_nil, arg);
        break;
      case object::type::Symbol:
        error(type_errc::not_symbol, arg);
        break;
      case object::type::Integer:
        error(type_errc::not_integer, arg);
        break;
      case object::type::Float:
        error(type_errc::not_float, arg);
        break;
      case object::type::Indirect:
        error(type_errc::not_indirect, arg);
        break;
      case object::type::Cons:
        error(type_errc::not_cons, arg);
        break;
      case object::type::String:
        error(type_errc::not_string, arg);
        break;
      case object::type::Subr:
        error(type_errc::not_subr, arg);
        break;
      case object::type::Lambda:
        error(type_errc::not_lambda, arg);
        break;
      case object::type::Closure:
        error(type_errc::not_closure, arg);
        break;
      case object::type::Environ:
        error(type_errc::not_environ, arg);
        break;
      case object::type::File:
        error(type_errc::not_filet, arg);
        break;
      case object::type::Cvariable:
        error(type_errc::not_cvariable, arg);
        break;
    }
    error(error_errc::illegal_arg, arg);
  }
}

template<typename T, typename... Ts>
void check(const lisp_t& arg, T type, Ts... types)
{
  if(type_of(arg) == type)
    return;
  check(arg, types...);
}

} // namespace lisp

#endif
