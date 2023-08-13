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

#ifndef LISP_DETAILS_ALLOC_HH
#define LISP_DETAILS_ALLOC_HH

#include <lisp/types.hh>

namespace lisp::details::alloc
{
lisp_t mkstring(const std::string&);
lisp_t mknumber(std::int64_t);
lisp_t mkfloat(double);
lisp_t obarray();
inline lisp_t freecount() { return mknumber(static_cast<std::int64_t>(object::freecount())); }

/// @brief Creates a lambda function.
///
/// @details Not meant to be used directly. Call the functions LAMBDA and
/// NLAMBDA instead.
///
/// @param args The parameters of the lambda function. For a spread type
/// function this is a list of atoms. For a nospread function it's a single
/// atom. For a half spread function the list should end with a dotted pair.
/// @param def The definition of the lambda function. The body of the function
/// should be a list of expressions.
/// @param eval True means lambda while false means nlambda.
///
/// @returns A lambda function.
lisp_t mklambda(lisp_t args, lisp_t def, bool eval);
lisp_t intern(std::string_view pname);
lisp_t mkatom(std::string_view);

/// @brief Register a primitive function.
///
/// @param pname [in] The print name of the internal function.  This is put in
/// the global symbol table using intern.
/// @param subr [in] The calling details of the function.  This in includes
/// number of parameters, whether the function is spread, nospread, or
/// halfspread, whether the function should evaluate it's arguments or not.
///
inline void mkprim(subr_t subr)
{
  lisp_t f = intern(subr.name);
  f->value(new object(subr_index{subr_t::put(subr)}));
}

inline cvariable_t& initcvar(std::string_view name, lisp_t val)
{
  auto t = mkatom(name);
  t->value(new object(cvariable_t(val)));
  return t->value()->cvariable();
}

inline lisp_t makecvar(std::string_view name, lisp_t val)
{
  auto t = mkatom(name);
  t->value(new object(cvariable_t(val)));
  return t->value();
}

void init();

} // namespace lisp::details::alloc

#endif
