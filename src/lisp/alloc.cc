//
// Lips, lisp shell
// Copyright 1988, 2020-2023 Krister Joas
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

#include <cstdint>

#include "alloc.hh"

namespace lisp::details::alloc
{
lisp_t obarray()
{
  lisp_t o = nil;
  for(auto i: symbol::symbol_t::store())
    o = cons(i.second->self, o);
  return o;
}

lisp_t mkstring(const std::string& str) { return getobject(new string_t(str)); }

/// @brief Creates an integer number.
lisp_t mknumber(std::int64_t number) { return getobject(number); }

/// @brief Create a double.
///
lisp_t mkfloat(double number) { return getobject(number); }

/// @brief Builds an argument list.
///
/// @details The list is constructed from the ALIST given in a lambda
/// definition.  This list may end in an atom if the function is halfspread,
/// or it could be an atom for a nospread function.  COUNT is set to the
/// number of arguments and is negative if halfspread or nospread.
///
/// @param alist [in] The argument list in a lambda definitions.
/// @param count [in, out] The number of arguments in the argument list.
/// Negative for nospread and halfspread.
///
/// @return A straight list of arguments.
lisp_t mkarglist(lisp_t alist, std::int8_t& count)
{
  if(type_of(alist) == object::type::Cons)
  {
    ++count;
    return cons(alist->car(), mkarglist(alist->cdr(), count));
  }
  if(is_nil(alist))
    return nil;
  ++count;
  count = -count;
  return cons(alist, nil);
}

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
lisp_t mklambda(lisp_t args, lisp_t def, bool eval)
{
  auto lambda = ref_lambda_t::create();
  lambda->body = def;
  std::int8_t count = 0;
  lambda->args = mkarglist(args, count);
  lambda->count = count;
  lambda->eval = eval;
  return getobject(lambda);
}

/// @brief Make interned symbol in obarray.
///
/// @details Create an interned symbol in the global symbol table.
///
/// @param pname Name of the symbol.
/// @returns The symbol as a LISP object.
lisp_t intern(std::string_view pname)
{
  auto* sym = symbol::symbol_t::intern(pname);
  if(sym->self == nil)
    sym->self = getobject(sym);
  return sym->self;
}

/// @brief Get an existing symbol.
///
/// @details Get an existing symbol in either the global symbol table or the
/// local one. If the symbol doesn't exist then create a new one in the local
/// symol table.
///
/// @param str The name of the symbol.
/// @returns The symbol as a LISP object.
lisp_t mkatom(std::string_view pname) { return intern(pname); }

namespace pn
{
inline constexpr std::string_view CONS = "cons";           // Make a new cons cell
inline constexpr std::string_view FREECOUNT = "freecount"; // Number of free cells
inline constexpr std::string_view OBARRAY = "obarray";     // Return list of all atoms
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::CONS,       cons,       subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::FREECOUNT,  freecount,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::OBARRAY,    obarray,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::details::alloc
