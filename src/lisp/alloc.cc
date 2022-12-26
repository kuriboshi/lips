//
// Lips, lisp shell
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

#include <cstdint>

#include "alloc.hh"

namespace lisp::details::alloc
{
/// @brief Creates a cons pair.
///
/// @param a The car of the pair.
/// @param b The cdr of the pair.
/// @returns The cons pair.
lisp_t cons(context&, lisp_t a, lisp_t b) { return getobject(cons_t{a, b}); }

lisp_t obarray(context&)
{
  lisp_t o = nil;
  for(auto i: symbol::symbol_t::store())
    o = cons(i.second->self, o);
  return o;
}

lisp_t mkstring(const std::string& str) { return getobject(str); }

/// @brief Creates an integer number.
lisp_t mknumber(int number) { return getobject(number); }

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
  if(type_of(alist) == type::Cons)
  {
    ++count;
    return cons(alist->car(), mkarglist(alist->cdr(), count));
  }
  if(is_nil(alist))
    return nil;
  ++count;
  count = static_cast<std::int8_t>(-count);
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
lisp_t intern(const std::string& pname)
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
lisp_t mkatom(const std::string& pname)
{
  return intern(pname);
}

namespace pn
{
inline constexpr auto CONS = "cons";           // Make a new cons cell
inline constexpr auto FREECOUNT = "freecount"; // Number of free cells
inline constexpr auto OBARRAY = "obarray";     // Return list of all atoms
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
