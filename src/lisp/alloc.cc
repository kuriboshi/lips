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

#include "lisp/alloc.hh"

namespace lisp::alloc
{
/// @brief Returns the global symbol table.
symbol::symbol_store_t& global_symbols()
{
  return lisp_t::symbol_collection().symbol_store(symbol::symbol_collection::global_id);
}

/// @brief Allocates an object from the list of free objects.
///
/// @details If there are no free objects available a new page is allocated.
///
/// @returns A new lisp_t object.
LISPT getobject() { return {new lisp_t}; }

/// @brief Creates a cons pair.
///
/// @param a The car of the pair.
/// @param b The cdr of the pair.
/// @returns The cons pair.
LISPT cons(lisp&, LISPT a, LISPT b)
{
  auto f = getobject();
  f->set(cons_t{a, b});
  return f;
}

LISPT obarray(lisp&)
{
  LISPT o = NIL;
  for(auto i: global_symbols())
    o = cons(i.self, o);
  return o;
}

LISPT mkstring(const std::string& str)
{
  auto s = getobject();
  s->set(str);
  return s;
}

/// @brief Creates an integer number.
LISPT mknumber(int number)
{
  auto c = getobject();
  c->set(number);
  return c;
}

/// @brief Create a double.
///
LISPT mkfloat(double number)
{
  auto c = getobject();
  c->set(number);
  return c;
}

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
LISPT mkarglist(LISPT alist, std::int8_t& count)
{
  if(type_of(alist) == type::Cons)
  {
    ++count;
    return cons(alist->car(), mkarglist(alist->cdr(), count));
  }
  if(is_NIL(alist))
    return NIL;
  ++count;
  count = static_cast<std::int8_t>(-count);
  return cons(alist, NIL);
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
/// @param type The type is either type::LAMBDA or type::NLAMBDA.
///
/// @returns A lambda function.
LISPT mklambda(LISPT args, LISPT def, type type)
{
  lambda_t lambda;
  lambda.body = def;
  std::int8_t count = 0;
  lambda.args = mkarglist(args, count);
  lambda.count = count;
  auto s = getobject();
  s->set(lambda, type == type::Lambda);
  return s;
}

/// @brief Make interned symbol in obarray.
///
/// @details Create an interned symbol in the global symbol table.
///
/// @param pname Name of the symbol.
/// @returns The symbol as a LISP object.
LISPT intern(const std::string& pname)
{
  auto& glob = global_symbols();
  auto& sym = glob.get(pname);
  if(sym.self == NIL)
  {
    sym.self = getobject();
    sym.self->set(sym);
  }
  return sym.self;
}

/// @brief Get an existing symbol.
///
/// @details Get an existing symbol in either the global symbol table or the
/// local one. If the symbol doesn't exist then create a new one in the local
/// symol table.
///
/// @param str The name of the symbol.
/// @returns The symbol as a LISP object.
LISPT mkatom(const std::string& str)
{
  if(global_symbols().exists(str))
    return global_symbols().get(str).self;
  auto& sym = global_symbols().get(str);
  if(sym.self == NIL)
  {
    sym.self = getobject();
    sym.self->set(sym);
  }
  return sym.self;
}

namespace pn
{
inline constexpr auto CONS = "cons";           // Make a new cons cell
inline constexpr auto FREECOUNT = "freecount"; // Number of free cells
inline constexpr auto OBARRAY = "obarray";     // Return list of all atoms
}

void init()
{
  // clang-format off
  mkprim(pn::CONS,       cons,       subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::FREECOUNT,  freecount,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::OBARRAY,    obarray,    subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::alloc
