//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
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
/// @brief Allocates an object from the list of free objects.
///
/// @details If the free cell list is empty a new block of objects is
/// allocated.  The lisp_t ref_ptr created by this function will have its delete
/// function overridden with a function which puts the cell back on the free
/// cell list.
///
/// @return A new empty lisp object.
inline lisp_t getobject() { return {new object}; }

/// @brief Templated version of getobject which returns a lisp_t object
/// initialized with a typed object object.
template<typename T>
lisp_t getobject(T x)
{
  return {new object(x)};
}

lisp_t mkstring(const std::string&);
lisp_t mknumber(int);
lisp_t mkfloat(double);
lisp_t cons(context&, lisp_t, lisp_t);
lisp_t obarray(context&);
inline lisp_t freecount(context&) { return mknumber(static_cast<int>(object::freecount())); }

/// @brief Make a lambda object.
///
/// @details Make a lambda object with the argument ARGS and definition DEF
/// and the type TYPE, which is either LAMBDA or NLAMBDA.
///
/// @return The lambda object.
///
lisp_t mklambda(lisp_t args, lisp_t def, bool eval);
lisp_t intern(const std::string& pname);
lisp_t mkatom(const std::string&);

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
  auto s = new object;
  s->set(subr_index{subr_t::put(subr)});
  lisp_t f = intern(subr.name);
  f->value(s);
}

inline cvariable_t& initcvar(const std::string& name, lisp_t val)
{
  auto t = mkatom(name);
  t->symbol()->value = new object;
  t->value()->set(cvariable_t(val));
  return t->value()->cvarval();
}

void init();

} // namespace lisp::details::alloc

#endif
