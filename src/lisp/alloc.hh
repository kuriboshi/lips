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

#ifndef LISP_ALLOC_HH
#define LISP_ALLOC_HH

#include <string>

#include "io.hh"
#include "types.hh"

namespace lisp::details::alloc
{
/// @brief Return a cons cell from local storage.
///
/// @details If the free cell list is empty a new block of cons cells is
/// allocated.  The LISPT ref_ptr created by this function will have its
/// delete function overridden with a function which puts the cell back on
/// the free cell list.
///
/// @return A new empty cons cell.
///
LISPT getobject();

/// @brief Create a string.
///
LISPT mkstring(const std::string&);

/// @brief Create an integer number.
///
/// @param number The integer number.
/// @returns An integer number as a LISP object.
///
LISPT mknumber(int);

/// @brief Create a floating point number.
///
/// @param number The floating point number.
/// @returns A floating point number as a LISP object.
///
LISPT mkfloat(double);

/// @brief Builds a cons cell out of the arguments.
///
/// @details The most basic of lisp functions.  Allocate a cons cell and fill in
/// the cell's car and cdr parts.
///
/// @param a [in] The value to put in the head (car) of the cons cell.
/// @param b [in] The value to put in the tail (cdr) of the cons cell.
///
/// @return The cons cell.
///
LISPT cons(context&, LISPT, LISPT);

/// @brief Build a list of symbols in the local symbol table.
///
/// @return Returns a list of local symbols in no particular order.
///
LISPT obarray(context&);

/// @brief Number of free cell in the free cell list.
///
/// @return The number of free cells.
///
inline LISPT freecount(context&) { return mknumber(static_cast<int>(lisp_t::freecount())); }

/// @brief Make a lambda object.
///
/// @details Make a lambda object with the argument ARGS and definition DEF
/// and the type TYPE, which is either LAMBDA or NLAMBDA.
///
/// @return The lambda object.
///
LISPT mklambda(LISPT args, LISPT def, type type);

/// @brief Create a symbol in the global symbol table, accessable from all
/// lisp instances.
///
/// @param pname The print name.
///
/// @return The interned symbol.
///
LISPT intern(const std::string& pname);

LISPT mkatom(const std::string&);

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
  auto s = new lisp_t;
  s->set(subr_index{subr_t::put(subr)});
  LISPT f = intern(subr.name);
  f->value(s);
}

/// @brief Initializes a lisp symbol for use in the C++ program.
///
/// @details This function links a variable in lisp with a variable in C++ so
/// that changing the value in one domain will be reflected in the other.
/// The lisp variable will have the print name NAME.  In C++ the type
/// cvariable_t will work in many contexts which expects a value of type
/// LISPT.  If assigned to the lisp value changes if the value is set with
/// setq in lisp the C++ value will change.
///
/// @param name The lisp print name.
/// @param val The initial value.
///
/// @return A reference of type cvariable which wraps the LISPT value.
///
inline cvariable_t& initcvar(const std::string& name, LISPT val)
{
  auto t = mkatom(name);
  t->symbol().value = new lisp_t;
  t->value()->set(cvariable_t(val));
  return t->value()->cvarval();
}

void init();

} // namespace lisp::details::alloc

namespace lisp
{
inline LISPT mkstring(const std::string& s) { return details::alloc::mkstring(s); }
inline LISPT mknumber(int i) { return details::alloc::mknumber(i); }
inline LISPT mkfloat(double d) { return details::alloc::mkfloat(d); }

inline LISPT cons(LISPT a, LISPT b) { return details::alloc::cons(context::current(), a, b); }
inline LISPT obarray() { return details::alloc::obarray(context::current()); }
inline LISPT freecount() { return details::alloc::freecount(context::current()); }

inline LISPT intern(const std::string& s) { return details::alloc::intern(s); }
inline LISPT mkatom(const std::string& s) { return details::alloc::mkatom(s); }

inline void mkprim(const std::string& pname, subr_t::func0_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

inline void mkprim(const std::string& pname, subr_t::func1_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

inline void mkprim(const std::string& pname, subr_t::func2_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

inline void mkprim(const std::string& pname, subr_t::func3_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

inline cvariable_t& initcvar(const std::string& name, LISPT val) { return details::alloc::initcvar(name, val); }

/// @brief Terminates the list create function.
inline LISPT mklist(LISPT t) { return cons(t, NIL); }

/// @brief Creates a list from a variadic list of items.
template<typename... Ts>
LISPT mklist(LISPT t, Ts... ts)
{
  return cons(t, mklist(ts...));
}

/// @brief Creates a lisp string.
inline LISPT operator"" _s(const char* s, std::size_t) { return details::alloc::mkstring(s); }

/// @brief Simpler way to create an atom.
inline LISPT operator"" _a(const char* s, std::size_t) { return details::alloc::mkatom(s); }

/// @brief Creates a number.
inline LISPT operator"" _l(unsigned long long i) { return details::alloc::mknumber(i); }

/// @brief Creates a floating point value.
inline LISPT operator"" _l(long double d) { return details::alloc::mkfloat(d); }

/// @brief Evaluates a lisp expression in a string.
inline LISPT operator"" _e(const char* s, std::size_t) { return eval(s); }

} // namespace lisp

#endif
