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

#include "eval.hh"
#include "io.hh"
#include "types.hh"
#include "details/alloc.hh"

namespace lisp
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

/// @brief Create a string.
///
/// @param s The string.
/// @return A LISP object of type string.
///
inline lisp_t mkstring(const std::string& s) { return details::alloc::mkstring(s); }

/// @brief Create an integer number.
///
/// @param number The integer number.
/// @return An integer number as a LISP object.
///
inline lisp_t mknumber(int i) { return details::alloc::mknumber(i); }

/// @brief Create a floating point number.
///
/// @param number The floating point number.
/// @return A floating point number as a LISP object.
///
inline lisp_t mkfloat(double number) { return details::alloc::mkfloat(number); }

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
inline lisp_t cons(lisp_t a, lisp_t b) { return details::alloc::cons(context::current(), a, b); }

/// @brief Build a list of symbols in the local symbol table.
///
/// @return Returns a list of local symbols in no particular order.
///
inline lisp_t obarray() { return details::alloc::obarray(context::current()); }

/// @brief Number of free cell in the free cell list.
///
/// @return The number of free cells.
///
inline lisp_t freecount() { return details::alloc::freecount(context::current()); }

/// @brief Create a symbol in the global symbol table, accessable from all
/// lisp instances.
///
/// @param pname The print name.
///
/// @return The interned symbol.
///
inline lisp_t intern(const std::string& s) { return details::alloc::intern(s); }

/// @brief Create a literal atom.
///
/// @details Currently there is no difference between `intern` and `mkatom` as
/// they both create a symbol in the global symbol table.
///
/// @return The literal atom.
///
inline lisp_t mkatom(const std::string& s) { return details::alloc::mkatom(s); }

/// @brief Overload of mkprim for a function with zero parameters.
inline void mkprim(const std::string& pname, subr_t::func0_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

/// @brief Overload of mkprim for a function with one parameter.
inline void mkprim(const std::string& pname, subr_t::func1_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

/// @brief Overload of mkprim for a function with two parameters.
inline void mkprim(const std::string& pname, subr_t::func2_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

/// @brief Overload of mkprim for a function with three parameters.
inline void mkprim(const std::string& pname, subr_t::func3_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, fun, subr, spread));
}

/// @brief Initializes a lisp symbol for use in the C++ program.
///
/// @details This function links a variable in lisp with a variable in C++ so
/// that changing the value in one domain will be reflected in the other.
/// The lisp variable will have the print name NAME.  In C++ the type
/// cvariable_t will work in many contexts which expects a value of type
/// lisp_t.  If assigned to the lisp value changes if the value is set with
/// setq in lisp the C++ value will change.
///
/// @param name The lisp print name.
/// @param val The initial value.
///
/// @return A reference of type cvariable which wraps the lisp_t value.
///
inline cvariable_t& initcvar(const std::string& name, lisp_t val) { return details::alloc::initcvar(name, val); }

/// @brief Terminates the list create function.
inline lisp_t mklist(lisp_t t) { return cons(t, nil); }

/// @brief Creates a list from a variadic list of items.
template<typename... Ts>
lisp_t mklist(lisp_t t, Ts... ts)
{
  return cons(t, mklist(ts...));
}

/// @brief Creates a lisp string.
inline lisp_t operator"" _s(const char* s, std::size_t) { return details::alloc::mkstring(s); }

/// @brief Simpler way to create an atom.
inline lisp_t operator"" _a(const char* s, std::size_t) { return details::alloc::mkatom(s); }

/// @brief Creates a number.
inline lisp_t operator"" _l(unsigned long long i) { return details::alloc::mknumber(static_cast<int>(i)); }

/// @brief Creates a floating point value.
inline lisp_t operator"" _l(long double d) { return details::alloc::mkfloat(d); }

/// @brief Evaluates a lisp expression in a string.
inline lisp_t operator"" _e(const char* s, std::size_t) { return eval(s); }

} // namespace lisp

#endif
