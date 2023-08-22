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

#ifndef LISP_ALLOC_HH
#define LISP_ALLOC_HH

#include <limits>
#include <string>
#include <string_view>

#include "details/alloc.hh"
#include "io.hh"
#include "types.hh"
#include "vm.hh"

namespace lisp
{

/// @brief Allocates an object from the list of free objects.
///
/// @return A lisp_t object initialized with a typed object.
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
inline lisp_t mknumber(std::int64_t i) { return details::alloc::mknumber(i); }

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
inline lisp_t cons(lisp_t a, lisp_t b) { return getobject(new cons_t{a, b}); }

/// @brief Build a list of symbols in the local symbol table.
///
/// @return Returns a list of local symbols in no particular order.
///
inline lisp_t obarray() { return details::alloc::obarray(); }

/// @brief Number of free cell in the free cell list.
///
/// @return The number of free cells.
///
inline lisp_t freecount() { return details::alloc::freecount(); }

/// @brief Make interned symbol in obarray.
///
/// @details Create an interned symbol in the global symbol table.
///
/// @param pname The print name of the symbol.
/// @returns The symbol as a LISP object.
inline lisp_t intern(std::string_view s) { return details::alloc::intern(s); }

/// @brief Create a literal atom.
///
/// @details Currently there is no difference between `intern` and `mkatom` as
/// they both create a symbol in the global symbol table.
///
/// @return The literal atom.
///
inline lisp_t mkatom(std::string_view s) { return details::alloc::mkatom(s); }

/// @brief Templated function which registers a primary function.
template<typename Fun>
void mkprim(std::string_view pname, Fun fun, enum subr_t::subr subr, enum subr_t::spread spread)
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
inline cvariable_t& initcvar(std::string_view name, lisp_t val) { return details::alloc::initcvar(name, val); }

/// @brief Creates a lisp_t object containing a cvariable_t.
///
/// @details Similar to initcvar but returns a lisp_t object instead.
///
/// @param name The lisp print name.
/// @param val The initial value.
///
/// @return A lisp_t object containing a cvariable_t value.
///
inline lisp_t makecvar(std::string_view name, lisp_t val) { return details::alloc::makecvar(name, val); }

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
inline lisp_t operator"" _l(unsigned long long i) { return details::alloc::mknumber(static_cast<std::int64_t>(i)); }

/// @brief Creates a floating point value.
inline lisp_t operator"" _l(long double d)
{
  constexpr auto max_double{std::numeric_limits<double>::max()};
  constexpr auto max_long_double{std::numeric_limits<long double>::max()};
  if constexpr(max_long_double > max_double)
  {
    if(d > max_double)
    {
      lisp_t err{mkstring(fmt::format("{} too large", d))};
      error(error_errc::illegal_arg, err);
    }
  }
  return details::alloc::mkfloat(static_cast<double>(d));
}

/// @brief Evaluates a lisp expression in a string.
inline lisp_t operator"" _e(const char* s, std::size_t) { return eval(s); }

} // namespace lisp

#endif
