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

/// @file alloc.hh
///
/// ### Memory Allocation
///
/// Functions which are responsible for allocating objects and managing object
/// life cycle.

#ifndef LISP_ALLOC_HH
#define LISP_ALLOC_HH

#include <concepts>
#include <limits>
#include <string>
#include <string_view>
#include <utility>

#include "details/alloc.hh"
#include "types.hh"
#include "vm.hh" // For error functions

namespace lisp
{

/// @brief Allocates an object from the list of free objects.
///
/// The _getobject_ functions allocates an _object_ and initializes it with the
/// value passed as an argument to _getobject_.
///
/// @tparam T The type to be stored.
/// @param value A value of a type which can be stored in an _object_.
///
/// @returns A lisp_t object initialized with an object of type T.
template<typename T>
lisp_t getobject(T value)
{
  return {new object(value)};
}

/// @brief Partial specialization when T is convertible to an integer.
///
/// @tparam T The integer type.
/// @param value An integer type value convertible to a 64 bit integer.
///
/// @returns A lisp_t object initialised to hold an integer value.
template<typename T>
  requires std::convertible_to<T, integer_t::value_type>
lisp_t getobject(T value)
{
  return {new object(value)};
}

/// @brief Specialization for a double which then becomes a floating point
/// object.
///
/// @param value A floating point value (double).
///
/// @returns A lisp_t object initialized to hold a floating point value.
template<>
inline lisp_t getobject(double_t::value_type value)
{
  return {new object(value)};
}

/// @brief Create a string.
///
/// @param str The string.
///
/// @returns A lisp_t object of type string.
inline lisp_t mkstring(const std::string& str) { return details::alloc::mkstring(str); }

/// @brief Create an integer number.
///
/// @param value The integer number.
///
/// @returns An integer number as a lisp_t object.
inline lisp_t mknumber(integer_t::value_type value) { return details::alloc::mknumber(value); }

/// @brief Create a floating point number.
///
/// @param value The floating point number.
///
/// @returns A floating point number as a LISP object.
inline lisp_t mkfloat(double_t::value_type value) { return details::alloc::mkfloat(value); }

/// @brief Builds a cons cell out of the arguments.
///
/// The most basic of lisp functions.  Allocate a cons cell and fill in the
/// cell's car and cdr parts.
///
/// @param a [in] The value to put in the head (car) of the cons cell.
/// @param b [in] The value to put in the tail (cdr) of the cons cell.
///
/// @returns The cons cell.
inline lisp_t cons(lisp_t a, lisp_t b) { return getobject(new cons_t{a, b}); }

/// @brief Build a list of symbols in the local symbol table.
///
/// @returns A list of local symbols in no particular order.
inline lisp_t obarray() { return details::alloc::obarray(); }

/// @brief Number of free cell in the free cell list.
///
/// @returns The number of free cells.
inline lisp_t freecount() { return details::alloc::freecount(); }

/// @brief Make interned symbol in obarray.
///
/// Create an interned symbol in the global symbol table.
///
/// @param pname The print name of the symbol.
///
/// @returns The symbol as a LISP object.
inline lisp_t intern(std::string_view s) { return details::alloc::intern(s); }

/// @brief Create a literal atom.
///
/// Currently there is no difference between `intern` and `mkatom` as they both
/// create a symbol in the global symbol table.
///
/// @returns The literal atom.
inline lisp_t mkatom(std::string_view s) { return details::alloc::mkatom(s); }

/// @brief Templated function which registers a primary function.
///
/// The function registered can have one of the following signatures.
///
/// @verbatim
///   lisp_t fun()
///   lisp_t fun(lisp_t)
///   lisp_t fun(lisp_t, lisp_t)
///   lisp_t fun(lisp_t, lisp_t, lisp_t)
/// @endverbatim
///
/// When called from C++ the arguments are always evaluated since it's
/// following the C++ conventions.
///
/// @param pname Print name.
/// @param fun The function to register.
/// @param subr Specifies if the function should evaluate (SUBR) or not (FSUBR)
/// its arguments.
/// @param spread Specifies if the function should take a specific number of
/// arguments (SPREAD) or not (NOSPREAD).
template<typename Fun>
void mkprim(std::string_view pname, Fun&& fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  details::alloc::mkprim(subr_t(pname, make_fun(std::forward<Fun&&>(fun)), subr, spread));
}

/// @brief Initializes a lisp symbol for use in the C++ program.
///
/// This function links a variable in lisp with a variable in C++ so that
/// changing the value in one domain will be reflected in the other.  The lisp
/// variable will have the print name _name_.  In C++ the type cvariable_t
/// will work in many contexts which expects a value of type lisp_t.  If
/// assigned to in C++ the lisp value will change, if the value is set with
/// setq in lisp the C++ value will change.
///
/// @param name The lisp print name.
/// @param val The initial value.
///
/// @returns A reference of type cvariable_t which wraps the lisp_t value.
inline cvariable_t& initcvar(std::string_view name, lisp_t val)
{
  return details::alloc::initcvar(name, std::move(val));
}

/// @brief Creates a lisp_t object containing a cvariable_t.
///
/// Similar to initcvar but returns a lisp_t object instead.
///
/// @param name The lisp print name.
/// @param val The initial value.
///
/// @returns A lisp_t object containing a cvariable_t value.
inline lisp_t makecvar(std::string_view name, lisp_t val) { return details::alloc::makecvar(name, std::move(val)); }

/// @brief Convenience function which creates a list from a variadic list of
/// items.
///
/// This function takes one or more objects of type lisp_t and creates a list.
///
/// @tparam Ts List of types, all of which have to be of type lisp_t.
/// @param first First object in the list.
/// @param rest Rest of the list of objects.
///
/// @returns The list.
template<typename... Ts>
  requires(std::same_as<Ts, lisp_t> && ...)
lisp_t mklist(lisp_t first, Ts... rest)
{
  if constexpr(sizeof...(Ts) > 0)
    return cons(std::move(first), mklist(rest...));
  return cons(std::move(first), nil);
}

/// @brief Creates a lisp string.
inline lisp_t operator"" _s(const char* s, std::size_t) { return details::alloc::mkstring(s); }

/// @brief Creates an atom.
inline lisp_t operator"" _a(const char* s, std::size_t) { return details::alloc::mkatom(s); }

/// @brief Creates a number.
inline lisp_t operator"" _l(unsigned long long i)
{
  return details::alloc::mknumber(static_cast<integer_t::value_type>(i));
}

/// @brief Creates a floating point value.
inline lisp_t operator"" _l(long double d)
{
  constexpr auto max_double{std::numeric_limits<double>::max()};
  constexpr auto max_long_double{std::numeric_limits<long double>::max()};
  if constexpr(max_long_double > max_double)
  {
    if(d > max_double)
    {
      const lisp_t err{mkstring(fmt::format("{} too large", d))};
      error(error_errc::illegal_arg, err);
    }
  }
  return details::alloc::mkfloat(static_cast<double>(d));
}

/// @brief Evaluates a lisp expression in a string.
inline lisp_t operator"" _e(const char* s, std::size_t) { return eval(s); }

/// @brief Checks if the parameter is equal to the symbol "t".
inline bool is_T(const lisp_t& x) { return x == mkatom("t"); }
/// @brief Checks if the lisp_t value is equal to `nil`.
inline bool is_nil(const lisp_t& x) { return type_of(x) == object::type::Nil; }
/// @brief Checks if the object value is equal to `nil`.
inline bool is_nil(const object& x) { return type_of(x) == object::type::Nil; }
/// @brief Checks if the cvariable_t value is equal to `nil`.
inline bool is_nil(const cvariable_t& x) { return type_of(x) == object::type::Nil; }

} // namespace lisp

#endif
