//
// Lips, lisp shell.
// Copyright 1989, 2020-2022 Krister Joas
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

#ifndef LISP_TYPES_HH
#define LISP_TYPES_HH

#include <cstdint>
#include <functional>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>

#include "error.hh"
#include "except.hh"
#include "pool.hh"
#include "ref_ptr.hh"
#include "symbol.hh"

namespace lisp
{

class syntax;
class context;
class vm;
class file_t;
class object;
using lisp_t = ref_ptr<object>;

enum class type: std::uint8_t
{
  Nil = 0,  // so that nullptr also becomes nil
  Symbol,   // an atomic symbol
  Integer,  // 24 bit integer in same word
  Float,    // floating point value
  Indirect, // used when a value is stored in a closure
  Cons,     // a pair
  String,   // character strings
  Subr,     // primitive function
  Lambda,   // lambda function
  Closure,  // static binding object
  Environ,  // environment stack type for gc use
  File,     // file pointer
  Cvariable // is a pointer to c-variable
};

inline constexpr auto nil = nullptr;

/// @brief The cons cell.
///
/// @details A cons cell contains two pieces of data: The head (traditionall
/// called car) and the tail (traditionally called cdr).
///
class cons_t: public ref_count<cons_t>
{
public:
  cons_t() = default;
  cons_t(lisp_t a, lisp_t d): car(a), cdr(d) {}
  ~cons_t() = default;

  lisp_t car = nil;
  lisp_t cdr = nil;

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(cons_t* x, std::destroying_delete_t) { _pool.deallocate(x); }

  static std::size_t freecount() { return _pool.size(); }

private:
  cons_t(pool_test_t) { throw std::runtime_error("cons_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = pool<cons_t, 256>;
  static pool_t _pool;
};

/// @brief Structure describing a built-in function.
///
/// @details Built-in function can have zero, one, two, or three parameters.
/// They can either evaluate their parameters or not (special forms).  Function
/// can be either spread (fixed number of arguments) or nospread (variable
/// number of arguments).
///
struct subr_t
{
  enum class subr
  {
    EVAL,
    NOEVAL
  };
  enum class spread
  {
    SPREAD,
    NOSPREAD
  };

  using func0_t = std::function<lisp_t(context&)>;
  using func1_t = std::function<lisp_t(context&, lisp_t)>;
  using func2_t = std::function<lisp_t(context&, lisp_t, lisp_t)>;
  using func3_t = std::function<lisp_t(context&, lisp_t, lisp_t, lisp_t)>;

  subr_t(std::string_view pname, func0_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  subr_t(std::string_view pname, func1_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  subr_t(std::string_view pname, func2_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  subr_t(std::string_view pname, func3_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  constexpr std::size_t argcount() const noexcept { return f.index(); }

  lisp_t operator()(context& ctx) const { return std::get<func0_t>(f)(ctx); }
  lisp_t operator()(context& ctx, lisp_t a) const { return std::get<func1_t>(f)(ctx, a); }
  lisp_t operator()(context& ctx, lisp_t a, lisp_t b) const { return std::get<func2_t>(f)(ctx, a, b); }
  lisp_t operator()(context& ctx, lisp_t a, lisp_t b, lisp_t c) const { return std::get<func3_t>(f)(ctx, a, b, c); }

  std::string name;
  std::variant<func0_t, func1_t, func2_t, func3_t> f;
  enum subr subr = subr::EVAL;
  enum spread spread = spread::SPREAD;

  using subr_vector = std::vector<subr_t>;
  using subr_index = subr_vector::size_type;
  static std::unordered_map<std::string, subr_index> subr_map;
  static subr_vector subr_store;
  static const subr_t& get(subr_index index) { return subr_store[index]; }
  static subr_index put(const subr_t& subr)
  {
    auto p = subr_map.find(subr.name);
    if(p != subr_map.end())
      throw lisp_error("redefinition of subr not allowed");
    auto index = subr_store.size();
    subr_store.push_back(subr);
    subr_map.insert(std::pair(subr.name, index));
    return index;
  }
};

/// @brief Lambda representation.
///
class lambda_t: public ref_count<lambda_t>
{
public:
  lambda_t() = default;
  ~lambda_t() = default;

  /// @brief The S-expression representation of the lambda function.
  lisp_t body = nil;
  /// @brief The list of arguments.
  lisp_t args = nil;
  /// @brief The number of arguments.
  std::int8_t count = 0;
  /// @brief True if arguments are evaluated, false if not (nlambda).
  bool eval = true;

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(lambda_t* x, std::destroying_delete_t) { _pool.deallocate(x); }

  static std::size_t freecount() { return _pool.size(); }

private:
  lambda_t(pool_test_t) { throw std::runtime_error("lambda_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = pool<lambda_t, 256>;
  static pool_t _pool;
};

/// @brief A closure (static binding).
///
class closure_t: public ref_count<closure_t>
{
public:
  closure_t() = default;
  ~closure_t() = default;

  lisp_t cfunction = nil;
  lisp_t closed = nil;
  lisp_t cvalues = nil;
  std::uint8_t count = 0;

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(closure_t* x, std::destroying_delete_t) { _pool.deallocate(x); }

  static std::size_t freecount() { return _pool.size(); }

private:
  closure_t(pool_test_t) { throw std::runtime_error("closure_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = pool<closure_t, 256>;
  static pool_t _pool;
};

class string_t: public ref_count<string_t>
{
public:
  string_t() = default;
  string_t(const std::string& s) : string(s) {}
  ~string_t() = default;

  std::string string;

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(string_t* x, std::destroying_delete_t) { _pool.deallocate(x); }

  static std::size_t freecount() { return _pool.size(); }

private:
  string_t(pool_test_t) { throw std::runtime_error("string_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = pool<string_t, 256>;
  static pool_t _pool;
};

using ref_cons_t = ref_ptr<cons_t>;
using ref_lambda_t = ref_ptr<lambda_t>;
using ref_closure_t = ref_ptr<closure_t>;
using ref_file_t = ref_ptr<file_t>;
using ref_string_t = ref_ptr<string_t>;

struct subr_index
{
  subr_t::subr_index index;
};

/// @brief A representation of a C++ variable linked to a lisp variable.
///
/// @details Wraps a lisp_t value in such a way that the value can be changed
/// from either the C++ context of the lisp context and have the value be
/// reflected to both.
///
class cvariable_t
{
public:
  explicit cvariable_t(lisp_t value)
    : _value(value)
  {}
  cvariable_t() = delete;
  ~cvariable_t() = default;
  cvariable_t(const cvariable_t& other) = delete;
  cvariable_t(cvariable_t&& other) noexcept { _value = std::move(other._value); }
  cvariable_t& operator=(cvariable_t&& other) noexcept
  {
    _value = std::move(other._value);
    return *this;
  }
  cvariable_t& operator=(const cvariable_t& other) = delete;
  cvariable_t& operator=(lisp_t value)
  {
    _value = value;
    return *this;
  }

  /// @brief Automatically convert to the lisp_t value in a lisp_t context.
  operator lisp_t() const noexcept { return _value; }
  /// @brief Dereference the wrapped lisp_t value.
  lisp_t operator*() const noexcept { return _value; }
  /// @brief Dereference the wrapped lisp_t value.
  lisp_t operator->() const noexcept { return _value; }

private:
  /// @brief The wrapped lisp_t value.
  lisp_t _value;
};

struct indirect_t
{
  lisp_t value;
};

class destblock_t;

/// @brief A class able to hold a value of any lisp type
///
/// @details The lisp objects are stored in a variant with accessor methods to
/// set or get the values. There is no checking of the correct type for the
/// accessor functions so calling them for the wrong type throws an exception.
class object final: public ref_count<object>
{
public:
  object() = default;
  ~object() = default;

  /// @brief Constructor for anything with a defined set function
  template<typename T>
  explicit object(T x): _u(x) {}

  /// @brief Construct an object conaining a cvariable_t value.
  object(cvariable_t&& x) { _u = std::move(x); }

  /// @brief Copy consructor
  object(const object&) = delete;
  /// @brief Assignment operator
  object& operator=(const object& x) = delete;

  /// @brief Move constructor
  object(object&& x) { *this = std::move(x); }

  /// @brief Move assignment operator
  object& operator=(object&& x) noexcept
  {
    if(this != &x)
    {
      _u = std::move(x._u);
      x._u = std::monostate{};
    }
    return *this;
  }

  /// @brief Litatom
  auto symbol() -> symbol::ref_symbol_t { return std::get<symbol::ref_symbol_t>(_u); }

  /// @brief Get and set the value of a litatom
  auto value() const -> lisp_t { return std::get<symbol::ref_symbol_t>(_u)->value; }
  void value(lisp_t);

  /// @brief Integer
  auto intval() const -> std::int64_t { return std::get<std::int64_t>(_u); }

  /// @brief Floating point (double)
  auto floatval() const -> double { return std::get<double>(_u); }

  /// @brief Get the indirect value
  auto indirect() const -> lisp_t { return std::get<indirect_t>(_u).value; }

  /// @brief Cons cell and car/cdr
  auto cons() const -> const cons_t& { return *std::get<ref_cons_t>(_u); }
  auto car() const -> lisp_t { return std::get<ref_cons_t>(_u)->car; }
  void car(lisp_t x) { std::get<ref_cons_t>(_u)->car = x; }
  auto cdr() const -> lisp_t { return std::get<ref_cons_t>(_u)->cdr; }
  void cdr(lisp_t x) { std::get<ref_cons_t>(_u)->cdr = x; }

  /// @brief Character string
  auto string() const -> const std::string& { return std::get<ref_string_t>(_u)->string; }

  /// @brief Compiled function (subr)
  auto subr() const -> const subr_t& { return subr_t::get(std::get<subr_index>(_u).index); }

  /// @brief Lambda expression
  auto lambda() -> ref_lambda_t { return std::get<ref_lambda_t>(_u); }

  /// @brief Closure
  auto closure() -> ref_closure_t& { return std::get<ref_closure_t>(_u); }

  /// @brief Destination environment
  auto environ() -> destblock_t* { return std::get<destblock_t*>(_u); }

  /// @brief File reference
  auto file() -> ref_file_t { return std::get<ref_file_t>(_u); }

  /// @brief Link to a c/c++ variable
  auto cvariable() -> cvariable_t& { return std::get<cvariable_t>(_u); }

  /// @brief Get the string if the object holds a litatom or a proper string
  const std::string& getstr() const
  {
    return gettype() == type::String ? std::get<ref_string_t>(_u)->string : std::get<symbol::ref_symbol_t>(_u)->pname;
  }

  /// @brief Access the type of object
  type gettype() const { return static_cast<type>(_u.index()); }

  /// @brief The new and delete operators uses a memory pool to create objects.
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(object* x, std::destroying_delete_t) { _pool.deallocate(x); }
  static std::size_t freecount() { return _pool.size(); }

private:
  // One entry for each type.  Types that has no, or just one, value are
  // indicated by a comment.
  std::variant<std::monostate, // Nil
    symbol::ref_symbol_t,      // Symbol
    std::int64_t,              // Integer
    double,                    // Float
    indirect_t,                // Indirect
    ref_cons_t,                // Cons
    ref_string_t,              // String
    subr_index,                // Subr
    ref_lambda_t,              // Lambda/Nlambda
    ref_closure_t,             // Closure
    destblock_t*,              // Environ
    ref_file_t,                // File
    cvariable_t                // Cvariable
    > _u;

  /// @brief Used to achieve coverage of operator delete(void*) when an
  /// exception is thrown in the constructor.
  object(pool_test_t) { throw std::runtime_error("object"); }
  /// @brief Uses the exception throwing constructor for coverage.
  template<class T>
  friend void pool_test();

  /// @brief Memory pool for objects.
  using pool_t = pool<object, 256>;
  static pool_t _pool;
};

//
// All lisp constants used internally.
//
extern lisp_t T;
extern lisp_t C_AUTOLOAD;
extern lisp_t C_BROKEN;
extern lisp_t C_BT;
extern lisp_t C_CLOSURE;
extern lisp_t C_CONS;
extern lisp_t C_DOT;
extern lisp_t C_ENDOFFILE;
extern lisp_t C_ENVIRON;
extern lisp_t C_EOF;
extern lisp_t C_ERROR;
extern lisp_t C_FILE;
extern lisp_t C_FLOAT;
extern lisp_t C_FSUBR;
extern lisp_t C_GO;
extern lisp_t C_INDIRECT;
extern lisp_t C_INTEGER;
extern lisp_t C_LAMBDA;
extern lisp_t C_NLAMBDA;
extern lisp_t C_OLDDEF;
extern lisp_t C_QUOTE;
extern lisp_t C_REDEFINED;
extern lisp_t C_RESET;
extern lisp_t C_RETURN;
extern lisp_t C_STRING;
extern lisp_t C_SUBR;
extern lisp_t C_SYMBOL;
extern lisp_t C_READ;
extern lisp_t C_WRITE;
extern lisp_t C_APPEND;
extern lisp_t C_CVARIABLE;

/// @brief The lisp interpreter.
///
inline type type_of(const lisp_t& a) { return a == nullptr ? type::Nil : a->gettype(); }
inline type type_of(const object& a) { return a.gettype(); }
inline type type_of(const cvariable_t& a) { return *a == nullptr ? type::Nil : a->gettype(); }
inline bool is_T(const lisp_t& x) { return x == T; }
inline bool is_nil(const lisp_t& x) { return type_of(x) == type::Nil; }
inline bool is_nil(const object& x) { return type_of(x) == type::Nil; }
inline bool is_nil(const cvariable_t& x) { return type_of(x) == type::Nil; }

inline void object::value(lisp_t x)
{
  auto& var = std::get<symbol::ref_symbol_t>(_u);
  if(type_of(var->value) == type::Cvariable)
  {
    auto& cvar = var->value->cvariable();
    cvar = x;
  }
  else
    var->value = x;
}

} // namespace lisp

#endif
