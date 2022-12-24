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

#include <functional>
#include <memory>
#include <string>
#include <sstream>
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
class lisp_t;
using LISPT = ref_ptr<lisp_t>;

enum class type
{
  Nil = 0,  // so that nullptr also becomes NIL
  T,        // the truth object
  Symbol,   // an atomic symbol
  Integer,  // 24 bit integer in same word
  Float,    // floating point value
  Indirect, // used when a value is stored in a closure
  Cons,     // a pair
  String,   // character strings
  Subr,     // primitive function
  Lambda,   // lambda function
  Closure,  // static binding object
  Unbound,  // unbound indicator
  Environ,  // environment stack type for gc use
  File,     // file pointer
  Eof,      // returned from read at end of file
  Error,    // returned from primitive when an error occured
  Cvariable // is a pointer to c-variable
};

inline constexpr auto NIL = nullptr;

/// @brief The cons cell.
///
/// @details A cons cell contains two pieces of data: The head (traditionall
/// called car) and the tail (traditionally called cdr).
///
struct cons_t
{
  LISPT car = NIL;
  LISPT cdr = NIL;
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

  using func0_t = std::function<LISPT(context&)>;
  using func1_t = std::function<LISPT(context&, LISPT)>;
  using func2_t = std::function<LISPT(context&, LISPT, LISPT)>;
  using func3_t = std::function<LISPT(context&, LISPT, LISPT, LISPT)>;

  subr_t(const std::string& pname, func0_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  subr_t(const std::string& pname, func1_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  subr_t(const std::string& pname, func2_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  subr_t(const std::string& pname, func3_t fun, enum subr subr, enum spread spread)
    : name(pname),
      f(fun),
      subr(subr),
      spread(spread)
  {}
  constexpr std::size_t argcount() const noexcept { return f.index(); }

  LISPT operator()(context& ctx) const { return std::get<func0_t>(f)(ctx); }
  LISPT operator()(context& ctx, LISPT a) const { return std::get<func1_t>(f)(ctx, a); }
  LISPT operator()(context& ctx, LISPT a, LISPT b) const { return std::get<func2_t>(f)(ctx, a, b); }
  LISPT operator()(context& ctx, LISPT a, LISPT b, LISPT c) const { return std::get<func3_t>(f)(ctx, a, b, c); }

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
struct lambda_t
{
  /// @brief The S-expression representation of the lambda function.
  LISPT body = NIL;
  /// @brief The list of arguments.
  LISPT args = NIL;
  /// @brief The number of arguments.
  std::int8_t count = 0;
  /// @brief True if arguments are evaluated, false if not (nlambda).
  bool eval = true;
};

/// @brief A closure (static binding).
///
class closure_t: public ref_count<closure_t>
{
public:
  LISPT cfunction = NIL;
  LISPT closed = NIL;
  LISPT cvalues = NIL;
  std::uint8_t count = 0;

  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(closure_t* x, std::destroying_delete_t) { _pool.deallocate(x); }

private:
  using pool_t = pool<closure_t, 256>;
  static pool_t _pool;
};

using ref_closure_t = ref_ptr<closure_t>;
using ref_file_t = ref_ptr<file_t>;

struct subr_index
{
  subr_t::subr_index index;
};

/// @brief A representation of a C++ variable linked to a lisp variable.
///
/// @details Wraps a LISPT value in such a way that the value can be changed
/// from either the C++ context of the lisp context and have the value be
/// reflected to both.
///
class cvariable_t
{
public:
  explicit cvariable_t(LISPT value)
    : _value(value)
  {}
  cvariable_t() = delete;
  ~cvariable_t() = default;
  cvariable_t(const cvariable_t& other) = delete;
  cvariable_t(cvariable_t&& other) noexcept { _value = std::move(other._value); }
  cvariable_t& operator=(cvariable_t&& other) noexcept
  {
    std::swap(_value, other._value);
    return *this;
  }
  cvariable_t& operator=(const cvariable_t& other) = delete;
  cvariable_t& operator=(LISPT value)
  {
    _value = value;
    return *this;
  }

  /// @brief Automatically convert to the LISPT value in a LISPT context.
  operator LISPT() const noexcept { return _value; }
  /// @brief Dereference the wrapped LISPT value.
  LISPT operator*() const noexcept { return _value; }
  /// @brief Dereference the wrapped LISPT value.
  LISPT operator->() const noexcept { return _value; }

private:
  /// @brief The wrapped LISPT value.
  LISPT _value;
};

struct indirect_t
{
  LISPT value;
};

class destblock_t;

/// @brief A class able to hold a value of any lisp type
///
/// @details The lisp objects are stored in a variant with accessor methods to
/// set or get the values. There is no checking of the correct type for the
/// accessor functions so calling them for the wrong type throws an exception.
class lisp_t final: public ref_count<lisp_t>
{
public:
  lisp_t() = default;
  ~lisp_t() = default;
  lisp_t(const lisp_t&) = delete;

  /// @brief Constructor for anything with a defined set function
  template<typename T>
  lisp_t(T x) { set(x); }

  /// @brief Litatom
  auto symbol() -> symbol::symbol_t& { return symbol_collection().get(std::get<symbol::symbol_id>(_u)); }
  void set(const symbol::symbol_t& sym)
  {
    _type = type::Symbol;
    _u = sym.id;
  }

  /// @brief Get and set the value of a litatom
  auto value() const -> LISPT { return symbol_collection().get(std::get<symbol::symbol_id>(_u)).value; }
  void value(LISPT x) { symbol_collection().get(std::get<symbol::symbol_id>(_u)).value = x; }

  /// @brief Integer
  auto intval() const -> int { return std::get<int>(_u); }
  void set(int x)
  {
    _type = type::Integer;
    _u = x;
  }

  /// @brief Floating point (double)
  auto floatval() const -> double { return std::get<double>(_u); }
  void set(double f)
  {
    _type = type::Float;
    _u = f;
  }

  auto indirectval() const -> LISPT { return std::get<indirect_t>(_u).value; }
  void set(indirect_t x)
  {
    _type = type::Indirect;
    _u = x;
  }

  /// @brief Cons cell and car/cdr
  auto cons() const -> const cons_t& { return std::get<cons_t>(_u); }
  void set(cons_t x)
  {
    _type = type::Cons;
    _u = x;
  }
  auto car() const -> LISPT { return std::get<cons_t>(_u).car; }
  void car(LISPT x) { std::get<cons_t>(_u).car = x; }
  auto cdr() const -> LISPT { return std::get<cons_t>(_u).cdr; }
  void cdr(LISPT x) { std::get<cons_t>(_u).cdr = x; }

  /// @brief Character string
  auto string() const -> const std::string& { return std::get<std::string>(_u); }
  void set(const std::string& s)
  {
    _type = type::String;
    _u = s;
  }

  /// @brief Compiled function (subr)
  auto subr() const -> const subr_t& { return subr_t::get(std::get<subr_index>(_u).index); }
  void set(subr_index x)
  {
    _type = type::Subr;
    _u = x;
  }

  /// @brief Lambda expression
  auto lambda() -> lambda_t& { return std::get<lambda_t>(_u); }
  void set(lambda_t x)
  {
    _type = type::Lambda;
    _u = x;
  }

  /// @brief Closure
  auto closure() -> ref_closure_t& { return std::get<ref_closure_t>(_u); }
  void set(ref_closure_t x)
  {
    _type = type::Closure;
    _u = x;
  }

  /// @brief Destination environment
  auto envval() -> destblock_t* { return std::get<destblock_t*>(_u); }
  void set(destblock_t* env)
  {
    _type = type::Environ;
    _u = env;
  }

  /// @brief File reference
  auto file() -> ref_file_t { return std::get<ref_file_t>(_u); }
  void set(ref_file_t f)
  {
    _type = type::File;
    _u = f;
  }

  /// @brief Link to a c/c++ variable
  auto cvarval() -> cvariable_t& { return std::get<cvariable_t>(_u); }
  void set(cvariable_t&& x)
  {
    _type = type::Cvariable;
    _u = std::move(x);
  }

  /// @brief Get the string if the lisp_t holds a litatom or a proper string
  const std::string& getstr() const
  {
    return _type == type::String ? string() : symbol_collection().get(std::get<symbol::symbol_id>(_u)).pname;
  }

  /// @brief Access the type of object
  type gettype() const { return _type; }
  void settype(type t) { _type = t; }

  static symbol::symbol_collection& symbol_collection()
  {
    static symbol::symbol_collection all_symbols;
    return all_symbols;
  }

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(lisp_t* x, std::destroying_delete_t) { _pool.deallocate(x); }

  static std::size_t freecount() { return _pool.size(); }

private:
  using pool_t = pool<lisp_t, 256>;
  static pool_t _pool;

  type _type = type::Nil;

  // One entry for each type.  Types that has no, or just one, value are
  // indicated by a comment.
  std::variant<std::monostate, // Nil
    std::nullptr_t,            // Empty
    symbol::symbol_id,         // Symbol
    int,                       // Integer
    double,                    // Float
    indirect_t,                // Indirect
    cons_t,                    // Cons
    std::string,               // String
    subr_index,                // Subr
    lambda_t,                  // Lambda/Nlambda
    ref_closure_t,             // Closure
    destblock_t*,              // Environ
    ref_file_t,                // File
    cvariable_t                // Cvariable
    >
    _u;
};

//
// All lisp constants used internally.
//
extern LISPT T;
extern LISPT C_AUTOLOAD;
extern LISPT C_BROKEN;
extern LISPT C_BT;
extern LISPT C_CLOSURE;
extern LISPT C_CONS;
extern LISPT C_DOT;
extern LISPT C_ENDOFFILE;
extern LISPT C_ENVIRON;
extern LISPT C_EOF;
extern LISPT C_ERROR;
extern LISPT C_FILE;
extern LISPT C_FLOAT;
extern LISPT C_FSUBR;
extern LISPT C_GO;
extern LISPT C_INDIRECT;
extern LISPT C_INTEGER;
extern LISPT C_LAMBDA;
extern LISPT C_NLAMBDA;
extern LISPT C_OLDDEF;
extern LISPT C_QUOTE;
extern LISPT C_REDEFINED;
extern LISPT C_RESET;
extern LISPT C_RETURN;
extern LISPT C_STRING;
extern LISPT C_SUBR;
extern LISPT C_SYMBOL;
extern LISPT C_READ;
extern LISPT C_WRITE;
extern LISPT C_APPEND;
extern LISPT C_CVARIABLE;

/// @brief The lisp interpreter.
///
inline type type_of(LISPT a) { return a == nullptr ? type::Nil : a->gettype(); }
inline type type_of(lisp_t& a) { return a.gettype(); }
inline bool is_T(LISPT x) { return type_of(x) == type::T; }
inline bool is_NIL(LISPT x) { return type_of(x) == type::Nil; }

} // namespace lisp

#endif
