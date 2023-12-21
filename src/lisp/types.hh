//
// Lips, lisp shell.
// Copyright 1989, 2020-2023 Krister Joas
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
#include <algorithm>
#include <concepts>
#include <functional>
#include <memory>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include <fmt/format.h>

#include "error.hh"
#include "except.hh"
#include "io.hh"
#include "pool.hh"
#include "ref_ptr.hh"
#include "symbol.hh"

namespace lisp
{

class object;
using lisp_t = ref_ptr<object>;

/// @brief The nil object is the same as the null pointer.
inline constexpr auto nil = nullptr;

/// @brief The cons cell.
///
/// A cons cell contains two pieces of data: The head (traditionally called
/// car) and the tail (traditionally called cdr).
///
class cons_t final: public ref_count<cons_t>
{
public:
  /// @brief Default constructor.
  ///
  /// Constructs a cons cell with both car and cdr set to nil.
  cons_t() = default;
  /// @brief Construct a cons cell with two values.
  cons_t(lisp_t a, lisp_t d)
    : car(std::move(a)),
      cdr(std::move(d))
  {}

  /// @brief The contents of the address register (car).
  lisp_t car;
  /// @brief The contents of the data register (cdr).
  lisp_t cdr;

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return pool().allocate(); }
  static void operator delete(void* x) { pool().deallocate(x); }
  static void operator delete(cons_t* x, std::destroying_delete_t) { pool().deallocate(x); }

  /// @brief Returns the current number or free cells in the pool.
  static std::size_t freecount() { return pool().size(); }

private:
  cons_t(pool_test_t) { throw std::runtime_error("cons_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = lisp::pool<cons_t, 256>;
  static pool_t& pool()
  {
    static pool_t _pool; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    return _pool;
  }
};

/// @brief Destination block is used to collect the parameters to a function.
///
/// The destblock_t is used to store variables and their values.  Each block of
/// variable/value pairs is proceeded by a control block which contains the
/// following pieces of information: The size of the block, the index of the
/// variable/value pair currently being set, and a link to another destblock_t
/// in a chain of blocks.
///
class destblock_t final
{
private:
  struct control_block
  {
    std::int8_t size{0};
    std::int8_t index{0};
    destblock_t* link{nullptr};
  };
  struct var_val_pair
  {
    lisp_t var;
    lisp_t val;
  };
  std::variant<std::monostate, control_block, var_val_pair> u;

public:
  void reset() { u = var_val_pair{nil, nil}; }

  void num(std::int8_t size) { u = control_block{size, size, nullptr}; }
  int size() const { return std::get<control_block>(u).size; }
  int index() const { return std::get<control_block>(u).index; }
  destblock_t* link() const { return std::get<control_block>(u).link; }
  void link(destblock_t* dest) { std::get<control_block>(u).link = dest; }
  void decr()
  {
    if(std::get<control_block>(u).index > 0)
      --std::get<control_block>(u).index;
  }

  void var(lisp_t x) { std::get<var_val_pair>(u).var = std::move(x); }
  lisp_t var() const { return std::get<var_val_pair>(u).var; }
  void val(lisp_t x) { std::get<var_val_pair>(u).val = std::move(x); }
  lisp_t val() const { return std::get<var_val_pair>(u).val; }
};

/// @brief The @a make_tuple overloads creates a tuple from a vector.
///
/// @tparam Result Parameter pack used for the tuple.
/// @tparam Sequqnce Index sequence of result types.
/// @param values A vector with zero or more lisp_t values.
///
/// @returns A tuple with the values from the vector @a values.
template<typename... Result, std::size_t... Sequence>
auto make_tuple(const std::vector<lisp_t>& values, std::index_sequence<Sequence...>)
{
  return std::make_tuple(values[Sequence]...);
}

/// @brief Makes a tuple with the values in a vector.
///
/// @tparam Result The pack of tuple types.
/// @param values A vector with zero or more lisp_t values.
///
/// @returns A tuple with the values from the vector @a values.
template<typename... Result>
std::tuple<Result...> make_tuple(const std::vector<lisp_t>& values)
{
  return make_tuple<Result...>(values, std::make_index_sequence<sizeof...(Result)>());
}

/// @brief Base class for type erasure of the lisp function.
class func_base
{
public:
  /// @brief Default constructor.
  func_base() = default;
  /// @brief Delete copy constructor.
  func_base(const func_base&) = delete;
  /// @brief Delete move constructor.
  func_base(func_base&&) = delete;
  /// @brief Delete copy assignment.
  func_base& operator=(const func_base&) = delete;
  /// @brief Delete move assignment.
  func_base& operator=(func_base&&) = delete;
  /// @brief Default virtual destructor.
  virtual ~func_base() = default;
  /// @brief Returns the number of arguments.
  virtual std::size_t argcount() const noexcept = 0;
  /// @brief Calls the stored function with the arguments in the vector.
  virtual lisp_t operator()(const std::vector<lisp_t>&) const = 0;
};

/// @brief This class can store a function object with a variable number of
/// arguments.
///
/// The stored function can be called either via a variadic operator() or an
/// operator() taking a vector of arguments.
template<typename... Args>
class func_t: public func_base
{
public:
  func_t(std::function<lisp_t(Args...)> fun)
    : _fun(std::move(fun))
  {}
  lisp_t operator()(Args&&... args) const { return _fun(std::forward<Args>(args)...); }
  lisp_t operator()(const std::vector<lisp_t>& vec) const override
  {
    return std::apply(_fun, make_tuple<std::remove_cvref_t<Args>...>(vec));
  }
  std::size_t argcount() const noexcept override { return sizeof...(Args); }

private:
  std::function<lisp_t(Args...)> _fun;
};

/// @brief The @a make_fun set of overloads creates an object of type func_t<Args...>.
///
/// The object created can then be stored in the subr_t object and later called
/// with the right number of parameters. There are three overloads taking care
/// of different cases of functions.
///
/// The first one handles simple function pointers.
///
/// @tparam Args The zero or more function argument types.
/// @param fun The function pointer.
///
/// @returns A pointer to @a func_base which can then be stored in a subr_t.
template<typename... Args>
auto make_fun(lisp_t (*fun)(Args...))
{
  return new func_t<Args...>(fun);
}

/// @brief Helper function to create a func_base pointer for lambdas.
///
/// @tparam Lambda The lambda function type.
/// @tparam Args The zero or more function argument types.
/// @param fun The lambda function.
///
/// @returns A pointer to @a func_base which can then be stored in a subr_t.
template<typename Lambda, typename... Args>
auto make_fun(Lambda fun, lisp_t (Lambda::*)(Args...) const)
{
  return new func_t<Args...>(fun);
}

/// @brief Create a func_base pointer for lambdas.
///
/// @tparam Lambda The lambda function type.
/// @param fun The lambda function.
///
/// @returns A pointer to @a func_base which can then be stored in a subr_t.
template<typename Lambda>
auto make_fun(Lambda&& fun)
{
  return make_fun(std::forward<Lambda>(fun), &Lambda::operator());
}

/// @Brief Structure describing a built-in function.
///
/// Built-in function can have zero, one, two, or three parameters.  They can
/// either evaluate their parameters or not (special forms).  Function can be
/// either spread (fixed number of arguments) or nospread (variable number of
/// arguments).
///
class subr_t final
{
public:
  /// @brief Indicates if the arguments are to be evaluated or not.
  enum class subr
  {
    EVAL,
    NOEVAL
  };
  /// @brief Indicates if the function is a spread or no spread function.
  ///
  /// With a spread function the arguments are bound to individual parameters
  /// and in the case of a no spread function the arguments are passed to the
  /// function as a single list parameter.
  enum class spread
  {
    SPREAD,
    NOSPREAD
  };

  /// @brief Constructor of a primitive function.
  ///
  /// A primitive function is made up of it's print name, the function to call,
  /// an indicator to tell if the arguments are to be evaluated or not, and the
  /// spread/no spread indicator.
  ///
  /// @tparam Args The zero or more function argument types.
  /// @param pname The print name.
  /// @param fun A function which can take zero or more arguments.
  /// @param subr The eval/noeval indicator (subr or fsubr (special form)).
  /// @param spread Spread/no spread.
  template<typename... Args>
  subr_t(std::string_view pname, func_base* fun, enum subr subr, enum spread spread)
    : name(pname),
      subr(subr),
      spread(spread),
      _fun(fun)
  {}

  /// @brief The print name.
  std::string name;
  /// @brief Eval or no eval.
  enum subr subr
  {
    subr::EVAL
  };
  /// @brief Spread or no spread.
  enum spread spread
  {
    spread::SPREAD
  };

  /// @brief Number of arguments.
  ///
  /// @returns The argument count.
  std::size_t argcount() const noexcept { return _fun->argcount(); }

  /// @brief Call the stored function. The lisp_t arguments to the function are
  /// taken from the destination block.
  ///
  /// @param dest A destination block.
  ///
  /// @returns The result of calling the function.
  lisp_t operator()(destblock_t* dest) const;

  /// @brief The vector type storing the subr functions.
  using subr_vector = std::vector<subr_t>;
  /// @brief The index type.
  using subr_index = subr_vector::size_type;

  /// @brief Register a primitive function (subr).
  ///
  /// @returns The index which uniquely identifies the function.
  static subr_index put(subr_t&& subr);
  /// @brief Retrieve a primitive function given its index.
  ///
  /// @returns A const subr_t object reference used for inspection.
  static const subr_t& get(subr_index index) { return subr_store[index]; }

private:
  /// @brief The subr_t object owns the pointer to the function object.
  std::unique_ptr<func_base> _fun;

  // NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
  /// @brief Maps a print name to the subr index.
  static std::unordered_map<std::string, subr_index> subr_map;
  /// @brief Each primitive function is stored in a vector and the subr_index
  /// is the index into this vector.
  static subr_vector subr_store;
  // NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)
};

inline subr_t::subr_index subr_t::put(subr_t&& subr)
{
  auto p = subr_map.find(subr.name);
  if(p != subr_map.end())
    throw lisp_error(error_errc::redefinition_of_subr, subr.name);
  auto index = subr_store.size();
  subr_map.insert(std::pair(subr.name, index));
  subr_store.push_back(std::move(subr));
  return index;
}

inline lisp_t subr_t::operator()(destblock_t* dest) const
{
  if(argcount() != _fun->argcount())
    throw lisp_error(error_errc::wrong_number_of_args, name);
  auto make_vector = [](destblock_t* dest, std::size_t count) -> std::vector<lisp_t> {
    std::vector<lisp_t> result;
    std::ranges::for_each(std::views::iota(std::size_t(1), count + 1) | std::views::reverse,
      [&result, dest](auto arg) { result.push_back(dest[arg].val()); });
    return result;
  };
  return (*_fun)(make_vector(dest, argcount()));
}

/// @brief Lambda representation.
///
class lambda_t final: public ref_count<lambda_t>
{
public:
  /// @brief Default constructor.
  lambda_t() = default;

  /// @brief The S-expression representation of the lambda function.
  lisp_t body;
  /// @brief The list of arguments.
  lisp_t args;
  /// @brief The number of arguments.
  std::int8_t count{0};
  /// @brief True if arguments are evaluated, false if not (nlambda).
  bool eval{true};

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return pool().allocate(); }
  static void operator delete(void* x) { pool().deallocate(x); }
  static void operator delete(lambda_t* x, std::destroying_delete_t) { pool().deallocate(x); }

  /// @brief Count of the number of free lambda objects in the pool.
  static std::size_t freecount() { return pool().size(); }

private:
  lambda_t(pool_test_t) { throw std::runtime_error("lambda_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = lisp::pool<lambda_t, 256>;
  static pool_t& pool()
  {
    static pool_t _pool; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    return _pool;
  }
};

/// @brief A closure (static binding).
///
class closure_t final: public ref_count<closure_t>
{
public:
  /// @brief Default constructor.
  closure_t() = default;

  lisp_t cfunction;
  lisp_t closed;
  lisp_t cvalues;
  std::uint8_t count{0};

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return pool().allocate(); }
  static void operator delete(void* x) { pool().deallocate(x); }
  static void operator delete(closure_t* x, std::destroying_delete_t) { pool().deallocate(x); }

  static std::size_t freecount() { return pool().size(); }

private:
  closure_t(pool_test_t) { throw std::runtime_error("closure_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = lisp::pool<closure_t, 256>;
  static pool_t& pool()
  {
    static pool_t _pool; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    return _pool;
  }
};

/// @brief Wraps an integer value.
class integer_t final
{
public:
  using value_type = std::int64_t;

  integer_t()
    : _value(0)
  {}
  explicit integer_t(value_type i)
    : _value(i)
  {}
  operator value_type() const { return _value; }

  integer_t& operator=(value_type i)
  {
    _value = i;
    return *this;
  }

  value_type value() const { return _value; }

private:
  value_type _value;
};

inline auto format_as(integer_t value) { return value.value(); }

/// @brief Wraps an floating point value.
class double_t final
{
public:
  using value_type = double;

  double_t()
    : _value(0.0)
  {}
  explicit double_t(value_type d)
    : _value(d)
  {}
  operator value_type() const { return _value; }

  double_t& operator=(value_type d)
  {
    _value = d;
    return *this;
  }

  value_type value() const { return _value; }

private:
  value_type _value;
};

inline auto format_as(double_t value) { return value.value(); }

class string_t final: public ref_count<string_t>
{
public:
  string_t() = default;
  string_t(std::string s)
    : _value(std::move(s))
  {}

  const std::string& value() const { return _value; }

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return pool().allocate(); }
  static void operator delete(void* x) { pool().deallocate(x); }
  static void operator delete(string_t* x, std::destroying_delete_t) { pool().deallocate(x); }

  static std::size_t freecount() { return pool().size(); }

private:
  string_t(pool_test_t) { throw std::runtime_error("string_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = lisp::pool<string_t, 256>;
  static pool_t& pool()
  {
    static pool_t _pool; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    return _pool;
  }

  std::string _value;
};

struct subr_index
{
  subr_t::subr_index index;
};

/// @brief A representation of a C++ variable linked to a lisp variable.
///
/// Wraps a lisp_t value in such a way that the value can be changed from
/// either the C++ context or the lisp context and have the value be reflected
/// in both.
///
class cvariable_t final
{
public:
  explicit cvariable_t(lisp_t value)
    : _value(std::move(value))
  {}
  cvariable_t() = delete;
  ~cvariable_t() = default;
  cvariable_t(const cvariable_t& other) = delete;
  cvariable_t(cvariable_t&& other) noexcept
    : _value(std::move(other._value))
  {}
  cvariable_t& operator=(cvariable_t&& other) noexcept
  {
    _value = std::move(other._value);
    return *this;
  }
  cvariable_t& operator=(const cvariable_t& other) = delete;
  cvariable_t& operator=(lisp_t value)
  {
    _value = std::move(value);
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

class file_t final: public ref_count<file_t>
{
public:
  file_t(std::unique_ptr<io::source> source)
    : _source(std::move(source))
  {}
  file_t(std::unique_ptr<io::sink> sink)
    : _sink(std::move(sink))
  {}
  file_t(std::istream& stream)
    : _source(std::make_unique<io::stream_source>(stream))
  {}
  file_t(std::ostream& stream)
    : _sink(std::make_unique<io::stream_sink>(stream))
  {}
  file_t(const std::string& string)
    : _source(std::make_unique<io::string_source>(string))
  {}

  file_t(const file_t&) = delete;
  file_t(file_t&& file) noexcept
    : _source(std::move(file._source)),
      _sink(std::move(file._sink))
  {}
  file_t& operator=(const file_t&) = delete;
  file_t& operator=(file_t&& file) noexcept
  {
    if(this != &file)
    {
      _source = std::move(file._source);
      _sink = std::move(file._sink);
    }
    return *this;
  }
  ~file_t() = default;

  bool has_source() { return !!_source; }
  bool has_sink() { return !!_sink; }

  // source
  io::source& source() { return *_source; }
  char getch()
  {
    ptrcheck<io::source>();
    return _source->getch();
  }
  void ungetch(char c)
  {
    ptrcheck<io::source>();
    _source->ungetch(c);
  }
  std::optional<std::string> getline()
  {
    ptrcheck<io::source>();
    return _source->getline();
  }

  // sink
  io::sink& sink() { return *_sink; }
  void putch(char c, io::escape esc = io::escape::NO)
  {
    ptrcheck<io::sink>();
    _sink->putch(c, esc);
  }
  void puts(std::string_view s)
  {
    ptrcheck<io::sink>();
    _sink->puts(s);
  }
  void terpri()
  {
    ptrcheck<io::sink>();
    _sink->terpri();
  }
  void flush()
  {
    ptrcheck<io::sink>();
    _sink->flush();
  }

  template<typename... Ts>
  void format(std::string_view f, const Ts&... t)
  {
    auto ret = fmt::vformat(f, fmt::make_format_args(t...));
    _sink->puts(ret);
  }

  void close()
  {
    _source.release();
    if(_sink)
      _sink->flush();
    _sink.release();
  }

private:
  std::unique_ptr<io::source> _source;
  std::unique_ptr<io::sink> _sink;

  template<typename S>
  void ptrcheck() const
  {
    if constexpr(std::is_same_v<S, io::source>)
    {
      if(!_source)
        throw lisp_error(error_errc::no_source);
    }
    else if constexpr(std::is_same_v<S, io::sink>)
    {
      if(!_sink)
        throw lisp_error(error_errc::no_sink);
    }
  }
};

using ref_closure_t = ref_ptr<closure_t>;
using ref_cons_t = ref_ptr<cons_t>;
using ref_file_t = ref_ptr<file_t>;
using ref_lambda_t = ref_ptr<lambda_t>;
using ref_string_t = ref_ptr<string_t>;

/// @brief A class able to hold a value of any lisp type
///
/// The lisp objects are stored in a variant with accessor methods to set or
/// get the values. There is no checking of the correct type for the accessor
/// functions so calling them for the wrong type throws an exception.
class object final: public ref_count<object>
{
public:
  object() = default;
  ~object() = default;

  /// @brief Constructor for T which is a direct member of the variant holding
  /// the value.
  template<typename T>
  explicit object(T x)
    : _u(x)
  {}

  /// @brief Specialization for plain integer types.
  template<typename T>
    requires std::convertible_to<T, integer_t::value_type>
  explicit object(T i)
    : _u(integer_t{i})
  {}

  /// @brief Constructor for the plain double type.
  explicit object(double_t::value_type d)
    : _u(double_t{d})
  {}

  /// @brief Construct an object containing a cvariable_t value.
  object(cvariable_t&& x)
    : _u(std::move(x))
  {}

  /// @brief Copy consructor
  object(const object&) = delete;
  /// @brief Assignment operator
  object& operator=(const object& x) = delete;

  /// @brief Move constructor
  object(object&& x) noexcept { *this = std::move(x); }

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

  /// @brief This enum class indicates the type of value stored in the object.
  ///
  /// Each of the values in the enum maps one to one directly to the type
  /// stored in the variant. The enum exists to make the value type more
  /// readable.
  enum class type : std::uint8_t
  {
    Nil = 0,  // Mapped to std::monostate
    Symbol,   // An atomic symbol
    Integer,  // A 32 bit integer
    Float,    // A floating point value
    Indirect, // Used when a value is stored in a closure
    Cons,     // A pair
    String,   // A character string
    Subr,     // A primitive function
    Lambda,   // A lambda function
    Closure,  // A static binding object
    Environ,  // An environment stack type for gc use
    File,     // A file pointer
    Cvariable // A pointer to c-variable
  };

  /// @brief The symbol (literal atom).
  auto as_symbol() -> ref_symbol_t { return std::get<ref_symbol_t>(_u); }

  /// @brief Get and set the value of a symbol.
  auto value() const -> lisp_t { return std::get<ref_symbol_t>(_u)->value; }
  void value(const lisp_t&);

  /// @brief The integer value.
  auto as_integer() const -> integer_t { return std::get<integer_t>(_u); }

  /// @brief The floating point value (double)
  auto as_double() const -> double_t
  {
    if(gettype() == type::Integer)
      return double_t{static_cast<double_t::value_type>(std::get<integer_t>(_u).value())};
    return std::get<double_t>(_u);
  }

  /// @brief Get the indirect value.
  auto indirect() const -> lisp_t { return std::get<indirect_t>(_u).value; }

  /// @brief Cons cell and car/cdr.
  auto cons() const -> const cons_t& { return *std::get<ref_cons_t>(_u); }
  /// @brief The 'car' of the cons cell, no type checks.
  auto car() const -> lisp_t { return std::get<ref_cons_t>(_u)->car; }
  /// @brief Set the 'car' of the cons cell, no type checks.
  void car(lisp_t x) { std::get<ref_cons_t>(_u)->car = std::move(x); }
  /// @brief The 'cdr' of the cons cell, no type checks.
  auto cdr() const -> lisp_t { return std::get<ref_cons_t>(_u)->cdr; }
  /// @brief Set the 'cdr' of the cons cell, no type checks.
  void cdr(lisp_t x) { std::get<ref_cons_t>(_u)->cdr = std::move(x); }

  /// @brief Character string.
  auto as_string() const -> const std::string& { return std::get<ref_string_t>(_u)->value(); }

  /// @brief Compiled function (subr).
  auto subr() const -> const subr_t& { return subr_t::get(std::get<subr_index>(_u).index); }

  /// @brief Lambda expression.
  auto lambda() const -> const lambda_t& { return *std::get<ref_lambda_t>(_u); }

  /// @brief Closure.
  auto closure() -> closure_t& { return *std::get<ref_closure_t>(_u); }

  /// @brief Destination environment.
  auto environ() -> destblock_t* { return std::get<destblock_t*>(_u); }

  /// @brief File reference.
  auto file() -> ref_file_t& { return std::get<ref_file_t>(_u); }

  /// @brief Link to a c/c++ variable.
  auto cvariable() -> cvariable_t& { return std::get<cvariable_t>(_u); }

  /// @brief Get the string if the object holds a symbol or a proper string.
  const std::string& getstr() const
  {
    return gettype() == type::String ? std::get<ref_string_t>(_u)->value() : std::get<ref_symbol_t>(_u)->pname;
  }

  /// @brief Access the type of object.
  type gettype() const { return static_cast<type>(_u.index()); }

  /// @brief The new and delete operators uses a memory pool to create objects.
  static void* operator new(std::size_t) { return pool().allocate(); }
  static void operator delete(void* x) { pool().deallocate(x); }
  static void operator delete(object* x, std::destroying_delete_t) { pool().deallocate(x); }
  static std::size_t freecount() { return pool().size(); }

private:
  // One entry for each type.  Types that has no, or just one, value are
  // indicated by a comment.
  std::variant<std::monostate, // Nil
    ref_symbol_t,              // Symbol
    integer_t,                 // Integer
    double_t,                  // Floating point (double)
    indirect_t,                // Indirect
    ref_cons_t,                // Cons
    ref_string_t,              // String
    subr_index,                // Subr
    ref_lambda_t,              // Lambda/Nlambda
    ref_closure_t,             // Closure
    destblock_t*,              // Environ
    ref_file_t,                // File
    cvariable_t                // Cvariable
    >
    _u;

  /// @brief Used to achieve coverage of operator delete(void*) when an
  /// exception is thrown in the constructor.
  object(pool_test_t) { throw std::runtime_error("object"); }
  /// @brief Uses the exception throwing constructor for coverage.
  template<class T>
  friend void pool_test();

  /// @brief Memory pool for objects.
  using pool_t = lisp::pool<object, 256>;
  static pool_t& pool()
  {
    static pool_t _pool; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    return _pool;
  }
};

/// @brief Return the type of the object or the object inside a lisp_t object.
///
/// Since lisp_t may be nullptr, which represents the nil value, it's not safe
/// to call lisp_t->gettype() directly.
inline object::type type_of(const lisp_t& a) { return a == nullptr ? object::type::Nil : a->gettype(); }
inline object::type type_of(const object& a) { return a.gettype(); }
inline object::type type_of(const cvariable_t& a) { return *a == nullptr ? object::type::Nil : a->gettype(); }

inline void object::value(const lisp_t& x)
{
  auto& var = std::get<ref_symbol_t>(_u);
  if(type_of(var->value) == object::type::Cvariable)
  {
    auto& cvar = var->value->cvariable();
    cvar = x;
  }
  else
    var->value = x;
}

/// @brief Symbols which are used internally
extern const lisp_t T;
extern const lisp_t C_APPEND;
extern const lisp_t C_AUTOLOAD;
extern const lisp_t C_BROKEN;
extern const lisp_t C_BT;
extern const lisp_t C_CLOSURE;
extern const lisp_t C_CONS;
extern const lisp_t C_CVARIABLE;
extern const lisp_t C_DOT;
extern const lisp_t C_ENDOFFILE;
extern const lisp_t C_ENVIRON;
extern const lisp_t C_EOF;
extern const lisp_t C_ERROR;
extern const lisp_t C_FILE;
extern const lisp_t C_FLOAT;
extern const lisp_t C_FSUBR;
extern const lisp_t C_GO;
extern const lisp_t C_INDIRECT;
extern const lisp_t C_INTEGER;
extern const lisp_t C_LAMBDA;
extern const lisp_t C_NLAMBDA;
extern const lisp_t C_OLDDEF;
extern const lisp_t C_QUOTE;
extern const lisp_t C_READ;
extern const lisp_t C_REDEFINED;
extern const lisp_t C_RESET;
extern const lisp_t C_RETURN;
extern const lisp_t C_STRING;
extern const lisp_t C_SUBR;
extern const lisp_t C_SYMBOL;
extern const lisp_t C_VERSION;
extern const lisp_t C_WRITE;

} // namespace lisp

#endif
