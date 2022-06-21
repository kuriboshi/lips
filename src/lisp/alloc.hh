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

#include <array>
#include <deque>
#include <list>
#include <vector>

#include "lisp.hh"

namespace lisp
{
/// @brief Destination block is used to collect the parameters to a function.
///
/// @details The destblock_t is used to store variables and their values.  Each
/// block of variable/value pairs is proceeded by a control block which
/// contains the following pieces of information: The size of the block, the
/// index of the variable/value pair currently being set, and a link to another
/// destblock_t in a chain of blocks.
///
class destblock_t
{
private:
  struct control_block
  {
    std::int8_t size;
    std::int8_t index;
    destblock_t* link;
  };
  struct var_val_pair
  {
    LISPT var;
    LISPT val;
  };
  std::variant<control_block, var_val_pair> u;

public:
  void reset() { u = var_val_pair{NIL, NIL}; }
  
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

  void var(LISPT x) { std::get<var_val_pair>(u).var = x; }
  LISPT var() const { return std::get<var_val_pair>(u).var; }
  void val(LISPT x) { std::get<var_val_pair>(u).val = x; }
  LISPT val() const { return std::get<var_val_pair>(u).val; }
};

class alloc
{
public:
  alloc();
  ~alloc();

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
  cvariable_t& initcvar(const std::string& name, LISPT val)
  {
    auto t = mkatom(name);
    t->symbol().value = new lisp_t;
    t->value()->set(cvariable_t(val));
    return t->value()->cvarval();
  }

  /// @brief Create a symbol in the global symbol table, accessable from all
  /// lisp instances.
  ///
  /// @param pname The print name.
  ///
  /// @return The interned symbol.
  ///
  static LISPT intern(const std::string& pname);

  /// @brief Register a primitive function.
  ///
  /// @param pname [in] The print name of the internal function.  This is put in
  /// the global symbol table using intern.
  /// @param subr [in] The calling details of the function.  This in includes
  /// number of parameters, whether the function is spread, nospread, or
  /// halfspread, whether the function should evaluate it's arguments or not.
  ///
  static void mkprim(subr_t subr)
  {
    auto s = new lisp_t;
    s->set(subr_index{subr_t::put(subr)});
    LISPT f = intern(subr.name);
    f->value(s);
  }

  LISPT mkatom(const std::string&);

  /// @brief Create a string.
  ///
  LISPT mkstring(const std::string&);

  /// @brief Create an integer number.
  ///
  LISPT mknumber(int);

  /// @brief Create a double.
  ///
  LISPT mkfloat(double);

  /// @brief Make a lambda object.
  ///
  /// @details Make a lambda object with the argument ARGS and definition DEF
  /// and the type TYPE, which is either LAMBDA or NLAMBDA.
  ///
  /// @return The lambda object.
  ///
  LISPT mklambda(LISPT args, LISPT def, type type);

  /// @brief Allocate a destination block.
  ///
  /// @param size The size of the destination block.
  ///
  destblock_t* dalloc(int size);

  /// @brief Free a destination block.
  ///
  /// @details The destination blocks are freed in the reverse order of their
  /// allocation.
  ///
  /// @param The block to free.
  ///
  void dfree(destblock_t* block);

  /// @brief Release all destination blocks.
  ///
  void dzero();

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
  LISPT cons(LISPT, LISPT);

  /// @brief Build a list of symbols in the local symbol table.
  ///
  /// @return Returns a list of local symbols in no particular order.
  ///
  LISPT obarray();

  /// @brief Number of free cell in the free cell list.
  ///
  /// @return The number of free cells.
  ///
  LISPT freecount();
  
private:

  /// @brief Returns the global symbol table.
  static symbol::symbol_store_t& global_symbols()
  {
    return lisp_t::symbol_collection().symbol_store(symbol::symbol_collection::global_id);
  }
  /// @brief The local symbol table, local for each lisp instance.
  symbol::symbol_store_t& local_symbols;

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
  LISPT mkarglist(LISPT alist, std::int8_t& count);

  /// @brief Size of destination block area
  static constexpr int DESTBLOCKSIZE = 3000;
  /// @brief Destination block area.
  std::array<destblock_t, DESTBLOCKSIZE> destblock;
  /// @brief Index to last slot in destblock.
  int destblockused = 0;
};

inline LISPT intern(const std::string& s) { return alloc::intern(s); }

inline LISPT mkstring(lisp& l, const std::string& s) { return l.a().mkstring(s); }
inline LISPT mkstring(const std::string& s) { return mkstring(lisp::current(), s); }
inline LISPT mkatom(lisp& l, const std::string& s) { return l.a().mkatom(s); }
inline LISPT mkatom(const std::string& s) { return mkatom(lisp::current(), s); }
inline LISPT mknumber(lisp& l, int i) { return l.a().mknumber(i); }
inline LISPT mknumber(int i) { return mknumber(lisp::current(), i); }
inline LISPT mkfloat(lisp& l, double d) { return l.a().mkfloat(d); }
inline LISPT mkfloat(double d) { return mkfloat(lisp::current(), d); }

/// @brief Creates a lisp string.
inline LISPT operator"" _s(const char* s, std::size_t)
{
  return mkstring(s);
}

/// @brief Simpler way to create an atom.
inline LISPT operator"" _a(const char* s, std::size_t)
{
  return mkatom(s);
}

/// @brief Creates a number.
inline LISPT operator"" _l(unsigned long long i)
{
  return mknumber(i);
}

/// @brief Creates a floating point value.
inline LISPT operator"" _l(long double d)
{
  return mkfloat(d);
}

/// @brief Evaluates a lisp expression in a string.
inline LISPT operator"" _e(const char* s, std::size_t)
{
  return eval(s);
}

inline LISPT mklist(lisp& l, LISPT t)
{
  return cons(l, t, NIL);
}

template<typename... Ts>
LISPT mklist(lisp& l, LISPT t, Ts ...ts)
{
  return cons(l, t, mklist(l, ts...));
}

inline LISPT mklist(LISPT t)
{
  return cons(t, NIL);
}

template<typename... Ts>
LISPT mklist(LISPT t, Ts ...ts)
{
  return cons(t, mklist(ts...));
}

inline cvariable_t& initcvar(const std::string& name, LISPT val) { return lisp::current().a().initcvar(name, val); }
inline cvariable_t& initcvar(alloc& a, const std::string& name, LISPT val) { return a.initcvar(name, val); }

inline void mkprim(const std::string& pname, subr_t::func0_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(subr_t(pname, fun, subr, spread));
}

inline void mkprim(const std::string& pname, subr_t::func1_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(subr_t(pname, fun, subr, spread));
}

inline void mkprim(const std::string& pname, subr_t::func2_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(subr_t(pname, fun, subr, spread));
}

inline void mkprim(const std::string& pname, subr_t::func3_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(subr_t(pname, fun, subr, spread));
}

} // namespace lisp

#endif
