//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <array>
#include <deque>
#include <list>
#include <vector>

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
namespace pn
{
inline constexpr auto RECLAIM = "reclaim";     // Initiate garbage collection
inline constexpr auto CONS = "cons";           // Make a new cons cell
inline constexpr auto FREECOUNT = "freecount"; // Number of free cells
inline constexpr auto OBARRAY = "obarray";     // Return list of all atoms
} // namespace pn

class evaluator;

//
// The destblock_t is used to store variables and their values.  Each block of
// variable/value pairs is proceeded by a control block which contains the
// following pieces of information: The size of the block, the index of the
// variable/value pair currently being set, and a link to another destblock_t
// in a chain of blocks.
//
struct destblock_t
{
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
  LISPT var() { return std::get<var_val_pair>(u).var; }
  void val(LISPT x) { std::get<var_val_pair>(u).val = x; }
  LISPT val() { return std::get<var_val_pair>(u).val; }
};

class alloc
{
public:
  alloc(lisp&);
  ~alloc();

  struct conscells_t
  {
    static constexpr int CONSCELLS = 10240; // Number of cells in each block
    std::array<lisp_t, CONSCELLS> cells;
  };

  static symbol::symbol_store_t& global_symbols()
  {
    return lisp_t::symbol_collection().symbol_store(symbol::symbol_collection::global_id);
  }
  symbol::symbol_store_t& local_symbols;

  std::deque<lisp_t*> freelist; // List of free objects

  LISPT getobject();

  //
  // Initializes a lisp symbol with the pname NAME to contain the same value as
  // the C variable that CVAR points to. CVAR is set to VAL.  Whenever CVAR is
  // changed the corresponding lisp variable changes and vice versa.
  //
  static cvariable& initcvar(const std::string& name, LISPT val)
  {
    auto t = intern(name);
    t->symbol().value = LISPT(new lisp_t);
    t->symvalue()->cvarval(cvariable(val));
    return t->symvalue()->cvarval();
  }

  static LISPT intern(const std::string&);

  static void mkprim(const std::string& pname, subr_t subr);
  static void mkprim(const std::string& pname, subr_t::func0_t fname, enum subr_t::subr, enum subr_t::spread);
  static void mkprim(const std::string& pname, subr_t::func1_t fname, enum subr_t::subr, enum subr_t::spread);
  static void mkprim(const std::string& pname, subr_t::func2_t fname, enum subr_t::subr, enum subr_t::spread);
  static void mkprim(const std::string& pname, subr_t::func3_t fname, enum subr_t::subr, enum subr_t::spread);

  LISPT mkatom(const std::string&);
  LISPT mkstring(const std::string&);
  LISPT mknumber(int);
  LISPT mkfloat(double);
  LISPT mklambda(LISPT args, LISPT def, type type);

  destblock_t* dalloc(int);
  void dfree(destblock_t*);
  void dzero();

  PRIMITIVE reclaim(LISPT incr); // Number of blocks to increase with
  PRIMITIVE cons(LISPT, LISPT);
  PRIMITIVE obarray();
  PRIMITIVE freecount();
  
private:
  lisp& _lisp;
  evaluator& e() { return _lisp.e(); }
  conscells_t* newpage();
  LISPT mkarglis(LISPT alist, int& count);

  std::list<conscells_t*> conscells;         // Cons cell storage.
  static constexpr int DESTBLOCKSIZE = 3000; // Size of destination block area
  destblock_t destblock[DESTBLOCKSIZE];      // Destblock area.
  int destblockused = 0;                     // Index to last slot in destblock.
};

inline LISPT cons(lisp& l, LISPT a, LISPT b) { return l.a().cons(a, b); }
inline LISPT cons(LISPT a, LISPT b) { return cons(lisp::current(), a, b); }
inline LISPT reclaim(lisp& l, LISPT a) { return l.a().reclaim(a); }
inline LISPT reclaim(LISPT a) { return reclaim(lisp::current(), a); }
inline LISPT obarray(lisp& l) { return l.a().obarray(); }
inline LISPT obarray() { return obarray(lisp::current()); }
inline LISPT freecount(lisp& l) { return l.a().freecount(); }
inline LISPT freecount() { return freecount(lisp::current()); }

inline LISPT intern(const std::string& s) { return alloc::intern(s); }

inline LISPT mklambda(lisp& l, LISPT args, LISPT def, type type) { return l.a().mklambda(args, def, type); }
inline LISPT mklambda(LISPT args, LISPT def, type type) { return mklambda(lisp::current(), args, def, type); }
inline LISPT mkstring(lisp& l, const std::string& s) { return l.a().mkstring(s); }
inline LISPT mkstring(const std::string& s) { return mkstring(lisp::current(), s); }
inline LISPT mknumber(lisp& l, int i) { return l.a().mknumber(i); }
inline LISPT mknumber(int i) { return mknumber(lisp::current(), i); }
inline LISPT mkatom(lisp& l, const std::string& s) { return l.a().mkatom(s); }
inline LISPT mkatom(const std::string& s) { return mkatom(lisp::current(), s); }
inline LISPT mkfloat(lisp& l, double d) { return l.a().mkfloat(d); }
inline LISPT mkfloat(double d) { return mkfloat(lisp::current(), d); }

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

inline LISPT getobject(lisp& l) { return l.a().getobject(); }
inline LISPT getobject() { return getobject(lisp::current()); }

inline cvariable& initcvar(const std::string& name, LISPT val) { return alloc::initcvar(name, val); }

inline void mkprim(const std::string& pname, subr_t::func0_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(pname, subr_t(subr, spread, fun));
}

inline void mkprim(const std::string& pname, subr_t::func1_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(pname, subr_t(subr, spread, fun));
}

inline void mkprim(const std::string& pname, subr_t::func2_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(pname, subr_t(subr, spread, fun));
}

inline void mkprim(const std::string& pname, subr_t::func3_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  alloc::mkprim(pname, subr_t(subr, spread, fun));
}

} // namespace lisp
