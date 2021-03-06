//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <array>
#include <vector>
#include <list>
#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_RECLAIM = "reclaim";     // Initiate garbage collection
inline constexpr auto PN_CONS = "cons";           // Make a new cons cell
inline constexpr auto PN_FREECOUNT = "freecount"; // Number of free cells
inline constexpr auto PN_OBARRAY = "obarray";     // Return list of all atoms

class evaluator;

class alloc
{
public:
  alloc(lisp&);
  ~alloc();

  static constexpr int CONSCELLS = 1000;     // Number of cells in each block
  static constexpr int DESTBLOCKSIZE = 3000; // Size of destination block area
  static constexpr int MINCONSES = 2000;     // Minimum number of cells after gc
  static constexpr int MAXHASH = 256;        // Max number of hash buckets

  struct conscells_t
  {
    std::array<lisp_t, CONSCELLS> cells;
  };

  //
  // Each hashbucket contains a symbol and a pointer to the next symbol in that
  // bucket.
  //
  struct bucket_t
  {
    LISPT sym;
    bucket_t* onext;
  };

  using obarray_t = std::array<bucket_t*, MAXHASH>;
  static obarray_t globals;     // Atoms created by 'intern' which are the same across all instances
  obarray_t obarray;            // Atoms local to each interpreter instance
  std::vector<LISPT> savearray; // Stack of objects which needs to be protected from gc
  LISPT freelist = nullptr;     // List of free objects
  LISPT gcgag = nullptr;        // Nonnil means print gc message

  LISPT getobject();

  //
  // Initializes a lisp symbol with the pname NAME to contain the same value as
  // the C variable that CVAR points to. CVAR is set to VAL.  Whenever CVAR is
  // changed the corresponding lisp variable changes and vice versa.
  //
  static void initcvar(LISPT* cvar, const std::string& name, LISPT val)
  {
    LISPT t = intern(name);
    set(t->symbol().value, lisp_type::CVARIABLE, new lisp_t);
    t->symvalue()->cvarval(cvar);
    *cvar = val;
  }

  static LISPT intern(const std::string&);

  static void mkprim(const std::string& pname, func0_t fname, subr_t::subr_type, subr_t::spread_type);
  static void mkprim(const std::string& pname, func1_t fname, subr_t::subr_type, subr_t::spread_type);
  static void mkprim(const std::string& pname, func2_t fname, subr_t::subr_type, subr_t::spread_type);
  static void mkprim(const std::string& pname, func3_t fname, subr_t::subr_type, subr_t::spread_type);

  LISPT mkatom(const std::string&);
  LISPT mkstring(const std::string&);
  LISPT mknumber(int);
  LISPT mkfloat(double);
  LISPT mklambda(LISPT args, LISPT def, lisp_type type);

  destblock_t* dalloc(int);
  void dfree(destblock_t*);
  void dzero();

  void save(LISPT v) { savearray.push_back(v); }
  LISPT unsave()
  {
    auto val = savearray.back();
    savearray.pop_back();
    return val;
  }

  PRIMITIVE reclaim(LISPT incr); /* Number of blocks to increase with */
  PRIMITIVE cons(LISPT, LISPT);
  PRIMITIVE xobarray();
  PRIMITIVE freecount();
  
  void gcprotect(LISPT& o) { markobjs.push_back(&o); }

private:
  lisp& _lisp;
  evaluator& e() { return _lisp.e(); }
  conscells_t* newpage();
  int sweep();
  void mark(LISPT);
  LISPT doreclaim(int incr = 0);
  LISPT mkarglis(LISPT alist, int& count);

  static LISPT buildatom(const std::string& s, LISPT newatom);
  static bucket_t* findatom(int hv, const std::string&, const obarray_t&);
  static int hash(const std::string& str);
  static LISPT puthash(int hv, const std::string&, obarray_t&, LISPT newatom);
  static LISPT mkprim(const std::string& pname, subr_t* subr);

  std::list<conscells_t*> conscells;     // Cons cell storage.
  destblock_t destblock[DESTBLOCKSIZE]; // Destblock area.
  int destblockused = 0;                // Index to last slot in destblock.
  std::vector<LISPT*> markobjs;
};

inline LISPT cons(lisp& l, LISPT a, LISPT b) { return l.a().cons(a, b); }
inline LISPT cons(LISPT a, LISPT b) { return cons(lisp::current(), a, b); }
inline LISPT reclaim(lisp& l, LISPT a) { return l.a().reclaim(a); }
inline LISPT reclaim(LISPT a) { return reclaim(lisp::current(), a); }
inline LISPT xobarray(lisp& l) { return l.a().xobarray(); }
inline LISPT xobarray() { return xobarray(lisp::current()); }
inline LISPT freecount(lisp& l) { return l.a().freecount(); }
inline LISPT freecount() { return freecount(lisp::current()); }
inline LISPT gcprotect(lisp& l, LISPT& a) { l.a().gcprotect(a); return C_NIL; }
inline LISPT gcprotect(LISPT& a) { return gcprotect(lisp::current(), a); }

inline LISPT intern(const std::string& s) { return alloc::intern(s); }

inline LISPT mklambda(lisp& l, LISPT args, LISPT def, lisp_type type) { return l.a().mklambda(args, def, type); }
inline LISPT mklambda(LISPT args, LISPT def, lisp_type type) { return mklambda(lisp::current(), args, def, type); }
inline LISPT mkstring(lisp& l, const std::string& s) { return l.a().mkstring(s); }
inline LISPT mkstring(const std::string& s) { return mkstring(lisp::current(), s); }
inline LISPT mknumber(lisp& l, int i) { return l.a().mknumber(i); }
inline LISPT mknumber(int i) { return mknumber(lisp::current(), i); }
inline LISPT mkatom(lisp& l, const std::string& s) { return l.a().mkatom(s); }
inline LISPT mkatom(const std::string& s) { return mkatom(lisp::current(), s); }
inline LISPT mkfloat(lisp& l, double d) { return l.a().mkfloat(d); }
inline LISPT mkfloat(double d) { return mkfloat(lisp::current(), d); }

inline LISPT getobject(lisp& l) { return l.a().getobject(); }
inline LISPT getobject() { return getobject(lisp::current()); }

inline void initcvar(LISPT* cvar, const std::string& name, LISPT var) { return alloc::initcvar(cvar, name, var); }

inline void mkprim(const std::string& pname, func0_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  alloc::mkprim(pname, fname, subr, spread);
}
inline void mkprim(const std::string& pname, func1_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  alloc::mkprim(pname, fname, subr, spread);
}
inline void mkprim(const std::string& pname, func2_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  alloc::mkprim(pname, fname, subr, spread);
}
inline void mkprim(const std::string& pname, func3_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  alloc::mkprim(pname, fname, subr, spread);
}

} // namespace lisp
