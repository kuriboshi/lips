//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <vector>
#include "lisp.hh"

namespace lisp
{
inline constexpr auto PN_RECLAIM = "reclaim";     // initiate garbage collection
inline constexpr auto PN_CONS = "cons";           // make a new cons cell
inline constexpr auto PN_FREECOUNT = "freecount"; // number of free cells
inline constexpr auto PN_OBARRAY = "obarray";     // return list of all atoms

class evaluator;

class alloc
{
public:
  alloc(lisp&);
  ~alloc();

  static constexpr int CONSCELLS = 1000;     // Number of cells in each block
  static constexpr int DESTBLOCKSIZE = 3000; // Size of destination block area
  static constexpr int MINCONSES = 2000;     // Minimum number of cells after gc
  static constexpr int MAXHASH = 255;        // Max number of hash buckets

  struct conscells_t
  {
    lisp_t cells[CONSCELLS];
    conscells_t* next;
  };

  /*
   * Each hashbucket contains a symbol and a pointer to the next
   * symbol in that bucket.
   */
  struct obarray_t
  {
    LISPT sym;
    obarray_t* onext;
  };

  static obarray_t* globals[MAXHASH]; // Atoms created by 'intern' which are the same across all instances
  std::vector<LISPT> savearray;       // Stack of objects which needs to be protected from gc
  obarray_t* obarray[MAXHASH];        // Atoms local to each interpreter instance
  LISPT freelist = nullptr;           // List of free objects
  LISPT gcgag = nullptr;              // Nonnil means print gc message

  LISPT getobject();

  //
  // Initializes a lisp symbol with the pname NAME to contain the same value as
  // the C variable that CVAR points to. CVAR is set to VAL.  Whenever CVAR is
  // changed the corresponding lisp variable changes and vice versa.
  //
  static void initcvar(LISPT* cvar, const std::string& name, LISPT val)
  {
    LISPT t = intern(name);
    set(t->symval().value, CVARIABLE, new lisp_t);
    t->symval().value->cvarval(cvar);
    *cvar = val;
  }

  static LISPT intern(const std::string&);

  static void mkprim(const std::string& pname, func0_t fname, subr_t::subr_type, subr_t::spread_type);
  static void mkprim(const std::string& pname, func1_t fname, subr_t::subr_type, subr_t::spread_type);
  static void mkprim(const std::string& pname, func2_t fname, subr_t::subr_type, subr_t::spread_type);
  static void mkprim(const std::string& pname, func3_t fname, subr_t::subr_type, subr_t::spread_type);

  LISPT mklambda(LISPT args, LISPT def, lisp_type type);
  LISPT mkstring(const std::string&);
  LISPT mknumber(int);
  LISPT mkatom(const std::string&);
  LISPT mkfloat(double);

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

  void add_mark_object(LISPT* o) { markobjs.push_back(o); }

private:
  lisp& _lisp; // Context

  evaluator& e() { return _lisp.e(); }
  conscells_t* newpage();
  int sweep();
  void mark(LISPT);
  LISPT doreclaim(int incr = 0);
  LISPT mkarglis(LISPT alist, int& count);

  static LISPT buildatom(const std::string& s, bool copy, LISPT newatom);
  static obarray_t* findatom(int hv, const std::string& str, obarray_t* obarray[]);
  static int hash(const std::string& str);
  static LISPT puthash(int hv, const std::string& str, obarray_t* obarray[], bool copy, LISPT newatom);
  static LISPT mkprim(const std::string& pname, subr_t* subr);

  conscells_t* conscells = nullptr;     // Cons cell storage.
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
inline LISPT gcprotect(lisp& l, LISPT& a) { l.a().add_mark_object(&a); return C_NIL; }
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
