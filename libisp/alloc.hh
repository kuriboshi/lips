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

  static constexpr int NOCONSARGS = 0; // Don't reclaim arguments of cons
  static constexpr int CONSARGS = 1;   // Reclaim called from cons

  struct conscells_t
  {
    lisp_t cells[CONSCELLS];
    conscells_t* next;
  };

  enum class block_type
  {
    EMPTY = 0,
    LISPT,
    ENVIRON
  };

  struct destblock_t
  {
    block_type type = block_type::EMPTY;
    union
    {
      LISPT d_lisp;
      int d_integer;
      destblock_t* d_environ;
    } var, val;
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
  static LISPT verboseflg;            // If T then some events will be more verbose (gc and function redefinitions)
  std::vector<LISPT> savearray; // Stack of objects which needs to be protected from gc
  int savept = 0;
  obarray_t* obarray[MAXHASH];        // Atoms local to each interpreter instance
  LISPT freelist = nullptr;           // List of free objects
  LISPT gcgag = nullptr;              // Nonnil means print gc message

  LISPT getobject();

  /*
   * Initializes a lisp symbol with the pname NAME to contain the same value as
   * the C variable that CVAR points to. CVAR is set to VAL.  Whenever CVAR is
   * changed the corresponding lisp variable changes and vice versa.
   */
  static void initcvar(LISPT* cvar, const char* name, LISPT val)
  {
    LISPT t = intern(name);
    set(t->symval().value, CVARIABLE, new lisp_t);
    t->symval().value->cvarval(cvar);
    *cvar = val;
  }

  static LISPT intern(const char*);
  static void mkprim(const char* pname, LISPT (*fname)(lisp&), short nrpar, lisp_type type);
  static void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT), short nrpar, lisp_type type);
  static void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT), short nrpar, lisp_type type);
  static void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT, LISPT), short nrpar, lisp_type type);

  LISPT mklambda(LISPT args, LISPT def, lisp_type type);
  LISPT mkstring(const char*);
  LISPT mknumber(int);
  LISPT mkatom(const char*);
  LISPT mkfloat(double);

  destblock_t* dalloc(int);
  void dfree(destblock_t*);
  void dzero();

  void save(LISPT v) { savearray.push_back(v); }
  LISPT unsave() { auto val = savearray.back(); savearray.pop_back(); return val; }

  PRIMITIVE reclaim(LISPT incr); /* Number of blocks to increase with */
  PRIMITIVE cons(LISPT, LISPT);
  PRIMITIVE xobarray();
  PRIMITIVE freecount();

  void add_mark_object(LISPT* o) { markobjs.push_back(o); }

  static char* realmalloc(unsigned int);

private:
  lisp& _lisp; // Context

  evaluator& e() { return _lisp.e(); }
  conscells_t* newpage();
  int sweep();
  void mark(LISPT);
  LISPT doreclaim(int doconsargs, int incr);
  LISPT mkarglis(LISPT alist, int& count);

  static LISPT buildatom(const char* s, bool copy, LISPT newatom);
  static obarray_t* findatom(int hv, const char* str, obarray_t* obarray[]);
  static int hash(const char* str);
  static LISPT puthash(int hv, const char* str, obarray_t* obarray[], bool copy, LISPT newatom);
  static LISPT mkprim(const char* pname, short nrpar, lisp_type type);

  LISPT foo1 = nullptr; // Protect arguments of cons when gc.
  LISPT foo2 = nullptr;
  conscells_t* conscells = nullptr;     // Cons cell storage.
  destblock_t destblock[DESTBLOCKSIZE]; // Destblock area.
  int destblockused = 0;                // Index to last slot in destblock.
  std::vector<LISPT*> markobjs;
};

inline LISPT cons(lisp& l, LISPT a, LISPT b) { return l.a().cons(a, b); }
inline LISPT reclaim(lisp& l, LISPT a) { return l.a().reclaim(a); }
inline LISPT xobarray(lisp& l) { return l.a().xobarray(); }
inline LISPT freecount(lisp& l) { return l.a().freecount(); }

inline LISPT intern(lisp& l, const char* s) { return l.a().intern(s); }

inline LISPT mklambda(lisp& l, LISPT args, LISPT def, lisp_type type) { return l.a().mklambda(args, def, type); }
inline LISPT mkstring(lisp& l, const char* s) { return l.a().mkstring(s); }
inline LISPT mknumber(lisp& l, int i) { return l.a().mknumber(i); }
inline LISPT mkatom(lisp& l, const char* s) { return l.a().mkatom(s); }
inline LISPT mkfloat(lisp& l, double d) { return l.a().mkfloat(d); }

inline LISPT getobject(lisp& l) { return l.a().getobject(); }

inline void initcvar(LISPT* cvar, const char* name, LISPT var) { return alloc::initcvar(cvar, name, var); }
inline void mkprim(const char* pname, LISPT (*fname)(lisp&), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}
inline void mkprim(lisp& l, const char* pname, LISPT (*fname)(lisp&, LISPT), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}
inline void mkprim(lisp& l, const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}
inline void mkprim(lisp& l, const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT, LISPT), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}

} // namespace lisp
