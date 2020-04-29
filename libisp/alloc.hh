//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <vector>
#include "lisp.hh"

namespace lisp
{
inline constexpr auto PN_RECLAIM = "reclaim";       // initiate garbage collection
inline constexpr auto PN_CONS = "cons";             // make a new cons cell
inline constexpr auto PN_FREECOUNT = "freecount";   // number of free cells
inline constexpr auto PN_OBARRAY = "obarray";       // return list of all atoms

class evaluator;

class alloc
{
public:
  alloc(lisp&);
  ~alloc();

  static const int CONSCELLS = 1000;     // Number of cells in each block
  static const int DESTBLOCKSIZE = 3000; // Size of destination block area
  static const int MINCONSES = 2000;     // Minimum number of cells after gc
  static const int SAVEARRAYSIZE = 1000; // Size of gc save array
  static const int MAXHASH = 255;        // Max number of hash buckets

  static const int NOCONSARGS = 0; // Don't reclaim arguments of cons
  static const int CONSARGS = 1;   // Reclaim called from cons

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

  /* variables */
  LISPT savearray[SAVEARRAYSIZE];
  int savept = 0;
  obarray_t* obarray[MAXHASH];
  LISPT freelist = nullptr;
  LISPT gcgag = nullptr; // Nonnil means print gc message.

  /* functions */
  LISPT intern(const char*);
  LISPT getobject();

  LISPT mkprim(const char* pname, short nrpar, lisp_type type);
  void mkprim(const char* pname, LISPT (*fname)(lisp&), short nrpar, lisp_type type);
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT), short nrpar, lisp_type type);
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT), short nrpar, lisp_type type);
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT, LISPT), short nrpar, lisp_type type);
  LISPT mkarglis(LISPT alist, int& count);
  LISPT mklambda(LISPT args, LISPT def, lisp_type type);
  LISPT mkstring(const char*);
  LISPT mknumber(int);
  LISPT mkatom(char*);
  LISPT mkfloat(double);

  destblock_t* dalloc(int);
  void dfree(destblock_t*);
  void dzero();

  char* realmalloc(unsigned int);
  void save(LISPT v) { savearray[savept++] = v; }
  LISPT unsave() { return savearray[--savept]; }

  PRIMITIVE reclaim(lisp&, LISPT incr); /* Number of blocks to increase with */
  PRIMITIVE cons(lisp&, LISPT, LISPT);
  PRIMITIVE xobarray(lisp&);
  PRIMITIVE freecount(lisp&);

  void add_mark_object(LISPT* o) { markobjs.push_back(o); }

private:
  lisp& _lisp;                  // Context

  evaluator& e() { return _lisp.e(); }
  conscells_t* newpage();
  int sweep();
  void mark(LISPT);
  LISPT doreclaim(int doconsargs, int incr);
  int hash(const char* str);
  LISPT buildatom(const char* s, int cpy);
  LISPT puthash(const char* str, obarray_t* obarray[], int cpy);

  LISPT foo1 = nullptr; // Protect arguments of cons when gc.
  LISPT foo2 = nullptr;
  conscells_t* conscells = nullptr;               // Cons cell storage.
  destblock_t destblock[DESTBLOCKSIZE]; // Destblock area.
  int destblockused = 0;                    // Index to last slot in destblock.
  std::vector<LISPT*> markobjs;
};

inline LISPT cons(lisp& l, LISPT a, LISPT b) { return l.a().cons(l, a, b); }
inline LISPT reclaim(lisp& l, LISPT a) { return l.a().reclaim(l, a); }
inline LISPT xobarray(lisp& l) { return l.a().xobarray(l); }
inline LISPT freecount(lisp& l) { return l.a().freecount(l); }

inline LISPT intern(lisp& l, const char* s) { return l.a().intern(s); }
inline void mkprim(lisp& l, const char* pname, LISPT (*fname)(lisp&), short nrpar, lisp_type type)
{
  l.a().mkprim(pname, fname, nrpar, type);
}
inline void mkprim(lisp& l, const char* pname, LISPT (*fname)(lisp&, LISPT), short nrpar, lisp_type type)
{
  l.a().mkprim(pname, fname, nrpar, type);
}
inline void mkprim(lisp& l, const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT), short nrpar, lisp_type type)
{
  l.a().mkprim(pname, fname, nrpar, type);
}
inline void mkprim(lisp& l, const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT, LISPT), short nrpar, lisp_type type)
{
  l.a().mkprim(pname, fname, nrpar, type);
}

#if 0
inline LISPT mkstring(const char* s) { return alloc::mkstring(s); }
inline LISPT mknumber(int i) { return alloc::mknumber(i); }
inline LISPT mkatom(char* s) { return alloc::mkatom(s); }
inline LISPT mkfloat(double d) { return alloc::mkfloat(d); }
inline LISPT getobject() { return alloc::getobject(); }
inline alloc::destblock_t* dalloc(int i) { return alloc::dalloc(i); }
inline void dfree(alloc::destblock_t* d) { alloc::dfree(d); }
inline void dzero() { alloc::dzero(); }
inline char* realmalloc(unsigned int u) { return alloc::realmalloc(u); }

/*
 * A simple way of protecting internal lisp objects from
 * the garbage collector.
 */
inline void save(LISPT v) { alloc::save(v); }
inline void unsave(LISPT& v) { v = alloc::unsave(); }
#endif

} // namespace lisp
