//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <vector>
#include "lisp.hh"

namespace lisp
{
class alloc
{
public:
  alloc() = delete;
  ~alloc() = delete;

  static const int CONSCELLS = 1000;     // Number of cells in each block
  static const int DESTBLOCKSIZE = 3000; // Size of destination block area
  static const int MINCONSES = 2000;     // Minimum number of cells after gc
  static const int SAVEARRAYSIZE = 1000; // Size of gc save array
  static const int MAXHASH = 255;        // Max number of hash buckets

  static const int NOCONSARGS = 0; // Don't reclaim arguments of cons
  static const int CONSARGS = 1;   // Reclaim called from cons

  struct conscells
  {
    struct lisp_t cells[CONSCELLS];
    struct conscells* next;
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
  struct obarray
  {
    LISPT sym;
    struct obarray* onext;
  };

  using conscells_t = struct conscells;
  using obarray_t = struct obarray;

  /* variables */
  static LISPT savearray[SAVEARRAYSIZE];
  static int savept;
  static obarray_t* obarray[MAXHASH];
  static LISPT freelist;
  static LISPT gcgag; // Nonnil means print gc message.

  /* functions */
  static LISPT intern(const char*);
  static LISPT getobject();

  static LISPT mkprim(const char* pname, short nrpar, lisp_type type);
  static void mkprim(const char* pname, LISPT (*fname)(), short nrpar, lisp_type type);
  static void mkprim(const char* pname, LISPT (*fname)(LISPT), short nrpar, lisp_type type);
  static void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT), short nrpar, lisp_type type);
  static void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT, LISPT), short nrpar, lisp_type type);
  static LISPT mklambda(LISPT args, LISPT def, lisp_type type);

  static LISPT mkstring(const char*);
  static LISPT mknumber(int);
  static LISPT mkatom(char*);
  static LISPT mkfloat(double);
  static destblock_t* dalloc(int);
  static void dfree(destblock_t*);
  static void dzero();
  static void init();
  static char* realmalloc(unsigned int);
  static void save(LISPT v) { savearray[savept++] = v; }
  static LISPT unsave() { return savearray[--savept]; }

  static PRIMITIVE reclaim(LISPT incr); /* Number of blocks to increase with */
  static PRIMITIVE cons(LISPT, LISPT);
  static PRIMITIVE xobarray();
  static PRIMITIVE freecount();

  static void add_mark_object(LISPT* o) { markobjs.push_back(o); }

private:
  static conscells_t* newpage();
  static int sweep();
  static void mark(LISPT);
  static LISPT doreclaim(int doconsargs, int incr);
  static int hash(const char* str);
  static LISPT buildatom(const char* s, int cpy);
  static LISPT puthash(const char* str, obarray_t* obarray[], int cpy);

  static LISPT foo1; // Protect arguments of cons when gc.
  static LISPT foo2;
  static int nrconses;                         // Number of conses since last gc.
  static conscells_t* conscells;               // Cons cell storage.
  static destblock_t destblock[DESTBLOCKSIZE]; // Destblock area.
  static int destblockused;                    // Index to last slot in destblock.
  static std::vector<LISPT*> markobjs;
};

inline LISPT intern(const char* s) { return alloc::intern(s); }
inline LISPT cons(LISPT a, LISPT b) { return alloc::cons(a, b); }
inline void mkprim(const char* pname, LISPT (*fname)(), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}
inline void mkprim(const char* pname, LISPT (*fname)(LISPT), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}
inline void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}
inline void mkprim(const char* pname, LISPT (*fname)(LISPT, LISPT, LISPT), short nrpar, lisp_type type)
{
  alloc::mkprim(pname, fname, nrpar, type);
}
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

} // namespace lisp
