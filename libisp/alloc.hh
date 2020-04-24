//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp {

class alloc
{
public:
  alloc();
  ~alloc() = default;

  static const int CONSCELLS = 1000;     // Number of cells in each block
  static const int DESTBLOCKSIZE = 3000; // Size of destination block area
  static const int MINCONSES = 2000;     // Minimum number of cells after gc
  static const int SAVEARRAYSIZE = 1000; // Size of gc save array
  static const int MAXHASH = 255;        // Max number of hash buckets

  static const int NOCONSARGS = 0; // Don't reclaim arguments of cons
  static const int CONSARGS = 1;   // Reclaim called from cons

  struct conscells
  {
    struct lispt cells[CONSCELLS];
    struct conscells* next;
  };

  struct destblock
  {
    char type;
    union
    {
      LISPT d_lisp;
      int d_integer;
      destblock* d_environ;
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
  using destblock_t = struct destblock;
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
  static LISPT mkstring(const char*);
  static LISPT mknumber(long);
  static LISPT mkatom(char*);
  static LISPT mkfloat(double);
  static destblock_t* dalloc(int);
  static void dfree(destblock_t*);
  static void dzero();
  static void init_alloc();
  static char* realmalloc(unsigned int);
  static void save(LISPT v) { savearray[savept++] = v; }
  static LISPT unsave() { return savearray[--savept]; }

  static PRIMITIVE reclaim(LISPT incr); /* Number of blocks to increase with */
  static PRIMITIVE cons(LISPT, LISPT);
  static PRIMITIVE xobarray();
  static PRIMITIVE freecount();

private:
  static conscells_t* newpage();
  static int sweep();
  static void mark(LISPT*);
  static LISPT doreclaim(int doconsargs, long incr);
  static int hash(const char* str);
  static LISPT buildatom(const char* s, int cpy);
  static LISPT puthash(const char* str, obarray_t* obarray[], int cpy);

  static LISPT* foo1;                          // Protect arguments of cons when gc.
  static LISPT* foo2;
  static int nrconses;                         // Number of conses since last gc.
  static conscells_t* conscells;               // Cons cell storage.
  static destblock_t destblock[DESTBLOCKSIZE]; // Destblock area.
  static int destblockused;                    // Index to last slot in destblock.
};

inline void init_alloc()
{
  alloc::init_alloc();
}
inline LISPT intern(const char* s)
{
  return alloc::intern(s);
}
inline LISPT cons(LISPT a, LISPT b)
{
  return alloc::cons(a, b);
}
inline LISPT mkstring(const char* s)
{
  return alloc::mkstring(s);
}
inline LISPT mknumber(long i)
{
  return alloc::mknumber(i);
}
inline LISPT mkatom(char* s)
{
  return alloc::mkatom(s);
}
inline LISPT mkfloat(double d)
{
  return alloc::mkfloat(d);
}
inline LISPT getobject()
{
  return alloc::getobject();
}
inline alloc::destblock_t* dalloc(int i)
{
  return alloc::dalloc(i);
}
inline void dfree(alloc::destblock_t* d)
{
  alloc::dfree(d);
}
inline void dzero()
{
  alloc::dzero();
}
inline char* realmalloc(unsigned int u)
{
  return alloc::realmalloc(u);
}

} // namespace lisp