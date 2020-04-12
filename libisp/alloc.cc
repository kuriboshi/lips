/*
 * Lips, lisp shell
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */
#include <string.h>
#include <stdlib.h>
#include "lisp.hh"

#define CONSCELLS 1000     /* Number of cells in each block */
#define DESTBLOCKSIZE 3000 /* Size of destination block area */
#define MINCONSES 2000     /* Minimum number of cells after gc */
#define SAVEARRAYSIZE 1000 /* Size of gc save array */

#define NOCONSARGS 0 /* Don't reclaim arguments of cons. */
#define CONSARGS 1   /* Reclaim called from cons. */

LISPT savearray[SAVEARRAYSIZE]; /* Gc save */
int savept = 0;
OBARRAY* obarray[MAXHASH]; /* Array containing global symbols */
LISPT freelist;            /* List of free cells */

extern void finish(int);

static LISPT gcgag;        /* Nonnil means print gc message. */
static LISPT *foo1, *foo2; /* Protect arguments of cons when gc. */
struct conscells
{
  struct lispt cells[CONSCELLS];
  struct conscells* next;
};
static struct conscells* conscells; /* Cons cell storage */
static int nrconses;                /* Number of conses since last gc. */
static struct destblock destblock[DESTBLOCKSIZE]; /* Destblock area */
static int destblockused; /* Index to last slot in destblock */

#ifdef FLOATING
static unsigned short point = 31;
static int p0 = 0;

/*
 * The structure floats contains the data for floating point values.
 * It contains 128 values together with four longs with a total of
 * 128 bits. When a float is allocated the corresponding mark bit is
 * set. Marks are also used during gc. Then next pointer is not used
 * currently so the number of floats is limited to 128.
 */
static struct floats
{
  long marks[4];
  double fdata[128];
  struct floats* fnext;
} floats;
#endif /* FLOATING */

#include "garb.hh"

/*
 * safemalloc is defined in terms of realmalloc depending on
 * whether the `lint' is defined or not.
 */
char* realmalloc(unsigned int size)
{
  char* cp = (char*)malloc(size);
  if (cp == NULL)
    {
      error(OUT_OF_MEMORY, C_NIL);
      return NULL;
    }
  return cp;
}

/* 
 * newpage - Allocates a new block of cons cells and links it into the 
 *           current list of blocks.
 */
static struct conscells* newpage()
{
  struct conscells* newp = new struct conscells;
  if (newp == nullptr)
    return conscells;
  newp->next = conscells;
  return newp;
}

/* 
 * sweep - Sweep up unused cons cells. Free objects that are pointers to 
 *         space allocated by malloc. These objects has the type field set 
 *         to NIL, and the rest of the field is the pointer.
 */
static int sweep()
{
  struct conscells* cc;

  int nrfreed = 0;
  int i = 0;
  for (cc = conscells; cc && cc->cells[i].gcmark; cc = cc->next, i = 0)
    {
      for (; i < CONSCELLS && cc->cells[i].gcmark; i++)
        {
          cc->cells[i].gcmark = 0;
        }
      if (i < CONSCELLS)
        break;
    }
  SET(freelist, FREE, (LISPT) &cc->cells[i]);
  nrfreed++;
  LISPT f = freelist;
  if (TYPEOF(f) == CPOINTER)
    free(CPOINTVAL(f));
  i++; /* Check *next* cell */
  for (; cc; cc = cc->next, i = 0)
    for (; i < CONSCELLS; i++)
      if (!cc->cells[i].gcmark)
        {
          nrfreed++;
          /*
	   * C pointers must be freed.
	   */
          if (TYPEOF(f) == CPOINTER)
            free(CPOINTVAL(f));
          SET(FREEVAL(f), FREE, (LISPT) &cc->cells[i]);
          f = FREEVAL(f);
        }
      else
        {
          cc->cells[i].gcmark = 0;
        }
  FREEVAL(f) = C_NIL;
  return nrfreed;
}

/*
 * mark - Mark a cell and traverse car and cdr of cons cells and all other
 *        fields of type LISPT.
 */
static void mark(LISPT* x)
{
#ifdef FLOATING
  int y;
#endif

  switch (TYPEOF(*x))
    {
    case CONS:
      if (MARKED(*x))
        break;
      MARK(*x);
      mark(&CAR(*x));
      mark(&CDR(*x));
      break;
#ifdef FLOATING
    case FLOAT:
      y = (int) (&FLOATVAL(*x) - &floats.fdata[0]);
      floats.marks[(y / 32)] |= 1 << (31 - y % 32);
      break;
#endif /* FLOATING */
    case SYMBOL:
      break;
    case LAMBDA:
    case NLAMBDA:
      MARK(*x);
      mark(&LAMVAL(*x).lambdarep);
      mark(&LAMVAL(*x).arglist);
      break;
    case CLOSURE:
      MARK(*x);
      mark(&CLOSVAL(*x).cfunction);
      mark(&CLOSVAL(*x).closed);
      mark(&CLOSVAL(*x).cvalues);
      break;
    case STRING:
      MARK(*x);
      break;
    case INDIRECT:
      MARK(*x);
      mark(&INDIRECTVAL(*x));
      break;
    case NIL:
      break;
    default:
      MARK(*x);
      break;
    }
}

/*
 * doreclaim - Garbage collector, mark all used cells then
 *	       sweep up garbage.  Argument doconsargs is nonzero
 *	       
 */
static LISPT doreclaim(int doconsargs, long incr)
{
  int i;

  if (ISNIL(gcgag))
    fprintf(primerr, "garbage collecting\n");
#ifdef FLOATING
  for (i = 0; i < 4; i++) floats.marks[i] = 0;
  point = 31;
  p0 = 0;
#endif /* FLOATING */
  if (C_T != NULL)
    MARK(C_T);
  if (doconsargs)
    {
      mark(foo1);
      mark(foo2);
    }
  if (dest != NULL)
    for (i = dest[0].val.d_integer; i > 0; i--)
      {
        mark(&dest[i].var.d_lisp);
        mark(&dest[i].val.d_lisp);
      }
  for (i = 0; markobjs[i] != NULL; i++) mark(markobjs[i]);
#if 0
  if (env != NULL && ENVVAL(env) != NULL)
    mark((LISPT *) &ENVVAL(env));
#endif
  for (i = 0; i < toctrl; i++)
    if (control[i].type == CTRL_LISP && control[i].u.lisp != NULL
      && TYPEOF(control[i].u.lisp) != ENVIRON)
      mark(&control[i].u.lisp);
  for (i = 0; i < MAXHASH; i++)
    for (auto* l = obarray[i]; l; l = l->onext)
      {
        MARK(l->sym);
        mark(&(SYMVAL(l->sym).value));
        mark(&(SYMVAL(l->sym).plist));
      }
  for (i = destblockused - 1; i >= 0; i--)
    {
      if (destblock[i].type != 0)
        {
          mark(&destblock[i].var.d_lisp);
          mark(&destblock[i].val.d_lisp);
        }
    }
  if (savept)
    for (i = savept; i; i--) mark(&savearray[i - 1]);
  /*
   * A new page is allocated if the number of conses is lower
   * than MINCONSES or if requested by calling doreclaim with
   * incr greater than 0.
   */
  if (nrconses < MINCONSES || incr > 0)
    {
      do
        conscells = newpage();
      while (incr-- > 0); /* At least one page more */
    }
  int nrfreed = sweep();
  nrconses = 0;
  if (ISNIL(gcgag))
    fprintf(primerr, "%d cells freed\n", nrfreed);
  return C_NIL;
}

/*
 * reclaim - Lips function reclaim interface. incr is the number of pages
 *           to inrease storage with.
 */
PRIMITIVE reclaim(LISPT incr) /* Number of blocks to increase with */
{
  long i;

  if (ISNIL(incr))
    i = 0;
  else
    {
      CHECK(incr, INTEGER);
      i = INTVAL(incr);
    }
  doreclaim(NOCONSARGS, i);
  return C_NIL;
}

LISPT getobject()
{
  if (ISNIL(freelist))
    doreclaim(NOCONSARGS, 0L);

  LISPT f;
  SET(f, CONS, (LISPT) freelist);
  freelist = FREEVAL(freelist);
  return f;
}

/*
 * cons - Builds a cons cell out of arguments A and B. Reclaims space
 *        and allocates new blocks if necessary.
 */
PRIMITIVE cons(LISPT a, LISPT b)
{
  if (ISNIL(freelist))
    {
      foo1 = &a;
      foo2 = &b;
      doreclaim(CONSARGS, 0L);
    }

  LISPT f;
  SET(f, CONS, (LISPT) freelist);
  freelist = FREEVAL(freelist);
  CAR(f) = a;
  CDR(f) = b;
  return f;
}

/*
 * mkstring - Strings are stored in a cons cell with car set to NIL and
 *            cdr is set to the string pointer.
 */
LISPT mkstring(const char* str)
{
  char* c = (char*) safemalloc((unsigned) strlen(str) + 1);
  if (c == NULL)
    return C_ERROR;
  strcpy(c, str);
  LISPT s = getobject();
  STRINGVAL(s) = c;
  s->type = STRING;
  return s;
}

LISPT mknumber(long i)
{
  LISPT c = getobject();
  INTVAL(c) = i;
  c->type = INTEGER;
  return c;
}

/*
 * Calculates hash value of string.
 */
static int hash(const char* str)
{
  int sum = 0;

  for (; *str; str++) sum += *str;
  return sum % MAXHASH;
}

/*
 * buildatom - Builds an atom with printname in S. Parameter CPY is non-zero
 *             if the printname should be saved.
 */
static LISPT buildatom(const char* s, int cpy)
{
  LISPT newatom;
  LISPT l;
  static LISPT unbound = NULL;

  if (unbound == NULL)
    SET(unbound, UNBOUND, getobject());
  newatom = getobject();
  if (newatom == C_ERROR)
    return C_ERROR;
  if (cpy)
    {
      char* pname = safemalloc((unsigned) strlen(s) + 1);
      if (pname == NULL)
        return C_ERROR;
      strcpy(pname, s);
      SYMVAL(newatom).pname = pname;
    }
  else
    SYMVAL(newatom).pname = s;
  SYMVAL(newatom).plist = C_NIL;
  SYMVAL(newatom).value = unbound;
  SET(l, SYMBOL, newatom);
  return l;
}

/*
 * puthash - Puts an atom with printname STR in the hash array OBARRAY.
 *           If the atom is already in obarray, no new atom is created.
 *           Copy str if CPY is non-zero. Returns the atom.
 */
static LISPT puthash(const char* str, OBARRAY* obarray[], int cpy)
{
  OBARRAY* ob;

  int hv = hash(str);
  for (ob = *(obarray + hv); ob; ob = ob->onext)
    {
      if (!strcmp(SYMVAL(ob->sym).pname, str))
        return ob->sym;
    }
  ob = (OBARRAY*) safemalloc(sizeof(OBARRAY));
  if (ob == NULL)
    return C_ERROR;
  ob->onext = obarray[hv];
  ob->sym = buildatom(str, cpy);
  if (EQ(ob->sym, C_ERROR))
    {
      free((char*) ob);
      return C_ERROR;
    }
  obarray[hv] = ob;
  return ob->sym;
}

/*
 * intern - Make interned symbol in hasharray obarray. Str is not copied
 *          so this is only used with constant strings during init.
 */
LISPT intern(const char* str)
{
  return puthash(str, obarray, 0);
}

/*
 * mkatom - Generates interned symbol like intern but copy str.
 */
LISPT mkatom(char* str)
{
  return puthash(str, obarray, 1);
}

/* This isn't converted yet */
/*
 * mkfloat - Make a floating point number.
 */
LISPT mkfloat(double num)
{
  LISPT rval;

#ifdef FLOATING
again:
  while (p0 < 4)
    if ((floats.marks[p0] & (1 << point)) == 0)
      break;
    else
      {
        point--;
        if (!point)
          {
            p0++;
            point = 31;
          }
      }
  if (p0 == 4)
    {
      nrconses = MINCONSES;
      doreclaim(NOCONSARGS, 0L);
      goto again;
    }
  SET(rval, FLOAT, &floats.fdata[p0 * 32 + (31 - point)]);
  floats.marks[p0] |= 1 << point;
  *((double*) POINTER(rval)) = num;
#endif /* FLOATING */
  SET(rval, FLOAT, getobject());
  return rval;
}

/*
 * dalloc - Allocates a destination block of size size. Returns NULL if
 *          no more space available.
 */
struct destblock* dalloc(int size)
{
  int i;

  if (size <= DESTBLOCKSIZE - destblockused)
    {
      destblockused += size;
      for (i = 0; i < size; i++)
        {
          destblock[destblockused - 1 - i].var.d_lisp = C_NIL;
          destblock[destblockused - 1 - i].val.d_lisp = C_NIL;
        }
    }
  else
    return NULL;
  return &destblock[destblockused - size];
}

/*
 * dfree - Free a destination block. The size of a block i (hopefully)
 *         stored in the cdr of the first element. If it isn't, look
 *         elsewhere.
 */
void dfree(struct destblock* ptr)
{
  destblockused -= ptr->val.d_integer + 1;
}

/*
 * dzero - Frees all destination blocks.
 */
void dzero()
{
  destblockused = 0;
}

void init_alloc()
{
  destblockused = 0;
  conscells = NULL;
  conscells = newpage(); /* Allocate one page of storage */
  if (conscells == NULL)
    {
      fprintf(stderr, "Sorry, no memory for cons cells\n");
      finish(1);
    }
  sweep();
  initcvar(&gcgag, "gcgag", C_NIL);
  mkprim(PN_RECLAIM, reclaim, 1, SUBR);
  mkprim(PN_CONS, cons, 2, SUBR);
}
