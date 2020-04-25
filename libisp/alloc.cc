/*
 * Lips, lisp shell
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */

#include "libisp.hh"

extern lisp::LISPT history;
extern lisp::LISPT histnum;
extern lisp::LISPT path;
extern lisp::LISPT home;
extern lisp::LISPT alias_expanded;
extern void finish(int);

namespace lisp {

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

/*
 * markobjs contains pointers to all LISPT type c variables that
 * contains data to be retained during gc.
 */
LISPT* markobjs[] = {&top, &rstack, &history, &histnum, &evaluator::fun, &evaluator::expression, &evaluator::args, &path,
                     &home, &verboseflg, &topprompt, &promptform, &brkprompt, &currentbase, &interactive, &version,
                     &alloc::gcgag, &alias_expanded, &C_EOF, nullptr};

LISPT* alloc::foo1 = nullptr; // Protect arguments of cons when gc.
LISPT* alloc::foo2 = nullptr;
int alloc::nrconses = 0;                                   // Number of conses since last gc.
alloc::conscells_t* alloc::conscells = nullptr;            // Cons cell storage.
alloc::destblock_t alloc::destblock[alloc::DESTBLOCKSIZE]; // Destblock area.
int alloc::destblockused = 0;                              // Index to last slot in destblock.

alloc::alloc()
{
  init_alloc();
}

/*
 * realmalloc - wrapper around malloc which returns a char*.
 */
char* alloc::realmalloc(unsigned int size)
{
  char* cp = (char*)malloc(size);
  if(cp == nullptr)
  {
    error(OUT_OF_MEMORY, C_NIL);
    return nullptr;
  }
  return cp;
}

/* 
 * newpage - Allocates a new block of cons cells and links it into the 
 *           current list of blocks.
 */
alloc::conscells_t* alloc::newpage()
{
  auto* newp = new conscells_t;
  if(newp == nullptr)
    return conscells;
  newp->next = conscells;
  return newp;
}

/* 
 * sweep - Sweep up unused cons cells. Free objects that are pointers to 
 *         space allocated by malloc. These objects has the type field set 
 *         to NIL, and the rest of the field is the pointer.
 */
int alloc::sweep()
{
  conscells_t* cc;

  int nrfreed = 0;
  int i = 0;
  for(cc = conscells; cc && cc->cells[i].gcmark; cc = cc->next, i = 0)
  {
    for(; i < CONSCELLS && cc->cells[i].gcmark; i++)
    {
      cc->cells[i].gcmark = 0;
    }
    if(i < CONSCELLS)
      break;
  }
  SET(freelist, FREE, (LISPT)&cc->cells[i]);
  nrfreed++;
  LISPT f = freelist;
  if(TYPEOF(f) == CPOINTER)
    free(CPOINTVAL(f));
  i++; /* Check *next* cell */
  for(; cc; cc = cc->next, i = 0)
    for(; i < CONSCELLS; i++)
      if(!cc->cells[i].gcmark)
      {
        nrfreed++;
        /*
	   * C pointers must be freed.
	   */
        if(TYPEOF(f) == CPOINTER)
          free(CPOINTVAL(f));
        SET(FREEVAL(f), FREE, (LISPT)&cc->cells[i]);
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
void alloc::mark(LISPT* x)
{
  switch(TYPEOF(*x))
  {
    case CONS:
      if(MARKED(*x))
        break;
      MARK(*x);
      mark(&CAR(*x));
      mark(&CDR(*x));
      break;
#ifdef FLOATING
    case FLOAT: {
      int y = (int)(&FLOATVAL(*x) - &floats.fdata[0]);
      floats.marks[(y / 32)] |= 1 << (31 - y % 32);
      break;
    }
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
LISPT alloc::doreclaim(int doconsargs, long incr)
{
  if(ISNIL(gcgag))
    fprintf(primerr, "garbage collecting\n");
#ifdef FLOATING
  for(int i = 0; i < 4; i++) floats.marks[i] = 0;
  point = 31;
  p0 = 0;
#endif /* FLOATING */
  if(C_T != nullptr)
    MARK(C_T);
  if(doconsargs)
  {
    mark(foo1);
    mark(foo2);
  }
  if(evaluator::dest != nullptr)
    for(int i = evaluator::dest[0].val.d_integer; i > 0; i--)
    {
      mark(&evaluator::dest[i].var.d_lisp);
      mark(&evaluator::dest[i].val.d_lisp);
    }
  for(int i = 0; markobjs[i] != nullptr; i++) mark(markobjs[i]);
#if 0
  if (env != nullptr && ENVVAL(env) != nullptr)
    mark((LISPT *) &ENVVAL(env));
#endif
  for(int i = 0; i < evaluator::toctrl; i++)
    if(evaluator::control[i].type == evaluator::CTRL_LISP && evaluator::control[i].u.lisp != nullptr
      && TYPEOF(evaluator::control[i].u.lisp) != ENVIRON)
      mark(&evaluator::control[i].u.lisp);
  for(int i = 0; i < MAXHASH; i++)
    for(auto* l = obarray[i]; l; l = l->onext)
    {
      MARK(l->sym);
      mark(&(SYMVAL(l->sym).value));
      mark(&(SYMVAL(l->sym).plist));
    }
  for(int i = destblockused - 1; i >= 0; i--)
  {
    if(destblock[i].type != 0)
    {
      mark(&destblock[i].var.d_lisp);
      mark(&destblock[i].val.d_lisp);
    }
  }
  if(savept)
    for(int i = savept; i; i--) mark(&savearray[i - 1]);
  /*
   * A new page is allocated if the number of conses is lower
   * than MINCONSES or if requested by calling doreclaim with
   * incr greater than 0.
   */
  if(nrconses < MINCONSES || incr > 0)
  {
    do
      conscells = newpage();
    while(incr-- > 0); /* At least one page more */
  }
  int nrfreed = sweep();
  nrconses = 0;
  if(ISNIL(gcgag))
    fprintf(primerr, "%d cells freed\n", nrfreed);
  return C_NIL;
}

/*
 * reclaim - Lips function reclaim interface. incr is the number of pages
 *           to inrease storage with.
 */
PRIMITIVE alloc::reclaim(LISPT incr) /* Number of blocks to increase with */
{
  long i;

  if(ISNIL(incr))
    i = 0;
  else
  {
    CHECK(incr, INTEGER);
    i = INTVAL(incr);
  }
  doreclaim(NOCONSARGS, i);
  return C_NIL;
}

LISPT alloc::getobject()
{
  if(ISNIL(freelist))
    doreclaim(NOCONSARGS, 0L);

  LISPT f = nullptr;
  SET(f, CONS, (LISPT)freelist);
  freelist = FREEVAL(freelist);
  return f;
}

/*
 * cons - Builds a cons cell out of arguments A and B. Reclaims space
 *        and allocates new blocks if necessary.
 */
PRIMITIVE alloc::cons(LISPT a, LISPT b)
{
  if(ISNIL(freelist))
  {
    foo1 = &a;
    foo2 = &b;
    doreclaim(CONSARGS, 0L);
  }

  LISPT f = nullptr;
  SET(f, CONS, (LISPT)freelist);
  freelist = FREEVAL(freelist);
  CAR(f) = a;
  CDR(f) = b;
  return f;
}

PRIMITIVE alloc::xobarray()
{
  LISPT o = C_NIL;
  for(int i = 0; i < MAXHASH; i++)
    for(auto* l = obarray[i]; l; l = l->onext) o = cons(l->sym, o);
  return o;
}

PRIMITIVE alloc::freecount()
{
  int i = 0;
  for(auto l = freelist; INTVAL(l); l = CDR(l)) i++;
  return mknumber((long)i);
}

/*
 * mkstring - Strings are stored in a cons cell with car set to NIL and
 *            cdr is set to the string pointer.
 */
LISPT alloc::mkstring(const char* str)
{
  auto* c = realmalloc((unsigned)strlen(str) + 1);
  if(c == nullptr)
    return C_ERROR;
  strcpy(c, str);
  LISPT s = getobject();
  STRINGVAL(s) = c;
  s->type = STRING;
  return s;
}

LISPT alloc::mknumber(long i)
{
  LISPT c = getobject();
  INTVAL(c) = i;
  c->type = INTEGER;
  return c;
}

/*
 * Calculates hash value of string.
 */
int alloc::hash(const char* str)
{
  int sum = 0;

  for(; *str; str++) sum += *str;
  return sum % MAXHASH;
}

/*
 * buildatom - Builds an atom with printname in S. Parameter CPY is non-zero
 *             if the printname should be saved.
 */
LISPT alloc::buildatom(const char* s, int cpy)
{
  static LISPT unbound = nullptr;

  if(unbound == nullptr)
    SET(unbound, UNBOUND, getobject());
  LISPT newatom = getobject();
  if(newatom == C_ERROR)
    return C_ERROR;
  if(cpy)
  {
    auto* pname = realmalloc((unsigned)strlen(s) + 1);
    if(pname == nullptr)
      return C_ERROR;
    strcpy(pname, s);
    SYMVAL(newatom).pname = pname;
  }
  else
    SYMVAL(newatom).pname = s;
  SYMVAL(newatom).plist = C_NIL;
  SYMVAL(newatom).value = unbound;
  LISPT l = nullptr;
  SET(l, SYMBOL, newatom);
  return l;
}

/*
 * puthash - Puts an atom with printname STR in the hash array OBARRAY.
 *           If the atom is already in obarray, no new atom is created.
 *           Copy str if CPY is non-zero. Returns the atom.
 */
LISPT alloc::puthash(const char* str, obarray_t* obarray[], int cpy)
{
  int hv = hash(str);
  obarray_t* ob;
  for(ob = *(obarray + hv); ob; ob = ob->onext)
  {
    if(!strcmp(SYMVAL(ob->sym).pname, str))
      return ob->sym;
  }
  ob = new obarray_t;
  if(ob == nullptr)
    return C_ERROR;
  ob->onext = obarray[hv];
  ob->sym = buildatom(str, cpy);
  if(EQ(ob->sym, C_ERROR))
  {
    delete ob;
    return C_ERROR;
  }
  obarray[hv] = ob;
  return ob->sym;
}

/*
 * intern - Make interned symbol in hasharray obarray. Str is not copied
 *          so this is only used with constant strings during init.
 */
LISPT alloc::intern(const char* str)
{
  return puthash(str, obarray, 0);
}

/*
 * mkatom - Generates interned symbol like intern but copy str.
 */
LISPT alloc::mkatom(char* str)
{
  return puthash(str, obarray, 1);
}

/* This isn't converted yet */
/*
 * mkfloat - Make a floating point number.
 */
LISPT alloc::mkfloat(double num)
{
  LISPT rval = nullptr;

#ifdef FLOATING
again:
  while(p0 < 4)
    if((floats.marks[p0] & (1 << point)) == 0)
      break;
    else
    {
      point--;
      if(!point)
      {
        p0++;
        point = 31;
      }
    }
  if(p0 == 4)
  {
    nrconses = MINCONSES;
    doreclaim(NOCONSARGS, 0L);
    goto again;
  }
  SET(rval, FLOAT, &floats.fdata[p0 * 32 + (31 - point)]);
  floats.marks[p0] |= 1 << point;
  *((double*)POINTER(rval)) = num;
#endif /* FLOATING */
  SET(rval, FLOAT, getobject());
  return rval;
}

/*
 * dalloc - Allocates a destination block of size size. Returns nullptr if
 *          no more space available.
 */
alloc::destblock_t* alloc::dalloc(int size)
{
  if(size <= DESTBLOCKSIZE - destblockused)
  {
    destblockused += size;
    for(int i = 0; i < size; i++)
    {
      destblock[destblockused - 1 - i].var.d_lisp = C_NIL;
      destblock[destblockused - 1 - i].val.d_lisp = C_NIL;
    }
  }
  else
    return nullptr;
  return &destblock[destblockused - size];
}

/*
 * dfree - Free a destination block. The size of a block i (hopefully)
 *         stored in the cdr of the first element. If it isn't, look
 *         elsewhere.
 */
void alloc::dfree(destblock_t* ptr)
{
  destblockused -= ptr->val.d_integer + 1;
}

/*
 * dzero - Frees all destination blocks.
 */
void alloc::dzero()
{
  destblockused = 0;
}

void alloc::init_alloc()
{
  destblockused = 0;
  conscells = nullptr;
  conscells = newpage(); /* Allocate one page of storage */
  if(conscells == nullptr)
  {
    fprintf(stderr, "Sorry, no memory for cons cells\n");
    finish(1);
  }
  sweep();
  initcvar(&gcgag, "gcgag", C_NIL);
  mkprim(PN_RECLAIM, reclaim, 1, SUBR);
  mkprim(PN_CONS, cons, 2, SUBR);
  mkprim(PN_FREECOUNT, freecount, 0, SUBR);
  mkprim(PN_OBARRAY, xobarray, 0, SUBR);
}

LISPT alloc::gcgag = nullptr;
LISPT alloc::savearray[];
int alloc::savept = 0;
alloc::obarray_t* alloc::obarray[];
LISPT alloc::freelist;

} // namespace lisp
