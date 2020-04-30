/*
 * Lips, lisp shell
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <cstdio>
#include "alloc.hh"
#include "error.hh"
#include "eval.hh"

// TODO:
extern FILE* primerr;
extern FILE* primout;
extern FILE* primin;

#if 0
// TODO:
extern void finish(int);
#endif

namespace lisp
{
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
  set(freelist, FREE, &cc->cells[i]);
  nrfreed++;
  LISPT f = freelist;
  if(type_of(f) == CPOINTER)
    free(f->cpointval());
  i++; /* Check *next* cell */
  for(; cc; cc = cc->next, i = 0)
    for(; i < CONSCELLS; i++)
      if(!cc->cells[i].gcmark)
      {
        nrfreed++;
        /*
	   * C pointers must be freed.
	   */
        if(type_of(f) == CPOINTER)
          free(f->cpointval());
        set(f->freeval(), FREE, &cc->cells[i]);
        f = f->freeval();
      }
      else
      {
        cc->cells[i].gcmark = 0;
      }
  f->freeval() = C_NIL;
  return nrfreed;
}

/*
 * mark - Mark a cell and traverse car and cdr of cons cells and all other
 *        fields of type LISPT.
 */
void alloc::mark(LISPT x)
{
  switch(type_of(x))
  {
    case CONS:
      if(x->marked())
        break;
      x->mark();
      mark(x->car());
      mark(x->cdr());
      break;
#ifdef FLOATING
    case FLOAT: {
      int y = &FLOATVAL(x) - &floats.fdata[0];
      floats.marks[(y / 32)] |= 1 << (31 - y % 32);
      break;
    }
#endif /* FLOATING */
    case SYMBOL:
      break;
    case LAMBDA:
    case NLAMBDA:
      x->mark();
      mark(x->lamval().lambdarep);
      mark(x->lamval().arglist);
      break;
    case CLOSURE:
      x->mark();
      mark(x->closval().cfunction);
      mark(x->closval().closed);
      mark(x->closval().cvalues);
      break;
    case STRING:
      x->mark();
      break;
    case INDIRECT:
      x->mark();
      mark(x->indirectval());
      break;
    case NIL:
      break;
    default:
      x->mark();
      break;
  }
}

/*
 * doreclaim - Garbage collector, mark all used cells then
 *	       sweep up garbage.  Argument doconsargs is nonzero
 *	       
 */
LISPT alloc::doreclaim(int doconsargs, int incr)
{
  if(is_NIL(gcgag))
    fprintf(primerr, "garbage collecting\n");
#ifdef FLOATING
  for(int i = 0; i < 4; i++) floats.marks[i] = 0;
  point = 31;
  p0 = 0;
#endif /* FLOATING */
  if(C_T != nullptr)
    C_T->mark();
  if(doconsargs)
  {
    mark(foo1);
    mark(foo2);
  }
  if(e().dest != nullptr)
    for(int i = e().dest[0].val.d_integer; i > 0; i--)
    {
      mark(e().dest[i].var.d_lisp);
      mark(e().dest[i].val.d_lisp);
    }
  for(auto i: markobjs) mark(*i);
#if 0
  // TODO:
  if (env != nullptr && ENVVAL(env) != nullptr)
    mark((LISPT *) &ENVVAL(env));
#endif
  for(int i = 0; i < e().toctrl; i++)
    if(e().control[i].type == e().CTRL_LISP && e().control[i].u.lisp != nullptr
      && type_of(e().control[i].u.lisp) != ENVIRON)
      mark(e().control[i].u.lisp);
  for(int i = 0; i < MAXHASH; i++)
    for(auto* l = obarray[i]; l; l = l->onext)
    {
      l->sym->mark();
      mark((l->sym->symval().value));
      mark((l->sym->symval().plist));
    }
  for(int i = destblockused - 1; i >= 0; i--)
  {
    switch(destblock[i].type)
    {
      case block_type::EMPTY:
        break;
      case block_type::LISPT:
        mark(destblock[i].var.d_lisp);
        mark(destblock[i].val.d_lisp);
        break;
      case block_type::ENVIRON:
        break;
    }
  }
  if(savept)
    for(int i = savept; i; i--) mark(savearray[i - 1]);
  int nrfreed = sweep();
  /*
   * A new page is allocated if the number of conses is lower
   * than MINCONSES or if requested by calling doreclaim with
   * incr greater than 0.
   */
  if(nrfreed < MINCONSES || incr > 0)
  {
    do
      conscells = newpage();
    while(incr-- > 0); /* At least one page more */
  }
  if(is_NIL(gcgag))
    fprintf(primerr, "%d cells freed\n", nrfreed);
  return C_NIL;
}

/*
 * reclaim - Lips function reclaim interface. incr is the number of pages
 *           to inrease storage with.
 */
PRIMITIVE alloc::reclaim(lisp&, LISPT incr) /* Number of blocks to increase with */
{
  int i;

  if(is_NIL(incr))
    i = 0;
  else
  {
    check(incr, INTEGER);
    i = incr->intval();
  }
  doreclaim(NOCONSARGS, i);
  return C_NIL;
}

LISPT alloc::getobject()
{
  if(is_NIL(freelist))
    doreclaim(NOCONSARGS, 0L);

  LISPT f = nullptr;
  set(f, CONS, freelist);
  freelist = freelist->freeval();
  return f;
}

/*
 * cons - Builds a cons cell out of arguments A and B. Reclaims space
 *        and allocates new blocks if necessary.
 */
PRIMITIVE alloc::cons(lisp&, LISPT a, LISPT b)
{
  if(is_NIL(freelist))
  {
    foo1 = a;
    foo2 = b;
    doreclaim(CONSARGS, 0L);
  }

  LISPT f = nullptr;
  set(f, CONS, freelist);
  freelist = freelist->freeval();
  f->car(a);
  f->cdr(b);
  return f;
}

PRIMITIVE alloc::xobarray(lisp& ctx)
{
  LISPT o = C_NIL;
  for(int i = 0; i < MAXHASH; i++)
    for(auto* l = obarray[i]; l; l = l->onext) o = cons(ctx, l->sym, o);
  return o;
}

PRIMITIVE alloc::freecount(lisp&)
{
  int i = 0;
  for(auto l = freelist; l->intval(); l = l->cdr()) i++;
  return mknumber(i);
}

/*
 * mkprim - Define the primitive with print name PNAME, to be the
 *          C function FNAME with NRPAR number of parameters. TYPE
 *          is the type of function: SUBR or FSUBR. If npar is negative
 *          it means the function is halfspread.
 */
LISPT alloc::mkprim(const char* pname, short nrpar, lisp_type type)
{
  LISPT s = getobject();
  LISPT f = intern(pname);
  s->subrval().argcount = nrpar;
  set(f->symval().value, type, s);
  return s;
}

void alloc::mkprim(const char* pname, LISPT (*fname)(lisp&), short nrpar, lisp_type type)
{
  mkprim(pname, nrpar, type)->subrval().function0 = fname;
}

void alloc::mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT), short nrpar, lisp_type type)
{
  mkprim(pname, nrpar, type)->subrval().function1 = fname;
}

void alloc::mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT), short nrpar, lisp_type type)
{
  mkprim(pname, nrpar, type)->subrval().function2 = fname;
}

void alloc::mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT, LISPT), short nrpar, lisp_type type)
{
  mkprim(pname, nrpar, type)->subrval().function3 = fname;
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
  s->stringval(c);
  return s;
}

LISPT alloc::mknumber(int i)
{
  LISPT c = getobject();
  c->intval(i);
  return c;
}

/*
 * mkarglist - builds a list out of the argument list ALIST given in a lambda
 *             definition. This list may end in an atom if the function is
 *             halfspread, or it could be an atom for a nospread
 *             function. COUNT is set to the number of arguments and is
 *             negative if halfspread or nospread.
 */
LISPT alloc::mkarglis(LISPT alist, int& count)
{
  if(type_of(alist) == CONS)
  {
    count++;
    return cons(_lisp, alist->car(), mkarglis(alist->cdr(), count));
  }
  else if(EQ(alist, C_NIL))
    return C_NIL;
  else
  {
    count = -(count + 1);
    return cons(_lisp, alist, C_NIL);
  }
}

/*
 * mklambda - Make a lambda object with the argument ARGS and definition DEF
 *            and the type TYPE, wich is LAMBDA or NLAMBDA.
 */
LISPT alloc::mklambda(LISPT args, LISPT def, lisp_type type)
{
  save(args);
  save(def);
  LISPT s = getobject();
  s->lamval().lambdarep = def;
  int count = 0;
  s->lamval().arglist = mkarglis(args, count);
  s->lamval().argcnt = count;
  LISPT t = nullptr;
  set(t, type, s);
  def = unsave();
  args = unsave();
  return t;
}

/*
 * Calculates hash value of string.
 */
int alloc::hash(const char* str) const
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
    set(unbound, UNBOUND, getobject());
  LISPT newatom = getobject();
  if(newatom == C_ERROR)
    return C_ERROR;
  if(cpy)
  {
    auto* pname = realmalloc((unsigned)strlen(s) + 1);
    if(pname == nullptr)
      return C_ERROR;
    strcpy(pname, s);
    newatom->symval().pname = pname;
  }
  else
    newatom->symval().pname = s;
  newatom->symval().plist = C_NIL;
  newatom->symval().value = unbound;
  LISPT l = nullptr;
  set(l, SYMBOL, newatom);
  return l;
}

alloc::obarray_t* alloc::findatom(const char* str, obarray_t* obarray[]) const
{
  auto hv = hash(str);
  for(auto* ob = *(obarray + hv); ob; ob = ob->onext)
  {
    if(!strcmp(ob->sym->symval().pname, str))
      return ob;
  }
  return nullptr;
}

/*
 * puthash - Puts an atom with printname STR in the hash array OBARRAY.
 *           If the atom is already in obarray, no new atom is created.
 *           Copy str if COPY is true. Returns the atom.
 */
LISPT alloc::puthash(const char* str, obarray_t* obarray[], bool copy)
{
  int hv = hash(str);
  obarray_t* ob = findatom(str, obarray);
  if(ob != nullptr)
    return ob->sym;
  ob = new obarray_t;
  if(ob == nullptr)
    return C_ERROR;
  ob->onext = obarray[hv];
  ob->sym = buildatom(str, copy);
  if(EQ(ob->sym, C_ERROR))
  {
    delete ob;
    return C_ERROR;
  }
  obarray[hv] = ob;
  return ob->sym;
}

/*
 * intern - Make interned symbol in hasharray obarray. Str is not copied so
 *          this is only used with global constant strings during init.
 */
LISPT alloc::intern(const char* str) { return puthash(str, globals, 0); }

/*
 * mkatom - Generates interned symbol like intern but copy str.
 */
LISPT alloc::mkatom(const char* str)
{
  // First we search for global interned atoms
  if(auto* ob = findatom(str, globals))
    return ob->sym;
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
    doreclaim(NOCONSARGS, 0L);
    goto again;
  }
  SET(rval, FLOAT, &floats.fdata[p0 * 32 + (31 - point)]);
  floats.marks[p0] |= 1 << point;
  *((double*)POINTER(rval)) = num;
#endif /* FLOATING */
  set(rval, FLOAT, getobject());
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
void alloc::dfree(destblock_t* ptr) { destblockused -= ptr->val.d_integer + 1; }

/*
 * dzero - Frees all destination blocks.
 */
void alloc::dzero() { destblockused = 0; }

alloc::alloc(lisp& lisp): _lisp(lisp)
{
#if 0
  // TODO: Fix me
  for(auto i: {&top, &rstack, &verboseflg, &topprompt, &promptform, &brkprompt, &currentbase, &interactive, &version,
        &gcgag, &C_EOF})
    add_mark_object(i);
#endif
  destblockused = 0;
  conscells = nullptr;
  conscells = newpage(); /* Allocate one page of storage */
  if(conscells == nullptr)
  {
    fprintf(stderr, "Sorry, no memory for cons cells\n");
#if 0
    // TODO:
    finish(1);
#endif
  }
  sweep();
  initcvar(&gcgag, "gcgag", C_NIL);
  mkprim(PN_RECLAIM, ::lisp::reclaim, 1, SUBR);
  mkprim(PN_CONS, ::lisp::cons, 2, SUBR);
  mkprim(PN_FREECOUNT, ::lisp::freecount, 0, SUBR);
  mkprim(PN_OBARRAY, ::lisp::xobarray, 0, SUBR);
}

alloc::~alloc()
{
  // TODO: Free all memory
}

alloc::obarray_t* alloc::globals[];

} // namespace lisp
