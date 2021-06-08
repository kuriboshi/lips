/*
 * Lips, lisp shell
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "alloc.hh"
#include "error.hh"
#include "eval.hh"
#include "io.hh"
#include "except.hh"

namespace lisp
{
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
  set(freelist, lisp_type::FREE, &cc->cells[i]);
  nrfreed++;
  LISPT f = freelist;
  if(type_of(f) == lisp_type::CPOINTER)
    free(f->cpointval());
  i++; /* Check *next* cell */
  for(; cc; cc = cc->next, i = 0)
  {
    for(; i < CONSCELLS; i++)
    {
      if(!cc->cells[i].gcmark)
      {
        nrfreed++;
        /*
	   * C pointers must be freed.
	   */
        if(type_of(f) == lisp_type::CPOINTER)
          free(f->cpointval());
        f->freeval(&cc->cells[i]);
        f->unmark();
        f = f->freeval();
      }
      else
      {
        cc->cells[i].gcmark = 0;
      }
    }
  }
  f->freeval(C_NIL);
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
    case lisp_type::CONS:
      if(x->marked())
        break;
      x->mark();
      mark(x->car());
      mark(x->cdr());
      break;
    case lisp_type::SYMBOL:
      break;
    case lisp_type::LAMBDA:
    case lisp_type::NLAMBDA:
      x->mark();
      mark(x->lamval().lambdarep);
      mark(x->lamval().arglist);
      break;
    case lisp_type::CLOSURE:
      x->mark();
      mark(x->closval().cfunction);
      mark(x->closval().closed);
      mark(x->closval().cvalues);
      break;
    case lisp_type::STRING:
      x->mark();
      break;
    case lisp_type::INDIRECT:
      x->mark();
      mark(x->indirectval());
      break;
    case lisp_type::NIL:
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
LISPT alloc::doreclaim(int incr)
{
  if(is_NIL(gcgag))
    l.primerr().format("garbage collecting\n");
  if(C_T != nullptr)
    C_T->mark();
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
      && type_of(e().control[i].u.lisp) != lisp_type::ENVIRON)
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
  for(auto i: savearray) mark(i);
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
    l.primerr().format("{} cells freed\n", nrfreed);
  return C_NIL;
}

/*
 * reclaim - Lips function reclaim interface. incr is the number of pages
 *           to inrease storage with.
 */
PRIMITIVE alloc::reclaim(LISPT incr) /* Number of blocks to increase with */
{
  int i;

  if(is_NIL(incr))
    i = 0;
  else
  {
    l.check(incr, lisp_type::INTEGER);
    i = incr->intval();
  }
  doreclaim(i);
  return C_NIL;
}

LISPT alloc::getobject()
{
  if(is_NIL(freelist))
    doreclaim();

  LISPT f = nullptr;
  set(f, lisp_type::CONS, freelist);
  freelist = freelist->freeval();
  return f;
}

/*
 * cons - Builds a cons cell out of arguments A and B. Reclaims space
 *        and allocates new blocks if necessary.
 */
PRIMITIVE alloc::cons(LISPT a, LISPT b)
{
  if(is_NIL(freelist))
  {
    mark(a);
    mark(b);
    doreclaim();
  }

  LISPT f = freelist;
  freelist = freelist->freeval();
  f->consval(cons_t());
  f->car(a);
  f->cdr(b);
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
  for(auto l = freelist; l->intval(); l = l->cdr()) i++;
  return mknumber(i);
}

/*
 * mkprim - Define the primitive with print name PNAME, to be the
 *          C function FNAME with NRPAR number of parameters. TYPE
 *          is the type of function: SUBR or FSUBR. If npar is negative
 *          it means the function is halfspread.
 */
LISPT alloc::mkprim(const std::string& pname, subr_t* subr)
{
  LISPT s = new lisp_t;
  s->subrval(subr);
  LISPT f = intern(pname);
  set(f->symval().value, lisp_type::SUBR, s);
  return s;
}

void alloc::mkprim(const std::string& pname, func0_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  mkprim(pname, new subr_t(subr, spread))->subrval().f = fname;
}

void alloc::mkprim(const std::string& pname, func1_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  mkprim(pname, new subr_t(subr, spread))->subrval().f = fname;
}

void alloc::mkprim(const std::string& pname, func2_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  mkprim(pname, new subr_t(subr, spread))->subrval().f = fname;
}

void alloc::mkprim(const std::string& pname, func3_t fname, subr_t::subr_type subr, subr_t::spread_type spread)
{
  mkprim(pname, new subr_t(subr, spread))->subrval().f = fname;
}

/*
 * mkstring - Strings are stored in a cons cell with car set to NIL and
 *            cdr is set to the string pointer.
 */
LISPT alloc::mkstring(const std::string& str)
{
  LISPT s = getobject();
  s->stringval(str);
  return s;
}

LISPT alloc::mknumber(int i)
{
  LISPT c = getobject();
  c->intval(i);
  return c;
}

/*
 * mkarglis - builds a list out of the argument list ALIST given in a lambda
 *            definition. This list may end in an atom if the function is
 *            halfspread, or it could be an atom for a nospread function. COUNT
 *            is set to the number of arguments and is negative if halfspread
 *            or nospread.
 */
LISPT alloc::mkarglis(LISPT alist, int& count)
{
  if(type_of(alist) == lisp_type::CONS)
  {
    count++;
    return cons(alist->car(), mkarglis(alist->cdr(), count));
  }
  else if(EQ(alist, C_NIL))
    return C_NIL;
  else
  {
    count = -(count + 1);
    return cons(alist, C_NIL);
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
  s->lamval(lambda_t());
  s->lamval().lambdarep = def;
  int count = 0;
  s->lamval().arglist = mkarglis(args, count);
  s->lamval().argcnt = count;
  LISPT t = s;
  t->type = type;
  def = unsave();
  args = unsave();
  return t;
}

/*
 * Calculates hash value of string.
 */
int alloc::hash(const std::string& str)
{
  int sum = 0;

  for(auto c: str)
    sum += c;
  return sum % MAXHASH;
}

/*
 * buildatom - Builds an atom with printname in S. Parameter CPY is non-zero
 *             if the printname should be saved.
 */
LISPT alloc::buildatom(const std::string& s, bool copy, LISPT newatom)
{
  static LISPT unbound = nullptr;
  if(unbound == nullptr)
    set(unbound, lisp_type::UNBOUND, new lisp_t);

  newatom->symval(symbol_t());
  newatom->symval().pname = s;
  newatom->symval().plist = C_NIL;
  newatom->symval().value = unbound;
  LISPT l = nullptr;
  set(l, lisp_type::SYMBOL, newatom);
  return l;
}

alloc::obarray_t* alloc::findatom(int hv, const std::string& str, obarray_t* obarray[])
{
  for(auto* ob = *(obarray + hv); ob; ob = ob->onext)
  {
    if(ob->sym->symval().pname == str)
      return ob;
  }
  return nullptr;
}

/*
 * puthash - Puts an atom with printname STR in the hash array OBARRAY.
 *           If the atom is already in obarray, no new atom is created.
 *           Copy str if COPY is true. Returns the atom.
 */
LISPT alloc::puthash(int hv, const std::string& str, obarray_t* obarray[], bool copy, LISPT newatom)
{
  auto* ob = new obarray_t;
  if(ob == nullptr)
    return C_ERROR;
  ob->onext = obarray[hv];
  ob->sym = buildatom(str, copy, newatom);
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
LISPT alloc::intern(const std::string& str)
{
  auto hv = hash(str);
  if(auto* ob = findatom(hv, str, globals))
    return ob->sym;
  return puthash(hv, str, globals, 0, new lisp_t);
}

/*
 * mkatom - Generates interned symbol like intern but copy str.
 */
LISPT alloc::mkatom(const std::string& str)
{
  // First we search for global interned atoms
  auto hv = hash(str);
  if(auto* ob = findatom(hv, str, globals))
    return ob->sym;
  if(auto* ob = findatom(hv, str, obarray))
    return ob->sym;
  return puthash(hv, str, obarray, 1, getobject());
}

/* This isn't converted yet */
/*
 * mkfloat - Make a floating point number.
 */
LISPT alloc::mkfloat(double num)
{
  LISPT rval = getobject();
  rval->floatval(num);
  return rval;
}

/*
 * dalloc - Allocates a destination block of size size. Returns nullptr if
 *          no more space available.
 */
destblock_t* alloc::dalloc(int size)
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

alloc::alloc(lisp& lisp): base(lisp)
{
  for(int i = 0; i != MAXHASH; ++i)
    obarray[i] = nullptr;

  add_mark_object(&l.topprompt);
  add_mark_object(&l.brkprompt);
  add_mark_object(&l.currentbase);
  add_mark_object(&l.version);
  add_mark_object(&l.verbose);
  add_mark_object(&gcgag);
  add_mark_object(&C_EOF);

  destblockused = 0;
  conscells = nullptr;
  conscells = newpage(); /* Allocate one page of storage */
  if(conscells == nullptr)
  {
    l.primerr().format("Cons cells memory exhausted\n");
    throw lisp_finish("Cons cells memory exhausted", 1);
  }
  sweep();
  initcvar(&gcgag, "gcgag", C_NIL);
  // clang-format off
  mkprim(PN_RECLAIM,   ::lisp::reclaim,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_CONS,      ::lisp::cons,      subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_FREECOUNT, ::lisp::freecount, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_OBARRAY,   ::lisp::xobarray,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

alloc::alloc(): alloc(lisp::current()) {}

alloc::~alloc()
{
  // TODO: Free all memory
}

alloc::obarray_t* alloc::globals[];

} // namespace lisp
