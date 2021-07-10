/*
 * Lips, lisp shell
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <doctest/doctest.h>
#include "alloc.hh"
#include "error.hh"
#include "eval.hh"
#include "io.hh"
#include "except.hh"
#include "low.hh"

#include <iostream>

namespace lisp
{
alloc::alloc(lisp& lisp): _lisp(lisp)
{
  for(int i = 0; i != MAXHASH; ++i)
    obarray[i] = nullptr;

  gcprotect(lisp.topprompt);
  gcprotect(lisp.brkprompt);
  gcprotect(lisp.currentbase);
  gcprotect(lisp.version);
  gcprotect(lisp.verbose);
  gcprotect(gcgag);
  gcprotect(C_EOF);

  destblockused = 0;
  newpage(); // Allocate one page of storage
  if(conscells.empty())
  {
    lisp.primerr().format("Cons cells memory exhausted\n");
    throw lisp_finish("Cons cells memory exhausted", 1);
  }
  sweep();
  initcvar(&gcgag, "gcgag", C_NIL);
  // clang-format off
  mkprim(PN_RECLAIM,   ::lisp::reclaim,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_CONS,      ::lisp::cons,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_FREECOUNT, ::lisp::freecount, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_OBARRAY,   ::lisp::xobarray,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

alloc::~alloc()
{
  // TODO: Free all memory
}

/* 
 * newpage - Allocates a new block of cons cells and links it into the 
 *           current list of blocks.
 */
alloc::conscells_t* alloc::newpage()
{
  auto* newp = new conscells_t;
  if(newp == nullptr)
    return conscells.front();
  conscells.push_front(newp);
  return newp;
}

/* 
 * sweep - Sweep up unused cons cells. Free objects that are pointers to 
 *         space allocated by malloc. These objects has the type field set 
 *         to NIL, and the rest of the field is the pointer.
 */
int alloc::sweep()
{
  int nrfreed = 0;
  for(auto cc: conscells)
  {
    for(auto& i: cc->cells)
    {
      if(!i.marked())
      {
        ++nrfreed;
        if(type_of(i) == lisp_type::CPOINTER)
          free(i.cpointval());
        i.unmark();
        i.freeval(freelist);
        freelist = &i;
      }
    }
  }
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
    _lisp.primerr().format("garbage collecting\n");
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
    if(e().control[i].type == evaluator::control::LISP && e().control[i].u.lisp != nullptr
      && type_of(e().control[i].u.lisp) != lisp_type::ENVIRON)
      mark(e().control[i].u.lisp);
  for(int i = 0; i < MAXHASH; i++)
    for(auto* l = obarray[i]; l; l = l->onext)
    {
      l->sym->mark();
      mark((l->sym->symvalue()));
      mark((l->sym->symbol().plist));
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
      newpage();
    while(incr-- > 0); /* At least one page more */
  }
  if(is_NIL(gcgag))
    _lisp.primerr().format("{} cells freed\n", nrfreed);
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
    _lisp.check(incr, lisp_type::INTEGER);
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
  set(f->symbol().value, lisp_type::SUBR, s);
  return s;
}

void alloc::mkprim(const std::string& pname, func0_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, new subr_t(subr, spread, fun));
}

void alloc::mkprim(const std::string& pname, func1_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, new subr_t(subr, spread, fun));
}

void alloc::mkprim(const std::string& pname, func2_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, new subr_t(subr, spread, fun));
}

void alloc::mkprim(const std::string& pname, func3_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, new subr_t(subr, spread, fun));
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
  t->settype(type);
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
LISPT alloc::buildatom(const std::string& s, LISPT newatom)
{
  static LISPT unbound = nullptr;
  if(unbound == nullptr)
    set(unbound, lisp_type::UNBOUND, new lisp_t);

  newatom->symbol(symbol_t());
  newatom->symbol().pname = s;
  newatom->symbol().plist = C_NIL;
  newatom->symvalue(unbound);
  LISPT l = nullptr;
  set(l, lisp_type::SYMBOL, newatom);
  return l;
}

alloc::bucket_t* alloc::findatom(int hv, const std::string& str, const obarray_t& obarray)
{
  for(auto* ob = obarray[hv]; ob; ob = ob->onext)
  {
    if(ob->sym->symbol().pname == str)
      return ob;
  }
  return nullptr;
}

/*
 * puthash - Puts an atom with printname STR in the hash array OBARRAY.  If the
 *           atom is already in obarray, no new atom is created.  Returns the
 *           atom.
 */
LISPT alloc::puthash(int hv, const std::string& str, obarray_t& obarray, LISPT newatom)
{
  auto* ob = new bucket_t;
  if(ob == nullptr)
    return C_ERROR;
  ob->onext = obarray[hv];
  ob->sym = buildatom(str, newatom);
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
  // Use new lisp_t here because we're never going to free interned symbols.
  return puthash(hv, str, globals, new lisp_t);
}

/*
 * mkatom - Generates interned symbol like intern but create it in the local
 *          obarray if not found in the global.
 */
LISPT alloc::mkatom(const std::string& str)
{
  // First we search for global interned atoms
  auto hv = hash(str);
  if(auto* ob = findatom(hv, str, globals))
    return ob->sym;
  if(auto* ob = findatom(hv, str, obarray))
    return ob->sym;
  return puthash(hv, str, obarray, getobject());
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

alloc::obarray_t alloc::globals;

/*
 * dzero - Frees all destination blocks.
 */
void alloc::dzero() { destblockused = 0; }

TEST_CASE("Create lisp objects")
{
  lisp lisp;

  SUBCASE("Multiple calls to intern should return the same object for the same string")
  {
    auto hello0 = intern("hello");
    auto hello1 = intern("hello");
    CHECK(hello0 == hello1);
  }

  SUBCASE("Check constants are the same as interned strings")
  {
    auto lambda = intern("lambda");
    CHECK(lambda == C_LAMBDA);
  }

  SUBCASE("Check constants are the same as a local atom")
  {
    auto lambda = mkatom(lisp, "lambda");
    CHECK(lambda == C_LAMBDA);
  }

  SUBCASE("Set variable")
  {
    auto i = mkatom(lisp, "i");
    auto j = mkatom(lisp, "j");
    auto a = mkstring(lisp, "a");
    auto b = mkstring(lisp, "b");

    set(lisp, i, a);
    set(lisp, j, b);
    CHECK(i != j);
    set(lisp, j, a);
    CHECK(i != j);

    file_t out0(std::make_unique<string_sink>());
    prin0(lisp, i, out0);
    CHECK(to_string(out0.sink()) == std::string(i->getstr()));

    file_t out1(std::make_unique<string_sink>());
    prin0(lisp, j, out1);
    CHECK(to_string(out1.sink()) == std::string(j->getstr()));

    std::string s_hello{"(hello)"};
    file_t in(s_hello);
    auto hello = lispread(lisp, in, false);
    file_t out2(std::make_unique<string_sink>());
    prin0(lisp, hello, out2);
    CHECK(to_string(out2.sink()) == s_hello);
  }
}

} // namespace lisp
