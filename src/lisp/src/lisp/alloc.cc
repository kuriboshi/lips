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
#include "low.hh"

namespace lisp
{
alloc::alloc(lisp& lisp): _lisp(lisp), symbols(lisp_t::symbol_collection().create())
{
  for(int i = 0; i != MAXHASH; ++i)
    obarray[i] = nullptr;

  destblockused = 0;
  newpage(); // Allocate one page of storage
  if(conscells.empty())
  {
    lisp.primerr().format("Cons cells memory exhausted\n");
    throw lisp_finish("Cons cells memory exhausted", 1);
  }
  initcvar(gcgag, "gcgag", NIL);

  // clang-format off
  mkprim(pn::RECLAIM,   ::lisp::reclaim,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::CONS,      ::lisp::cons,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FREECOUNT, ::lisp::freecount, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::OBARRAY,   ::lisp::xobarray,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
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
  for(auto& i: newp->cells)
  {
    i.settype(type::FREE);
    i.setnil();
    freelist.push_back(&i);;
  }
  conscells.push_front(newp);
  return newp;
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
    _lisp.check(incr, type::INTEGER);
    i = incr->intval();
  }
  for(; i > 0; --i)
    newpage();
  return NIL;
}

LISPT alloc::getobject()
{
  if(freelist.empty())
    newpage();

  auto f = freelist.front();
  freelist.pop_front();
  auto r = LISPT(f, [this](lisp_t* obj) {
    obj->setnil();
    freelist.push_back(obj);
  });
  return r;
}

/*
 * cons - Builds a cons cell out of arguments A and B. Reclaims space
 *        and allocates new blocks if necessary.
 */
PRIMITIVE alloc::cons(LISPT a, LISPT b)
{
  LISPT f = getobject();
  f->consval(cons_t());
  f->car(a);
  f->cdr(b);
  return f;
}

PRIMITIVE alloc::xobarray()
{
  LISPT o = NIL;
  for(int i = 0; i < MAXHASH; i++)
    for(auto* l = obarray[i]; l; l = l->onext) o = cons(l->sym, o);
  return o;
}

PRIMITIVE alloc::freecount()
{
  return mknumber(freelist.size());
}

/*
 * mkprim - Define the primitive with print name PNAME, to be the
 *          C function FNAME with NRPAR number of parameters. TYPE
 *          is the type of function: SUBR or FSUBR. If npar is negative
 *          it means the function is halfspread.
 */
LISPT alloc::mkprim(const std::string& pname, subr_t subr)
{
  auto s = LISPT(new lisp_t);
  s->subrval(subr);
  LISPT f = intern(pname);
  set(f->symbol().value, type::SUBR, s);
  return s;
}

void alloc::mkprim(const std::string& pname, subr_t::func0_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, subr_t(subr, spread, fun));
}

void alloc::mkprim(const std::string& pname, subr_t::func1_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, subr_t(subr, spread, fun));
}

void alloc::mkprim(const std::string& pname, subr_t::func2_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, subr_t(subr, spread, fun));
}

void alloc::mkprim(const std::string& pname, subr_t::func3_t fun, enum subr_t::subr subr, enum subr_t::spread spread)
{
  mkprim(pname, subr_t(subr, spread, fun));
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
  if(type_of(alist) == type::CONS)
  {
    count++;
    return cons(alist->car(), mkarglis(alist->cdr(), count));
  }
  else if(EQ(alist, NIL))
    return NIL;
  else
  {
    count = -(count + 1);
    return cons(alist, NIL);
  }
}

/*
 * mklambda - Make a lambda object with the argument ARGS and definition DEF
 *            and the type TYPE, wich is LAMBDA or NLAMBDA.
 */
LISPT alloc::mklambda(LISPT args, LISPT def, type type)
{
  LISPT s = getobject();
  s->lamval(lambda_t());
  s->lamval().lambdarep = def;
  int count = 0;
  s->lamval().arglist = mkarglis(args, count);
  s->lamval().argcnt = count;
  LISPT t = s;
  t->settype(type);
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
  if(!unbound)
  {
    unbound.reset(new lisp_t);
    unbound->settype(type::UNBOUND);
  }

  newatom->symbol(symbol_t());
  newatom->symbol().pname = s;
  newatom->symbol().plist = NIL;
  newatom->symvalue(unbound);
  return newatom;
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
  auto& glob = lisp_t::symbol_collection().symbol_store(symbol::symbol_collection::global_id);
  auto& sym = glob.get(str);

  auto hv = hash(str);
  if(auto* ob = findatom(hv, str, globals))
    return ob->sym;
  // Use new lisp_t here because we're never going to free interned symbols.
  return puthash(hv, str, globals, LISPT(new lisp_t));
}

/*
 * mkatom - Generates interned symbol like intern but create it in the local
 *          obarray if not found in the global.
 */
LISPT alloc::mkatom(const std::string& str)
{
  auto& glob = lisp_t::symbol_collection().symbol_store(symbol::symbol_collection::global_id);
  if(glob.exists(str))
    auto& sym = glob.get(str);
  else
    auto& sym = symbols.get(str);

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
  if(size <= DESTBLOCKSIZE - destblockused - 1)
  {
    auto* dest = &destblock[destblockused];
    destblockused += size + 1;
    dest->num(size);
    for(int i = 1; i <= size; ++i)
      destblock[destblockused - i].u = cons(NIL, NIL);
    return dest;
  }
  return nullptr;
}

/*
 * dfree - Free a destination block. The size of a block i (hopefully)
 *         stored in the cdr of the first element. If it isn't, look
 *         elsewhere.
 */
void alloc::dfree(destblock_t* ptr) { destblockused -= ptr->size() + 1; }

alloc::obarray_t alloc::globals;

/*
 * dzero - Frees all destination blocks.
 */
void alloc::dzero() { destblockused = 0; }

} // namespace lisp
