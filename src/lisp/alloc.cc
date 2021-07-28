//
// Lips, lisp shell
// Copyright 1988, 2020 Krister Joas
//

#include "alloc.hh"
#include "error.hh"
#include "eval.hh"
#include "io.hh"
#include "except.hh"
#include "low.hh"

namespace lisp
{
alloc::alloc(lisp& lisp): _lisp(lisp), local_symbols(lisp_t::symbol_collection().create())
{
  destblockused = 0;
  newpage(); // Allocate one page of storage

  // clang-format off
  mkprim(pn::RECLAIM,   ::lisp::reclaim,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::CONS,      ::lisp::cons,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::FREECOUNT, ::lisp::freecount, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::OBARRAY,   ::lisp::obarray,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

// TODO: Free all memory
alloc::~alloc() = default;

alloc::conscells_t* alloc::newpage()
{
  auto* newp = new conscells_t;
  for(auto& i: newp->cells)
  {
    i.settype(type::FREE);
    i.set();
    freelist.push_back(&i);;
  }
  conscells.push_front(newp);
  return newp;
}

LISPT alloc::reclaim(LISPT incr)
{
  int i = 0;
  if(!is_NIL(incr))
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
    obj->settype(type::FREE);
    obj->set();
    freelist.push_back(obj);
  });
  return r;
}

LISPT alloc::cons(LISPT a, LISPT b)
{
  auto f = getobject();
  f->set(cons_t{a, b});
  return f;
}

LISPT alloc::obarray()
{
  LISPT o = NIL;
  for(auto i: local_symbols)
    o = cons(i.self, o);
  return o;
}

LISPT alloc::freecount()
{
  return mknumber(freelist.size());
}

void alloc::mkprim(const std::string& pname, subr_t subr)
{
  auto s = LISPT(new lisp_t);
  s->set(subr);
  LISPT f = intern(pname);
  f->symvalue(s);
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

LISPT alloc::mkstring(const std::string& str)
{
  LISPT s = getobject();
  s->set(str);
  return s;
}

LISPT alloc::mknumber(int i)
{
  LISPT c = getobject();
  c->set(i);
  return c;
}

LISPT alloc::mkarglist(LISPT alist, std::int8_t& count)
{
  if(type_of(alist) == type::CONS)
  {
    ++count;
    return cons(alist->car(), mkarglist(alist->cdr(), count));
  }
  else if(EQ(alist, NIL))
    return NIL;
  else
  {
    count = -(count + 1);
    return cons(alist, NIL);
  }
}

LISPT alloc::mklambda(LISPT args, LISPT def, type type)
{
  lambda_t lambda;
  lambda.lambdarep = def;
  std::int8_t count = 0;
  lambda.arglist = mkarglist(args, count);
  lambda.argcnt = count;
  LISPT s = getobject();
  s->set(lambda, type == type::LAMBDA);
  return s;
}

/*
 * intern - Make interned symbol in hasharray obarray. Str is not copied so
 *          this is only used with global constant strings during init.
 */
LISPT alloc::intern(const std::string& str)
{
  auto& glob = lisp_t::symbol_collection().symbol_store(symbol::symbol_collection::global_id);
  auto& sym = glob.get(str);
  if(sym.self == NIL)
  {
    sym.self = LISPT(new lisp_t);
    sym.self->set(sym);
  }
  return sym.self;
}

/*
 * mkatom - Generates interned symbol like intern but create it in the local
 *          obarray if not found in the global.
 */
LISPT alloc::mkatom(const std::string& str)
{
  if(global_symbols().exists(str))
    return global_symbols().get(str).self;
  auto& sym = local_symbols.get(str);
  if(sym.self == NIL)
  {
    sym.self = getobject();
    sym.self->set(sym);
  }
  return sym.self;
}

/*
 * mkfloat - Make a floating point number.
 */
LISPT alloc::mkfloat(double num)
{
  LISPT rval = getobject();
  rval->set(num);
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
      destblock[destblockused - i].reset();
    return dest;
  }
  return nullptr;
}

/*
 * dfree - Free a destination block.
 */
void alloc::dfree(destblock_t* ptr) { destblockused -= ptr->size() + 1; }

/*
 * dzero - Frees all destination blocks.
 */
void alloc::dzero() { destblockused = 0; }

} // namespace lisp
