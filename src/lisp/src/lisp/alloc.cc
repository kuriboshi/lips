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
  if(conscells.empty())
  {
    lisp.primerr().format("Cons cells memory exhausted\n");
    throw lisp_finish("Cons cells memory exhausted", 1);
  }

  // clang-format off
  mkprim(pn::RECLAIM,   ::lisp::reclaim,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::CONS,      ::lisp::cons,      subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FREECOUNT, ::lisp::freecount, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::OBARRAY,   ::lisp::obarray,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
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

/// @brief Allocate a number of blocks of cons cells.
///
/// @detail The name is historical from when there was garbage collection.
///
/// @param incr [in] Number of blocks of cons cells to allocate.
PRIMITIVE alloc::reclaim(LISPT incr)
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

/// @brief Return a cons cell from storage.
///
/// @detail If the free cell list is empty a new block of cons cells is
/// allocated.  The LISPT shared_ptr created by this function will have its
/// delete function overridden with a function which puts the cell back on the
/// free cell list.
///
/// @return A new empty cons cell.
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

/// @brief Builds a cons cell out of the arguments.
///
/// @detail The most basic of lisp functions.  Allocate a cons cell and fill in
/// the cell's car and cdr parts.
///
/// @param a [in] The value to put in the head (car) of the cons cell.
/// @param b [in] The value to put in the tail (cdr) of the cons cell.
///
/// @return The cons cell.
PRIMITIVE alloc::cons(LISPT a, LISPT b)
{
  auto f = getobject();
  f->consval(cons_t{a, b});
  return f;
}

/// @brief Build a list of symbols in the local symbol table.
///
/// @return Returns a list of local symbols in no particular order.
PRIMITIVE alloc::obarray()
{
  LISPT o = NIL;
  for(auto i: local_symbols)
    o = cons(i.self, o);
  return o;
}

/// @brief Number of free cell in the free cell list.
///
/// @return The number of free cells.
PRIMITIVE alloc::freecount()
{
  return mknumber(freelist.size());
}

/// @brief Register a primitive function.
///
/// @param pname [in] The print name of the internal function.  This is put in
/// the global symbol table using intern.
/// @param subr [in] The calling details of the function.  This in includes
/// number of parameters, whether the function is spread, nospread, or
/// halfspread, whether the function should evaluate it's arguments or not.
void alloc::mkprim(const std::string& pname, subr_t subr)
{
  auto s = LISPT(new lisp_t);
  s->subrval(subr);
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
    sym.self->symbol(sym);
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
    sym.self->symbol(sym);
  }
  return sym.self;
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
