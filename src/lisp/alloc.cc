//
// Lips, lisp shell
// Copyright 1988, 2020-2022 Krister Joas
//

#include "alloc.hh"

namespace lisp
{
/// @brief Class handling allocation of objects.
alloc::alloc(): local_symbols(lisp_t::symbol_collection().create())
{
  destblockused = 0;
}

/// @brief Default destructor.
///
// TODO: Free all memory
alloc::~alloc() = default;

/// @brief Allocates an object from the list of free objects.
///
/// @details If there are no free objects available a new page is allocated.
///
/// @returns A new lisp_t object.
LISPT alloc::getobject()
{
  return LISPT(new lisp_t);
}

/// @brief Creates a cons pair.
///
/// @param a The car of the pair.
/// @param b The cdr of the pair.
/// @returns The cons pair.
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
  return mknumber(lisp_t::freecount());
}

LISPT alloc::mkstring(const std::string& str)
{
  auto s = getobject();
  s->set(str);
  return s;
}

/// @brief Creates an integer number.
///
/// @param number The integer number.
/// @returns An integer number as a LISP object.
LISPT alloc::mknumber(int number)
{
  auto c = getobject();;
  c->set(number);
  return c;
}

/// @brief Create a floating point number.
///
/// @param number The floating point number.
/// @returns A floating point number as a LISP object.
LISPT alloc::mkfloat(double number)
{
  auto c = getobject();
  c->set(number);
  return c;
}

inline LISPT alloc::mkarglist(LISPT alist, std::int8_t& count)
{
  if(type_of(alist) == type::CONS)
  {
    ++count;
    return cons(alist->car(), mkarglist(alist->cdr(), count));
  }
  if(is_NIL(alist))
    return NIL;
  count = -(count + 1);
  return cons(alist, NIL);
}

/// @brief Creates a lambda function.
///
/// @details Not meant to be used directly. Call the functions LAMBDA and
/// NLAMBDA instead.
///
/// @param args The parameters of the lambda function. For a spread type
/// function this is a list of atoms. For a nospread function it's a single
/// atom. For a half spread function the list should end with a dotted pair.
/// @param def The definition of the lambda function. The body of the function
/// should be a list of expressions.
/// @param type The type is either type::LAMBDA or type::NLAMBDA.
///
/// @returns A lambda function.
LISPT alloc::mklambda(LISPT args, LISPT def, type type)
{
  lambda_t lambda;
  lambda.body = def;
  std::int8_t count = 0;
  lambda.args = mkarglist(args, count);
  lambda.count = count;
  auto s = getobject();
  s->set(lambda, type == type::LAMBDA);
  return s;
}

/// @brief Make interned symbol in obarray.
///
/// @details Create an interned symbol in the global symbol table.
///
/// @param str Name of the symbol.
/// @returns The symbol as a LISP object.
LISPT alloc::intern(const std::string& str)
{
  auto& glob = lisp_t::symbol_collection().symbol_store(symbol::symbol_collection::global_id);
  auto& sym = glob.get(str);
  if(sym.self == NIL)
  {
    sym.self = new lisp_t; // NOLINT
    sym.self->set(sym);
  }
  return sym.self;
}

/// @brief Get an existing symbol.
///
/// @details Get an existing symbol in either the global symbol table or the
/// local one. If the symbol doesn't exist then create a new one in the local
/// symol table.
///
/// @param str The name of the symbol.
/// @returns The symbol as a LISP object.
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

///
/// @brief Allocates a destination block of size size.
///
/// @param size The size of the destination block.
/// @returns A destblock or nullptr if no more space available.
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

/// @brief Free a destination block.
///
/// @param ptr The destination block to free.
void alloc::dfree(destblock_t* ptr) { destblockused -= ptr->size() + 1; }

/// @brief Frees all destination blocks.
void alloc::dzero() { destblockused = 0; }

} // namespace lisp
