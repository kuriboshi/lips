//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include <doctest/doctest.h>
#include <cstring>              // For strerror
#include <cerrno>               // For errno
#include <iostream>
#include "libisp.hh"
#include "except.hh"
#include "error.hh"

namespace lisp
{
namespace pn
{
inline constexpr auto E = "e";                   // noeval version of eval
inline constexpr auto EVAL = "eval";             // evaluate exp
inline constexpr auto APPLY = "apply";           // apply function on args
inline constexpr auto APPLYSTAR = "apply*";      // apply nospread
inline constexpr auto BAKTRACE = "baktrace";     // control stack backtrace
inline constexpr auto TOPOFSTACK = "topofstack"; // return top of value stack
inline constexpr auto DESTBLOCK = "destblock";   // convert environment to list

inline constexpr auto RECLAIM = "reclaim";       // Initiate garbage collection
inline constexpr auto CONS = "cons";             // Make a new cons cell
inline constexpr auto FREECOUNT = "freecount";   // Number of free cells
inline constexpr auto OBARRAY = "obarray";       // Return list of all atoms
} // namespace pn

lisp::lisp(): _alloc(*new alloc()), _eval(*new evaluator(*this))
{
  if(_current == nullptr)
    _current = this;

  _primout = std::make_unique<file_t>(std::cout);
  _primerr = std::make_unique<file_t>(std::cerr);
  _primin = std::make_unique<file_t>(std::cin);
  _stdout = std::make_unique<file_t>(std::cout);
  _stderr = std::make_unique<file_t>(std::cerr);
  _stdin = std::make_unique<file_t>(std::cin);

  static auto global_set = false;
  if(!global_set)
  {
    global_set = true;

    messages[error_code(NO_MESSAGE)] = "";
    messages[error_code(ILLEGAL_ARG)] = "Illegal argument";
    messages[error_code(DIVIDE_ZERO)] = "Divide by zero";
    messages[error_code(BUG)] = "Internal bug";
    messages[error_code(NO_MATCH)] = "No match for";
    messages[error_code(CANT_CREATE)] = "Can't create file";
    messages[error_code(CANT_CREATE_OPEN)] = "Can't create or open file";
    messages[error_code(CANT_OPEN)] = "Can't open file";
    messages[error_code(NO_SUCH_JOB)] = "No such job";
    messages[error_code(NOT_PRINTABLE)] = "Not printable";
    messages[error_code(NO_DIRECTORY)] = "No directory";
    messages[error_code(NO_USER)] = "No such user";
    messages[error_code(ATTEMPT_TO_CLOBBER)] = "Attempt to clobber constant";
    messages[error_code(OUT_OF_MEMORY)] = "Out of memory";
    messages[error_code(UNEXPECTED_EOF)] = "Unexpected end of file";
    messages[error_code(EVENT_NOT_FOUND)] = "Event not found";
    messages[error_code(UNKNOWN_REQUEST)] = "Unknown request";
    messages[error_code(ILLEGAL_SIGNAL)] = "Illegal signal";
    messages[error_code(STACK_OVERFLOW)] = "Stack overflow";
    messages[error_code(CORRUPT_DATA)] = "Bug: corrupt data";
    messages[error_code(COMMAND_ABORTED)] = "Command aborted";
    messages[error_code(ALIAS_LOOP)] = "Alias loop";
    messages[error_code(ILLEGAL_FUNCTION)] = "Illegal function";
    messages[error_code(UNDEF_FUNCTION)] = "Undefined function";
    messages[error_code(UNBOUND_VARIABLE)] = "Unbound variable";
    messages[error_code(KBD_BREAK)] = "Break";
    messages[error_code(AMBIGUOUS)] = "Ambiguous";
    messages[error_code(USER_ERROR)] = "";
    messages[error_code(CANT_LOAD)] = "Can't load file";

    auto intern = [this](const auto s) { return a().intern(s); };

    // Must be early since it's used by symbol_store_t to initialize new
    // symbols.
    C_UNBOUND = intern("unbound");
    C_UNBOUND->symbol().constant = true;
    C_UNBOUND->set();
    C_UNBOUND->settype(type::UNBOUND);

    auto nil = intern("nil");
    nil->symvalue(NIL);
    nil->symbol().constant = true;

    auto t = intern("t");
    T = t;
    t->symvalue(T);
    t->settype(type::T);
    t->symbol().constant = true;

    C_AUTOLOAD = intern("autoload");
    C_BROKEN = intern("broken");
    C_BT = intern("bt");
    C_CLOSURE = intern("closure");
    C_CONS = intern(pn::CONS);
    C_DOT = intern(".");
    C_ENDOFFILE = intern("endoffile");
    C_ENVIRON = intern("environ");
    C_EOF = intern("eof");
    C_EOF->settype(type::ENDOFFILE);
    C_FILE = intern("file");
    C_FLOAT = intern("float");
    C_FREE = intern("free");
    C_FSUBR = intern("fsubr");
    C_GO = intern("go");
    C_INDIRECT = intern("indirect");
    C_INTEGER = intern("integer");
    C_OLDDEF = intern("olddef");
    C_REDEFINED = intern("redefined");
    C_RESET = intern("reset");
    C_RETURN = intern("return");
    C_STRING = intern("string");
    C_SUBR = intern("subr");
    C_SYMBOL = intern("symbol");
    C_READ = intern("read");
    C_WRITE = intern("write");
    C_APPEND = intern("append");

    e().undefhook(nullptr);
    e().breakhook(nullptr);

    Map::init();
    arith::init();
    debug::init();
    file::init();
    logic::init();
    low::init();
    pred::init();
    prim::init();
    prop::init();
    string::init();
    user::init();

    // clang-format off
    mkprim(pn::E,          eval,       subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
    mkprim(pn::EVAL,       eval,       subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::APPLY,      apply,      subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::APPLYSTAR,  apply,      subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
    mkprim(pn::BAKTRACE,   baktrace,   subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::TOPOFSTACK, topofstack, subr_t::subr::EVAL,   subr_t::spread::SPREAD);
    mkprim(pn::DESTBLOCK,  destblock,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);

    mkprim(pn::RECLAIM,   reclaim,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
    mkprim(pn::CONS,      cons,      subr_t::subr::EVAL, subr_t::spread::SPREAD);
    mkprim(pn::FREECOUNT, freecount, subr_t::subr::EVAL, subr_t::spread::SPREAD);
    mkprim(pn::OBARRAY,   obarray,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
    // clang-format on
 }

  _variables = std::make_unique<cvariables>(_alloc);

  io::set_read_table(*this);
}

lisp::~lisp()
{
  if(_current == this)
    _current = nullptr;
}

LISPT lisp::eval(lisp& l, LISPT expr) { return l._eval.eval(expr); }
LISPT lisp::apply(lisp& l, LISPT fun, LISPT args) { return l._eval.apply(fun, args); }
LISPT lisp::baktrace(lisp& l) { return l._eval.baktrace(); }
LISPT lisp::topofstack(lisp& l) { return l._eval.topofstack(); }
LISPT lisp::destblock(lisp& l, LISPT a) { return l._eval.destblock(a); }

LISPT lisp::cons(lisp& l, LISPT a, LISPT b) { return l._alloc.cons(a, b); }
LISPT lisp::reclaim(lisp& l, LISPT a) { return l._alloc.reclaim(a); }
LISPT lisp::obarray(lisp& l) { return l._alloc.obarray(); }
LISPT lisp::freecount(lisp& l) { return l._alloc.freecount(); }

void lisp::primout(std::unique_ptr<file_t> f)
{
  _primout = std::move(f);
}

void lisp::primerr(std::unique_ptr<file_t> f)
{
  _primerr = std::move(f);
}

void lisp::primin(std::unique_ptr<file_t> f)
{
  _primin = std::move(f);
}

inline std::string lisp::geterror(int messnr)
{
  if(NOT_A & messnr)
    return errmess[error_code(messnr)];
  return messages[error_code(messnr)];
}

LISPT lisp::perror(int messnr, LISPT arg)
{
  primerr().format("{} ", geterror(messnr));
  if(messnr & (PRINT_ARG | NOT_A))
    prin2(*this, arg, T);
  return C_ERROR;
}

LISPT lisp::error(int messnr, LISPT arg)
{
  perror(messnr, arg);
  throw lisp_error(geterror(messnr));
}

void lisp::fatal(int messnr)
{
  throw lisp_error(geterror(messnr));
}

LISPT lisp::syserr(LISPT fault)
{
  if(!is_NIL(fault))
  {
    prin2(*this, fault, T);
    primerr().format(": ");
  }
  primerr().format("{}", strerror(errno));
  return C_ERROR;
}

LISPT lisp::break0(LISPT exp)
{
  return repl(exp);
}

lisp::cvariables::cvariables(alloc& a)
  : _currentbase(initcvar("base", a.mknumber(10L))), _verbose(initcvar("verbose", NIL)),
    _version(initcvar("version", a.mkstring(VERSION)))
{}

lisp* lisp::_current = nullptr;
std::map<int, std::string> lisp::messages;
std::unordered_map<std::string, subr_t::subr_index> subr_t::subr_map;
subr_t::subr_vector subr_t::subr_store;

LISPT eval(lisp& l, const std::string& expr)
{
  file_t in(expr);
  auto e = lispread(l, in, false);
  return lisp::eval(l, e);
}

LISPT eval(const std::string& expr)
{
  return eval(lisp::current(), expr);
}

//
// All lisp constants needed internally.
//
LISPT T;
LISPT C_AUTOLOAD;
LISPT C_BROKEN;
LISPT C_BT;
LISPT C_CLOSURE;
LISPT C_CONS;
LISPT C_DOT;
LISPT C_ENDOFFILE;
LISPT C_ENVIRON;
LISPT C_EOF;
LISPT C_FILE;
LISPT C_FLOAT;
LISPT C_FREE;
LISPT C_FSUBR;
LISPT C_GO;
LISPT C_INDIRECT;
LISPT C_INTEGER;
LISPT C_OLDDEF;
LISPT C_REDEFINED;
LISPT C_RESET;
LISPT C_RETURN;
LISPT C_STRING;
LISPT C_SUBR;
LISPT C_SYMBOL;
LISPT C_UNBOUND;
LISPT C_READ;
LISPT C_WRITE;
LISPT C_APPEND;

} // namespace lisp
