//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>

#include <iostream>
#include <vector>
#include <string>
#include "libisp.hh"
#include "except.hh"

static int macro(lisp::lisp& l, lisp::LISPT*)
{
  l.e().unwind();
  throw lisp::lisp_reset();
}

class repl
{
public:
  repl(lisp::lisp& lisp) : l(lisp)
  {
    _prompt = lisp::mkstring("> ");
    lisp::gcprotect(_prompt);
    _break_prompt = lisp::mkstring(": ");
    lisp::gcprotect(_break_prompt);
  }
  ~repl() = default;

  class level
  {
  public:
    level(repl& repl) : _repl(repl) { ++_repl._level; }
    ~level() { --_repl._level; }
  private:
    repl& _repl;
  };

  void operator()()
  {
    while(true)
    {
      lisp::prin0(l, _prompt, l.primout());
      auto expr = lisp::lispread(l, l.primin(), false);
      if(expr == lisp::C_EOF)
        break;
      lisp::print(l, eval(l, expr), l.primout());
    }
  }

  lisp::LISPT operator()(lisp::LISPT exp)
  {
    level begin(*this);
    if(_level == 1)
    {
      operator()();
      return lisp::NIL;
    }
    while(true)
    {
      lisp::prin0(l, _break_prompt, l.primout());
      auto com = lisp::lispread(l, l.primin(), false);
      if(com == lisp::C_EOF)
        return com;
      /* OK, EVAL, ^, ... */
      if(type_of(com) != lisp::type::CONS)
        continue;
      else if(EQ(com->car(), lisp::C_GO))
        return print(l, eval(l, exp), lisp::NIL);
      else if(EQ(com->car(), lisp::C_RESET))
      {
        l.e().unwind();
        throw lisp::lisp_reset();
      }
      else if(EQ(com->car(), lisp::C_BT))
      {
        l.e().bt();
        continue;
      }
      else if(EQ(com->car(), lisp::C_RETURN))
        return is_NIL(com->cdr()) ? lisp::NIL : com->cdr()->car();
    }
  }

private:
  lisp::lisp& l;
  int _level = 0;
  lisp::LISPT _prompt;
  lisp::LISPT _break_prompt;
};

int main(int argc, const char** argv)
{
  lisp::lisp lisp;
  repl repl(lisp);
  lisp.repl = [&repl](lisp::LISPT) -> lisp::LISPT { return repl(lisp::NIL); };
  bool test = false;
  std::vector<std::string> args{argv + 1, argv + argc};
  for(auto f: args)
  {
    if(f == "--test")
    {
      test = true;
      continue;
    }
    lisp::load(lisp::mkstring(f));
  }
  if(test)
  {
    doctest::Context context;
    context.applyCommandLine(argc, argv);
    auto result = context.run();
    return result;
  }
  while(true)
  {
    try
    {
      lisp.repl(lisp::NIL);
      // If we return normally from repl we exit the program
      return 0;
    }
    catch(const lisp::lisp_reset& ex)
    {
      lisp.e().reset();
    }
    catch(const lisp::lisp_error& ex)
    {
      std::cout << ex.what() << std::endl;
    }
    catch(const std::exception& ex)
    {
      std::cout << ex.what() << std::endl;
    }
  }
  return 0;
}
