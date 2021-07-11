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
  repl()
  {
    _prompt = lisp::mkstring("> ");
    lisp::gcprotect(_prompt);
    _break_prompt = lisp::mkstring(": ");
    lisp::gcprotect(_break_prompt);
  }
  ~repl() = default;

  void operator()(lisp::lisp& lisp)
  {
    ++_level;
    while(true)
    {
      if(_level > 1)
        lisp::prin0(lisp, _break_prompt, lisp.primout());
      else
        lisp::prin0(lisp, _prompt, lisp.primout());
      auto expr = lisp::lispread(lisp, lisp.primin(), false);
      if(expr == lisp::C_EOF)
        break;
      lisp::print(lisp, eval(lisp, expr), lisp.primout());
    }
  }

private:
  int _level = 0;
  lisp::LISPT _prompt;
  lisp::LISPT _break_prompt;
};

int main(int argc, const char** argv)
{
  lisp::lisp lisp;
  repl repl;
  lisp.repl = [&lisp, &repl]() -> void { repl(lisp); };
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
      lisp.repl();
      // If we return normally from repl we exit the program
      return 0;
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
