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
#include "repl.hh"

static int macro(lisp::lisp& l, lisp::LISPT*)
{
  l.e().unwind();
  throw lisp::lisp_reset();
}

int main(int argc, const char** argv)
{
  lisp::lisp lisp;
  lisp::repl repl(lisp);
  lisp.repl = [&repl](lisp::LISPT) -> lisp::LISPT { return repl(lisp::NIL); };
  bool test = false;
  std::vector<std::string> args{argv + 1, argv + argc};
  for(auto f: args)
  {
    if(f == "--test")
    {
      test = true;
      break;
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
