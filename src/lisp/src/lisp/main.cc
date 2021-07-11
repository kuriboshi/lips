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

int main(int argc, const char** argv)
{
  lisp::lisp lisp;
  auto prompt = lisp::mkstring("> ");
  lisp::gcprotect(prompt);
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
      // lisp.repl(prompt, [](lisp::lisp& lisp, lisp::LISPT*) -> int { return macro(lisp, nullptr); });
      lisp.repl(prompt, nullptr);
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
