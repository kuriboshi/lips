//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>

#include <vector>
#include <string>
#include "libisp.hh"
#include "except.hh"
#include "repl.hh"
#include "main.hh"

int main(int argc, const char** argv)
{
  bool test = false;
  std::vector<std::string> args{argv + 1, argv + argc};
  lisp::lisp lisp;
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
  return lisp::run(lisp);
}
