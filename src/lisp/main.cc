//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#include <vector>
#include <string>
#include "libisp.hh"
#include "except.hh"
#include "repl.hh"
#include "main.hh"

int main(int argc, const char** argv)
{
  lisp::lisp lisp;
  std::vector<std::string> args{argv + 1, argv + argc};
  for(auto f: args)
  {
    try
    {
      lisp::load(lisp::mkstring(f));
    }
    catch(const lisp::lisp_finish& ex)
    {
      return ex.exit_code;
    }
    catch(const std::exception& ex)
    {
      std::cout << f << ": " << ex.what() << std::endl;
      return 1;
    }
  }
  return lisp::run(lisp);
}
