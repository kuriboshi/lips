//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//

#ifndef LIPS_MAIN_HH
#define LIPS_MAIN_HH

#include <iostream>
#include "lisp.hh"
#include "repl.hh"

namespace lisp
{

inline int run(lisp& lisp, std::ostream& out = std::cout)
{
  repl repl(lisp);
  lisp.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
  while(true)
  {
    try
    {
      lisp.repl(NIL);
      // If we return normally from repl we exit the program
      return 0;
    }
    catch(const lisp_reset& ex)
    {
      lisp.e().reset();
    }
    catch(const lisp_error& ex)
    {
      out << ex.what() << std::endl;
    }
    catch(const lisp_finish& ex)
    {
      return ex.exit_code;
    }
    catch(const std::exception& ex)
    {
      out << ex.what() << std::endl;
    }
  }
  return 1;
}

}

#endif
