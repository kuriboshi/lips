//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
//

#ifndef LIPS_ENV_HH
#define LIPS_ENV_HH

#include <string>
#include <lisp/lisp.hh>

namespace lisp
{

class environment
{
public:
  environment();

  cvariable_t& path;            // Search path for executables.
  cvariable_t& home;            // Home directory.
  cvariable_t& globsort;

private:
  /*
   * Processes the environment variable PATH and returns a list
   * of all directories in PATH.
   */
  LISPT mungepath(const std::string& pstr)
  {
    LISPT result = NIL;
    auto pos = pstr.size();
    for(;;)
    {
      auto next = pstr.rfind(':', pos);
      if(next == std::string::npos)
      {
        result = cons(mkstring(pstr.substr(0, pos - next)), result);
        break;
      }
      result = cons(mkstring(pstr.substr(next + 1, pos - next)), result);
      pos = next;
      if(pos == 0)
      {
        result = cons(mkstring(""), result);
        break;
      }
      --pos;
    }
    return result;
  }
};

}

#endif
