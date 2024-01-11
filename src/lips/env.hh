//
// Lips, lisp shell.
// Copyright 2021-2024 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#ifndef LIPS_ENV_HH
#define LIPS_ENV_HH

#include <string>
#include <lisp/lisp.hh>

namespace lisp
{

class env
{
public:
  env();

  static std::string get(const std::string& pstr)
  {
    auto* e = getenv(pstr.c_str());
    std::string result;
    if(e != nullptr)
      result = e;
    return result;
  }

  const cvariable_t& home() const { return _home->cvariable(); }
  const cvariable_t& path() const { return _path->cvariable(); }
  const cvariable_t& globsort() const { return _globsort->cvariable(); }

private:
  /*
   * Processes the environment variable PATH and returns a list
   * of all directories in PATH.
   */
  static lisp_t mungepath()
  {
    auto pstr = get("PATH");
    lisp_t result = nil;
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

  lisp_t _path; // Search path for executables.
  lisp_t _home; // Home directory.
  lisp_t _globsort;
};

} // namespace lisp

extern std::unique_ptr<lisp::env> environment; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

#endif
