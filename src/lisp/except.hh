//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

#ifndef LISP_EXCEPT_HH
#define LISP_EXCEPT_HH

#include <stdexcept>
#include <string>

namespace lisp
{
class lisp_error: public std::runtime_error
{
public:
  lisp_error(std::error_code ec)
    : std::runtime_error(ec.message())
  {}
  lisp_error(std::error_code ec, const std::string& error)
    : std::runtime_error(ec.message() + ": " + error),
      error_code(ec)
  {}
  std::error_code error_code;
};

class lisp_reset: public lisp_error
{
public:
  lisp_reset()
    : lisp_error(error_errc::reset)
  {}
};

class lisp_finish: public std::runtime_error
{
public:
  template<typename Int>
  lisp_finish(const std::string& message, Int code)
    : std::runtime_error(message),
      exit_code(static_cast<int>(code))
  {}
  int exit_code{0};
};

} // namespace lisp

#endif
