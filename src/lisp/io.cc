//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
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

#include <array>
#include <charconv>
#include <cstdint>
#include <string>

#include "check.hh"
#include "io.hh"
#include "parser.hh"
#include "pred.hh"
#include "prim.hh"
#include "vm.hh"
#include "details/file.hh"

namespace lisp::io
{
file_source::file_source(const std::string& name)
{
  _file = std::make_unique<std::ifstream>();
  _file->open(name, std::ios_base::in);
  if(_file->fail())
    _file->open((name + ".lisp"), std::ios_base::in);
  if(_file->fail())
    _file->open((name + ".lsp"), std::ios_base::in);
  // TODO: Throw different exception
  if(_file->fail())
    throw lisp_error(error_errc::cant_open, name);
}

file_sink::file_sink(const std::string& name, bool append)
{
  _file = std::make_unique<std::ofstream>(name, append ? std::ios_base::app : std::ios_base::out);
  if(_file->fail())
    throw lisp_error(error_errc::cant_open, name);
}

} // namespace lisp::io
