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

#pragma once

#include <optional>
#include <string>
#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include <lisp/lisp.hh>

namespace glob
{
void init();
using lisp_t = lisp::lisp_t;

std::optional<std::string> extilde(const std::string& w);
lisp_t expandfiles(const std::string& wild, bool sort);
lisp_t expand(lisp_t wild);
} // namespace glob

inline lisp::lisp_t expand(lisp::lisp_t wild) { return glob::expand(wild); }
