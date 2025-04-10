//
// Lips, lisp shell.
// Copyright 2022-2023, 2025 Krister Joas
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

#include <system_error>
#include <lisp/util.hh>

namespace lisp
{

enum class lips_errc
{
  event_not_found = 1,
  illegal_arg,
  no_match,
  no_such_job,
  alias_loop,
  error
};

class lips_category: public std::error_category
{
public:
  const char* name() const noexcept override { return "lips"; }
  std::string message(int condition) const override
  {
    switch(static_cast<lips_errc>(condition))
    {
      case lips_errc::event_not_found:
        return "Event not found";
      case lips_errc::illegal_arg:
        return "Illegal argument";
      case lips_errc::no_match:
        return "No match for";
      case lips_errc::no_such_job:
        return "No such job";
      case lips_errc::alias_loop:
        return "Alias loop";
      case lips_errc::error:
        return "Error";
    }
    return "";
  }
  static const std::error_category& category()
  {
    static const lips_category instance;
    return instance;
  }
};

inline std::error_code make_error_code(lips_errc e) { return {to_underlying(e), lips_category::category()}; }

inline std::error_condition make_error_condition(lips_errc e) { return {to_underlying(e), lips_category::category()}; }

} // namespace lisp

namespace std
{
template<>
struct is_error_code_enum<lisp::lips_errc>: public true_type
{};
} // namespace std
