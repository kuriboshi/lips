//
// Lips, lisp shell.
// Copyright 2020-2025 Krister Joas
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

#include <concepts>
#include <stdexcept>
#include <string>
#include <system_error>

namespace lisp
{
/// @brief General lisp error exception.
class lisp_error: public std::runtime_error
{
public:
  /// @brief Constructor taking a std::error_code.
  lisp_error(std::error_code ec)
    : std::runtime_error(ec.message())
  {}
  /// @brief Constructor taking a std::error_code and an error message.
  lisp_error(std::error_code ec, const std::string& error)
    : std::runtime_error(ec.message() + ": " + error),
      error_code(ec)
  {}
  /// @brief The error_code.
  std::error_code error_code;
};

/// @brief Exception used to reset the lisp vm to its initial state.
class lisp_reset: public lisp_error
{
public:
  /// @brief Default constructor.
  lisp_reset()
    : lisp_error(error_errc::reset)
  {}
};

/// @brief Exception signaling the end of the program.
///
/// Throwing this exception unwinds the stack and exits the lisp interpreter
/// with an exit code.
class lisp_finish: public std::runtime_error
{
public:
  /// @brief Constructor.
  ///
  /// @tparam Int An integer type.
  /// @param message A message to print before exiting.
  /// @param code An exit code.
  template<typename Int>
    requires std::convertible_to<Int, int>
  lisp_finish(const std::string& message, Int code = 0)
    : std::runtime_error(message),
      exit_code(static_cast<int>(code))
  {}
  /// @brief The exit code.
  int exit_code{0};
};

} // namespace lisp
