//
// Lips, lisp shell.
// Copyright 1989, 2020-2022 Krister Joas
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

#ifndef LISP_ERROR_HH
#define LISP_ERROR_HH

namespace lisp
{
inline constexpr int PRINT_ARG = 1 << 8;
inline constexpr int NOT_A = 1 << 9;

inline constexpr int NO_MESSAGE = 0;
inline constexpr int ILLEGAL_ARG = (PRINT_ARG | 1);
inline constexpr int DIVIDE_ZERO = 2;
inline constexpr int BUG = 3;
inline constexpr int NO_MATCH = (PRINT_ARG | 4);
inline constexpr int CANT_CREATE = (PRINT_ARG | 6);
inline constexpr int CANT_CREATE_OPEN = (PRINT_ARG | 7);
inline constexpr int CANT_OPEN = (PRINT_ARG | 8);
inline constexpr int NO_SUCH_JOB = (PRINT_ARG | 9);
inline constexpr int NOT_PRINTABLE = (PRINT_ARG | 10);
inline constexpr int NO_DIRECTORY = (PRINT_ARG | 11);
inline constexpr int NO_USER = (PRINT_ARG | 12);
inline constexpr int ATTEMPT_TO_CLOBBER = (PRINT_ARG | 13);
inline constexpr int OUT_OF_MEMORY = 14;
inline constexpr int UNEXPECTED_EOF = 15;
inline constexpr int EVENT_NOT_FOUND = 16;
inline constexpr int UNKNOWN_REQUEST = (PRINT_ARG | 17);
inline constexpr int ILLEGAL_SIGNAL = 18;
inline constexpr int STACK_OVERFLOW = 19;
inline constexpr int CORRUPT_DATA = (PRINT_ARG | 20);
inline constexpr int COMMAND_ABORTED = 21;
inline constexpr int ALIAS_LOOP = (PRINT_ARG | 22);
inline constexpr int ILLEGAL_FUNCTION = (PRINT_ARG | 23);
inline constexpr int UNDEF_FUNCTION = (PRINT_ARG | 24);
inline constexpr int UNBOUND_VARIABLE = (PRINT_ARG | 25);
inline constexpr int KBD_BREAK = (PRINT_ARG | 26);
inline constexpr int AMBIGUOUS = (PRINT_ARG | 27);
inline constexpr int USER_ERROR = (PRINT_ARG | 28);
inline constexpr int CANT_LOAD = (PRINT_ARG | 29);
inline constexpr int MAXMESSAGE = 30;

inline constexpr int error_code(int x) { return x & ~PRINT_ARG & ~NOT_A; }

} // namespace lisp

#endif
