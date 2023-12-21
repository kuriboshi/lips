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

#ifndef LISP_FILE_HH
#define LISP_FILE_HH

#include "types.hh"
#include "io.hh"
#include "vm.hh"
#include "details/file.hh"

namespace lisp
{

/// @brief Closes a file.
inline lisp_t close(lisp_t a) { return details::file::close(a); }
/// @brief Loads lisp expressions from a file.
inline lisp_t load(lisp_t a) { return details::file::load(a); }
/// @brief Opens a file.
inline lisp_t open(lisp_t a, lisp_t b) { return details::file::open(a, b); }
/// @brief Prints a lisp expression without escaping special characters.
inline lisp_t prin1(lisp_t a, lisp_t b) { return details::file::prin1(a, b); }
/// @brief Prints a lisp expression escaping special characters such as double
/// quotes.
inline lisp_t prin2(lisp_t a, lisp_t b) { return details::file::prin2(a, b); }
/// @brief Prints a lisp expression escaping special characters and outputing a
/// newline afterwards.
inline lisp_t print(lisp_t a, lisp_t b) { return details::file::print(a, b); }
/// @brief Sets the print level.
///
/// The print level determines how deep the printing of a lisp expression will
/// go. Deep lisp expressions will be replaced by an ampersand (&).
inline lisp_t printlevel(lisp_t a) { return details::file::printlevel(a); }
/// @brief Read an atom from FILE.
///
/// Reads one token from the file and creates a lisp object from that token.
///
/// @param l The lisp interpreter to use.
/// @param file The source file.
///
/// @returns A lisp object which is either an integer, float, symbol, or
/// string. This differs from Interlisp which will never return a
/// string. Instead the first double quote is returned as a symbol.
inline lisp_t ratom(lisp_t a) { return details::file::ratom(a); }
/// @brief Reads a lisp expression from an open file.
inline lisp_t read(lisp_t a) { return details::file::read(a); }
/// @brief Reads a single character from an open file.
///
/// @returns The character is returned as an integer value.
inline lisp_t readc(lisp_t a) { return details::file::readc(a); }
/// @brief Reads characters from an open file until the next newline.
///
/// The line is returned as a string.
inline lisp_t readline(lisp_t a) { return details::file::readline(a); }
/// @brief Prints n number of spaces.
inline lisp_t spaces(lisp_t n, lisp_t b) { return details::file::spaces(n, b); }
/// @brief Prints a newline.
inline lisp_t terpri(lisp_t a) { return details::file::terpri(a); }

bool loadfile(const std::string& filename);

lisp_t ratom(ref_file_t f);
lisp_t lispread(ref_file_t f);
inline lisp_t lispread(const std::string& s)
{
  auto f = ref_file_t::create(s);
  return lispread(f);
}
lisp_t readline(ref_file_t f);
lisp_t getline(lisp_t f);

lisp_t patom(lisp_t a, file_t& file, io::escape esc = io::escape::NO);
inline lisp_t patom(lisp_t a, io::output out, enum io::escape esc = io::escape::NO)
{
  return patom(a, out == io::output::PRIMARY ? *vm::primout() : *vm::primerr(), esc);
}
lisp_t terpri(file_t& f);
inline lisp_t terpri(io::output out = io::output::PRIMARY)
{
  return terpri(out == io::output::PRIMARY ? *vm::primout() : *vm::primerr());
}
lisp_t prinbody(lisp_t a, file_t& file, io::escape esc = io::escape::NO, integer_t::value_type = 0);
inline lisp_t prinbody(lisp_t a, io::output out, io::escape esc = io::escape::NO)
{
  return prinbody(a, out == io::output::PRIMARY ? *vm::primout() : *vm::primerr(), esc);
}
lisp_t prin0(lisp_t a, file_t& file, io::escape esc = io::escape::NO, integer_t::value_type = 0);
inline lisp_t prin0(lisp_t a, io::output out = io::output::PRIMARY, enum io::escape esc = io::escape::NO)
{
  return prin0(a, out == io::output::PRIMARY ? *vm::primout() : *vm::primerr(), esc, 0);
}
lisp_t print(lisp_t a, file_t& file);
inline lisp_t print(lisp_t a, io::output out = io::output::PRIMARY)
{
  return print(a, out == io::output::PRIMARY ? *vm::primout() : *vm::primerr());
}

/// @brief Creates a lisp expression.
inline lisp_t operator"" _l(const char* s, std::size_t)
{
  auto in = ref_file_t::create(s);
  auto e = lispread(in);
  return e;
}

/// @brief Splice an object into a list.
///
/// Splices list y into x keeping cdr of x. For example:
/// @code{.lisp}
/// (let ((x '(a b c))
///       (y '(x y z)))
///  (splice x y)
///  x)
/// @endcode
/// Modifies x to hold the value (x y x b c).
///
/// Another example:
/// @code{.lisp}
/// (let ((x '(a b c))
///       (y '(x y z)))
///  (splice (cdr x) y)
///  x)
/// @endcode
/// Modifies x to hold the value (a x y z c).
///
/// If y is not a list put it in car of x and return x, otherwise return last
/// cell of y with cdr set to original (cdr x). If tailp is true, don't clobber
/// car of x.
///
lisp_t splice(lisp_t x, lisp_t y, bool tailp);

} // namespace lisp

/// @brief Outpus a lisp expression to a regular C++ output stream.
inline std::ostream& operator<<(std::ostream& os, const lisp::lisp_t& obj)
{
  lisp::file_t out(os);
  lisp::prin0(obj, out, lisp::io::escape::YES);
  return os;
}

#endif
