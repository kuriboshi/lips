//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

/// @file file.hh
///
/// # Input and Output Functions
///
/// Functions managing files.

#pragma once

#include "types.hh"
#include "io.hh"
#include "vm.hh"
#include "details/file.hh"

namespace lisp
{

/// @brief Opens a file.
/// @lisp{(open filename mode),Function}
///
/// @param filename String or symbol file name.
/// @param mode One of `read`, `write`, or `append`.
///
/// @returns A file type object.
inline lisp_t open(const lisp_t& filename, const lisp_t& mode) { return details::file::open(filename, mode); }
/// @brief Closes a file.
/// @lisp{(close file),Function}
///
/// @param file A file type object.
///
/// @returns `t`.
inline lisp_t close(const lisp_t& file) { return details::file::close(file); }
/// @brief Loads lisp expressions from a file.
/// @lisp{(load filename),Filename}
///
/// @param filename String or symbol file name.
///
/// @returns The file name.
inline lisp_t load(const lisp_t& filename) { return details::file::load(filename); }
/// @brief Prints a lisp expression without escaping special characters.
/// @lisp{(prin1 expr file),Function}
///
/// The result may not be readable by `read`.
///
/// ```lisp
/// (prin1 "hello")
/// hello
///   => "hello"
/// ```
///
/// @param expr A lisp expression to print.
/// @param file If `nil` print on primary output, if `t` print on primary
/// error, else `file` has to be of type _file_.
///
/// @returns The expression.
inline lisp_t prin1(const lisp_t& expr, const lisp_t& file) { return details::file::prin1(expr, file); }
/// @brief Prints a lisp expression escaping special characters such as double
/// quotes.
/// @lisp{(prin2 args...),Function}
///
/// Prints a lisp expression in a way that can be read back.
///
/// ```lisp
/// (prin2 "hello")
/// "hello" => "hello"
/// ```
///
/// @param expr A lisp expression to print.
/// @param file If `nil` print on primary output, if `t` print on primary
/// error, else `file` has to be of type _file_.
///
/// @returns The expression.
inline lisp_t prin2(const lisp_t& expr, const lisp_t& file) { return details::file::prin2(expr, file); }
/// @brief Prints a lisp expression escaping special characters and outputing a
/// newline afterwards.
/// @lisp{(print args...),Function}
///
/// Prints a lisp expression escaping special characters and ending with a
/// newline.
///
/// ```lisp
/// (print "hello")
/// "hello"
///   => "hello"
/// ```
///
/// @param expr A lisp expression to print.
/// @param file If `nil` print on primary output, if `t` print on primary
/// error, else `file` has to be of type _file_.
///
/// @returns The expression.
inline lisp_t print(const lisp_t& expr, const lisp_t& file) { return details::file::print(expr, file); }
/// @brief Prints _n_ number of spaces.
/// @lisp{(spaces n file),Function}
///
/// @param file If `nil` print on primary output, if `t` print on primary
/// error, else `file` has to be an open file.
/// @param n The number of spaces to print.
///
/// @returns `nil`.
inline lisp_t spaces(const lisp_t& n, const lisp_t& file) { return details::file::spaces(n, file); }
/// @brief Print a newline on the output file.
/// @lisp{(terpri file),Function}
///
/// @param file If `nil` print on primary output, if `t` print on primary
/// error, else `file` has to be an open file.
///
/// @returns `nil`.
inline lisp_t terpri(const lisp_t& file) { return details::file::terpri(file); }
/// @brief Sets the print level.
/// @lisp{(printlevel level),Function}
///
/// The print level determines how deep the printing of a lisp expression will
/// go. Deep lisp expressions will be replaced by an ampersand (&). If the
/// `level` argument is left out or `nil` the current print level is returned.
///
/// ```lisp
/// (print '(a (b (c (d]
/// (a (b (c (d))))
///   => (a (b (c (d))))
/// (printlevel 2)
///   => 0
/// (print '(a (b (c (d]
/// (a (b &))
///   => (a (b &))
/// ```
///
/// @param level The depth to which S-expressions are printed.
///
/// @returns The previous level.
inline lisp_t printlevel(const lisp_t& level) { return details::file::printlevel(level); }
/// @brief Reads one token from the file and creates a lisp object from that
/// token.
/// @lisp{(ratom file),Function}
///
/// @param file An open file or if `nil` read from primary output, if `t` read
/// from stdin.
///
/// @returns A lisp object which is either an integer, float, symbol, or
/// string. This differs from Interlisp which will never return a
/// string. Instead the first double quote is returned as a symbol.
inline lisp_t ratom(const lisp_t& file) { return details::file::ratom(file); }
/// @brief Reads a lisp expression from an open file.
/// @lisp{(read file),Function}
///
/// @param file An open file or if `nil` read from primary input, if `t` read
/// from stdin.
///
/// @returns A lisp expression or the symbol `eof` on end of file.
inline lisp_t read(const lisp_t& file) { return details::file::read(file); }
/// @brief Reads a single character from an open file.
/// @lisp{(readc file),Function}
///
/// @param file An open file or if `nil` read from primary input, if `t` read
/// from stdin.
///
/// @returns The character read as an integer value.
inline lisp_t readc(const lisp_t& file) { return details::file::readc(file); }
/// @brief Reads characters from an open file until the next newline.
/// @lisp{(readline file),Filename}
///
/// The line is split according to the defined break characters and the result
/// is returned as a string. If a blank line is read the symbol `eof` is
/// returned.
///
/// ```text
/// hello world => (hello world)
/// "hello world" => ("hello world")
/// (hello world) => (hello world)
///
/// ```
///
/// @param file An open file or if `nil` read from primary output, if `t` read
/// from stdin.
///
/// @returns A lisp expression. The symbol `eof` is returned if a blank line is
/// read.
inline lisp_t readline(const lisp_t& file) { return details::file::readline(file); }
/// @brief Splice an object into a list.
/// @lisp{(splice x y tailp),Function}
///
/// Splices list y into x keeping cdr of x. For example:
///
/// ```lisp
/// (let ((x '(a b c))
///       (y '(x y z)))
///  (splice x y)
///  x)
/// ```
///
/// Modifies x to hold the value (x y z b c).
///
/// Another example:
///
/// ```lisp
/// (let ((x '(a b c))
///       (y '(x y z)))
///  (splice (cdr x) y)
///  x)
/// ```
///
/// Modifies x to hold the value (a x y z c).
///
/// If y is not a list put it in car of x and return x, otherwise return last
/// cell of y with cdr set to original (cdr x). If tailp is `t`, don't clobber
/// car of x.
inline lisp_t splice(const lisp_t& x, const lisp_t& y, const lisp_t& tailp)
{
  return details::file::splice(x, y, tailp);
}

/// @brief Loads a file from _filename_.
///
/// Read S-expressions from the file until end of file it reached.
///
/// @param filename A file name.
///
/// @returns True if the file was loaded successfully, false otherwise.
bool loadfile(const std::string& filename);

lisp_t lispread(ref_file_t f);
inline lisp_t lispread(const std::string& s)
{
  auto f = ref_file_t::create(s);
  return lispread(f);
}
lisp_t readline(const ref_file_t& f);
lisp_t getline(const lisp_t& f);

lisp_t patom(lisp_t a, file_t& file, io::escape esc = io::escape::NO);
inline lisp_t patom(lisp_t a, io::output out, enum io::escape esc = io::escape::NO)
{
  return patom(std::move(a), out == io::output::PRIMARY ? *vm::primout() : *vm::primerr(), esc);
}
lisp_t terpri(file_t& f);
inline lisp_t terpri(io::output out = io::output::PRIMARY)
{
  return terpri(out == io::output::PRIMARY ? *vm::primout() : *vm::primerr());
}
lisp_t prinbody(lisp_t a, file_t& file, io::escape esc = io::escape::NO, integer_t::value_type = 0);
inline lisp_t prinbody(lisp_t a, io::output out, io::escape esc = io::escape::NO)
{
  return prinbody(std::move(a), out == io::output::PRIMARY ? *vm::primout() : *vm::primerr(), esc);
}
lisp_t prin0(lisp_t a, file_t& file, io::escape esc = io::escape::NO, integer_t::value_type = 0);
inline lisp_t prin0(lisp_t a, io::output out = io::output::PRIMARY, enum io::escape esc = io::escape::NO)
{
  return prin0(std::move(a), out == io::output::PRIMARY ? *vm::primout() : *vm::primerr(), esc, 0);
}
lisp_t print(lisp_t a, file_t& file);
inline lisp_t print(lisp_t a, io::output out = io::output::PRIMARY)
{
  return print(std::move(a), out == io::output::PRIMARY ? *vm::primout() : *vm::primerr());
}

inline namespace literals
{
/// @brief Creates a lisp expression from a string.
inline lisp::lisp_t operator""_l(const char* s, std::size_t)
{
  auto in = lisp::ref_file_t::create(s);
  auto e = lisp::lispread(in);
  return e;
}
} // namespace literals

} // namespace lisp

/// @brief Outputs a lisp expression to a regular C++ output stream.
inline std::ostream& operator<<(std::ostream& os, const lisp::lisp_t& obj)
{
  lisp::file_t out(os);
  lisp::prin0(obj, out, lisp::io::escape::YES);
  return os;
}
