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

/// @file file.hh
///
/// ### File Functions
///
/// Functions managing files.

#ifndef LISP_FILE_HH
#define LISP_FILE_HH

#include "types.hh"
#include "io.hh"
#include "vm.hh"
#include "details/file.hh"

namespace lisp
{

/// @brief `(open filename mode)` (_Function_)
///
/// Opens a file.
///
/// @param filename String or symbol file name.
/// @param mode One of `read`, `write`, or `append`.
///
/// @returns A file object.
inline lisp_t open(lisp_t filename, lisp_t mode) { return details::file::open(filename, mode); }
/// @brief `(close file)` (_Function_)
///
/// Closes a file.
///
/// @param file An file type object.
inline lisp_t close(lisp_t file) { return details::file::close(file); }
/// @brief `(load filename)` (_Filename_)
///
/// Loads lisp expressions from a file.
///
/// @param filename String or symbol file name.
///
/// @returns The file name.
inline lisp_t load(lisp_t a) { return details::file::load(a); }
/// @brief `(prin1 expr file)`
///
/// Prints a lisp expression without escaping special characters. The result
/// may not be readable by `read`.
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
/// @return The expression.
inline lisp_t prin1(lisp_t expr, lisp_t file) { return details::file::prin1(expr, file); }
/// @brief Prints a lisp expression escaping special characters such as double
/// quotes.
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
inline lisp_t prin2(lisp_t expr, lisp_t file) { return details::file::prin2(expr, file); }
/// @brief Prints a lisp expression escaping special characters and outputing a
/// newline afterwards.
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
inline lisp_t print(lisp_t a, lisp_t b) { return details::file::print(a, b); }
/// @brief `(spaces n file)` (_Function_)
///
/// Prints _n_ number of spaces.
///
/// @param file If `nil` print on primary output, if `t` print on primary
/// error, else `file` has to be an open file.
/// @param n The number of spaces to print.
///
/// @returns `nil`.
inline lisp_t spaces(lisp_t n, lisp_t file) { return details::file::spaces(n, file); }
/// @brief `(terpri file)` (_Function_)
///
/// Print a newline on the output file.
///
/// @param file If `nil` print on primary output, if `t` print on primary
/// error, else `file` has to be an open file.
///
/// @returns `nil`.
inline lisp_t terpri(lisp_t a) { return details::file::terpri(a); }
/// @brief `(printlevel level)` (_Function_)
///
/// Sets the print level.
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
inline lisp_t printlevel(lisp_t a) { return details::file::printlevel(a); }
/// @brief `(ratom file)` (_Function_)
///
/// Reads one token from the file and creates a lisp object from that token.
///
/// @param file An open file or if `nil` read from primary output, if `t` read
/// from stdin.
///
/// @returns A lisp object which is either an integer, float, symbol, or
/// string. This differs from Interlisp which will never return a
/// string. Instead the first double quote is returned as a symbol.
inline lisp_t ratom(lisp_t file) { return details::file::ratom(file); }
/// @brief `(read file)` (_Function_)
///
/// Reads a lisp expression from an open file.
///
/// @param file An open file or if `nil` read from primary output, if `t` read
/// from stdin.
///
/// @returns A lisp expression or the symbol `eof` on end of file.
inline lisp_t read(lisp_t file) { return details::file::read(file); }
/// @brief `(readc file)` (_Function_)
///
/// Reads a single character from an open file.
///
/// @param file An open file or if `nil` read from primary output, if `t` read
/// from stdin.
///
/// @returns The character read as an integer value.
inline lisp_t readc(lisp_t file) { return details::file::readc(file); }
/// @brief `(readline file)` (_Filename_)
///
/// Reads characters from an open file until the next newline. The line is
/// split according to the defined break characters and the result is returned
/// as a string. If a blank line is read the symbol `eof` is returned.
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
/// @returns A lisp expression. The value `nil` is returned if a blank line is read.
inline lisp_t readline(lisp_t file) { return details::file::readline(file); }

bool loadfile(const std::string& filename);

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

/// @brief Creates a lisp expression from a string.
inline lisp_t operator"" _l(const char* s, std::size_t)
{
  auto in = ref_file_t::create(s);
  auto e = lispread(in);
  return e;
}

/// @brief Splice an object into a list.
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
/// Modifies x to hold the value (x y x b c).
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
/// cell of y with cdr set to original (cdr x). If tailp is true, don't clobber
/// car of x.
///
lisp_t splice(lisp_t x, lisp_t y, bool tailp);

} // namespace lisp

/// @brief Outputs a lisp expression to a regular C++ output stream.
inline std::ostream& operator<<(std::ostream& os, const lisp::lisp_t& obj)
{
  lisp::file_t out(os);
  lisp::prin0(obj, out, lisp::io::escape::YES);
  return os;
}

#endif
