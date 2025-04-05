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

#include <fstream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>

namespace lisp::io
{
enum class output
{
  PRIMARY,
  ERROR
};

enum class escape
{
  YES,
  NO
};

/// @brief Base class for an input source.
class source
{
public:
  /// @brief Default constructor.
  source() = default;
  /// @brief Default virtual destructor.
  virtual ~source() = default;

  /// @brief Delete copy constructor.
  source(const source&) = delete;
  /// @brief Delete move constructor.
  source(source&&) = delete;
  /// @brief Delete copy assignment.
  source& operator=(const source&) = delete;
  /// @brief Delete move assignment.
  source& operator=(source&&) = delete;

  /// @brief Get a character from the source.
  virtual char getch() = 0;
  /// @brief Put a character back to the source to be read by the next call to
  /// getch.
  virtual void ungetch(char) = 0;
  /// @brief Close the source.
  virtual void close() {}
  /// @brief Read from a source until the next newline. Returns an empty
  /// optional if the end of file is encountered.
  virtual std::optional<std::string> getline() = 0;

  /// @brief Derived sources can use this iterator.
  using iterator = std::istreambuf_iterator<char>;

protected:
  /// @brief Default implementation of 'getch'.
  static char getch(std::istream& stream)
  {
    char ch{0};
    stream.get(ch);
    return ch;
  }

  /// @brief Default implementation of 'getline'.
  static std::optional<std::string> getline(std::istream& stream)
  {
    std::string buf;
    std::getline(stream, buf);
    if(stream.fail())
      return {};
    return buf;
  }

private:
  /// @brief Returns the beginning of the internal stream.
  virtual iterator begin() = 0;

  /// @brief Free function.
  friend source::iterator begin(source& src) { return src.begin(); }
  /// @brief Free function.
  friend source::iterator end(source&) { return {}; }
};

/// @brief A file source.
class file_source: public source
{
public:
  /// @brief Constructor taking a file name as argument.
  ///
  /// The file is opened in read mode.
  file_source(const std::string& filename);
  /// @brief Default destructor.
  ~file_source() override = default;

  /// @brief Delete copy constructor.
  file_source(const file_source&) = delete;
  /// @brief Delete move constructor.
  file_source(file_source&&) = delete;
  /// @brief Delete copy assignment.
  file_source& operator=(const file_source&) = delete;
  /// @brief Delete move assignment.
  file_source& operator=(file_source&&) = delete;

  /// @brief Leverage the default 'getch' implementation.
  using source::getch;
  /// @brief Leverage the default 'getline' implementation.
  using source::getline;

  /// @brief Read a character from the file stream.
  char getch() override { return getch(*_file); }
  /// @brief Put back a character on the file stream.
  void ungetch(char c) override { _file->putback(c); }
  /// @brief Close the file stream.
  void close() override { _file->close(); }
  /// @brief Return a line from the file, empty on end of file.
  std::optional<std::string> getline() override { return getline(*_file); }

private:
  /// @brief Returns the beginning of the file stream.
  iterator begin() override { return {*_file}; }

  /// @brief The input file stream.
  std::unique_ptr<std::ifstream> _file;
};

/// @brief A source taking a std::istream as its input source.
class stream_source: public source
{
public:
  /// @brief Constructor taking an std::istream as in input source.
  ///
  /// This could be a file input stream or a string stream.
  stream_source(std::istream& stream)
    : _stream(&stream)
  {}
  /// @brief Default destructor.
  ~stream_source() override = default;

  /// @brief Delete copy constructor.
  stream_source(const stream_source&) = delete;
  /// @brief Delete move constructor.
  stream_source(stream_source&&) = delete;
  /// @brief Delete copy assignment.
  stream_source& operator=(const stream_source&) = delete;
  /// @brief Delete move assignment.
  stream_source& operator=(stream_source&&) = delete;

  /// @brief Leverage the default 'getch' implementation.
  using source::getch;
  /// @brief Leverage the default 'getline' implementation.
  using source::getline;

  /// @brief Read a character from the input stream.
  char getch() override { return getch(*_stream); }
  /// @brief Put back a character to the input stream.
  void ungetch(char c) override { _stream->putback(c); }
  /// @brief Read from the input stream until the next newline, empty on end of
  /// file.
  std::optional<std::string> getline() override { return getline(*_stream); }

private:
  /// @brief Returns the beginning of the input stream.
  iterator begin() override { return {*_stream}; }

  /// @brief A pointer to the input stream.
  std::istream* _stream;
};

/// @brief A source which reads from a string.
class string_source: public source
{
public:
  /// @brief Constructor taking a string as its source.
  string_source(const std::string& string)
    : _string(string)
  {}

  /// @brief Leverage the default 'getch' implementation.
  using source::getch;
  /// @brief Leverage the default 'getline' implementation.
  using source::getline;

  /// @brief Read a character from the input string.
  char getch() override { return getch(_string); }
  /// @brief Put back a character to the input string.
  void ungetch(char c) override { _string.putback(c); }
  /// @brief Read from the input string until the next newline, empty on end of
  /// string.
  std::optional<std::string> getline() override { return getline(_string); }

private:
  /// @brief Returns the beginning of the input stream.
  iterator begin() override { return {_string}; }

  /// @brief An input string stream is used as the internal source.
  std::istringstream _string;
};

/// @brief Base class for an output sink.
class sink
{
public:
  /// @brief Default constructor.
  sink() = default;
  /// @brief Default virtual destructor.
  virtual ~sink() = default;

  sink(const sink&) = delete;
  sink(sink&&) = delete;
  sink& operator=(const sink&) = delete;
  sink& operator=(sink&&) = delete;

  virtual void putch(char, enum escape esc) = 0;
  virtual void puts(std::string_view) = 0;
  virtual void terpri() = 0;
  virtual void flush() = 0;
  virtual void close() {}

protected:
  //
  // Put a character c, on stream file, escaping enabled if esc is true.
  //
  static void putch(char c, std::ostream& file, enum escape esc)
  {
    if(esc == escape::YES && (c == '(' || c == '"' || c == ')' || c == '\\'))
      pputc('\\', file);
    pputc(c, file);
  }

  // Put a character on stdout prefixing it with a ^ if it's a control
  // character.
  static void pputc(char c, std::ostream& file)
  {
    if(c >= 0 && c < 0x20 && c != '\n' && c != '\r' && c != '\t' && c != '\a')
    {
      file.put('^');
      file.put(static_cast<char>(c + 0x40));
    }
    else
      file.put(c);
  }
};

class file_sink final: public sink
{
public:
  file_sink(const std::string& filename, bool append = false);

  using sink::putch;

  void putch(char c, enum escape esc) override { putch(c, *_file, esc); }
  void puts(std::string_view s) override { _file->write(s.data(), static_cast<std::streamsize>(s.size())); }
  void terpri() override { _file->put('\n'); }
  void flush() override { _file->flush(); }
  void close() override
  {
    _file->flush();
    _file->close();
  }

private:
  std::unique_ptr<std::ofstream> _file;
};

class stream_sink final: public sink
{
public:
  stream_sink(std::ostream& stream)
    : _stream(&stream)
  {}
  ~stream_sink() override = default;

  stream_sink(const stream_sink&) = delete;
  stream_sink(stream_sink&&) = delete;
  stream_sink& operator=(const stream_sink&) = delete;
  stream_sink& operator=(stream_sink&&) = delete;

  using sink::putch;

  void putch(char c, enum escape esc) override { putch(c, *_stream, esc); }
  void puts(std::string_view s) override { _stream->write(s.data(), static_cast<std::streamsize>(s.size())); }
  void terpri() override { _stream->put('\n'); }
  void flush() override { _stream->flush(); }
  void close() override { _stream->flush(); }

private:
  std::ostream* _stream;
};

class string_sink final: public sink
{
public:
  string_sink() = default;

  using sink::putch;

  std::string string() const { return _stream.str(); }

  void putch(char c, enum escape esc) override { putch(c, _stream, esc); }
  void puts(std::string_view s) override { _stream.write(s.data(), static_cast<std::streamsize>(s.size())); }
  void terpri() override { _stream.put('\n'); }
  void flush() override { _stream.flush(); }

private:
  std::ostringstream _stream;
};
} // namespace lisp::io

namespace lisp
{
template<typename T>
std::string to_string(T& sink)
{
  return dynamic_cast<io::string_sink&>(sink).string();
}

} // namespace lisp
