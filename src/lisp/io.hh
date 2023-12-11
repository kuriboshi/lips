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

#ifndef LISP_IO_HH
#define LISP_IO_HH

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

class source
{
public:
  source() = default;
  virtual ~source() = default;

  source(const source&) = delete;
  source(source&&) = delete;
  source& operator=(const source&) = delete;
  source& operator=(source&&) = delete;

  virtual char getch() = 0;
  virtual void ungetch(char) = 0;
  virtual void close() {}
  virtual std::optional<std::string> getline() = 0;

  using iterator = std::istreambuf_iterator<char>;

protected:
  static char getch(std::istream& stream)
  {
    char ch{0};
    stream.get(ch);
    return ch;
  }

  static std::optional<std::string> getline(std::istream& stream)
  {
    std::string buf;
    std::getline(stream, buf);
    if(stream.fail())
      return {};
    return buf;
  }

private:
  virtual iterator begin() = 0;

  friend source::iterator begin(source& src)
  {
    return src.begin();
  }

  friend source::iterator end(source&)
  {
    return {};
  }
};

class file_source: public source
{
public:
  file_source(const std::string& filename);
  ~file_source() override = default;

  file_source(const file_source&) = delete;
  file_source(file_source&&) = delete;
  file_source& operator=(const file_source&) = delete;
  file_source& operator=(file_source&&) = delete;

  using source::getch;
  using source::getline;

  char getch() override { return getch(*_file); }
  void ungetch(char c) override { _file->putback(c); }
  void close() override { _file->close(); }
  std::optional<std::string> getline() override { return getline(*_file); }

private:
  iterator begin() override { return {*_file}; }

  std::unique_ptr<std::ifstream> _file;
};

class stream_source: public source
{
public:
  stream_source(std::istream& stream)
    : _stream(&stream)
  {}
  ~stream_source() override = default;

  stream_source(const stream_source&) = delete;
  stream_source(stream_source&&) = delete;
  stream_source& operator=(const stream_source&) = delete;
  stream_source& operator=(stream_source&&) = delete;

  using source::getch;
  using source::getline;

  char getch() override { return getch(*_stream); }
  void ungetch(char c) override { _stream->putback(c); }
  std::optional<std::string> getline() override { return getline(*_stream); }

private:
  iterator begin() override { return {*_stream}; }

  std::istream* _stream;
};

class string_source: public source
{
public:
  string_source(const std::string& string)
    : _string(string)
  {}

  using source::getch;
  using source::getline;

  char getch() override { return getch(_string); }
  void ungetch(char c) override { _string.putback(c); }
  std::optional<std::string> getline() override { return getline(_string); }

private:
  iterator begin() override { return {_string}; }

  std::istringstream _string;
};

class sink
{
public:
  sink() = default;
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

#endif
