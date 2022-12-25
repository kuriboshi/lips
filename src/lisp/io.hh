//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
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

#include <fmt/format.h>

#include "context.hh"
#include "types.hh"

namespace lisp::io
{
lisp_t ratom(ref_file_t);
lisp_t lispread(ref_file_t);
lisp_t readline(ref_file_t);
lisp_t getline(lisp_t);

lisp_t patom(lisp_t, file_t&, bool esc = false);
lisp_t prinbody(context&, lisp_t, file_t&, bool esc = false);
lisp_t prin0(context&, lisp_t, file_t&, bool esc = false);
lisp_t print(context&, lisp_t, file_t&);
lisp_t terpri(file_t&);

lisp_t splice(context&, lisp_t, lisp_t, bool);

class source
{
public:
  source() {}
  virtual ~source() = default;

  virtual int getch() = 0;
  virtual void ungetch(int) = 0;
  virtual void close() {}
  virtual std::optional<std::string> getline() = 0;

  using iterator = std::istreambuf_iterator<char>;
  virtual iterator begin() = 0;
  iterator end() { return iterator(); }

protected:
  int getch(std::istream& stream) { return stream.get(); }

  std::optional<std::string> getline(std::istream& stream)
  {
    std::string buf;
    std::getline(stream, buf);
    if(stream.fail())
      return {};
    return buf;
  }

  int _curc = 0;
};

class file_source: public source
{
public:
  file_source(const std::string& filename);
  ~file_source() = default;

  using source::getch;
  using source::getline;

  int getch() override { return getch(*_file); }
  void ungetch(int c) override { _file->putback(c); }
  void close() override { _file->close(); }
  std::optional<std::string> getline() override { return getline(*_file); }

  iterator begin() override { return iterator(*_file); }

private:
  std::unique_ptr<std::ifstream> _file;
};

class stream_source: public source
{
public:
  stream_source(std::istream& stream)
    : _stream(stream)
  {}
  ~stream_source() = default;

  using source::getch;
  using source::getline;

  int getch() override { return getch(_stream); }
  void ungetch(int c) override { _stream.putback(c); }
  std::optional<std::string> getline() override { return getline(_stream); }

  iterator begin() override { return iterator(_stream); }

private:
  std::istream& _stream;
};

class string_source: public source
{
public:
  string_source(const std::string& string)
    : _string(string)
  {}

  using source::getch;
  using source::getline;

  int getch() override { return getch(_string); }
  void ungetch(int c) override { _string.putback(c); }
  std::optional<std::string> getline() override { return getline(_string); }

  iterator begin() override { return iterator(_string); }

private:
  std::istringstream _string;
};

class sink
{
public:
  sink() {}
  virtual ~sink() = default;

  virtual void putch(int, bool esc = false) = 0;
  virtual void puts(const std::string_view) = 0;
  virtual void terpri() = 0;
  virtual void flush() = 0;
  virtual void close() {}

protected:
  //
  // Put a character c, on stream file, escaping enabled if esc is true.
  //
  void putch(int c, std::ostream& file, bool esc)
  {
    if(esc && (c == '(' || c == '"' || c == ')' || c == '\\'))
      pputc('\\', file);
    pputc(c, file);
  }

  // Put a character on stdout prefixing it with a ^ if it's a control
  // character.
  void pputc(int c, std::ostream& file)
  {
    if(c >= 0 && c < 0x20 && c != '\n' && c != '\r' && c != '\t' && c != '\a')
    {
      file.put('^');
      file.put(c + 0x40);
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

  void putch(int c, bool esc) override { putch(c, *_file, esc); }
  void puts(const std::string_view s) override { _file->write(s.data(), s.size()); }
  void terpri() override { _file->put('\n'); }
  void flush() override { _file->flush(); }
  void close() override { _file->close(); }

private:
  std::unique_ptr<std::ofstream> _file;
};

class stream_sink final: public sink
{
public:
  stream_sink(std::ostream& stream)
    : _stream(stream)
  {}
  ~stream_sink() = default;

  using sink::putch;

  void putch(int c, bool esc) override { putch(c, _stream, esc); }
  void puts(const std::string_view s) override { _stream.write(s.data(), s.size()); }
  void terpri() override { _stream.put('\n'); }
  void flush() override { _stream.flush(); }
  void close() override { _stream.flush(); }

private:
  std::ostream& _stream;
};

class string_sink final: public sink
{
public:
  string_sink() {}

  using sink::putch;

  std::string string() const { return _stream.str(); }

  void putch(int c, bool esc) override { putch(c, _stream, esc); }
  void puts(const std::string_view s) override { _stream.write(s.data(), s.size()); }
  void terpri() override { _stream.put('\n'); }
  void flush() override { _stream.flush(); }

private:
  std::ostringstream _stream;
};
} // namespace lisp::io

namespace lisp
{
class file_t final: public ref_count<file_t>
{
public:
  file_t(std::unique_ptr<io::source> source)
    : _source(std::move(source))
  {}
  file_t(std::unique_ptr<io::sink> sink)
    : _sink(std::move(sink))
  {}
  file_t(std::istream& stream)
    : _source(std::make_unique<io::stream_source>(stream))
  {}
  file_t(std::ostream& stream)
    : _sink(std::make_unique<io::stream_sink>(stream))
  {}
  file_t(const std::string& string)
    : _source(std::make_unique<io::string_source>(string))
  {}
  file_t(file_t&& file) noexcept
    : _source(std::move(file._source)),
      _sink(std::move(file._sink))
  {}
  ~file_t() {}

  bool has_source() { return !!_source; }
  bool has_sink() { return !!_sink; }

  // source
  io::source& source() { return *_source; }
  int getch()
  {
    ptrcheck(_source);
    return _source->getch();
  }
  void ungetch(int c)
  {
    ptrcheck(_source);
    _source->ungetch(c);
  }
  std::optional<std::string> getline()
  {
    ptrcheck(_source);
    return _source->getline();
  }

  // sink
  io::sink& sink() { return *_sink; }
  void putch(char c, bool esc = false)
  {
    ptrcheck(_sink);
    _sink->putch(c, esc);
  }
  void puts(const std::string_view s)
  {
    ptrcheck(_sink);
    _sink->puts(s);
  }
  void terpri()
  {
    ptrcheck(_sink);
    _sink->terpri();
  }
  void flush()
  {
    ptrcheck(_sink);
    _sink->flush();
  }

  template<typename... Ts>
  void format(std::string_view f, Ts&&... t)
  {
    auto ret = fmt::vformat(f, fmt::make_format_args(t...));
    _sink->puts(ret);
  }

  void close()
  {
    _source.release();
    _sink.release();
  }

private:
  std::unique_ptr<io::source> _source;
  std::unique_ptr<io::sink> _sink;

  void ptrcheck(const std::unique_ptr<io::source>&) const
  {
    if(!_source)
      throw lisp_error("file_t: No source");
  }

  void ptrcheck(const std::unique_ptr<io::sink>&) const
  {
    if(!_sink)
      throw lisp_error("file_t: No sink");
  }
};

inline lisp_t ratom(ref_file_t f) { return io::ratom(f); }
inline lisp_t lispread(ref_file_t f) { return io::lispread(f); }
inline lisp_t lispread(const std::string& s)
{
  auto f = ref_file_t::create(s);
  return io::lispread(f);
}
inline lisp_t readline(ref_file_t f) { return io::readline(f); }
inline lisp_t getline(lisp_t f) { return io::getline(f); }

inline lisp_t patom(lisp_t a, file_t& f, bool esc = false) { return io::patom(a, f, esc); }
inline lisp_t patom(lisp_t a, bool out = false, bool esc = false)
{
  auto& ctx = context::current();
  return io::patom(a, out ? *ctx.primerr() : *ctx.primout(), esc);
}
inline lisp_t terpri(file_t& f) { return io::terpri(f); }
inline lisp_t terpri(bool out = false)
{
  auto& ctx = context::current();
  return io::terpri(out ? *ctx.primerr() : *ctx.primout());
}
inline lisp_t prinbody(lisp_t a, file_t& f, bool esc = false) { return io::prinbody(context::current(), a, f, esc); }
inline lisp_t prinbody(lisp_t a, bool out = false, bool esc = false)
{
  auto& ctx = context::current();
  return io::prinbody(ctx, a, out ? *ctx.primerr() : *ctx.primout(), esc);
}
inline lisp_t prin0(lisp_t a, file_t& f, bool esc = false) { return io::prin0(context::current(), a, f, esc); }
inline lisp_t prin0(lisp_t a, bool out = false, bool esc = false)
{
  auto& ctx = context::current();
  return io::prin0(ctx, a, out ? *ctx.primerr() : *ctx.primout(), esc);
}
inline lisp_t print(lisp_t a, file_t& f) { return io::print(context::current(), a, f); }
inline lisp_t print(lisp_t a, bool out = false)
{
  auto& ctx = context::current();
  return io::print(ctx, a, out ? *ctx.primerr() : *ctx.primout());
}

template<typename T>
std::string to_string(T& sink)
{
  return dynamic_cast<io::string_sink&>(sink).string();
}

/// @brief Creates a lisp expression.
inline lisp_t operator"" _l(const char* s, std::size_t)
{
  auto in = ref_file_t::create(s);
  auto e = lispread(in);
  return e;
}
} // namespace lisp

inline std::ostream& operator<<(std::ostream& os, const lisp::lisp_t& obj)
{
  lisp::file_t out(os);
  prin0(obj, out, true);
  return os;
}

#endif
