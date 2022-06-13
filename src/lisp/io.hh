//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LISP_IO_HH
#define LISP_IO_HH

#include <optional>
#include <sstream>
#include <fstream>
#include <string>
#include <string_view>
#include <fmt/format.h>
#include "lisp.hh"

namespace lisp::io
{
LISPT ratom(lisp& l, ref_file_t);
LISPT lispread(lisp& l, ref_file_t);
LISPT readline(lisp& l, ref_file_t);
LISPT getline(lisp& l, LISPT);

LISPT patom(lisp& l, LISPT, file_t&, bool esc = false);
LISPT prinbody(lisp& l, LISPT, file_t&, bool esc = false);
LISPT prin0(lisp& l, LISPT, file_t&, bool esc = false);
LISPT print(lisp& l, LISPT, file_t&);
LISPT terpri(lisp& l, file_t&);

LISPT splice(lisp&, LISPT, LISPT, bool);

class source
{
public:
  source() {}
  virtual ~source() = default;

  virtual int getch() = 0;
  virtual void ungetch(int) = 0;
  virtual bool close() = 0;
  virtual std::optional<std::string> getline() = 0;

  using iterator = std::istreambuf_iterator<char>;
  virtual iterator begin() = 0;
  iterator end() { return iterator(); }  

protected:
  int getch(std::istream& stream)
  {
    return stream.get();
  }

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

  int getch() override
  {
    return getch(*_file);
  }
  void ungetch(int c) override { _file->putback(c); }
  bool close() override
  {
    _file->close();
    return !_file->is_open();
  }
  std::optional<std::string> getline() override
  {
    return getline(*_file);
  }

  iterator begin() override { return iterator(*_file); }

private:
  std::unique_ptr<std::ifstream> _file;
};

class stream_source: public source
{
public:
  stream_source(std::istream& stream): _stream(stream) {}
  ~stream_source() = default;

  using source::getch;
  using source::getline;

  int getch() override
  {
    return getch(_stream);
  }
  void ungetch(int c) override { _stream.putback(c); }
  bool close() override { return true; }
  std::optional<std::string> getline() override
  {
    return getline(_stream);
  }

  iterator begin() override { return iterator(_stream); }

private:
  std::istream& _stream;
};

class string_source: public source
{
public:
  string_source(const std::string& string): _string(string) {}

  using source::getch;
  using source::getline;

  int getch() override
  {
    return getch(_string);
  }
  void ungetch(int c) override { _string.putback(c); }
  bool close() override { return true; }
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
  virtual void flush() {}
  virtual bool close() = 0;

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
  bool close() override
  {
    _file->close();
    return !_file->is_open();
  }

private:
  std::unique_ptr<std::ofstream> _file;
};

class stream_sink final: public sink
{
public:
  stream_sink(std::ostream& stream): _stream(stream) {}
  ~stream_sink() = default;

  using sink::putch;

  void putch(int c, bool esc) override { putch(c, _stream, esc); }
  void puts(const std::string_view s) override { _stream.write(s.data(), s.size()); }
  void terpri() override { _stream.put('\n'); }
  void flush() override { _stream.flush(); }
  bool close() override
  {
    _stream.flush();
    return true;
  }

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
  bool close() override { return true; }

private:
  std::ostringstream _stream;
};
} // namespace lisp::io

namespace lisp
{
class file_t final: public ref_count<file_t>
{
public:
  file_t(std::unique_ptr<io::source> source): _source(std::move(source)) {}
  file_t(std::unique_ptr<io::sink> sink): _sink(std::move(sink)) {}
  file_t(std::unique_ptr<io::source> source, std::unique_ptr<io::sink> sink): _source(std::move(source)), _sink(std::move(sink)) {}
  file_t(std::istream& stream): _source(std::make_unique<io::stream_source>(stream)) {}
  file_t(std::ostream& stream): _sink(std::make_unique<io::stream_sink>(stream)) {}
  file_t(const std::string& string): _source(std::make_unique<io::string_source>(string)) {}
  file_t(file_t&& file) noexcept: _source(std::move(file._source)), _sink(std::move(file._sink)) {}
  ~file_t() {}

  bool has_source() { return !!_source; }
  bool has_sink() { return !!_sink; }

  // source
  io::source& source() { return *_source; }
  int getch() { ptrcheck(_source); return _source->getch(); }
  void ungetch(int c) { ptrcheck(_source); _source->ungetch(c); }
  std::optional<std::string> getline() { ptrcheck(_source); return _source->getline(); }

  // sink
  io::sink& sink() { return *_sink; }
  void putch(char c, bool esc = false) { ptrcheck(_sink); _sink->putch(c, esc); }
  void puts(const std::string_view s) { ptrcheck(_sink); _sink->puts(s); }
  void terpri() { ptrcheck(_sink); _sink->terpri(); }
  void flush() { ptrcheck(_sink); _sink->flush(); }

  template<typename... Ts>
  void format(std::string_view f, Ts&&... t)
  {
    auto ret = fmt::vformat(f, fmt::make_format_args(t...));
    _sink->puts(ret);
  }

  bool close()
  {
    _source.release();
    _sink.release();
    return true;
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

inline LISPT ratom(lisp& l, ref_file_t f) { return io::ratom(l, f); }
inline LISPT ratom(ref_file_t f) { return io::ratom(lisp::current(), f); }
inline LISPT lispread(lisp& l, ref_file_t f) { return io::lispread(l, f); }
inline LISPT lispread(ref_file_t f) { return io::lispread(lisp::current(), f); }
inline LISPT lispread(const std::string& s)
{
  auto f = ref_file_t::create(s);
  return io::lispread(lisp::current(), f);
}
inline LISPT readline(lisp& l, ref_file_t f) { return io::readline(l, f); }
inline LISPT readline(ref_file_t f) { return io::readline(lisp::current(), f); }
inline LISPT getline(lisp& l, LISPT f) { return io::getline(l, f); }
inline LISPT getline(LISPT f) { return io::getline(lisp::current(), f); }

inline LISPT patom(lisp& l, LISPT a, file_t& f, bool esc = false) { return io::patom(l, a, f, esc); }
inline LISPT patom(LISPT a, file_t& f, bool esc = false) { return io::patom(lisp::current(), a, f, esc); }
inline LISPT patom(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io::patom(l, a, out ? *l.primerr() : *l.primout(), esc);
}
inline LISPT patom(LISPT a, bool out = false, bool esc = false)
{
  auto& l = lisp::current();
  return io::patom(l, a, out ? *l.primerr() : *l.primout(), esc);
}
inline LISPT terpri(lisp& l, file_t& f) { return io::terpri(l, f); }
inline LISPT terpri(file_t& f) { return io::terpri(lisp::current(), f); }
inline LISPT terpri(lisp& l, bool out = false) { return io::terpri(l, out ? *l.primerr() : *l.primout()); }
inline LISPT terpri(bool out = false)
{
  auto& l = lisp::current();
  return io::terpri(l, out ? *l.primerr() : *l.primout());
}
inline LISPT prinbody(lisp& l, LISPT a, file_t& f, bool esc = false) { return io::prinbody(l, a, f, esc); }
inline LISPT prinbody(LISPT a, file_t& f, bool esc = false) { return io::prinbody(lisp::current(), a, f, esc); }
inline LISPT prinbody(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io::prinbody(l, a, out ? *l.primerr() : *l.primout(), esc);
}
inline LISPT prinbody(LISPT a, bool out = false, bool esc = false)
{
  auto& l = lisp::current();
  return io::prinbody(l, a, out ? *l.primerr() : *l.primout(), esc);
}
inline LISPT prin0(lisp& l, LISPT a, file_t& f, bool esc = false) { return io::prin0(l, a, f, esc); }
inline LISPT prin0(LISPT a, file_t& f, bool esc = false) { return io::prin0(lisp::current(), a, f, esc); }
inline LISPT prin0(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io::prin0(l, a, out ? *l.primerr() : *l.primout(), esc);
}
inline LISPT prin0(LISPT a, bool out = false, bool esc = false)
{
  auto& l = lisp::current();
  return io::prin0(l, a, out ? *l.primerr() : *l.primout(), esc);
}
inline LISPT print(lisp& l, LISPT a, file_t& f) { return io::print(l, a, f); }
inline LISPT print(LISPT a, file_t& f) { return io::print(lisp::current(), a, f); }
inline LISPT print(lisp& l, LISPT a, bool out = false) { return io::print(l, a, out ? *l.primerr() : *l.primout()); }
inline LISPT print(LISPT a, bool out = false)
{
  auto& l = lisp::current();
  return io::print(l, a, out ? *l.primerr() : *l.primout());
}

template<typename T>
std::string to_string(T& sink)
{
  return dynamic_cast<io::string_sink&>(sink).string();
}

/// @brief Creates a lisp expression.
inline LISPT operator"" _l(const char* s, std::size_t)
{
  auto in = ref_file_t::create(s);
  auto e = lispread(lisp::current(), in);
  return e;
}
} // namespace lisp

inline std::ostream& operator<<(std::ostream& os, const lisp::LISPT& obj)
{
  lisp::file_t out(os);
  prin0(obj, out, true);
  return os;
}

#endif
