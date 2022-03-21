//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#pragma once

#include <optional>
#include <sstream>
#include <fstream>
#include <string>
#include <string_view>
#include <fmt/format.h>
#include "lisp.hh"
#include "except.hh"

namespace lisp
{
// class lisp;

namespace io
{
LISPT ratom(lisp& l, file_t&);
LISPT lispread(lisp& l, file_t&);
LISPT readline(lisp& l, file_t&);
LISPT getline(lisp& l, LISPT);

LISPT patom(lisp& l, LISPT, file_t&, bool esc = false);
LISPT prinbody(lisp& l, LISPT, file_t&, bool esc = false);
LISPT prin0(lisp& l, LISPT, file_t&, bool esc = false);
LISPT print(lisp& l, LISPT, file_t&);
LISPT terpri(lisp& l, file_t&);

LISPT splice(lisp&, LISPT, LISPT, bool);
LISPT rmdquote(lisp&, file_t&, LISPT, char);
LISPT rmsquote(lisp&, file_t&, LISPT, char);
void set_read_table(lisp&);
}

class io_source
{
public:
  io_source() {}
  virtual ~io_source() = default;

  static inline constexpr char COMMENTCHAR = '#';

  virtual int getch(bool inside_string = false) = 0;
  virtual void ungetch(int) = 0;
  virtual bool eoln() = 0;
  virtual bool close() = 0;
  virtual std::optional<std::string> getline() = 0;

  using iterator = std::istreambuf_iterator<char>;
  virtual iterator begin() = 0;
  virtual iterator end() { return iterator(); }  

protected:
  int getch(std::istream& stream, bool inside_string)
  {
    auto _curc = stream.get();
    if(!inside_string && _curc == COMMENTCHAR) /* Skip comments.  */
      while((_curc = stream.get()) != '\n')
        ;
    return _curc;
  }

  bool eoln(std::istream& stream)
  {
    while(true)
    {
      if(stream.eof())
        return true;
      if(_curc != ' ' && _curc != '\t' && _curc != '\n')
        return false;
      if(_curc == '\n')
        return true;
      _curc = stream.get();
    }
    return true;
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

class file_source: public io_source
{
public:
  file_source(const std::string& filename);
  ~file_source() = default;

  using io_source::getch;
  using io_source::eoln;
  using io_source::getline;

  virtual int getch(bool inside_string) override
  {
    return getch(*_file, inside_string);
  }
  virtual void ungetch(int c) override { _file->putback(c); }
  virtual bool eoln() override
  {
    return eoln(*_file);
  }
  virtual bool close() override
  {
    _file->close();
    return !_file->is_open();
  }
  virtual std::optional<std::string> getline() override
  {
    return getline(*_file);
  }

  iterator begin() override { return iterator(*_file); }

private:
  std::unique_ptr<std::ifstream> _file;
};

class stream_source: public io_source
{
public:
  stream_source(std::istream& stream): _stream(stream) {}
  ~stream_source() = default;

  using io_source::getch;
  using io_source::eoln;
  using io_source::getline;

  virtual int getch(bool inside_string) override
  {
    return getch(_stream, inside_string);
  }
  virtual void ungetch(int c) override { _stream.putback(c); }
  virtual bool eoln() override
  {
    return eoln(_stream);
  }
  virtual bool close() override { return true; }
  virtual std::optional<std::string> getline() override
  {
    return getline(_stream);
  }

  iterator begin() override { return iterator(_stream); }

private:
  std::istream& _stream;
};

class string_source: public io_source
{
public:
  string_source(const std::string& string): _string(string) {}

  using io_source::getch;
  using io_source::eoln;
  using io_source::getline;

  virtual int getch(bool inside_string) override
  {
    return getch(_string, inside_string);
  }
  virtual void ungetch(int c) override { _string.putback(c); }
  virtual bool eoln() override { return eoln(_string); }
  virtual bool close() override { return true; }
  virtual std::optional<std::string> getline() override { return getline(_string); }

  iterator begin() override { return iterator(_string); }

private:
  std::istringstream _string;
};

class io_sink
{
public:
  io_sink() {}
  virtual ~io_sink() = default;

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

class file_sink final: public io_sink
{
public:
  file_sink(const std::string& filename, bool append = false);

  using io_sink::putch;

  virtual void putch(int c, bool esc) override { putch(c, *_file, esc); }
  virtual void puts(const std::string_view s) override { _file->write(s.data(), s.size()); }
  virtual void terpri() override { _file->put('\n'); }
  virtual void flush() override { _file->flush(); }
  virtual bool close() override
  {
    _file->close();
    return !_file->is_open();
  }

private:
  std::unique_ptr<std::ofstream> _file;
};

class stream_sink final: public io_sink
{
public:
  stream_sink(std::ostream& stream): _stream(stream) {}
  ~stream_sink() = default;

  using io_sink::putch;

  virtual void putch(int c, bool esc) override { putch(c, _stream, esc); }
  virtual void puts(const std::string_view s) override { _stream.write(s.data(), s.size()); }
  virtual void terpri() override { _stream.put('\n'); }
  virtual void flush() override { _stream.flush(); }
  virtual bool close() override { return true; }

private:
  std::ostream& _stream;
};

class string_sink final: public io_sink
{
public:
  string_sink() {}

  std::string string() const { return _string; }

  virtual void putch(int c, bool) override { _string.push_back(static_cast<char>(c)); }
  virtual void puts(const std::string_view s) override { _string.append(s); }
  virtual void terpri() override { _string.push_back('\n'); }
  virtual bool close() override { return true; }

private:
  std::string _string;
};

class file_t final
{
public:
  file_t(std::unique_ptr<io_source> source): _source(std::move(source)) {}
  file_t(std::unique_ptr<io_sink> sink): _sink(std::move(sink)) {}
  file_t(std::unique_ptr<io_source> source, std::unique_ptr<io_sink> sink): _source(std::move(source)), _sink(std::move(sink)) {}
  file_t(std::istream& stream): _source(std::make_unique<stream_source>(stream)) {}
  file_t(std::ostream& stream): _sink(std::make_unique<stream_sink>(stream)) {}
  file_t(const std::string& string): _source(std::make_unique<string_source>(string)) {}
  file_t(file_t&& file) noexcept: _source(std::move(file._source)), _sink(std::move(file._sink)) {}
  ~file_t() {}

  // io_source
  io_source& source() { return *_source.get(); }
  int getch(bool inside_string = false) { ptrcheck(_source); return _source->getch(inside_string); }
  void ungetch(int c) { ptrcheck(_source); _source->ungetch(c); }
  bool eoln() { ptrcheck(_source); return _source->eoln(); }
  std::optional<std::string> getline() { ptrcheck(_source); return _source->getline(); }

  // io_sink
  io_sink& sink() { return *_sink.get(); }
  void putch(char c, bool esc = false) { ptrcheck(_sink); _sink->putch(c, esc); }
  void puts(const std::string_view s) { ptrcheck(_sink); _sink->puts(s); }
  void terpri() { ptrcheck(_sink); _sink->terpri(); }
  void flush() { ptrcheck(_sink); _sink->flush(); }

  template<typename... Ts>
  void format(Ts&&... t)
  {
    auto ret = fmt::format(t...);
    _sink->puts(ret);
  }

  bool close()
  {
    _source.release();
    _sink.release();
    return true;
  }

private:
  std::unique_ptr<io_source> _source;
  std::unique_ptr<io_sink> _sink;

  void ptrcheck(const std::unique_ptr<io_source>&) const
  {
    if(!_source)
      throw lisp_error("file_t: No source");
  }

  void ptrcheck(const std::unique_ptr<io_sink>&) const
  {
    if(!_sink)
      throw lisp_error("file_t: No sink");
  }
};

inline bool isascii(int c) { return c >= 0 && c <= 127; }
inline bool is_charclass(lisp& l, int c, char_class cc)
{
  return isascii(c) && l.currentrt.chclass[c] == cc;
}

inline bool issepr(lisp& l, int c) { return is_charclass(l, c, char_class::SEPR); }
inline bool issepr(int c) { return issepr(lisp::current(), c); }
inline bool isbrk(lisp& l, int c) { return is_charclass(l, c, char_class::BRK); }
inline bool isbrk(int c) { return isbrk(lisp::current(), c); }
inline bool isctrl(lisp& l, int c) { return is_charclass(l, c, char_class::CTRL); }
inline bool isctrl(int c) { return isctrl(lisp::current(), c); }
inline bool isinsert(lisp& l, int c) { return is_charclass(l, c, char_class::INSERT); }
inline bool isinsert(int c) { return isinsert(lisp::current(), c); }
inline bool issplice(lisp& l, int c) { return is_charclass(l, c, char_class::SPLICE); }
inline bool issplice(int c) { return issplice(lisp::current(), c); }
inline bool isinfix(lisp& l, int c) { return is_charclass(l, c, char_class::INFIX); }
inline bool isinfix(int c) { return isinfix(lisp::current(), c); }

inline LISPT ratom(lisp& l, file_t& f) { return io::ratom(l, f); }
inline LISPT ratom(file_t& f) { return io::ratom(lisp::current(), f); }
inline LISPT lispread(lisp& l, file_t& f) { return io::lispread(l, f); }
inline LISPT lispread(file_t& f) { return io::lispread(lisp::current(), f); }
inline LISPT lispread(const std::string& s) { file_t f(s); return io::lispread(lisp::current(), f); }
inline LISPT readline(lisp& l, file_t& f) { return io::readline(l, f); }
inline LISPT readline(file_t& f) { return io::readline(lisp::current(), f); }
inline LISPT getline(lisp& l, LISPT f) { return io::getline(l, f); }
inline LISPT getline(LISPT f) { return io::getline(lisp::current(), f); }

inline LISPT patom(lisp& l, LISPT a, file_t& f, bool esc = false) { return io::patom(l, a, f, esc); }
inline LISPT patom(LISPT a, file_t& f, bool esc = false) { return io::patom(lisp::current(), a, f, esc); }
inline LISPT patom(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io::patom(l, a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT patom(LISPT a, bool out = false, bool esc = false)
{
  auto& l = lisp::current();
  return io::patom(l, a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT terpri(lisp& l, file_t& f) { return io::terpri(l, f); }
inline LISPT terpri(file_t& f) { return io::terpri(lisp::current(), f); }
inline LISPT terpri(lisp& l, bool out = false) { return io::terpri(l, out ? l.primerr() : l.primout()); }
inline LISPT terpri(bool out = false)
{
  auto& l = lisp::current();
  return io::terpri(l, out ? l.primerr() : l.primout());
}
inline LISPT prinbody(lisp& l, LISPT a, file_t& f, bool esc = false) { return io::prinbody(l, a, f, esc); }
inline LISPT prinbody(LISPT a, file_t& f, bool esc = false) { return io::prinbody(lisp::current(), a, f, esc); }
inline LISPT prinbody(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io::prinbody(l, a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT prinbody(LISPT a, bool out = false, bool esc = false)
{
  auto& l = lisp::current();
  return io::prinbody(l, a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT prin0(lisp& l, LISPT a, file_t& f, bool esc = false) { return io::prin0(l, a, f, esc); }
inline LISPT prin0(LISPT a, file_t& f, bool esc = false) { return io::prin0(lisp::current(), a, f, esc); }
inline LISPT prin0(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io::prin0(l, a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT prin0(LISPT a, bool out = false, bool esc = false)
{
  auto& l = lisp::current();
  return io::prin0(l, a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT print(lisp& l, LISPT a, file_t& f) { return io::print(l, a, f); }
inline LISPT print(LISPT a, file_t& f) { return io::print(lisp::current(), a, f); }
inline LISPT print(lisp& l, LISPT a, bool out = false) { return io::print(l, a, out ? l.primerr() : l.primout()); }
inline LISPT print(LISPT a, bool out = false)
{
  auto& l = lisp::current();
  return io::print(l, a, out ? l.primerr() : l.primout());
}

template<typename T>
std::string to_string(T& sink)
{
  return dynamic_cast<string_sink&>(sink).string();
}

} // namespace lisp

inline std::ostream& operator<<(std::ostream& os, const lisp::LISPT& obj)
{
  lisp::file_t out(os);
  prin0(obj, out, true);
  return os;
}
