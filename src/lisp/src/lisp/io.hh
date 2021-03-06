//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <optional>
#include <sstream>
#include <fstream>
#include <string>
#include <string_view>
#include <fmt/format.h>
#include "lisp.hh"
#include "base.hh"
#include "except.hh"

namespace lisp
{
class lisp;

class io: public base
{
public:
  io(): base() {}
  io(lisp& lisp): base(lisp) {}
  ~io() = default;
  static void init(lisp&);

  void pushr(LISPT w) { l.rstack = cons(l, w, l.rstack); }
  void popr(LISPT& w)
  {
    w = l.rstack->car();
    l.rstack = l.rstack->cdr();
  }

  LISPT ratom(file_t&);
  LISPT lispread(file_t&, bool line = false);
  LISPT readline(file_t&);

  LISPT patom(LISPT, file_t&, bool esc = false);
  LISPT prinbody(LISPT, file_t&, bool esc = false);
  LISPT prin0(LISPT, file_t&, bool esc = false);
  LISPT print(LISPT, file_t&);
  LISPT terpri(file_t&);

  static LISPT rmexcl(lisp&, file_t&, LISPT, char);
  static LISPT rmdquote(lisp&, file_t&, LISPT, char);
  static LISPT rmsquote(lisp&, file_t&, LISPT, char);
  static LISPT rmpipe(lisp&, file_t&, LISPT, char);
  static LISPT rmredir(lisp&, file_t&, LISPT, char);
  static LISPT rmbg(lisp&, file_t&, LISPT, char);
  static LISPT rmuser(lisp&, file_t&, LISPT, char);

#if 0
  static LISPT userreadmacros[128];
#endif

private:
  LISPT splice(LISPT, LISPT, bool);
  LISPT parsebuf(const std::string&);
  bool integerp(const std::string&, int& res);
  bool floatp(const std::string&);
  bool checkeof(lisp& l, int c, bool line);
  std::pair<bool, int> getchar(lisp& l, file_t& file, bool line);
};

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

class file_sink: public io_sink
{
public:
  file_sink(const std::string& filename, bool append = false)
    : _file(std::make_unique<std::ofstream>(filename, append ? std::ios_base::ate : std::ios_base::out))
  {}
  ~file_sink() = default;

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

class stream_sink: public io_sink
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

class string_sink: public io_sink
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

inline LISPT ratom(lisp& l, file_t& f) { return io(l).ratom(f); }
inline LISPT ratom(file_t& f) { return io().ratom(f); }
inline LISPT lispread(lisp& l, file_t& f, bool esc = false) { return io(l).lispread(f, esc); }
inline LISPT lispread(file_t& f, bool esc = false) { return io().lispread(f, esc); }
inline LISPT lispread(const std::string& s, bool esc = false) { file_t f(s); return io().lispread(f, esc); }
inline LISPT readline(lisp& l, file_t& f) { return io(l).readline(f); }
inline LISPT readline(file_t& f) { return io().readline(f); }

inline LISPT patom(lisp& l, LISPT a, file_t& f, bool esc = false) { return io(l).patom(a, f, esc); }
inline LISPT patom(LISPT a, file_t& f, bool esc = false) { return io().patom(a, f, esc); }
inline LISPT patom(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io(l).patom(a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT patom(LISPT a, bool out = false, bool esc = false) { return patom(lisp::current(), a, out, esc); }
inline LISPT terpri(lisp& l, file_t& f) { return io(l).terpri(f); }
inline LISPT terpri(file_t& f) { return io().terpri(f); }
inline LISPT terpri(lisp& l, bool out = false) { return io(l).terpri(out ? l.primerr() : l.primout()); }
inline LISPT terpri(bool out = false) { return terpri(lisp::current(), out); }
inline LISPT prinbody(lisp& l, LISPT a, file_t& f, bool esc = false) { return io(l).prinbody(a, f, esc); }
inline LISPT prinbody(LISPT a, file_t& f, bool esc = false) { return io().prinbody(a, f, esc); }
inline LISPT prinbody(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io(l).prinbody(a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT prinbody(LISPT a, bool out = false, bool esc = false) { return prinbody(lisp::current(), a, out, esc); }
inline LISPT prin0(lisp& l, LISPT a, file_t& f, bool esc = false) { return io(l).prin0(a, f, esc); }
inline LISPT prin0(LISPT a, file_t& f, bool esc = false) { return io().prin0(a, f, esc); }
inline LISPT prin0(lisp& l, LISPT a, bool out = false, bool esc = false)
{
  return io(l).prin0(a, out ? l.primerr() : l.primout(), esc);
}
inline LISPT prin0(LISPT a, bool out = false, bool esc = false) { return prin0(lisp::current(), a, out, esc); }
inline LISPT print(lisp& l, LISPT a, file_t& f) { return io(l).print(a, f); }
inline LISPT print(LISPT a, file_t& f) { return io().print(a, f); }
inline LISPT print(lisp& l, LISPT a, bool out = false) { return io(l).print(a, out ? l.primerr() : l.primout()); }
inline LISPT print(LISPT a, bool out = false) { return print(lisp::current(), a, out); }
inline file_t& primout() { return lisp::current().primout(); }
inline file_t& primin() { return lisp::current().primin(); }
inline file_t& primerr() { return lisp::current().primerr(); }

template<typename T>
std::string to_string(T& sink)
{
  return dynamic_cast<string_sink&>(sink).string();
}

} // namespace lisp
