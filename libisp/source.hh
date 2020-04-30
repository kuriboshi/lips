//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

namespace lisp
{
inline constexpr char COMMENTCHAR = '#';

class source
{
public:
  source() {}
  ~source() = default;

  virtual int getch() = 0;
  virtual void ungetch(int) = 0;
  virtual bool eoln() = 0;
};

class filesource: public source
{
public:
  filesource(const char* filename)
  {
    _file = fopen(filename, "r");
  }

  virtual int getch() override
  {
    auto c = getc(_file);
    if(c == COMMENTCHAR) /* Skip comments.  */
      while((c = getc(_file)) != '\n')
        ;
    return c;
  }
  virtual void ungetch(int c) override
  {
    ungetc(c, _file);
  }
  virtual bool eoln() override
  {
    return false;
  }

private:
  FILE* _file;
};

class stringsource : public source
{
public:
  stringsource(const char* string) : _string(string), _len(strlen(string)) {}

  virtual int getch() override
  {
    if(_pos == _len)
      return -1;
    auto c = _string[_pos++];
    if(c == COMMENTCHAR) /* Skip comments.  */
      while((c = _string[_pos++]) != '\n')
        ;
    return c;
  }
  virtual void ungetch(int c) override
  {
    --_pos;
  }
  virtual bool eoln() override
  {
    return false;
  }

private:
  const char* _string;
  int _pos = 0;
  int _len = 0;
};

} // namespace lisp
