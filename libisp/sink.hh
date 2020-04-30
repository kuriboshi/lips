//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

namespace lisp
{
class sink
{
public:
  sink() {}
  ~sink() = default;

  virtual void putch(int, int) = 0;
};

class filesink : public sink
{
public:
  filesink(FILE* file) : _file(file) {}
  filesink(const char* filename)
  {
    _file = fopen(filename, "w");
  }

  virtual void putch(int c, int esc) override
  {
    putch(c, _file, esc);
  }

private:
  // Put a character on stdout prefixing it with a ^ if it's a control
  // character.
  void pputc(int c, FILE* file)
  {
    if(c < 0x20 && c != '\n' && c != '\t')
    {
      putc('^', file);
      putc(c + 0x40, file);
    }
    else
      putc(c, file);
  }

  /*
   * Put a character c, on stream file, escaping enabled if esc != 0.
   */
  void putch(int c, FILE* file, int esc)
  {
    if((c == '(' || c == '"' || c == ')' || c == '\\') && esc)
      pputc('\\', file);
    pputc(c, file);
  }

  FILE* _file;
};

} // namespace lisp
