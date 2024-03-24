# Input and Output Functions

Functions managing files.

> `(open filename mode)` (_Function_)

```cpp
inline lisp_t open(const lisp_t& filename, const lisp_t& mode)
```

Opens a file.

- _filename_ String or symbol file name.
- _mode_ One of `read`, `write`, or `append`.

**Returns**: A file type object.

> `(close file)` (_Function_)

```cpp
inline lisp_t close(const lisp_t& file)
```

Closes a file.

- _file_ A file type object.

**Returns**: `t`.

> `(load filename)` (_Filename_)

```cpp
inline lisp_t load(const lisp_t& filename)
```

Loads lisp expressions from a file.

- _filename_ String or symbol file name.

**Returns**: The file name.

> `(prin1 expr file)` (_Function_)

```cpp
inline lisp_t prin1(const lisp_t& expr, const lisp_t& file)
```

Prints a lisp expression without escaping special characters.

The result may not be readable by `read`.

```lisp
(prin1 "hello")
hello
  => "hello"
```

- _expr_ A lisp expression to print.
- _file_ If `nil` print on primary output, if `t` print on primary
error, else `file` has to be of type _file_.

**Returns**: The expression.

> `(prin2 args...)` (_Function_)

```cpp
inline lisp_t prin2(const lisp_t& expr, const lisp_t& file)
```

Prints a lisp expression escaping special characters such as double
quotes.

Prints a lisp expression in a way that can be read back.

```lisp
(prin2 "hello")
"hello" => "hello"
```

- _expr_ A lisp expression to print.
- _file_ If `nil` print on primary output, if `t` print on primary
error, else `file` has to be of type _file_.

**Returns**: The expression.

> `(print args...)` (_Function_)

```cpp
inline lisp_t print(const lisp_t& expr, const lisp_t& file)
```

Prints a lisp expression escaping special characters and outputing a
newline afterwards.

Prints a lisp expression escaping special characters and ending with a
newline.

```lisp
(print "hello")
"hello"
  => "hello"
```

- _expr_ A lisp expression to print.
- _file_ If `nil` print on primary output, if `t` print on primary
error, else `file` has to be of type _file_.

**Returns**: The expression.

> `(spaces n file)` (_Function_)

```cpp
inline lisp_t spaces(const lisp_t& n, const lisp_t& file)
```

Prints _n_ number of spaces.

- _file_ If `nil` print on primary output, if `t` print on primary
error, else `file` has to be an open file.
- _n_ The number of spaces to print.

**Returns**: `nil`.

> `(terpri file)` (_Function_)

```cpp
inline lisp_t terpri(const lisp_t& file)
```

Print a newline on the output file.

- _file_ If `nil` print on primary output, if `t` print on primary
error, else `file` has to be an open file.

**Returns**: `nil`.

> `(printlevel level)` (_Function_)

```cpp
inline lisp_t printlevel(const lisp_t& level)
```

Sets the print level.

The print level determines how deep the printing of a lisp expression will
go. Deep lisp expressions will be replaced by an ampersand (&). If the
`level` argument is left out or `nil` the current print level is returned.

```lisp
(print '(a (b (c (d]
(a (b (c (d))))
  => (a (b (c (d))))
(printlevel 2)
  => 0
(print '(a (b (c (d]
(a (b &))
  => (a (b &))
```

- _level_ The depth to which S-expressions are printed.

**Returns**: The previous level.

> `(ratom file)` (_Function_)

```cpp
inline lisp_t ratom(const lisp_t& file)
```

Reads one token from the file and creates a lisp object from that
token.

- _file_ An open file or if `nil` read from primary output, if `t` read
from stdin.

**Returns**: A lisp object which is either an integer, float, symbol, or
string. This differs from Interlisp which will never return a
string. Instead the first double quote is returned as a symbol.

> `(read file)` (_Function_)

```cpp
inline lisp_t read(const lisp_t& file)
```

Reads a lisp expression from an open file.

- _file_ An open file or if `nil` read from primary input, if `t` read
from stdin.

**Returns**: A lisp expression or the symbol `eof` on end of file.

> `(readc file)` (_Function_)

```cpp
inline lisp_t readc(const lisp_t& file)
```

Reads a single character from an open file.

- _file_ An open file or if `nil` read from primary input, if `t` read
from stdin.

**Returns**: The character read as an integer value.

> `(readline file)` (_Filename_)

```cpp
inline lisp_t readline(const lisp_t& file)
```

Reads characters from an open file until the next newline.

The line is split according to the defined break characters and the result
is returned as a string. If a blank line is read the symbol `eof` is
returned.

```text
hello world => (hello world)
"hello world" => ("hello world")
(hello world) => (hello world)

```

- _file_ An open file or if `nil` read from primary output, if `t` read
from stdin.

**Returns**: A lisp expression. The symbol `eof` is returned if a blank line is
read.

> `(splice x y tailp)` (_Function_)

```cpp
inline lisp_t splice(const lisp_t& x, const lisp_t& y, const lisp_t& tailp)
```

Splice an object into a list.

Splices list y into x keeping cdr of x. For example:

```lisp
(let ((x '(a b c))
      (y '(x y z)))
 (splice x y)
 x)
```

Modifies x to hold the value (x y z b c).

Another example:

```lisp
(let ((x '(a b c))
      (y '(x y z)))
 (splice (cdr x) y)
 x)
```

Modifies x to hold the value (a x y z c).

If y is not a list put it in car of x and return x, otherwise return last
cell of y with cdr set to original (cdr x). If tailp is `t`, don't clobber
car of x.

> ```cpp
> bool loadfile(const std::string& filename);
> ```

Loads a file from _filename_.

Read S-expressions from the file until end of file it reached.

- _filename_ A file name.

**Returns**: True if the file was loaded successfully, false otherwise.

> ```cpp
> inline lisp::lisp_t operator"" _l(const char* s, std::size_t)
> ```

Creates a lisp expression from a string.

> ```cpp
> inline std::ostream& operator<<(std::ostream& os, const lisp::lisp_t& obj)
> ```

Outputs a lisp expression to a regular C++ output stream.
