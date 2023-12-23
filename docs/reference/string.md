# String Functions

> `(concat args...)` (_Function_)

```cpp
inline lisp_t concat(lisp_t x)
```

Concatenate strings.

> `(strcmp s1 s2)` (_Function_)

```cpp
inline lisp_t strcmp(lisp_t s1, lisp_t s2)
```

Compare two strings.

Compares the two strings _s1_ and _s2_ lexicographically and returns a
negative number, zero, or a positive number if _s1_ is less than, equal, or
greater than _s2_.

- _s1_ A string.
- _s2_ A string.

**Returns**: A positive number of x is lexiographically greater than y, a
negative number if x is lexiographically less than y and zero if they are
equal.

> `(strequal s1 s2)` (_Function_)

```cpp
inline lisp_t strequal(lisp_t s1, lisp_t s2)
```

Compare if to strings are equal.

Compares the strings _s1_ and _s2_ and returns `t` if the strings are equal
or `nil` if they are not equal.

- _s1_ A string.
- _s2_ A string.

**Returns**: `t` if _s1_ is equal to _s2_, `nil` otherwise.

> `(stringp s)` (_Function_)

```cpp
inline lisp_t stringp(lisp_t s)
```

Check if parameter is a string.

@param s

**Returns**: Returns the string if it's a string, nil otherwise.

> `(strlen s)` (_Function_)

```cpp
inline lisp_t strlen(lisp_t s)
```

Returns the length of a string.

- _s_ A string.
**Returns**: The length of the string.

> `(substring str n m)` (_Function_)

```cpp
inline lisp_t substring(lisp_t str, lisp_t n, lisp_t m)
```

Extract a substring from start to end.

Creates a new string which is a substring of _str_ starting from the _n_th
character through the _m_th character. If _m_ is `nil` the substring starts
from the _n_th character through to the end.

Negative numbers are treated as the _n_th or _m_th character from the
end.

```lisp
(substring "hello" 2 3)
  => "el"
(substring "hello" 2 1)
  => nil
(substring "hello" 2 -2)
  => "ell"
(substring "hello" -3 -1)
  => "llo"
(substring "hello" -1 -3)
  => nil
```

@param str
@param n
@param m

**Returns**: Returns a substring.  If start or end is out of bounds, return
`nil`.  If end is one less than start the zero length string is returned.
End equal to zero if start is equal to one is accepted.

> ```cpp
> inline lisp_t symstr(lisp_t sym)
> ```

Return symbol's print name as a string.

- _sym_ A symbol.

**Returns**: Returns the print name of a symbol as a string. If sym is `nil`
then returns the string "nil".
