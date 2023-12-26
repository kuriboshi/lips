# List Functions

> `(car a)` (_Function_)

```cpp
inline lisp_t car(lisp_t a)
```

The car of the cons cell.

> `(cdr a)` (_Function_)

```cpp
inline lisp_t cdr(lisp_t a)
```

The cdr of the cons cell.

> `(cadr a)` (_Function_)

```cpp
inline lisp_t cadr(lisp_t a)
```

Same as (car (cdr a))

> `(cdar a)` (_Function_)

```cpp
inline lisp_t cdar(lisp_t a)
```

Same as (cdr (car a))

> `(caar a)` (_Function_)

```cpp
inline lisp_t caar(lisp_t a)
```

Same as (car (car a))

> `(cddr a)` (_Function_)

```cpp
inline lisp_t cddr(lisp_t a)
```

Same as (cdr (cdr a))

> `(cdddr a)` (_Function_)

```cpp
inline lisp_t cdddr(lisp_t a)
```

Same as (cdr (cdr (cdr a)))

> `(caddr a)` (_Function_)

```cpp
inline lisp_t caddr(lisp_t a)
```

Same as (car (cdr (cdr a)))

> `(cdadr a)` (_Function_)

```cpp
inline lisp_t cdadr(lisp_t a)
```

Same as (cdr (car (cdr a)))

> `(caadr a)` (_Function_)

```cpp
inline lisp_t caadr(lisp_t a)
```

Same as (car (car (cdr a)))

> `(cddar a)` (_Function_)

```cpp
inline lisp_t cddar(lisp_t a)
```

Same as (cdr (cdr (car a)))

> `(cadar a)` (_Function_)

```cpp
inline lisp_t cadar(lisp_t a)
```

Same as (car (cdr (car a)))

> `(cdaar a)` (_Function_)

```cpp
inline lisp_t cdaar(lisp_t a)
```

Same as (cdr (car (car a)))

> `(caaar a)` (_Function_)

```cpp
inline lisp_t caaar(lisp_t a)
```

Same as (car (car (car a)))

> `(nconc args...)` (_NoSpread Function_)

```cpp
inline lisp_t nconc(lisp_t args)
```

Same as `append` but modifies the arguments _args_.

> `(attach a b)` (_Function_)

```cpp
inline lisp_t attach(lisp_t a, lisp_t b)
```

Destructive version of `cons` which prepends _x_ to _y_ and any
reference to _y_ will contain the modified list.

> `(null a)` (_Function_)

```cpp
inline lisp_t null(lisp_t a)
```

`t` if _x_ is `nil`, otherwise `nil`.

> `(list args...)` (_NoSpread Function_)

```cpp
inline lisp_t list(lisp_t a)
```

Create a list of the items _args_.

> `(length list)` (_Function_)

```cpp
inline lisp_t length(lisp_t list)
```

Returns the length of the list _l_.

> `(nth x n)` (_Function_)

```cpp
inline lisp_t nth(lisp_t a, lisp_t b)
```

Returns the _n_th tail of _x_.

Returns `nil` if there are fewer elements in _x_ than _n_.

```lisp
(nth '(a b c) 2)
  => (b c)
(nth '(a b c) 4)
  => nil
```

> `(rplaca x y)` (_Function_)

```cpp
inline lisp_t rplaca(lisp_t x, lisp_t y)
```

Replaces `car` of _x_ with _y_ destructively.

> `(rplacd x y)` (_Function_)

```cpp
inline lisp_t rplacd(lisp_t x, lisp_t y)
```

Replaces `cdr` of _x_ with _y_ destructively.

> `(append args...)` (_NoSpread Function_)

```cpp
inline lisp_t append(lisp_t a)
```

Append all the arguments, i.e. _x2_ is appended to _x1_, _x3_ to
_x2_, and so on.

All arguments must be lists. Any argument which is `nil` is ignored. All
lists are copied which means that `(append x)` makes a copy of _x_. All
arguments have to be lists.

```lisp
(append '(a b) '(c d) '(e f))
  => (a b c d e f)
```

> `(tconc l o)` (_Function_)

```cpp
inline lisp_t tconc(lisp_t l, lisp_t o)
```

The `car` of _l_ is a list and the `cdr` of _l_ is a pointer to the
first element of the list.

The object _o_ is added to the end of the list and the `cdr` is updated.
An empty _l_ should be `(nil)` but if _l_ is `nil` it is initialized to
`((o) o)`.  All pointers to _l_ points to the new list since the changes
are destructive.
