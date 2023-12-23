# Map Functions

Map functions iterate over a list of items and applies a function to either
each item or each tail of the list.

> `(map list fn1 fn2)` (_Function_)

```cpp
inline lisp_t map(lisp_t list, lisp_t fn1, lisp_t fn2)
```

Apply _fn1_ on each tail of _list_.

If _fn2_ is `nil`, apply the function _fn1_ on each tail of
_list_. First _fn1_ is applied to _list_, then `(cdr list)`,
and so on. If _fn2_ is not `nil`\ then _fn2_ is called instead
of `cdr` to get the next value on which to apply _fn1_.
`map` returns `nil`.

```lisp
(map '(a b c) (lambda (l) (print l)))
(a b c)
(b c)
(c)
  => nil
```

- _list_ A list of items.
- _fn1_ The function to apply on each tail of obj.
- _fn2_ Function to apply to get the next element of the list (default is CDR).

**Returns**: nil

> `(mapc list fn1 fn2)` (_Function_)

```cpp
inline lisp_t mapc(lisp_t list, lisp_t fn1, lisp_t fn2)
```

Apply _fn1_ on each _car_ of _list_.

`mapc` is the same as `map` except that _fn1_ is applied to `(car list)`
instead of the list. Effectively applying _fn1_ on each element of the
list. `mapc` returns `nil`.

- _list_ A list of items. If not a list the function is a no-op.
- _fn1_ The function to apply on each CAR of the list.
- _fn2_ Function to apply to get the next element (default is CDR).

**Returns**: nil

> `(maplist list fn1 fn2)` (_Function_)

```cpp
inline lisp_t maplist(lisp_t list, lisp_t fn1, lisp_t fn2)
```

Collect the result of applying _fn1_ on each tail of _list_.

The same as `map` but collects the results from applying _fn1_ on each tail
and returning a list of the results.

```lisp
(maplist '(a b c) (lambda (l) (length l)))
  => (3 2 1)
```

- _list_ A list of items.
- _fn1_ The function to apply on each tail of the list.
- _fn2_ Function to apply to get the next element (default is CDR).

**Returns**: A list of the result of applying FN1 on each element in the list.

> `(mapcar list fn1 fn2)` (_Function_)

```cpp
inline lisp_t mapcar(lisp_t list, lisp_t fn1, lisp_t fn2)
```

Collect the result of applying _fn1_ on each _car_ of _list_.

Equivalent to `mapc` but collects the results in a list like `maplist`.

```lisp
(mapcar '(1 2 3) (lambda (n) (plus n 1)))
  => (2 3 4)
```

- _list_ A list of items.
- _fn1_ The function to apply on each element of the list.
- _fn2_ Function to apply to get the next element (default is CDR).

**Returns**: A list of the result of applying FN1 on each element in the list.
