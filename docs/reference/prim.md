# Primary Functions

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

> `(eq a b)` (_Function_)

```cpp
inline lisp_t eq(lisp_t a, lisp_t b)
```

`t` if the objects _a_ and _b_ are the same object. Integers are
considered 'eq' if their values are the same.

> `(atom a)` (_Function_)

```cpp
inline lisp_t atom(lisp_t a)
```

`t` if _a_ is `nil`, `t`, a symbol, an integer, or a floating point
object.

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

> `(quote a)` (_NLambda Function_)

```cpp
inline lisp_t quote(lisp_t a)
```

Returns _x_ unevaluated.

> `(lambda x . y)` (_NoSpread Function_)

```cpp
inline lisp_t lambda(lisp_t x, lisp_t y)
```

Creates a lambda object.

The parameter _x_ is the parameters of the function being defined. If it's
a list of atoms the function is a spread function, if it's a single atoms
the function is a nospread function, if it's dotted pair the function is a
half spread function.

A _spread_ function binds each formal parameter to the actual parameters
when the function is called. Any excess parameter is ignored and any
missing actual parameter is bound to `nil`.

A _nospread_ function binds the formal parameter to a list of all actual
parameters when called.

A _half spread_ function is a combination of the above where the actual
parameters are bound to each formal parameter and any excess actual
parameters are bound to the formal parameter in the symbol in the `cdr` of
the list of formal parameters.

> ```cpp
> inline lisp_t nlambda(lisp_t x, lisp_t y)
> ```

Creates an nlambda function object.
@lisp{(nlambda x . y)

Same as `lambda` except that the function object is an nlambda function
object and parameters are not evaluated when the function is called.

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

> `(closure f v)` (_Function_)

```cpp
inline lisp_t closure(lisp_t a, lisp_t b)
```

Eval function that forms the closure of function _f_, with variables
listed in _v_ statically bound.

This is close to function in other lisp dialects. Any closure that is
created within another closure and lists a variable contained in that
closure refers to the same variable. This makes it possible for two
closures to share one or more variables.

Here is an example of defining a simple function which maintains the
balance of a bank account.

```lisp
(defineq
  (make-account
   (lambda (balance)
     ((closure
          '(progn
            (setq withdraw
             (closure
                 (lambda (amount)
                   (setq balance (difference balance amount)))
                 '(balance)))
            (setq deposit
             (closure
                 (lambda (amount)
                   (setq balance (plus balance amount)))
                 '(balance)))
            (lambda ()
              (closure
                  (lambda (m)
                    (cond
                      ((eq m 'withdraw) withdraw)
                      ((eq m 'deposit) deposit)
                      (t nil)))
                  '(withdraw deposit))))
          '(balance withdraw deposit))))))
```

The function `make-account` creates and returns a closure object which
binds the three symbols on line 24 in their lexical scope. It sets the
symbols `withdraw` and `deposit` each to a closure over `balance` with a
lambda expression which subtracts or adds an `amount` to the `balance`.

> `(nth x n)` (_Function_)

```cpp
inline lisp_t nth(lisp_t a, lisp_t b)
inline lisp_t error(lisp_t a)
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
