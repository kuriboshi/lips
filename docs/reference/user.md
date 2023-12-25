# User Functions

> `(define name lambda)` (_Function_)

```cpp
inline lisp_t define(lisp_t name, lisp_t lambda)
```

Defines a function.

The arguments are evaluated. Because the lambda and nlambda expressions are
functions the second argument should not be quoted. It's more common to use
the defineq function.

In lisp it would look like this:

```lisp
(define 'add2 (lambda (a) (plus a 2)))
  => add2
```

- _name_ Name of the function.
- _lambda_ A lambda or nlambda expression.

**Returns**: The name of the function defined.

> `(defineq (name lambda)...)` (_NoSpread Function_)

```cpp
inline lisp_t defineq(lisp_t list)
```

Defines one or more functions.

The argument is a list of the form

```lisp
(defineq
  (add2
   (lambda (a) (plus a 2)))
  (sub2
   (lambda (a) (difference a 2))))
  => (add2 sub2)
```

- _list_ A list of lists of two elements, a name and a lambda or nlambda
expression. The lambda expression is evaluated to yield a lambda object.

**Returns**: List of function names defined.

> `(getrep lambda)` (_Function_)

```cpp
inline lisp_t getrep(lisp_t lambda)
```

Get the function representation of a lambda object.

- _lambda_ A lambda object.

**Returns**: The representation of a lambda function which if evaluated will
yield a lambda object.  If the parameter is not of the type lambda or if
it's empty then the return value is nil.
