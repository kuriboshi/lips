# Function Evaluation

Functions in this section relate to the evaluation of lisp expressions.

> ```cpp
> template<Context Context>
> class vm_t final: public vm
> ```

Templated derived class which implements the virtual functions of
vm.

The vm_t template provides access to some global symbols like the primary
input and output file, the stdin and stdout, and some other global
symbols. It also provides access to the implementation of error functions.

The Context needs to satisfy the _Context_ concept. The type
`lisp::context_t` provides a default implementation.

- _Context_ The _context_ holds the actual implementation.

> `(eval fn)` (_Function_)

```cpp
inline lisp_t eval(lisp_t expr)
```

Evaluate a lisp expression.

- _expr_ The lisp expression to evaluate.

**Returns**: Returns the result of the evaluation.

> ```cpp
> inline lisp_t eval(const std::string& expr)
> ```

Evaluate a lisp expression read from a string.

> `(apply fn lis)` (_Function_)

```cpp
inline lisp_t apply(lisp_t fn, lisp_t list)
```

Apply a function to a list of arguments.

Applies the function _fn_ to the arguments in the list _list_ as if _fn_ is
called with the list as its arguments.

```lisp
(apply car '((a b c)))
  => a
```

> `(apply* fn args...)` (_NoSpread Function_)

A nospread version of `apply`.

```lisp
(apply* car '(a b c))
  => a
```

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

> `(backtrace)` (_Function_)

```cpp
inline lisp_t backtrace()
```

Print a backtrace of the control stack.

Subject to change between any version of the interpeter.

**Returns**: `nil`

> `(topofstack)` (_Function_)

```cpp
inline lisp_t topofstack()
```

Retrieves the most recent environment.

Subject to change between any version of the interpeter.

**Returns**: An object of type environment.

> `(destblock)` (_Function_)

```cpp
inline lisp_t destblock(lisp_t a)
```

Converts an object of type environment to a list.

Subject to change between any version of the interpeter.

**Returns**: A list representing a destblock.

> `(error code)` (_Function_)

```cpp
inline lisp_t error(lisp_t code)
```

Exits with an error code.
