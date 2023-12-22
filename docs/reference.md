# `Lips` Reference Manual

Version 3.0.0, December, 2023

> Copyright (c) 1988-1989, 1992, 2020-2023 Krister Joas
>
> Licensed under the Apache License, Version 2.0 (the "License"); you
> may not use this file except in compliance with the License.  You may
> obtain a copy of the License at
>
> [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
> implied.  See the License for the specific language governing
> permissions and limitations under the License.

## Introduction

`lips` is an embeddable lisp interpreter written in C++-20.  `lips` is
inspired by _Interlisp_ and functions are usually named as they are
named in _Interlisp_.  Functions also tend to behave in the same way
as they do in _Interlisp_.  There are exceptions and `lips` is not a
faithful implementation of _Interlisp_ by any stretch.

`lips` can be used as a stand alone lisp interpreter or as a library
linked with other applications.  Two such applications are includes as
examples.  One is a simple program containing a read-eval-print loop
which, after reading and evaluating the expressions in any file given
as arguments, prints a prompt, reads expressions, evaluates them, and
prints the result.

`lips` also includes a proof of concept implementation of a command
shell.  The shell adds functions for program composition with pipes
and input/output redirection.  Read macros are used to transform the
input to make the input syntax resemble the standard shells.

This document is a reference manual for `lips`.  See the section
_Functions_ for a complete list of lisp functions provided.

## Lips Input Syntax

The types of objects recognized by the lisp reader are integers,
floating point numbers, literal atoms, lists, and strings.  All
objects except lists and dotted pairs are called atoms.

Strings start and end with a double quote `"`.  To enter a double
quote in a string precede it with a backslash `\`.  For instance,
`"Foo bar"` is a string and so is `"I like \"lips\""` which contains
two double quotes.  Newlines may be embedded inside strings without
escaping them.

Lists start with a left parenthesis followed by zero or more lisp
expressions separated by separators (see below).  The list ends with a
matching right parenthesis.  Separators breaks the atoms, and by
default these are blanks, tabs, and newlines.  There is also a class
of characters called break characters.  These characters breaks
literal atoms and there is no need to add separators around them.
Break characters are also literal atoms.  The default break characters
are: `(`, `)`, `&`, `<`, `>` and `|`.  Thus,

```lisp
foo
(foo)
(foo fie fum)
(foo (fie (x y (z))) (fum))
```

are all valid lisp expressions.  The nesting of parenthesis may have
arbitrary depth.

`lips` supports super parenthesis. Super parenthesis are square
brackets and when an opening square bracket is closed by a matching
closing square bracket any missing right round parenthesis is added. A
final closing square bracket closes any open round parenthesis and
finishes the expression. Here are some examples:

```lisp
(cond [(null nil) "hello"]
      (t "world"))
  => "hello"

(cons 'a (cons 'b (cons 'c]
  => (a b c)
```

The elements of lists are stored in the car part of the cons cells,
and are then linked together by the cdr's.  The last cdr is always
`nil`.  It's possible to enter so called dotted pairs in `lips`.  A
dotted pair is a cons cell with two expressions with no restrictions
in the car and cdr of the cell.  Dotted pairs are entered starting
with a left parenthesis, a `lips` expression, followed by a dot, `.`,
another expression, and terminated with a right parenthesis.
`(a . b)` is an example of a dotted pair. Note that the blanks
around the dot are necessary, the dot is not a break character.  The
dot is recognized in this special way only if it occurs as the second
element from the end of a list.  In other cases it is treated as an
ordinary atom.  The list `(. . . .)` is a list with three
elements ending in a dotted pair.

A list is just a special case of dotted pairs.  `(a . (b . nil))` is
equivalent to `(a b)`.  The second format is just a convenience since
lists are so common.

Integers consist of a sequence of digits.  No check for overflow is
made.  Floats are a bit complicated, but in most cases an atom that
looks like float is a float.  A float is a sequence of digits that
have either a decimal point and/or an exponentiation character, `e`,
inside.  At most one decimal point is allowed.  If the exponentiation
character is given it must be followed by at least one digit.  If
these rules are not followed the atom will be interpreted as a literal
atom.

Examples:

|atom   |type          |
|-------|--------------|
|`foo`  |literal atom  |
|`"fie"`|string        |
|`123`  |integer       |
|`12a`  |literal atom  |
|`1.0`  |floating point|
|`1e5`  |floatint point|
|`1e`   |literal atom  |
|`.e4`  |literal atom  |
|`1.4.` |literal atom  |

Comments are allowed in `lips` files.  In order to allow for the
_Unix_ "shebang" interpreter directive a `#` character in the first
column of a line is recognized as a comment that ends with a
newline. A `#` in any other colmn is treated as a regular character.
Comments may also start with the semicolon character and the comment
again continues until the end of the line.

## The Evaluation Process

The `lips` interpreter has _dynamic scope_ and _shallow binding_.
_Dynamic scope_ means that free variables are looked up in the call
stack. For example:

```lisp
((lambda (a)
 ((lambda (b) (plus a b)) 1)) 2)
  => 3
```

_Lexical scope_ can be simulated using the `closure` function (see
section _Closure_) which creates a lexical scope for variables.

_Shallow binding_ means that every time a symbol is bound to a value
the previous value is pushed onto a stack.  The current value of an
atom is stored in the _value cell_ and can be retrieved immediately.
The opposite of _shallow binding_, _deep binding_, in contrast has to
traverse the stack to find the current value of an atom.

In `lips` there is no top level value.  `lips` is a Lisp-1 dialect and
consequently there is no _function definition cell_, only a _value
cell_.  Defining a function and binding it to a symbol simply sets the
value cell for that symbol.  Symbols are case sensitive and all system
symbols are in lower case.  `lips` does not use a stop and sweep
garbage collector.  Instead objects are reference counted and freed
when the reference count goes down to zero.

If the expression to be evaluated is a list the first element (the
_car_) in the list is evaluated and then applied to the arguments.  The
car of the expression is reevalutated until either a proper functional
form is recognized or if it's evaluated to an illegal functional form,
in which case an error is signalled.

If the functional form is an unbound atom, `lips` looks for the
property `autoload` on the property list of the atom.  If it is
found, and the value is a symbol or a string, the file with that name
is loaded (if possible).  The interpreter then checks if the atom is
no longer unbound, in wich case evaluation continues.

Note that the arguments for the command are never evaluated.  If you
want them evaluated you must use the function `apply`.

## Variables

What follows is a list of all user accessable variables that in some
way guides the behavior of `lips`.

- `histmax` Controls the number of commands to save on the history
  list.
- `verbose` If `verbose` is non-`nil`, `lips` prints a messages
  whenever a function is redefined or a variable is reset to a new
  value.
- `path` The `path` variable contains a list of directories to be
  searched for executable programs.
- `home` This variable contains the home directory of the user.
- `history` In the `history` variable the history list is built.  This
  is for internal use and proper functioning of `lips` is not
  garantueed if history is set to funny values.
- `histnum` The variable `histnum` contains the number on the current
  `lips` interaction.  The same warnings apply to histnum as to
  history.
- `prompt` This variable contains the string that is printed as the
  prompt.  An exclamation mark is replaced with the current history
  number from `histnum`.  The default prompt is `!_`.
- `brkprompt` Same as `prompt` but controls prompting in a break.  The
  default is `!:`.
- `promptform` This variable is evaluated before the prompt is
  printed.  If an error occurs during evaluation of promptform it is
  reset to the default prompt.

## Using `lips` as an Embedded Lisp Interpreter

### Basic Usage

The most basic types in `lips` are the following.

- `lisp::vm` The lisp interpreter, or virtual machine. There
    may be only one lisp interpreter in the same program.
- `lisp::context_t` The context contains some globally
    accessable values mostly related to I/O such as the current
    primary output or primary error. Currently there can be only one
    context per program.
- `lisp_t` This is a basic type which can contain a value
    of several fundamental types. `lisp_t` is a pointer to a
    reference counted object of type `object`.

The `object` type can contain values of the following types.

- `Nil` This is the value `nil` which also happens to be the
    `nullptr` value of `lisp_t`.
- `Symbol` A literal atom.
- `Integer` An integer number. This is a 64 bit integer.
- `Float` A double floating point number.
- `Indirect` Used in a closure.
- `Cons` A cons cell consisting of two `lisp_t` values.
- `String` A string.
- `Subr` A compiled (C++) function. A compiled
    function may be spread or no spread and it may evaluate its
    arguments or not (fsubr). A compiled function may have zero to
    three arguments.
- `Lambda` A lambda function. As with a compiled function a
    lambda may be a spread or a no spread function and it may evaluate
    or not evaluates its arguments (nlambda).
- `Closure` A static binding.
- `Environ` Internal environment stack type.
- `File` A file object.
- `Cvariable` A C++ variable which can be accessed
    and changed from both C++ and from a lisp expression.

To check the type you can use the `type_of` function which
returns a class enum of type `lisp::object::type`.

If you have a value of type `object` (always indirectly via a
`lisp_t` pointer) you can get the actual value using various
getter functions and set the value with setter functions. There is no
type checking when in the getters so calling a getter with the wrong
type results in an exception.

- `auto as_symbol() const -> symbol::ref_symbol_t` The literal atom.
- `auto value() const -> lisp_t` Returns the value of a literal atom.
- `void value(lisp_t)` Set the value of a literal atom.
- `auto as_integer() const -> integer_t::value_type` Returns the 64
  bit integer value.
- `auto as_double() const -> double_t::value_type` Returns the
  floating point value (double).
- `auto indirect() const -> lisp_t` Returns the real value of the
  indirect value.
- `auto cons() const -> const cons_t&` Returns the cons cell.
- `auto car() const -> lisp_t` Returns the `car` of the cons cell.
- `void car(lisp_t)` Sets the `car` of the cons cell.
- `auto cdr() -> lisp_t` Returns the `cdr` of the cons cell.
- `void cdr(lisp_t)` Sets the `cdr` of the cons cell.

There are some literals which simplify creating lisp objects.
`operator""_s` creates a string, `operator""_a` creates a literal
atom, `operator""_l` creates a number or a floating point value
depending on the argument type. Finally the `operator""_e` evaluates a
string as a lisp expression and returns the result of the evaluation.

```c++
#include <lisp/lisp.hh>

int main()
{
  // Create the context.
  auto ctx = std::make_unique<lisp::context_t>();
  // Create the lisp interpreter.
  lisp::vm_t vm(std::move(ctx));
  // The _a suffix creates a lisp symbol by parsing
  // the string.
  lisp::print(lisp::cons("a"_a, "b"_a)); // => (a . b)
}
```

### The REPL

`lisp::repl` is an object which has an `lisp_t operator()(lisp_t)`
which implements a simple read, eval, and print loop.  The
`lisp::vm_t` object has a member variable called `repl` which is a
function taking a `lisp_t` type object and returns a `lisp_t` type
object.

Typical usage.

```c++
auto ctx = std::make_unique<lisp::context_t>();
lisp::vm vm(std::move(ctx));
lisp::repl repl(vm);
lisp.repl = [&repl](lisp::lisp_t) -> lisp::lisp_t
  {
    return repl(lisp::nil);
  };
lisp.repl(lisp::nil);
```

If the evaluation of an expression results in a break condition,
i.e. the evaluation cannot continue due to an error, then the
`lisp::vm::repl` function is called recursively. The
`lisp::repl` class recognizes some simple commands which allows
the state to be examined or evaluation to continue, possibly after
some changes to the environment or the program.

### Input/Output

Input and output is handled with base classes called `source` and
`sink`. A `source` needs to implement the following pure virtual
functions.

- `int` `getch()` Read one character from the input source.
- `void` `ungetch(int)` Put a character back on the input
    stream to be read next time `getch` is called.
- `bool` `close()` Close the input source.
- `std::optional<std::string>` `getline()` Read one line
    from the input source. Returns an empty optional at end of file.
- `iterator` `begin()` Returns an iterator which when
    incremented reads the next character from the source.

A `sink` needs to override the following pure virtual functions.

- `void` `putch(int, bool)` Puts one character on
    the output stream. If the second bool parameter is `true` then
    characters are quoted with a backslash if non-printable.
- `void` `puts(const std::string_view)` Puts a
    string on the output stream.
- `void` `terpri()` Prints a newline.
- `void` `flush()` Flushes the sink.
- `bool` `close()` Closes the sink.

Several sources and sinks are predefined.

- `file_source` The source is an existing file. The constructor
    takes a file name as its argument.
- `stream_source` The stream source takes a `std::istream`
    as its argument.
- `string_source` The string sources takes a `std::string`
    as its argument.
- `file_sink` Takes a file name as its first argument. The
    second argument is a `bool` where `true` means append
    mode.
- `stream_sink` Accepts a `std::ostream` as its argument.
- `string_sink` Takes no argument. The data written to the
    string can be retrieved by calling the
    `string_sink::string()` member function.

### Sharing Variables Between `lips` and C++

The type `cvariable_t` is a class which enables sharing a
variable of type `lisp_t` between lisp and C++.

### Adding New Primitives

It's possible to create new primitive functions. Functions are
registered with the lisp interpreter and are callable from a lisp
program. Registering a new primitive is done using the `lisp::mkprim`
function. `lisp::mkprim` takes four arguments. The first is a string
which is the literal atom to which the function is bound. The second
argument is a C++ function which takes a lisp interpreter and zero or
more `lisp_t` values and returns a `lisp_t` value. It can be a lambda
function or a function pointer. Arguments may be of type `const
lisp_t&`.  The third and fourth argument specifies if the function
should evaluate it's argument (`subr_t::subr::EVAL`) or not
(`subr_t::subr::NOEVAL`) and if the function is a spread
(`subr_t::spread::SPREAD`) a no spread (`subr_t::spread::NOSPREAD`)
function.

Here is an example of defining a function called `printall` which
takes any number of arguments and prints them.

```c++
  mkprim(
    "printall",
    [&result](lisp_t a) -> lisp_t {
      for(auto p: a)
      {
        print(p);
      }
      terpri();
      return NIL;
    },
    subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
```

## Lisp Functions

This section describes functions available when programming in lisp.
Functions are shown in their lisp forms.  In C++ the return type of
each function is `lisp_t`.  Every function has an optional first
argument which is the lisp interpreter.  If left out the currently
active lisp interpreter is used.  For spread functions the number of
arguments of type `lisp_t` are the same as their lisp counterpart.
For nospread functions the C++ function has only one argument which
should be a list.  There is no distinction between lambda and nlambda
functions in C++ as the normal C++ evaluation rules apply.

### Arithmetic Functions

`lips` supports both integer and floating point numbers. There are
functions specific for either integers or floating points as well as
generic functions which can take either type.

> `(abs n)` (_Function_)

The absolute value of _n_.

> `(add1 n)` (_Function_)

Add `1` to $n$ and return the value.

> `(difference x y)` (_Function_)

Calculates the difference between _x_ and _y_.

```lisp
(difference 8 3)
  => 5
```

> `(divide x y)` (_Function_)

Divides $x$ by $y$. The result may be an integer or a floating point
depending on the types of $x$ and $y$. If both are integers the
result will be an integer and if either $x$ or $y$, or both, is a
floating point number the result will be a floating point number.

> `(fdifference x y)` (_Function_)

Floating point difference between _x_ and _y_.

> `(fdivide x y)` (_Function_)

Floating point division of _x_ by _y_.

> `(fplus args)` (_NoSpread Function_)

Floating point addition of _args_.

> `(ftimes args)` (_NoSpread Function_)

Multiplies the floating point values _args_.

> `(idifference x y)` (_Function_)

Integer difference between _x_ and _y_.

> `(iminus x)` (_Function_)

Same as `(idifference 0 x)`.

> `(iplus args)` (_NoSpread Function_)

Adds the values _args_.

> `(iquotient x y)` (_Function_)

The quotient of the integers _x_ and _y_.

> `(iremainder x y)` (_Function_)

The integer remainder of _x_ and _y_.

> `(itimes args)` (_NoSpread Function_)

Multiplies the integer values _args_.

> `(itof x)` (_Function_)

Convert an integer _x_ to a value of floating point type.

> `(minus x)` (_Function_)

Returns the equivalent of `(difference 0 x)`.

> `(minusp x)` (_Function_)

Returns `t` if _x_ is less than zero, `nil` otherwise.

> `(numberp x)` (_Function_)

Returns _x_ if _x_ is either an integer or a floating point number,
`nil` otherwise.

> `(plus args)` (_NoSpread Function_)

Sum up the values _args_.

> `(sub1 x)` (_Function_)

Returns `(plus x 1)`.

> `(times args)` (_NoSpread Function_)

Multiplies the numbers _args_.

### Setting Variables

Functions for setting the value cell of an atom.

> `(set a e)` (_Function_)

Sets the value cell of _a_ to the value of the expression _e_.

> `(setq a e)` (_NLambda Function_)

Same as `set` but doesn't evaluate the first argument _a_.

> `(setqq a e)` (_NLambda Function_)

Same as `set` but neither _a_ nor _e_ are evaluated.

### Logic Functions

All logic functions return `nil` if the function evaluates to false
and non-`nil` otherwise.

> `(and args)` (_NoSpread NLambda Function_)

Evaluates each argument _args_ in sequence and returns the value of
the last expression if all arguments evaluates to non-`nil`. As soon
as an argument which evaluates to `nil` is encountered, `nil` is
returned. Thus the function short circuits the arguments.

> `(not x)` (_Function_)

Returns `nil` if _x_ is non-`nil` and `t` otherwise.

> `(or args)` (_NoSpread NLambda Function_)

Evaluates each argument _args_ in sequence and returns `nil` if all
arguments evaluates to `nil`. As soon as an argument is evaluated to a
non-`nil` value that value is returned.

### String Functions

> `(concat args)` (_NoSpread Function_)

Concatenate the strings _args_ Strings are copied.

> `(strcmp s1 s2)` (_Function_)

Compares the two strings _s1_ and _s2_ lexicographically and returns a
negative number, zero, or a positive number if _s1_ is less than,
equal, or greater than _s2_.

> `(strequal s1 s2)` (_Function_)

Compares the strings _s1_ and _s2_ and returns `t` if the strings are
equal or `nil` if they are not equal.

> `(stringp s)` (_Function_)

Returns `t` if _s_ is a string, `nil` otherwise.

> `(strlen s)` (_Function_)

Returns the length of the string _s_.

> `(substring x n m)` (_Function_)

Creates a new string which is a substring of _x_ starting from the
_n_th character through the _m_th character. If _m_ is `nil` the
substring starts from the _n_th character through to the end.

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

> `(symstr a)` (_Function_)

Returns a symbol's print string.

### List Functions

> `(append args)` (_NoSpread Function_)

Append all the arguments, i.e. _x2_ is appended to _x1_, _x3_ to _x2_,
and so on. All arguments must be lists. Any argument which is `nil`\
is ignored. All lists are copied which means that `(append x)` makes a
copy of _x_. All arguments have to be lists.

```lisp
(append '(a b) '(c d) '(e f))
  => (a b c d e f)
```

> `(attach x y)` (_Function_)

Destructive version of `cons` which prepends _x_ to _y_ and any
reference to _y_ will contain the modified list.

> `(car x)` (_Function_)

Returns the value stored in the head of the cons cell. If _x_ is
`nil` it returns `nil`.

> `(cdr x)` (_Function_)

Returns the value stored in the tail of the cons cell. If _x_ is
`nil` it returns `nil`.

> `(caaar x)` (_Function_)

Same as `(car (car (car x)))`.

> `(caadr x)` (_Function_)

Same as `(car (car (cdr x)))`.

> `(caar x)` (_Function_)

Same as `(car (car x))`.

> `(cadar x)` (_Function_)

Same as `(car (cdr (car x)))`.

> `(caddr x)` (_Function_)

Same as `(car (cdr (cdr x)))`.

> `(cadr x)` (_Function_)

Same as `(car (cdr x))`.

> `(cdaar x)` (_Function_)

Same as `(cdr (car (car x)))`.

> `(cdadr x)` (_Function_)

Same as `(cdr (car (cdr x)))`.

> `(cdar x)` (_Function_)

Same as `(cdr (car x))`.

> `(cddar x)` (_Function_)

Same as `(cdr (cdr (car x)))`.

> `(cdddr x)` (_Function_)

Same as `(cdr (cdr (cdr x)))`.

> `(cddr x)` (_Function_)

Same as `(cdr (cdr x))`.

> `(cons a b)` (_Function_)

Create a `cons` cell and populate the head and the tail with
the values _a_ and _b_. If _b_ is left out the tail will be `nil`. If
both _a_ and _b_ are left out then both the head and the tail will
be `nil`.

```lisp
(cons 'a 'b)
  => (a . b)
(cons 'a '(b))
  => (a b)
(cons)
  => (nil)
```

> `(length l)` (_Function_)

Returns the length of the list _l_.

> `(list args)` (_NoSpread Function_)

Create a list of the items _args_.

> `(nconc args)` (_NoSpread Function_)

Same as `append` but modifies the arguments _args_.

> `(nth x n)` (_Function_)

Returns the _n_th tail of _x_. Returns `nil` if there are fewer
elements in _x_ than _n_.

```lisp
(nth '(a b c) 2)
  => (b c)
(nth '(a b c) 4)
  => nil
```

> `(memb x y)` (_Function_)

Looks for an element _x_ in _y_ using `eq`, returning the tail with
that element at the head. Returns `nil` if not found.

```lisp
(memb 'a '(a b c))
  => (a b c)
(memb 'b '(a b c))
  => (b c)
(memb 'd '(a b c))
  => nil
```

> `(rplaca x y)` (_Function_)

Replaces `car` of _x_ with _y_ destructively.

> `(rplacd x y)` (_Function_)

Replaces `cdr` of _x_ with _y_ destructively.

> `(tconc l o)` (_Function_)

The `car` of _l_ is a list and the `cdr` of _l_ is a pointer to the
first element of the list.  The object _o_ is added to the end of the
list and the `cdr` is updated.  An empty _l_ should be `(nil)` but if
_l_ is `nil` it is initialized to `((o) o)`.  All pointers to _l_
points to the new list since the changes are destructive.

### Functions to Function and Evaluate Functions

> `(apply fn l)` (_Function_)

Applies the function _fn_ to the arguments in the list _l_ as if _fn_
is called with the list as its arguments.

```lisp
(apply car '((a b c)))
  => a
```

> `(apply* fn args)` (_NoSpread Function_)

A nospread version of `apply`.

```lisp
(apply* car '(a b c))
  => a
```

> `(closure f v)` (_Function_)

Eval function that forms the closure of function _f_, with variables
listed in _v_ statically bound.  This is close to function in other
lisp dialects. Any closure that is created within another closure and
lists a variable contained in that closure refers to the same
variable. This makes it possible for two closures to share one or more
variables.

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
symbols `withdraw` and `deposit` each to a closure over `balance` with
a lambda expression which subtracts or adds an `amount` to the
`balance`.

> `(define x)` (_Function_)

Defines functions according to _x_. Each element of the list _x_
is a list of the form `(name args . body)`. The list is
evaluated.

> `(defineq args)` (_NLambda NoSpread Function_)

Defines functions according to the list _x_. Each element of the list
_x_ is of the form `(name def)` where name is the name of the function
and the def is a lambda expression.

```lisp
(defineq (double (lambda (x) (times 2 x))))
  => (double)
```

> `(eval e)` (_Function_)

Evaluate the expression _e_.

> `(evaltrace n)` (_Function_)

Sets the trace variable to _n_. If _n_ is greater than zero the
interpreter will print some more details on how expressions are
evaluated. Call `evaltrace` with a zero argument to turn off the
tracing.

> `(e x)` (_NLambda Function_)

Nlambda version of `eval`. Evaluates _x_. To illustrate the
difference between the functions `eval` and `e`:

```lisp
(setq f '(plus 1 2))
  => (plus 1 2)
(e f)
  => (plus 1 2)
(eval f)
  => 3
```

> `(getrep x)` (_Function_)

Returns the function definition of a `lambda` or a `nlambda` function
object. Calling `getrep` on any other type of object (including `subr`
and `fsubr`) returns `nil`.

```lisp
(defineq (double (lambda (x) (times 2 x))))
  => (double)
(getrep double)
  => (lambda (x) (times 2 x))
double
  => #<lambda 7fec0a810ce0>
(getrep apply)
  => nil
```

> `(lambda x . y)` (_NoSpread Function_)

Creates a lambda object. The parameter _x_ is the parameters of the
function being defined. If it's a list of atoms the function is a
spread function, if it's a single atoms the function is a nospread
function, if it's dotted pair the function is a half spread function.

A _spread_ function binds each formal parameter to the actual
parameters when the function is called. Any excess parameter is
ignored and any missing actual parameter is bound to `nil`.

A _nospread_ function binds the formal parameter to a list of all
actual parameters when called.

A _half spread_ function is a combination of the above where the
actual parameters are bound to each formal parameter and any excess
actual parameters are bound to the formal parameter in the symbol in
the `cdr` of the list of formal parameters.

> `(nlambda x . y)` (_NoSpread Function_)

Same as `lambda` except that the function object is an nlambda
function object and parameters are not evaluated when the function is
called.

> `(prog1 args)` (_NoSpread Function_)

Evaluate each expression in sequence, returning the result of the
first expression.

> `(progn args)` (_NoSpread Function_)

Similar to `prog1` but instead the value of the last expression is
returned.

> `(quote x)` (_NLambda Function_)

Returns _x_ unevaluated.

### Predicates

> `(atom x)` (_Function_)

Predicate which is true if _x_ is an atom, either a symbol or a
number. Strings, for example, are not atoms.

> `(boundp x)` (_Function_)

Evaluates to `t` if _x_ is not bound to a value. Note that the
argument _x_ is evaluated.

```lisp
(boundp 'x)
  => t
```

> `(eq x y)` (_Function_)

Returns `t` if _x_ is the same object as _y_.

> `(eqp x y)` (_Function_)

If both _x_ and _y_ are numbers then `eqp` returns `t` if the numbers
are the same, otherwise return the `(eq x y)`.

> `(equal x y)` (_Function_)

Return `t` if _x_ and _y_ are `eq`, or if _x_ and _y_ are `eqp`, or if
_x_ and _y_ are `strequal`, or if _x_ and _y_ are lists `(and (equal
(car x) (car x)) (equal (cdr x) (cdr y)))`.

> `(geq x y)` (_Function_)

`t` if $x \ge y$, otherwise `nil`.

> `(greaterp x y)` (_Function_)

`t` if $x > y$, otherwise `nil`.

> `(leq x y)` (_Function_)

`t` if $x \le y$, otherwise `nil`.

> `(lessp x y)` (_Function_)

`t` if $x < y$, otherwise `nil`.

> `(listp x)` (_Function_)

`t` if _x_ is a list, otherwise `nil`.

> `(litatom x)` (_Function_)

`t` if _x_ is a literal atom, otherwise `nil`.

> `(neq x y)` (_Function_)

`t` if _x_ is not `eq` to _y_, otherwise `nil`. Equivalent to `(not
(eq x y))`.

> `(neqp x y)` (_Function_)

`t` if _x_ is not `eqp` to _y_, otherwise `nil`. Equivalent to `(not
(eqp x y))`.

> `(nlistp x)` (_Function_)

`t` if _x_ is not a list, otherwise `nil`. Same as `(not (listp x))`.

> `(null x)` (_Function_)

`t` if _x_ is `nil`, otherwise `nil`.

> `(symbolp x)` (_Function_)

Same as `(litatom x)`.

> `(zerop x)` (_Function_)

`t` if _x_ is zero, otherwise `nil`.

### Property List Functions

`lips` supports property lists on literal atoms. A property list is a
list of values stored in the _property cell_ of a literal atom. A
property list is list of alternating properties and values. For
example the property list `(a 1 b 2)` has two properties `a` and `b`
with the values `1` and `2` respectively. `eq` is used to compare
properties when manipulating the property list with the below
functions.

> `(getplist a)` (_Function_)

Returns the entire property list stored in the property cell of _a_.

> `(getprop a p)` (_Function_)

Returns the value of property _p_ stored in the property cell of the
literal atom _a_.

> `(putprop a p v)` (_Function_)

Puts the value _v_ in the property _p_ of _a_.

> `(remprop a p)` (_Function_)

Removes the property _p_ from the literal atom _a_.

> `(setplist a pl)` (_Function_)

Sets the property list of _a_ to _pl_.

### Input and Output Functions

All output functions taking a file descriptor _f_ as a parameter
operates on the primary output if _f_ is `nil`, or on primary error if
_f_ is `t`, otherwise _f_ must be a file handle open for writing.

For input functions the file descriptor _f_ is instead primary input
if _f_ is `nil`, stdin if _f_ is `t`, or a file handle of a file open
for reading.

> `(close f)` (_Function_)

Closes the file associated with f.

> `(load fn)` (_Function_)

Loads a file, evaluating each s-expression.

> `(open fn)` (_Function_)

Open a file.

> `(print)` (_Function_)

Print in such a way that whatever is printed can be read back by the
interpreter. This means including double quotes around strings and
quoting special characters with backslashes.

> `(prin1 x f)` (_Function_)

Prints the arguments without escapes, i.e. strings are printed without
surrounding double quotes, and without a terminating newline.

> `(prin2 args)` (_Function_)

Same as `print` but without a terminating newline.

> `(printlevel)` (_Function_)

Specifies how deep printing should go.

> `(ratom f)` (_Function_)

Read an atom from _f_.

> `(read f)` (_Function_)

Read an S-expression from _f_.

> `(readc f)` (_Function_)

Read one character from _f_.

> `(readline f)` (_Function_)

Read a line up to a newline from _f_.

> `(spaces n f)` (_Function_)

Print _n_ spaces to _f_.

> `(terpri f)` (_Function_)

Print a newline to _f_.

### Conditionals and Control Functions

> `(cond (p1 e1 ...) (p2 e2 ...) ...)` (_NoSpread NLambda Function_)

The predicates _pn_ are evaluated in order and the first that is
evaluated to anything other than `nil`, the expressions after that
predicate is evaluated.  The last expression evaluated is returned as
in progn.  If no predicate evaluates to non-`nil`, `nil` is returned.

> `(if p t . f)` (_NLambda Function_)

If the predicate _p_ evaluates to a non-`nil`\ value the expression
_t_ is evaluated and returned from the function. If _p_ evaluates to
`nil` then the value of the expression _f_ is returned.

> `(while p args)` (_NLambda Function_)

While the predicate _p_ evaluates to a non-`nil` value the sequence of
expressions _args_ are evaluated in sequence. `while` always returns
`nil`.

### Map Functions

Map functions iterate over a list of items and applies a function to
either each item or each tail of the list.

> `(map list fn1 fn2)` (_Function_)

If _fn2_ is `nil`, apply the function _fn1_ on each tail of
_list_. First _fn1_ is applied to _list_, then `(cdr list)`,
and so on. If _fn2_ is not `nil`\ then _fn2_ is called instead
of `cdr` to get the next value on which to apply _fn1_.
`map` returns `nil`.

```lisp
(map '(a b c) (lambda (l) (print l)))
  =>
(a b c)
(b c)
(c)
nil
```

> `(mapc list fn1 fn2)` (_Function_)

`mapc` is the same as `map` except that _fn1_ is applied to `(car
list)` instead of the list. Effectively applying _fn1_ on each element
of the list. `mapc` returns `nil`.

> `(maplist list fn1 fn2)` (_Function_)

The same as `map` but collects the results from applying _fn1_ on each
tail and returning a list of the results.

```lisp
(maplist '(a b c) (lambda (l) (length l)))
  => (3 2 1)
```

> `(mapcar list fn1 fn2)` (_Function_)

Equivalent to `mapc` but collects the results in a list like
`maplist`.

```lisp
(mapcar '(1 2 3) (lambda (n) (plus n 1)))
  => (2 3 4)
```

### Special Functions

> `(backtrace)` (_Function_)

Prints a backtrace of the evaluation stack when called.

> `(exit code)` (_Function_)

Throws the exception `lisp::lisp_finish` which contains the member
variable `exit_code`.  This member variable is set to the value of
_code_. The main function of the C++ program should catch this
exception and call `exit(ex.exit_code)`.

> `(freecount)` (_Function_)

Returns the number of free cons cells available. Since `lips` doesn't
manage memory with garbage collection this value is rather useless.

> `(topofstack)` (_Function_)

Returns the current environment.

> `(destblock e)` (_Function_)

Returns the destination block from an environment. Here is an example
showing the destination block in some cases.

```lisp
(setq a 88)
  => 88
(defineq
  (f0 (lambda (a) (destblock (topofstack))))
  (f1 (lambda (a) (f0 a))))
  => (f0 f1)
(f1 99)
  => (1 (a . 99))
(f0 101)
  => (1 (a . 88))
```

> `(typeof x)` (_Function_)

Returns a literal atom describing the data type of _x_.

```lisp
(typeof 'a)
  => symbol
(typeof 100)
  => integer
(typeof nil)
  => nil
(typeof t)
  => t
(typeof 1.0)
  => float
(typeof "string")
  => string
```

> `(obarray)` (_Function_)

Returns a list of all literal atoms currently active in the system.

## The `lips` Shell

On the top level `lips` reads expressions from standard in and
evaluates the expressions.  Commands entered on the top level are
always treated as functions, even if no parameters are given or if the
expression is a single atom.  In order to print the value of a
variable the function print must be used.

All expressions typed at the top level prompt are treated as lists.
This means that `lips` supplies an extra pair of matching parenthesis
around all expressions.  If the first expression of a line is an atom,
and not a list, input terminates with either a return (providing that
parenthesis in subexpressions match), or an extra right parenthesis.
In the first case, a matching pair of parenthesis are added
surrounding the line, in the second case an extra left parenthesis is
added as the first character.  Again, if a left parenthesis is missing
a matching parenthesis is inserted.

Typing the outermost parenthesis explicitly will make `lips` print out
the return value of the expression.  This is the way normal lisp
systems behave, but when used only as a command shell the return value
is most often uninteresting.  The return value is also stored in the
variable `value`.

The `lips` shell extends finding the functional form in a lisp
expression when the symbol doesn't have an `autoload` property, or
loading the file didn't define the symbol. In that case `lips` looks
under the property `alias`.  The atom is replaced with the expression
stored under that property, if any, and the rest of the expression is
appended at the end and the evaluation process continues.  When all
else fails it is assumed the atom stands for an executable command.
The return value of an executable command is, at present, always `t`.

When `lips` reads an expression some characters are treated as
read-macros.  A read-macro can expand or transform the input in
different ways.  The current version of the reader in `lips` isn't
fully developed yet and it's likely to be improved in later versions.

In addition to read-macros the input is also transformed using a hook
which is evaluated after an expression is read and before it's
evaluated.  For example, the input `ls | wc -l` is rewritten by
the transform hook into `(pipe-cmd (ls) (wc -l))`.  The
`pipe-cmd` function evaluates each expression in a separate
process, connecting the stdout from the `ls` function (i.e. the
`ls` command) to the stdin of the `wc` function (command).

Redirection of stdout and stdin is also handled by the transform hook.

### Shell Functions

This section describes functions which implement shell features like
output redirection, changing the working directory, job control, etc.

All functions that in some way redirects its input or output are
executed in a fork.  This means that redirecting I/O of a lisp
function doesn't make permanent changes to the `lips` environment.  No
global variables are changed.

All functions in this section are nlambda functions.

> `(cd d)` (_NLambda Function_)

Changes current working directory to _d_.

> `(redir-to cmd file fd)` (_NLambda Function_)

Evaluates the expression _cmd_, redirecting the output from this
expression to a file with the file name _file_. The third parameter
_fd_ is optional and if given should be the file descriptor to
redirect. The default is to redirect stdout.

> `(redir-from cmd file fd)` (_NLambda Function_)

Evaluates the expression _cmd_, redirecting the file descriptor _fd_
(or stdin if not given) from the file _file_.

> `(append-to cmd file fd)` (_NLambda Function_)

Same as `redir-to` but append to the file _file_ instead of
overwriting it.

> `(pipe-cmd args)` (_NLambda Function_)

Connects the stdout of _x1_ to stdin of _x2_ and stdout of _x2_ to
stdin of _x3_ and so on. If there is only one expression it is simply
evaluated.

> `(back cmd)` (_NLambda Function_)

Creates a new process and evaluates the command _cmd_ in this
process. A new job is created which can be controlled by the job
controlling functions.

> `(stop)` (_NLambda Function_)

Sends a stop signal to itself.

> `(rehash)` (_NLambda Function_)

`lips` keeps a hash of all executable files in the `path` so that it
can fail fast if a program is not available. The `rehash` function
rebuilds this hash in order to pick up new programs.

> `(jobs)` (_NLambda Function_)

Lists all jobs currently in flight.

> `(fg job)` (_NLambda Function_)

Brings the job _job_ running in the background to the foreground.

> `(bg job)` (_NLambda Function_)

Continues the job _job_ in the background.

> `(setenv var val)` (_NLambda Function_)

Sets the environment variable _var_ to the value _val_.

> `(getenv var)` (_NLambda Function_)

Returns the value of the environment varable _var_ or `nil`\ if the
variable is not set.

> `(exec cmd)` (_NLambda Function_)

Replaces the current process with _cmd_.

## Using `lips` as a Library

This section describes using `lips` as a library. Most functions which
are available in lisp are also available in C++.

## Properties

This section describes some properties that are used internally by the
shell.

- `alias` this is used during alias expansion.  Let's say we want to
  define an alias for ls that uses the -F option of ls: `(putprop 'l
  'alias '(ls -F))`.  To simplify defining aliases the lisp function
  alias is provided (see below).
- `autoload` If during evaluation of a form, an undefined function is
  found the offending atom may have a file stored under this
  property. If this is the case the file is loaded and evaluation is
  allowed to continue.  If the function is still undefined error
  processing takes over.
