# `Lips` Reference Manual

Version 3.2.0, January, 2024

> Copyright (c) 1988-1989, 1992, 2020-2024 Krister Joas
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

- [Data Types](./reference/types.md)
- [Memory Management Functions](./reference/alloc.md)
- [Function Evaluation](./reference/vm.md)
- [List Functions](./reference/list.md)
- [Low Level Functions](./reference/low.md)
- [Predicates](./reference/predicate.md)
- [String Functions](./reference/string.md)
- [Input and Output Functions](./reference/file.md)
- [Arithmetic Functions](./reference/arith.md)
- [Logic Functions](./reference/logic.md)
- [Map Functions](./reference/map.md)
- [Property List Functions](./reference/property.md)
- [User Functions](./reference/user.md)

## The `lips` Shell

This section describes the sample `lips` application, a Unix shell.

On the top level `lips` reads expressions from standard in and
evaluates the expressions.  Commands entered on the top level are
always treated as functions, even if no parameters are given or if the
expression is a single atom.  In order to print the value of a
variable the function print must be used.

All expressions typed at the top level prompt are treated as lists.
This means that `lips` supplies an extra pair of matching parenthesis
around all expressions.

```lisp
echo "hello world"
hello world
  =>
(echo "hello world")
hello world
  => t
```

If the first expression of a line is an atom, and not a list, input
terminates with either a return (providing that parenthesis in
subexpressions match), or an extra right parenthesis.  In the first
case, a matching pair of parenthesis are added surrounding the line,
in the second case an extra left parenthesis is added as the first
character.  Again, if a left parenthesis is missing a matching
parenthesis is inserted.

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
evaluated.  For example, the input `ls | wc -l` is rewritten by the
transform hook into `(pipe-cmd (ls) (wc -l))`.  The `pipe-cmd`
function evaluates each expression in a separate process, connecting
the stdout from the `ls` function (i.e. the `ls` command) to the stdin
of the `wc` function (command).

Redirection of stdout and stdin is also handled by the transform hook.

- [Shell Functions](./reference/exec.md)
