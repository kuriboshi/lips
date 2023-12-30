# Shell Functions

This section describes functions which implement shell features like output
redirection, changing the working directory, job control, etc.

All functions that in some way redirects its input or output are executed
in a fork.  This means that redirecting I/O of a lisp function doesn't make
permanent changes to the `lips` environment.  No global variables are
changed.

> `(redir-to cmd file fd)` (_NLambda Function_)

```cpp
lisp_t redir_to(lisp_t cmd, lisp_t file, lisp_t filed);
```

Redirects output of command to a file.

The command in _cmd_ is executed with the file descriptor in _filed_
redirected to a file named _file_.

**Returns**: The exit status of the command once it exits. If something went
wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil` is
returned.

> `(redir-append cmd file filed)` (_NLambda Function_)

```cpp
lisp_t redir_append(lisp_t cmd, lisp_t file, lisp_t filed);
```

Redirects output, appending to a file.

The file descriptor in _filed_ is redirected and the output from the
command _cmd_ is redirected to a file named _file_.

**Returns**: The exit status of the command once it exits. If something went
wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil` is
returned.

> ```cpp
> lisp_t redir_from(lisp_t cmd, lisp_t file, lisp_t filed);
> ```

Redirect input to a command from a file.

The file descriptor in _filed_ is redirected to read from a file named
_file_ when _cmd_ is executed.

**Returns**: The exit status of the command once it exits. If something went
wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil` is
returned.

> `(pipe-cmd cmds)` (_NLambda Function_)

```cpp
lisp_t pipecmd(lisp_t cmds);
```

Connect output from one command to the input of the next.

The argument _cmds_ is a list of commands which are in themselves lists. For example:

```lisp
(pipe-cmd '((find . -name foo) (xargs grep bar)))
```

If the list _cmds_ only contains one command no piping of the output is done.

- _cmds_ A list of command lists.

**Returns**: The exit status of the last command once it exits. If something
went wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil`
is returned.

> `(back cmd)` (_NLambda Function_)

```cpp
lisp_t back(lisp_t cmd);
```

Runs a command in the background.

Runs the command _cmd_ in the background.

**Returns**: The process ID of the process running in the background. The
symbol `error` if the fork fails.

> `(stop-lips)` (_NLambda Function_)

```cpp
lisp_t stop();
```

Stop the lips shell and return to parent process.

**Returns**: `t`.

> `(jobs)` (_NLambda Function_)

```cpp
lisp_t jobs();
```

Lists active background jobs.

Prints a list of all active background jobs and their status.

**Returns**: `nil`.

> ```cpp
> lisp_t fg(lisp_t job);
> ```

Brings a background job to the foreground.
@lisp((fg job),NLambda Function}

Brings the specified _job_ to the foreground, or the current job if `nil`.

**Returns**: The exit status when the job finishes.

> `(bg job)` (_NLambda Function_)

```cpp
lisp_t bg(lisp_t job);
```

Runs a stopped job in the background.

Runs the _job_, which is normally in a stopped state, in the background.

**Returns**: `t`.

> `(setenv var val)` (_NLambda Function_)

```cpp
lisp_t setenv(lisp_t var, lisp_t val);
```

Sets an environment variable.

Sets the environment variable `var` to the value `val`. Each parameter must
be either a symbol or a string.

**Returns**: The variable name.

> `(getenv var)` (_NLambda Function_)

```cpp
lisp_t getenviron(lisp_t var);
```

Gets the value of an environment variable.

**Returns**: The value of the variable or `nil` if not set.

> `(cd dir emess)` (_NLambda Function_)

```cpp
lisp_t cd(lisp_t dir, lisp_t emess);
lisp_t doexec(lisp_t cmd);
```

Change current working directory.

Changes the current working directory to _dir_. If _emess_ is `nil` then an
error is signaled if the error does not exist. If non-`nil` the error is
ignored. The value of the environment variable PWD is updated to the
current working directory.

If _dir_ is `nil` then the current working directory is changed to the
user's home directory. Glob patterns are expanded.

**Returns**: `t` if the directory changed successfully, `nil' otherwise.

> ```cpp
> lisp_t rehash();
> ```

Rehashes the command hash.
@lisp{(rehash)}

The `lips` shell maintains a hash map of command to path. The `rehash`
function updates the hash map with any changes.

**Returns**: `nil`.

> `(exec cmd)` (_NLambda Function_)

```cpp
void do_rehash();
```

Replace the lips shell with a command.

**Returns**: If successful `exec` does not return, `nil` if not successful.
