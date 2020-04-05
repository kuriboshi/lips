.if t .tr *\(**
.ds * \s+2*\s0
.de LE          \" lips expression
.if t .ft I
.ps -2
\&\\$1
.ps
.ft R
..
.DA "26 June, 1989"
.TL
LIPS Task List
.SH
More Important
.IP "  *"
Append to last atom after history substituition.
.IP "  *"
Make sure all local lisp variables are protected from gc.
.IP "  *"
Interrupt is not reliable.
Commands inside lisp loops can't be interrupted.
.IP "  *"
Escaping is hardly useful in its present form.
It is only possible to avoid read macro interpretaion with it.
.IP "  *"
It isn't possible to put funny characters on the command line.
.IP "  *"
.LE EXPAND
includes . and .. in the expansion.
These should be included only if requested.
.IP "  *"
The functions
.LE CHDIR
and
.LE CD
should parse the new cwd instead of using
.I getwd().
.IP "  *"
Globbing can't handle ranges.
.IP "  *"
The posibility to collect output from commands,
the backquote facility of
.I /bin/sh.
.IP "  *"
Floating point numbers should be a run time option.
They are most likely more seldom used than filenames
looking like floats.
.IP "  *"
Catch all failing system calls and report with proper error message.
.IP "  *"
If
.I execve()
fails with a NOEXEC
error, try
.I /bin/sh.
.IP "  *"
Switches that look like integers should be treated as switches.
Unary plus is forbidden and negative numbers are converted to -n.
.IP "  *"
.LE LOAD
should consult a load path.
.SH
Less Important
.IP "  *"
Arrays.
.IP "  *"
Some way to turn the end of file character off.
Perhaps by a hook that is run at the end of file condition.
Default would be exit.
.IP "  *"
A set of functions manipulating the stack.
.IP "  *"
User read macros.
.IP "  *"
Real command hashtables.
.IP "  *"
Emacs style line editing.
.IP "  *"
Errors in subshells are not handled very well.
It would be nice if the failing process could tell
the main
.B lips
process that it failed and let the main process
talk to the user.
