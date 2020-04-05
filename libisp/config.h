/*
 * Lips, lisp shell.
 * Copyright 1989, Krister Joas
 *
 * $Id$
 */
/*
 * Defines where the source is.
 */
#define LIPSLIB "/usr/local/src/lips"
/*
 * File that is used to find source code for primitives.
 */
#define TAGSFILE "/usr/local/src/lips/src/Tags"
/*
 * Global init file.
 */
#define LIPSRC "/usr/local/lib/lipsrc"

/*
 * Defining this will use the select call to handle
 * default answer in getuser().
 */
#define SELECT
/*
 * Define this to enable job control, requires SIGCHLD, sigblock...
 */
#define JOB_CONTROL
/*
 * If defined makes lips use termcap to retype line nicer.
 */
#define TERMCAP
/*
 * Define SHELL if you want to use it as a shell.
 */
#define SHELL
/*
 * Define TRACE if you want to be able to trace calls to peval.
 * Useful for debugging.
 */
#define TRACE
/* 
 * Defining this includes code to take care of signals and ask
 * if the core should be dumped or not.
 */
#define FANCY_SIGNALS
/*
 * If this isn't defined you can't read floating point numbers.
 */
/* #define FLOATING */

/*
 * The rest is automatic...
 */
#ifdef SARGASSO
#define SHORT
#else
#define GENERIC
#endif SARGASSO

#ifdef pdp11
#define SMALL
#define SHORT
#endif pdp11
