/*
 * Lips, lisp shell.
 * Copyright 1989, Krister Joas
 *
 * $Id$
 */

extern LISPT top, rstack, history, histnum, fun, exp;
extern LISPT args, path, home, verboseflg, promptform, brkprompt, topprompt;
extern LISPT alias_expanded, interactive, version;

/*
 * markobjs contains pointers to all LISPT type c variables that
 * contains data to be retained during gc.
 */
LISPT * markobjs[] =
{
  & top,
  & rstack,
  & history,
  & histnum,
  & fun,
  & exp,
  & args,
  & path,
  & home,
  & verboseflg,
  & topprompt,
  & promptform,
  & brkprompt,
  & currentbase,
  & interactive,
  & version,
  & gcgag,
  & alias_expanded,
  & C_EOF,
  NULL
};
