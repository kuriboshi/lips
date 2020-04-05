/*
 * Lips, lisp shell.
 * Copyright 1989, Krister Joas
 *
 * $Id$
 */
#define SEPR         001        /* seperator */
#define BRK          002        /* break character */
#define INSERT       004        /* insert read macro */
#define SPLICE       010        /* splice read macro */
#define INFIX        014        /* infix read macro */
#define RMACRO       014        /* read macro mask */

#define issepr(c)    (currentrt.chclass[(c)] & SEPR)
#define isbrk(c)     (currentrt.chclass[(c)] & BRK)
#define isrm(c)      (currentrt.chclass[(c)] & RMACRO)
#define isinsert(c)  ((currentrt.chclass[(c)] & RMACRO) == INSERT)
#define issplice(c)  ((currentrt.chclass[(c)] & RMACRO) == SPLICE)
#define isinfix(c)   ((currentrt.chclass[(c)] & RMACRO) == INFIX)

struct rtinfo
{
  unsigned char chclass[128];
  LISPT (*rmacros[128])();
};
