/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
 */
#pragma once

#define PN_RECLAIM "reclaim"       /* initiate garbage collection */
#define PN_CONS "cons"             /* make a new cons cell */
#define PN_PLUS "+"                /* add */
#define PN_DIFFERENCE "-"          /* subtract */
#define PN_TIMES "*"               /* multiply */
#define PN_DIVIDE "/"              /* divide */
#define PN_IPLUS "i+"              /* integer add */
#define PN_IDIFFERENCE "i-"        /* integer subtract */
#define PN_ITIMES "i*"             /* integer multiply */
#define PN_IQUOTIENT "i/"          /* integer divide */
#define PN_IREMAINDER "i%"         /* integer mod */
#define PN_IMINUS "iminus"         /* integer change sign */
#define PN_MINUS "minus"           /* change sign generic */
#define PN_ADD1 "add1"             /* add one */
#define PN_SUB1 "sub1"             /* subtract one */
#define PN_ABS "abs"               /* absolute value */
#define PN_FPLUS "f+"              /* float add */
#define PN_FDIFFERENCE "f-"        /* float subtract */
#define PN_FTIMES "f*"             /* float multiply */
#define PN_FDIVIDE "f/"            /* float divide */
#define PN_ITOF "itof"             /* integer to float */
#define PN_GREATERP "greaterp"     /* t if greater than */
#define PN_GEQ "geq"               /* t if greater or eq */
#define PN_LESSP "lessp"           /* less than */
#define PN_LEQ "leq"               /* less or eq */
#define PN_ZEROP "zerop"           /* t if eq to 0 */
#define PN_EQP "eqp"               /* number eq */
#define PN_NEQP "neqp"             /* not eqp */
#define PN_MINUSP "minusp"         /* t if negative */
#define PN_FREECOUNT "freecount"   /* number of free cells */
#define PN_EVALTRACE "evaltrace"   /*  */
#define PN_OBARRAY "obarray"       /* return list of all atoms */
#define PN_E "e"                   /* noeval version of eval */
#define PN_EVAL "eval"             /* evaluate exp */
#define PN_APPLY "apply"           /* apply function on args */
#define PN_APPLYSTAR "apply*"      /* apply nospread */
#define PN_EXPAND "expand"         /* expand wildcards */
#define PN_TO "redir-to"           /* redirect to file */
#define PN_FROM "redir-from"       /* redirect from file */
#define PN_TOTO "append-to"        /* redirect appending to file */
#define PN_PIPECMD "pipe-cmd"      /* pipe commands */
#define PN_BACK "back"             /* run command in background */
#define PN_STOP "stop-lips"        /* stop lips, return to superior */
#define PN_REHASH "rehash"         /* recalculate hash table */
#define PN_JOBS "jobs"             /* list jobs */
#define PN_FG "fg"                 /* run job in foreground */
#define PN_BG "bg"                 /* run job in background */
#define PN_SETENV "setenv"         /* set environment variable */
#define PN_GETENV "getenv"         /* get value of variable */
#define PN_EXEC "exec"             /* overlay lips with command */
#define PN_LOAD "load"             /* load file */
#define PN_PRIN1 "prin1"           /* print without escapes */
#define PN_PRIN2 "prin2"           /* print without new-line */
#define PN_PRINT "print"           /* print */
#define PN_PLEVEL "printlevel"     /* how deep to print */
#define PN_RATOM "ratom"           /* read atom */
#define PN_READ "read"             /* read expression */
#define PN_READC "readc"           /* read characte */
#define PN_READLINE "readline"     /* read a line */
#define PN_SPACES "spaces"         /* print some spaces */
#define PN_TERPRI "terpri"         /* print new-line */
#define PN_CPPRINT "cpprint"       /* find and prettyprint c function */
#define PN_AND "and"               /* and */
#define PN_OR "or"                 /* or */
#define PN_NOT "not"               /* not */
#define PN_IF "if"                 /* if a then b else c */
#define PN_SET "set"               /* set variable */
#define PN_SETQ "setq"             /* set quoted variable */
#define PN_SETQQ "setqq"           /* noeval set */
#define PN_COND "cond"             /* cond */
#define PN_WHILE "while"           /* while t */
#define PN_PROGN "progn"           /* return last expression */
#define PN_PROG1 "prog1"           /* return first expression */
#define PN_PROG2 "prog2"           /* return second expression */
#define PN_TOPOFSTACK "topofstack" /* return top of value stack */
#define PN_ENVGET "envget"         /* examine environment */
#define PN_BAKTRACE "baktrace"     /* control stack backtrace */
#define PN_MAP "map"               /* map */
#define PN_MAPC "mapc"             /* map on car */
#define PN_MAPLIST "maplist"       /* map and build result */
#define PN_MAPCAR "mapcar"         /* mapc and build result */
#define PN_LISTP "listp"           /* t if cons */
#define PN_NLISTP "nlistp"         /* not listp */
#define PN_NEQ "neq"               /* not eq */
#define PN_NUMBERP "numberp"       /* integer of float */
#define PN_MEMB "memb"             /* t if a in l */
#define PN_EQUAL "equal"           /* equal */
#define PN_BOUNDP "boundp"         /* t if var bound */
#define PN_LITATOM "litatom"       /* t if literal atom */
#define PN_TYPEOF "typeof"         /* return type as an atom */
#define PN_ATOM "atom"             /* t if atom */
#define PN_ATTACH "attach"         /* attach object at front of list */
#define PN_APPEND "append"         /* append lists */
#define PN_CAR "car"               /* car */
#define PN_CDR "cdr"               /* cdr */
#define PN_CADR "cadr"
#define PN_CDAR "cdar"
#define PN_CAAR "caar"
#define PN_CDDR "cddr"
#define PN_CDDDR "cdddr"
#define PN_CADDR "caddr"
#define PN_CDADR "cdadr"
#define PN_CAADR "caadr"
#define PN_CDDAR "cddar"
#define PN_CADAR "cadar"
#define PN_CDAAR "cdaar"
#define PN_CAAAR "caaar"
#define PN_CLOSURE "closure" /* create static environment */
#define PN_EQ "eq"           /* pointer equal */
#define PN_ERROR "error"
#define PN_LAMBDA "lambda"     /* create lambda object */
#define PN_LENGTH "length"     /* length of list */
#define PN_LIST "list"         /* make list of args */
#define PN_NCONC "nconc"       /* destructive append */
#define PN_NLAMBDA "nlambda"   /* make nlambda object */
#define PN_NTH "nth"           /* nth car in list */
#define PN_NULL "null"         /* t if nil */
#define PN_QUOTE "quote"       /* don't eval arg */
#define PN_RPLACA "rplaca"     /* replace car */
#define PN_RPLACD "rplacd"     /* replace cdr */
#define PN_TCONC "tconc"       /* add to end of list */
#define PN_NTHD "nthd"         /* return nth cdr of list */
#define PN_SETPLIST "setplist" /* set property list */
#define PN_GETPLIST "getplist" /* get property list */
#define PN_PUTPROP "putprop"   /* put property on atom */
#define PN_GETPROP "getprop"   /* get property value */
#define PN_REMPROP "remprop"   /* remove prop */
#define PN_STRINGP "stringp"   /* t if string */
#define PN_STREQ "streq"       /* string equal */
#define PN_CONCAT "concat"     /* concatenate strings */
#define PN_STRLEN "strlen"     /* length of string */
#define PN_SUBSTR "substr"     /* get sub string */
#define PN_SYMSTR "symstr"     /* make symbol a string */
#define PN_STRCMP "strcmp"     /* compare strings */
#define PN_PRINTHIST "??"      /* print history */
#define PN_UXACCESS "access"   /* check file access */
#define PN_UXALARM "alarm"     /* set alarm clock */
#define PN_UXCHDIR "chdir"     /* change directory */
#define PN_UXCHMOD "chmode"    /* change mode of file */
#define PN_UXCLOSE "close"     /* close file */
#define PN_UXCREAT "creat"     /* create file */
#define PN_UXDUP "dup"         /* duplicate fileno */
#define PN_UXERRNO "errno"     /* return latest error */
#define PN_EXIT "exit"         /* exit lips */
#define PN_UXGETUID "getuid"   /* get user id */
#define PN_UXGETEUID "geteuid" /* get effective user id */
#define PN_UXGETGID "getgid"   /* set group id */
#define PN_UXGETEGID "getegid" /* get effective group id */
#define PN_UXGETPID "getpid"   /* get process id */
#define PN_UXKILL "killproc"   /* kill process */
#define PN_UXLINK "link"       /* link file */
#define PN_UXNICE "setnice"    /* set nice */
#define PN_UXOPEN "open"       /* open file */
#define PN_UXSETUID "setuid"   /* set user id */
#define PN_UXSETGID "setgid"   /* set group id */
#define PN_SIGNAL "signal"     /* install signal handler */
#define PN_UXUNLINK "unlink"   /* unlink file */
#define PN_DEFINE "define"     /* define function */
#define PN_GETREP "getrep"     /* get function representation */
#define PN_DE "de"             /* defile lambda function */
#define PN_DF "df"             /* define nlambda function */
#define PN_CD "cd"             /* change directory */
