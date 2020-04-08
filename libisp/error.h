/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
 */
#pragma once

extern LISPT error(int, LISPT);

#define PRINT_ARG (1 << 8)
#define NOT_A (1 << 9)
#define ERRNO(x) ((x) & ~PRINT_ARG & ~NOT_A)

#define NO_MESSAGE 0
#define ILLEGAL_ARG (PRINT_ARG | 1)
#define DIVIDE_ZERO 2
#define BUG 3
#define NO_MATCH (PRINT_ARG | 4)
#define CANT_CREATE (PRINT_ARG | 6)
#define CANT_CREATE_OPEN (PRINT_ARG | 7)
#define CANT_OPEN (PRINT_ARG | 8)
#define NO_SUCH_JOB (PRINT_ARG | 9)
#define NOT_PRINTABLE (PRINT_ARG | 10)
#define NO_DIRECTORY (PRINT_ARG | 11)
#define NO_USER (PRINT_ARG | 12)
#define ATTEMPT_TO_RESET (PRINT_ARG | 13)
#define OUT_OF_MEMORY 14
#define UNEXPECTED_EOF 15
#define EVENT_NOT_FOUND 16
#define UNKNOWN_REQUEST (PRINT_ARG | 17)
#define ILLEGAL_SIGNAL 18
#define STACK_OVERFLOW 19
#define CORRUPT_DATA (PRINT_ARG | 20)
#define COMMAND_ABORTED 21
#define ALIAS_LOOP (PRINT_ARG | 22)
#define ILLEGAL_FUNCTION (PRINT_ARG | 23)
#define UNDEF_FUNCTION (PRINT_ARG | 24)
#define UNBOUND_VARIABLE (PRINT_ARG | 25)
#define KBD_BREAK (PRINT_ARG | 26)
#define AMBIGUOUS (PRINT_ARG | 27)
#define USER_ERROR (PRINT_ARG | 28)
#define MAXMESSAGE 29

#define CHECK(arg, typ) \
  if (TYPEOF(arg) != typ) \
  return error(NOT_A | typ, arg)
#define CHECK2(arg, typ1, typ2) \
  if (TYPEOF(arg) != typ1 && TYPEOF(arg) != typ2) \
    return error(ILLEGAL_ARG, arg);
