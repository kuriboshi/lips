#
# Makefile for lips.
# Copyright 1992, Krister Joas.
#
# $Id$
#

RM = /bin/rm
CC = cc

DESTDIR = /usr/local/bin
DFLAGS = -g

CPPFLAGS = -I../libisp
CFLAGS = $(DFLAGS) $(CPPFLAGS)
LDFLAGS = $(DFLAGS) -L../libisp

LISPLIB = ../libisp/libisp.a
LIBS = -ltermcap -lisp

SOURCES = main.c exec.c glob.c term.c top.c
OBJECTS = $(SOURCES:.c=.o)

all:	lips

lips:	../libisp/libisp.a $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBS)

lint:
	lint $(SOURCES)

clean:
	$(RM) -f *.o *.a *~ lips

depend:
	makedepend $(CPPFLAGS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.

main.o: ../libisp/lisp.h
main.o: ../libisp/config.h
main.o: ../libisp/libisp.h ../libisp/defs.h ../libisp/io.h
main.o: ../libisp/constants.h ../libisp/error.h ../libisp/func.h
exec.o: lips.h ../libisp/lisp.h /usr/include/stdio.h
exec.o: ../libisp/config.h ../libisp/libisp.h
exec.o: ../libisp/defs.h ../libisp/io.h ../libisp/constants.h
exec.o: ../libisp/error.h ../libisp/func.h
glob.o: lips.h ../libisp/lisp.h
glob.o: ../libisp/config.h ../libisp/libisp.h
glob.o: ../libisp/defs.h ../libisp/io.h ../libisp/constants.h
glob.o: ../libisp/error.h ../libisp/func.h /usr/include/setjmp.h
term.o: lips.h ../libisp/lisp.h
term.o: ../libisp/config.h
term.o: ../libisp/libisp.h ../libisp/defs.h ../libisp/io.h
term.o: ../libisp/constants.h ../libisp/error.h ../libisp/func.h
top.o: lips.h ../libisp/lisp.h /usr/include/stdio.h
top.o: ../libisp/config.h ../libisp/libisp.h
top.o: ../libisp/defs.h ../libisp/io.h ../libisp/constants.h
top.o: ../libisp/error.h ../libisp/func.h
