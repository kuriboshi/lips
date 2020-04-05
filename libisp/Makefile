#
# Makefile for lisplib.
# Copyright 1992, Krister Joas.
#
# $Id$
#

RM = /bin/rm
CC = cc

DESTDIR = /usr/local/bin
DFLAGS = -g

CPPFLAGS = -I.
CFLAGS = $(DFLAGS) $(CPPFLAGS)
LDFLAGS = $(DFLAGS) -L.

LISPLIB = libisp.a
.PRECIOUS:	$(LISPLIB)

C1 = arith.c logic.c low.c map.c pred.c prim.c
C2 = misc.c string.c init.c user.c os.c file.c
C3 = eval.c alloc.c
C4 = prop.c read.c
C5 = version.c
SOURCES = $(C1) $(C2) $(C3) $(C4) $(C5)
OBJECTS = $(SOURCES:.c=.o)
LIBFILES = $(OBJECTS:%=$(LISPLIB)(%))

all:	$(LISPLIB)

$(LISPLIB):	$(OBJECTS)
	$(AR) $(ARFLAGS) $@ $?
	ranlib $@

lint:
	lint $(SOURCES)

tags:	$(SOURCES) defs.h
	util/maketags

clean:
	$(RM) -f $(LISPLIB) *.o core

depend:
	makedepend -p"$$(LISPLIB)(" -o".o)" $(CPPFLAGS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.

arith.o: lisp.h
arith.o: config.h libisp.h defs.h io.h constants.h error.h func.h
logic.o: lisp.h
logic.o: config.h libisp.h defs.h io.h constants.h error.h func.h
low.o: lisp.h
low.o: libisp.h defs.h io.h constants.h error.h func.h
map.o: lisp.h config.h
map.o: libisp.h defs.h io.h constants.h error.h func.h
pred.o: lisp.h
pred.o: config.h libisp.h defs.h io.h constants.h error.h func.h
prim.o: lisp.h
prim.o: config.h libisp.h defs.h io.h constants.h error.h func.h
misc.o: lisp.h
misc.o: config.h libisp.h defs.h io.h constants.h error.h func.h
string.o: lisp.h
string.o: config.h libisp.h defs.h io.h
string.o: constants.h error.h func.h
init.o: lisp.h
init.o: config.h libisp.h defs.h io.h constants.h error.h func.h
user.o: lisp.h
user.o: config.h libisp.h defs.h io.h constants.h error.h func.h
file.o: lisp.h
file.o: config.h
file.o: libisp.h defs.h io.h constants.h error.h func.h
eval.o: lisp.h
eval.o: config.h libisp.h defs.h io.h
eval.o: constants.h error.h func.h
alloc.o: lisp.h
alloc.o: config.h libisp.h defs.h io.h
alloc.o: constants.h error.h func.h garb.h
prop.o: lisp.h
prop.o: config.h libisp.h defs.h io.h constants.h error.h func.h
read.o: lisp.h
read.o: config.h libisp.h defs.h io.h
read.o: constants.h error.h func.h
