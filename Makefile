#
# Makefile to make libisp and lips.
# Copyright 1992, Krister Joas.
#
# $Id$
#
SUBDIRS = libisp src

all clean:	FORCE
	for i in $(SUBDIRS); do (cd $$i; $(MAKE) $@); done

FORCE:
