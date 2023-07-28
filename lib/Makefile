SBCL_HOME?=$(HOME)/sbcl
SBCL?=$(SBCL_HOME)/run-sbcl.sh

.PHONY: all clean

all: libquilc.dylib

libquilc.core libquilc.c libquilc.h libquilc.py: src/libquilc.lisp
	$(SBCL) --dynamic-space-size 4096 --load "src/build-image.lisp"

libquilc.dylib: libquilc.core libquilc.c
	gcc -dynamiclib -o $@ libquilc.c -lsbcl -L$(SBCL_HOME)/src/runtime

example: example.c libquilc.dylib
	gcc src/example.c -o example -lsbcl -lquilc -L. -L$(SBCL_HOME)/src/runtime

clean:
	rm -f libquilc.c libquilc.h libquilc.core libquilc.py libquilc.dylib example
