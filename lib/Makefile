.PHONY: all clean

all: libquilc.dylib

libquilc.core libquilc.c libquilc.h libquilc.py: src/libquilc.lisp
	sbcl --load "$<"

libquilc.dylib: libquilc.core libquilc.c
	gcc -dynamiclib -o $@ libquilc.c -lsbcl

example: example.c libquilc.dylib
	gcc src/example.c -o example -lsbcl -lquilc -L.

clean:
	rm -f libquilc.c libquilc.h libquilc.core libquilc.py libquilc.dylib example
