.PHONY: all clean

OS:=$(shell uname -s)
ifeq ($(OS), Darwin)
	LIBQUILC_TARGET = libquilc.dylib
	CCFLAGS = -dynamiclib
else
	LIBQUILC_TARGET = libquilc.so
	CCFLAGS = -shared
endif

all: $(LIBQUILC_TARGET)

libquilc.core libquilc.c libquilc.h libquilc.py: src/libquilc.lisp
	sbcl --dynamic-space-size 4096 --load "src/build-image.lisp"

$(LIBQUILC_TARGET): libquilc.core libquilc.c
	gcc $(CCFLAGS) -o $@ libquilc.c -lsbcl

example: example.c libquilc.dylib
	gcc src/example.c -o example -lsbcl -lquilc -L.

clean:
	rm -f libquilc.c libquilc.h libquilc.core libquilc.py libquilc.dylib example
