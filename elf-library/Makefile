all: quil_example.out quil_example_static.out

clean:
	rm *.so *.a *.out

run_shared:
	LD_LIBRARY_PATH=`pwd` ./quil_example.out

run_static:
	./quil_example_static.out

quil_example.out: example.c cl-quil--all-systems.so
	$(CC) `ecl-config --cflags` -o $@ example.c cl-quil--all-systems.so \
	      `ecl-config --ldflags` -lecl

# in principle it is possible to link statically even with libecl but
# it requires more work and is not well supported.
quil_example_static.out: example.c cl-quil--all-systems.a
	$(CC) `ecl-config --cflags` -o $@ example.c cl-quil--all-systems.a \
	      `ecl-config --ldflags` -lecl

cl-quil--all-systems.so: build.lisp
	ecl --load "build.lisp" --eval "(build-shared)" --eval "(quit)"

cl-quil--all-systems.a: build.lisp
	ecl --load "build.lisp" --eval "(build-static)" --eval "(quit)"
