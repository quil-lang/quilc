
all: quilc

deps:
	sbcl --quit --eval "(ql:quickload ':quilc)"

quilc: src/entry-point.lisp src/package.lisp src/printers.lisp src/server.lisp
	buildapp --output quilc \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system quilc \
		 --compress-core \
		 --entry quilc::entry-point

quilc-unsafe:
	buildapp --output quilc \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system quilc \
		 --compress-core \
		 --entry quilc::%entry-point

test:
	sbcl --noinform --non-interactive \
		 --eval "(ql:quickload :quilc-tests)" \
		 --eval "(asdf:test-system :quilc)"

clean:
	rm -f quilc build-output.log
