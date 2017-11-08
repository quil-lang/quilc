# Heap space for QVM in MiB.
QVM_WORKSPACE ?= 2048

all: quilc

quilc:
	buildapp --output quilc \
		 --dynamic-space-size $(QVM_WORKSPACE) \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system quilc \
		 --compress-core \
		 --entry quilc::entry-point

quilc-unsafe:
	buildapp --output quilc \
		 --dynamic-space-size $(QVM_WORKSPACE) \
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
