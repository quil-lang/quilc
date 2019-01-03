RIGETTI_LISP_LIBRARY_HOME=../
LISP_CACHE ?= $(HOME)/.cache/common-lisp


SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)' \
	--eval '(push :hunchentoot-no-ssl *features*)' \
	--eval "(push (truename \"$(RIGETTI_LISP_LIBRARY_HOME)\") ql:*local-project-directories*)"
QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp



all: quilc

$(QUICKLISP_SETUP):
	mkdir -p $(QUICKLISP_HOME)
	curl -o $(QUICKLISP_HOME)/quicklisp-bootstrap.lisp \
		$(QUICKLISP_BOOTSTRAP_URL)
	$(SBCL) --load $(QUICKLISP_HOME)/quicklisp-bootstrap \
		--eval "(quicklisp-quickstart:install :path \"$(QUICKLISP_HOME)\")"

system-index.txt: $(QUICKLISP_SETUP)
	$(QUICKLISP)  \
		$(FOREST_SDK_FEATURE) \
		--eval '(ql:quickload "quilc")' \
		--eval '(ql:write-asdf-manifest-file "system-index.txt")'

.PHONY: quilc
quilc: system-index.txt
	buildapp --output quilc \
		 --manifest-file system-index.txt \
		 --eval '(push :hunchentoot-no-ssl *features*)' \
		 --asdf-path . \
		 --load-system quilc \
		 $(FOREST_SDK_LOAD) \
		 --eval '(quilc::zap-info)' \
		 --eval '(quilc::setup-debugger)' \
		 --compress-core \
		 --entry quilc::entry-point

quilc-sdk-base: FOREST_SDK_FEATURE=--eval '(pushnew :forest-sdk *features*)'
quilc-sdk-base: clean clean-cache quilc

# By default, relocate shared libraries on SDK builds
quilc-sdk: FOREST_SDK_LOAD=--load app/src/mangle-shared-objects.lisp
quilc-sdk: quilc-sdk-base

# Don't relocate shared libraries on barebones SDK builds
quilc-sdk-barebones: quilc-sdk-base



quilc-unsafe: system-index.txt
	buildapp --output quilc \
		 --manifest-file system-index.txt \
		 --asdf-path . \
		 --load-system quilc \
		 --compress-core \
		 --entry quilc::%entry-point

test:
	$(QUICKLISP) \
		 --eval "(ql:quickload :cl-quil-tests)" \
		 --eval "(asdf:test-system :cl-quil)" \
		 --eval "(ql:quickload :quilc-tests)" \
		 --eval "(asdf:test-system :quilc)"

test-ccl:
	ccl -n --batch --load $(QUICKLISP_HOME)/setup.lisp \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval "(push (truename \"$(RIGETTI_LISP_LIBRARY_HOME)\") ql:*local-project-directories*)" \
		--eval "(ql:quickload :quilc)" \
		--eval '(quit)'

clean:
	rm -f quilc system-index.txt build-output.log
	rm -f coverage-report/*.html

clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	rm -rf "$(LISP_CACHE)"
