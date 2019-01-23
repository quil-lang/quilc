COMMIT_HASH=$(shell git rev-parse --short HEAD)
LISP_CACHE ?= $(HOME)/.cache/common-lisp
RIGETTI_LISP_LIBRARY_HOME=../
SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)' \
	--eval '(push :hunchentoot-no-ssl *features*)' \
	--eval "(push (truename \"$(RIGETTI_LISP_LIBRARY_HOME)\") ql:*local-project-directories*)"
QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp
UNAME_S=$(shell uname -s)
ZMQ_REPO=https://download.opensuse.org/repositories/network:/messaging:/zeromq:/release-stable/xUbuntu_16.04/

all: quilc

###############################################################################
# SETUP
###############################################################################

$(QUICKLISP_SETUP):
	mkdir -p $(QUICKLISP_HOME)
	curl -o $(QUICKLISP_HOME)/quicklisp-bootstrap.lisp \
		$(QUICKLISP_BOOTSTRAP_URL)
	$(SBCL) --load $(QUICKLISP_HOME)/quicklisp-bootstrap \
		--eval "(quicklisp-quickstart:install :path \"$(QUICKLISP_HOME)\")"

system-index.txt: $(QUICKLISP_SETUP)
	$(QUICKLISP) \
		$(FOREST_SDK_FEATURE) \
		--eval '(ql:quickload "quilc")' \
		--eval '(ql:write-asdf-manifest-file "system-index.txt")'

###############################################################################
# DEPENDENCIES
###############################################################################

dump-version-info:
	sbcl --noinform --non-interactive \
		--eval '(format t "~A ~A" (lisp-implementation-type) (lisp-implementation-version))' \
		--eval '(print (ql-dist:find-system "alexa"))' \
		--eval '(print (ql-dist:find-system "magicl"))' \
		--eval '(print (ql-dist:find-system "rpcq"))' \
		--eval '(terpri)' --quit

install-test-deps:
ifeq ($(UNAME_S),Linux)
ifeq ($(shell sed -n "s/^ID=//p" /etc/os-release),debian)
	echo "deb $(ZMQ_REPO) ./" >> /etc/apt/sources.list
	curl $(ZMQ_REPO)/Release.key | apt-key add -
	apt-get install -y git libblas-dev libffi-dev liblapack-dev libzmq3-dev
else
	echo "Centos-based platforms unsupported"
endif
else
	echo "Non-Linux-based platforms unsupported"
endif

install-build-deps: install-test-deps
	sbcl --noinform --non-interactive \
		--eval '(ql:quickload "buildapp")' \
		--eval '(buildapp:build-buildapp "/usr/local/bin/buildapp")'

###############################################################################
# BUILD
###############################################################################

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

DOCKER_BUILD_TARGET=all
DOCKER_TAG=rigetti/quilc:$(COMMIT_HASH)
.PHONY: docker
docker: Dockerfile
	docker build --build-arg build_target=$(DOCKER_BUILD_TARGET) \
		-t $(DOCKER_TAG) .

docker-sdk: DOCKER_BUILD_TARGET=quilc-sdk
docker-sdk: DOCKER_TAG=quilc-sdk
docker-sdk: docker

docker-sdk-barebones: DOCKER_BUILD_TARGET=quilc-sdk-barebones
docker-sdk-barebones: DOCKER_TAG=quilc-sdk-barebones
docker-sdk-barebones: docker


###############################################################################
# TEST
###############################################################################

test:
	$(QUICKLISP) \
		--eval "(ql:quickload :cl-quil-tests)" \
		--eval "(asdf:test-system :cl-quil)" \
		--eval "(ql:quickload :quilc-tests)" \
		--eval "(asdf:test-system :quilc)"

test-cl-quil:
	$(QUICKLISP) \
		--eval "(ql:quickload :cl-quil-tests)" \
		--eval "(asdf:test-system :cl-quil)"

test-quilc:
	$(QUICKLISP) \
		--eval "(ql:quickload :quilc-tests)" \
		--eval "(asdf:test-system :quilc)"

test-ccl:
	ccl -n --batch --load $(QUICKLISP_HOME)/setup.lisp \
		--eval '(push (truename ".") asdf:*central-registry*)' \
		--eval "(push (truename \"$(RIGETTI_LISP_LIBRARY_HOME)\") ql:*local-project-directories*)" \
		--eval "(ql:quickload :quilc)" \
		--eval '(quit)'

###############################################################################
# CLEAN
###############################################################################

clean:
	rm -f quilc system-index.txt build-output.log
	rm -f coverage-report/*.html

clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	rm -rf "$(LISP_CACHE)"
