# Makefile for Docker images

BASE_TARGET=quilc

.PHONY: all
all: quilc-base quilc-tests quilc

.PHONY: quilc-base
quilc-base: Dockerfile
	docker build --build-arg build_target=$(BASE_TARGET) -t quilc-base -f Dockerfile.base ../../

.PHONY: quilc-sdk
quilc-sdk: BASE_TARGET=quilc-sdk
quilc-sdk: quilc-base

.PHONY: quilc-tests
quilc-tests: Dockerfile.test
	docker build -t quilc-tests -f Dockerfile.test ../../

.PHONY: quilc
quilc: Dockerfile
	docker build -t quilc -f Dockerfile ../../

.PHONY: test
test: test.sh
	./test.sh

.PHONY: push
push: push.sh
	./push.sh

.PHONY: clean
clean: clean.sh
	./clean.sh
