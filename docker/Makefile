# Makefile for Docker images

.PHONY: all
all: quilc-base quilc-tests quilc

.PHONY: quilc-base
quilc-base: Dockerfile
	docker build -t quilc-base -f Dockerfile.base ../../

.PHONY: quilc-tests
quilc-tests: Dockerfile.test
	docker build -t quilc-tests -f Dockerfile.test ../../

.PHONY: quilc
quilc: Dockerfile
	docker build -t quilc -f Dockerfile ../../

.PHONY: clean
clean: clean.sh
	./clean.sh
