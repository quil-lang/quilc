# Makefile for Docker images

.PHONY: all
all: quilc quilc-tests

.PHONY: quilc
quilc: Dockerfile
	docker build -t quilc -f Dockerfile ../

.PHONY: quilc-tests
quilc-tests: Dockerfile.test
	docker build -t quilc-tests -f Dockerfile.test ../

.PHONY: clean
clean: clean.sh
	./clean.sh
