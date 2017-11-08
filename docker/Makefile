# Makefile for Docker images

.PHONY: quilc
quilc:
	docker build -t docker.lab.rigetti.com/qcs/quilc -f Dockerfile ../

.PHONY: quilc-tests
quilc-tests:
	docker build -t docker.lab.rigetti.com/qcs/quilc-tests -f Dockerfile.test ../

.PHONY: clean
clean:
	./cleanup.sh
