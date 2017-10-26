# Makefile for Docker images

.PHONY: quilc
quilc:
	docker build -t docker.lab.rigetti.com/qcs/quilc -f Dockerfile ../

.PHONY: clean
clean:
	./cleanup.sh