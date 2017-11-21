#!/bin/bash
set -ex
###############################################################################
# Bamboo script for building the Docker image(s) in a docker Makefile
###############################################################################
make -C ../docker
