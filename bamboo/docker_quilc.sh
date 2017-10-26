#!/bin/bash
###############################################################################
# Bamboo script for building the quilc Docker image for running the worker
#
# Steps:
# 1) Log in to our internal Docker Harbor
# 2) Build the quilc Docker image
# 3) Push the quilc Docker image to the Docker Harbor
###############################################################################
set -ex

docker login -u bamboo -p Bamboo1@ docker.lab.rigetti.com
make -C ../docker quilc
docker push docker.lab.rigetti.com/qcs/quilc
