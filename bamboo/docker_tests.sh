#!/bin/bash
###############################################################################
# Bamboo script for running quilc tests in the quilc-tests Docker container
#
# Steps:
# 1) Log in to our internal Docker Harbor
# 2) Build the quilc-tests Docker image
# 3) Run the quilc-tests Docker image to execute the quilc tests
# 4) Push the quilc-tests Docker image to the Docker Harbor
###############################################################################
set -ex

docker login -u bamboo -p Bamboo1@ docker.lab.rigetti.com
make -C ../docker quilc-tests
docker run --name quilc-tests docker.lab.rigetti.com/qcs/quilc-tests
docker push docker.lab.rigetti.com/qcs/quilc-tests
