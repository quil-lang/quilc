#!/bin/bash
###############################################################################
# Script for cleaning up quilc-related Docker resources
###############################################################################
set -ex

docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/quilc) || true
docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/quilc-tests) || true

docker rmi docker.lab.rigetti.com/qcs/quilc || true
docker rmi docker.lab.rigetti.com/qcs/quilc-tests || true
