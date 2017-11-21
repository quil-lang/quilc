#!/bin/bash
set -ex
###############################################################################
# Script for cleaning up quilc-related Docker resources
###############################################################################
docker rm -f quilc || true
docker rm -f quilc-tests || true
docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/quilc) || true
docker rm $(docker ps -a -q --filter=ancestor=docker.lab.rigetti.com/qcs/quilc-tests) || true
docker rm $(docker ps -a -q --filter=ancestor=quilc) || true
docker rm $(docker ps -a -q --filter=ancestor=quilc-tests) || true
docker rmi docker.lab.rigetti.com/qcs/quilc || true
docker rmi docker.lab.rigetti.com/qcs/quilc-tests || true
docker rmi quilc || true
docker rmi quilc-tests || true
