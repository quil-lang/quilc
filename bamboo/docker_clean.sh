#!/bin/bash
set -ex
###############################################################################
# Bamboo script for cleaning up Docker resources after a build
#
# Steps:
# 1) Remove containers and images as specified by the docker Makefile's clean target
# 2) Remove all dangling Docker images and volumes from the worker
###############################################################################
make -C ../docker clean
docker system prune -af || true
