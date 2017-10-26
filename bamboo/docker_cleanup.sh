#!/bin/bash
###############################################################################
# Bamboo script for cleaning up Docker resources after a build
#
# Steps:
# 1) Stop and remove any existing quilc container
# 2) Remove all dangling Docker images and volumes from the worker
###############################################################################
set -ex

docker rm -f quilc || true
docker system prune -af || true
