#!/bin/bash
set -ex
###############################################################################
# Script for tagging and pushing Docker images to our internal Harbor
###############################################################################
if [ -z "$bamboo_DOCKER_IMAGE_TAG" ]; then bamboo_DOCKER_IMAGE_TAG="latest"; fi
echo "Tagging Docker image with: $bamboo_DOCKER_IMAGE_TAG"
docker tag quilc docker.lab.rigetti.com/qcs/quilc:$bamboo_DOCKER_IMAGE_TAG
docker push docker.lab.rigetti.com/qcs/quilc:$bamboo_DOCKER_IMAGE_TAG
