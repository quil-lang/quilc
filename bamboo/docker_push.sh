#!/bin/bash
set -ex
###############################################################################
# Bamboo script for tagging and pushing a Docker image to our internal Harbor
#
# Steps:
# 1) Log in to our internal Docker Harbor (docker.lab.rigetti.com) as user bamboo
# 2) If the bamboo_DOCKER_IMAGE_TAG variable is empty or unset, set to latest
# 3) Print the tag to use for the Docker image(s) (taken from global variables)
# 4) Assign the Docker image(s) our internal repository name and the tag
# 5) Push the tagged Docker image(s) to the Docker Harbor given by the repository
###############################################################################
docker login -u bamboo -p Bamboo1@ docker.lab.rigetti.com
if [ -z "$bamboo_DOCKER_IMAGE_TAG" ]; then bamboo_DOCKER_IMAGE_TAG="latest"; fi
echo "Tagging Docker image with: $bamboo_DOCKER_IMAGE_TAG"
docker tag quilc docker.lab.rigetti.com/qcs/quilc:$bamboo_DOCKER_IMAGE_TAG
docker tag quilc-tests docker.lab.rigetti.com/qcs/quilc-tests:$bamboo_DOCKER_IMAGE_TAG
docker push docker.lab.rigetti.com/qcs/quilc:$bamboo_DOCKER_IMAGE_TAG
docker push docker.lab.rigetti.com/qcs/quilc-tests:$bamboo_DOCKER_IMAGE_TAG
