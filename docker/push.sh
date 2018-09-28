#!/bin/bash
set -ex
###############################################################################
# Script for tagging and pushing Docker images to our internal Harbor
###############################################################################
GIT_BRANCH_NAME=`git name-rev --name-only HEAD | cut -d~ -f1 | tr "/" "-"`
if [ "$GIT_BRANCH_NAME" = "master" ] && [ -z "$bamboo_DOCKER_IMAGE_TAG" ]
then
    echo "Standard build on master, so using the 'latest' tag"
    bamboo_DOCKER_IMAGE_TAG="latest"
elif [ "$GIT_BRANCH_NAME" = "master" ]
then
    STABLE="stable"
    echo "Custom release build on master, so using overridden bamboo tag and updating stable"
    docker tag quilc docker.lab.rigetti.com/qcs/quilc:$STABLE
    docker push docker.lab.rigetti.com/qcs/quilc:$STABLE
elif [ "$GIT_BRANCH_NAME" != "master" ] && [ -z "$bamboo_DOCKER_IMAGE_TAG" ]
then
    echo "Standard build on a branch, so using the branch name as tag"
    bamboo_DOCKER_IMAGE_TAG="$GIT_BRANCH_NAME"
elif [ "$GIT_BRANCH_NAME" != "master" ]
then
    echo "Custom builds on branches not supported, falling back to branch name as tag"
    bamboo_DOCKER_IMAGE_TAG="$GIT_BRANCH_NAME"
fi
echo "Tagging Docker image with: $bamboo_DOCKER_IMAGE_TAG"
docker tag quilc docker.lab.rigetti.com/qcs/quilc:$bamboo_DOCKER_IMAGE_TAG
docker push docker.lab.rigetti.com/qcs/quilc:$bamboo_DOCKER_IMAGE_TAG
