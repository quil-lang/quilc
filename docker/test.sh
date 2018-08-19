#!/bin/bash
set -ex
###############################################################################
# Script for running dockerized quilc tests
###############################################################################
docker run --rm --name quilc-tests quilc-tests
