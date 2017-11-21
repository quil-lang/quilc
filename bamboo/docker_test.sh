#!/bin/bash
set -ex
###############################################################################
# Bamboo script for running quilc tests in the quilc-tests Docker container
###############################################################################
docker run --name quilc-tests quilc-tests
