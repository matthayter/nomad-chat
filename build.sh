#! /bin/bash
set -e -x

CABAL_FILE=$(ls *.cabal)
NAME=${CABAL_FILE%.*}
VERSION=$(stack query | grep version | grep -P -o '(\d\.?)+')
PACKAGE=${NAME}-${VERSION}
OUTPUT_DIR=./build/${PACKAGE}
mkdir -p $OUTPUT_DIR
stack setup
stack install --local-bin-path=${OUTPUT_DIR}
cp -r public ${NAME}.service ${OUTPUT_DIR}
pushd build
tar -cf ${PACKAGE}.tar.gz ./${PACKAGE}
