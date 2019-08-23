#!/usr/bin/bash

pushd clm
pushd build
make
popd
popd
cp -f clm/build/liblnd.a ../../lib
cp -f clm/build/compile/*.mod ../../include
