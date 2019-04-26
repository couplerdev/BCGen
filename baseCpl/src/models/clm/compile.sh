#!/usr/bin/bash

pushd clm
pushd build
make
popd
popd
cp clm/build/liblnd.a ../../lib
cp clm/build/compile/*.mod ../../include
