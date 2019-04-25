#!/usr/bin/bash

pushd clm
pushd build
make
popd
popd
cp clm/build/libclm.a ../../lib
cp clm/build/compile/*.mod ../../include
