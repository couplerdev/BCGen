#!/usr/bin/bash

pushd cice
pushd build
make
popd
popd
cp cice/build/libcice.a ../../lib
cp cice/build/compile/*.mod ../../include
