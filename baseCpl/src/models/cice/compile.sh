#!/usr/bin/bash

pushd cice
pushd build
make
popd
popd
cp -f cice/build/libice.a ../../lib
cp -f cice/build/compile/*.mod ../../include
