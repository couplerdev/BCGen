#!/usr/bin/bash

pushd rtm
pushd build
make
popd
popd
cp rtm/build/librtm.a ../../lib
cp rtm/build/compile/*.mod ../../include
