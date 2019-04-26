#!/usr/bin/bash

pushd rtm
pushd build
make
popd
popd
cp rtm/build/librof.a ../../lib
cp rtm/build/compile/*.mod ../../include
