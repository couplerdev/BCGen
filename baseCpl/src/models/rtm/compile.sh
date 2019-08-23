#!/usr/bin/bash

pushd rtm
pushd build
make
popd
popd
cp -f rtm/build/librof.a ../../lib
cp -f rtm/build/compile/*.mod ../../include
