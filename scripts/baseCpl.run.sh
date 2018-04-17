#!/bin/bash

RUNPATH=../baseCpl
cd $RUNPATH
mpirun -np 4 ./main
