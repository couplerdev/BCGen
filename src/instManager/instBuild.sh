#!/bin/bash

echo '-------------build your apps-----------'
models=`ls -l ./models | grep ^d | grep -v 'cpl' | awk '{print $9}'`
echo $models
cd ./models
for dir in $models;
do
	echo $dir;
	cd $dir;
	make;
	cd -;
done
cd cpl
make
mv main ../../bin
