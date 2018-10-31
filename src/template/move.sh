#!/bin/bash
BaseCplSrc="../../baseCpl/src"

cp ./baseCpl.F90 ../../baseCpl/src/model/cpl/
cp ./manage.F90 ../../baseCpl/src/procManage/
cp ./deploy_mod.F90 ../../baseCpl/src/procManage/
cp ./global_var.F90 ../../baseCpl/src/data_def
cp ./timeCesm.F90 ../../baseCpl/src/timeManage/
cp ./time_def.F90 ../../baseCpl/src/data_def
echo 'code moved'
