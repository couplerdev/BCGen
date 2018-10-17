#!/bin/bash
BaseCplSrc="../../baseCpl/src"

cp ./baseCpl.F90 ../../baseCpl/src/model/cpl/
cp ./proc_def.F90 ../../baseCpl/src/data_def/
cp ./manage.F90 ../../baseCpl/src/procManage/
cp ./deploy_mod.F90 ../../baseCpl/src/procManage/
cp ./timeM.F90 ../../baseCpl/src/timeManage/
echo 'code moved'
