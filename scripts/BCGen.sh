#!/bin/bash
HOMEPATH=`cd .. && pwd`
MAINPATH=$HOMEPATH/src/template
PYTHON=python2.7
OBJFILE=baseCpl.F90 proc_def.F90 manager.F90
BASECPLPATH=$HOMEPATH/baseCpl
SCRIPTPATH=$HOMEPATH/scripts
#
#  we need add some mechanism for user to choose which file to generate
#  mainly for deploy_mod.F90
#
checkBaseCpl(){
    echo "check baseCpl in directory: $1"
    cd $1
    dirCnt=`ls | wc -l`
    if [ $dirCnt -eq 0 ];then
        cd ..
        sudo rm -r baseCpl
     	git clone https://github.com/NeoGeon/baseCpl.git
    fi 
    cd -
}

if [ $1 -eq 0 ]; then
    echo '---------------BCGen v1.0 by Alex---------------------------'
    echo '----------A software support CESM-like applications---------'
    echo '--------------A generator for baseCpl-----------------------'
elif [ $1 -eq 1 ]; then
    cd $MAINPATH
    echo $?
    $PYTHON main.py
    checkBaseCpl $BASECPLPATH
    cd $MAINPATH
    ./move.sh
elif [ $1 -eq 2 ]; then
    cd $MAINPATH
    rm baseCpl.F90
    rm proc_def.F90
    rm manage.F90
    $PYTHON main.py
    checkBaseCok $BASECPLDIR
    cd $MAINPATH
    ./move.sh
elif [ $1 -eq -1 ]; then
    echo 'help u'
else
    echo 'please input ./BCGen.sh help for detail'
fi
