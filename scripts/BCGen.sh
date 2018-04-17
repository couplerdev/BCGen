#!/bin/bash
MAINPATH=../src/template
CODEPATH=../baseCpl
PYTHON=python2.7
OBJFILE=baseCpl.F90 proc_def.F90 manager.F90

if [ $1 -eq 0 ]; then
    echo '---------------BCGen v1.0 by Alex---------------------------'
    echo '----------A software support CESM-like applications---------'
    echo '--------------A generator for baseCpl-----------------------'
elif [ $1 -eq 1 ]; then
    cd $MAINPATH
    $PYTHON main.py
    ./move.sh
elif [ $1 -eq 2 ]; then
    cd $MAINPATH
    rm baseCpl.F90
    rm proc_def.F90
    rm manage.F90
    $PYTHON main.py
    ./move.sh
elif [ $1 -eq -1 ]; then
    echo 'help u'
else
    echo 'please input ./BCGen.sh help for detail'
fi
