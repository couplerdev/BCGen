#!/usr/bin/python
# @docCreator.py create the doc needed for 
# case to identify its loc and its metadata
import os
import sys
sys.path.append("../../parser")
sys.path.append("../../template")
from instParser import InstParser

def writeDoc(inst):
    docKV = {
       "case_name":"default name",
       "case_dir":"",
       "namelist":"", 
       "case_desc":"no desc",
       "baseCplDir":""
             }
    currDir = os.getcwd()
    baseCplDir = currDir+"/../../../baseCpl"
    os.chdir(baseCplDir)
    baseCplDir = os.getcwd()
    os.chdir(currDir)
    docKV["baseCplDir"] = baseCplDir

    f = open("descCase","w")
    for k in docKV: 
        s = k+":"+docKV[k]+"\n"
        f.write(s)
    

    f.close()


if __name__ =="__main__":
    writeDoc(None)

    

