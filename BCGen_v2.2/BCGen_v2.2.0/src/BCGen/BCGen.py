<<<<<<< HEAD
#
=======

>>>>>>> master
#     This module use as the command line tool, user can use it by
#     >/BCGen --version
#     There are many optional functions for users
#
#  reversion history:
#        2018,3,26        alex: add the module
#!/usr/bin/python
import argparse
<<<<<<< HEAD

class BCGen:
    def __init__(self):
        pass




if __name__ == "__main__":
=======
import sys
sys.path.append('../ir')
sys.path.append('../parser')
sys.path.append('../template')
sys.path.append('../ErrorHandle')
from parserMod import Parser
from ErrorHandle import *
#from parser import Parser
from ir import Model, AttrVect, Mapper, GsMap
#from parser import Parser
from NameManager import NameManager
from irCheetah import *

if __name__ == "__main__":
    models = {}
    attrVectCouple = {}
    Mapper = {}
    subroutine = {}
    couplerFile = "../../composing/coupler.xml"
    modelFile = "../../composing/models.xml"
    scheduleFile = "../../composing/schedule.xml"

    parser = Parser(couplerFile=couplerFile, modelFile=modelFile, scheduleFile=scheduleFile)
    #parser.parse()
    parser.modelsParse()    

    models = parser.models
    attrVectCouple = parser.attrVectCouple
    subroutine = parser.subroutine
    Mapper = parser.sMapper
    for av in models["a"].attrVects:
            print av.name
    
>>>>>>> master