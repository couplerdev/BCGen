#encoding:utf-8
#!/usr/bin/python
import sys
sys.path.append('../ir')
from ir import CoupleEntity, Subroutine
sys.path.append('../ErrorHandle')
from ErrorHandle import *

class FakeVariables:
    def __init__(self, varType, varName, grid):
        self.__varType = varType
        self.__varName = varName 
        self.__grid = grid
        self.__method = {}
        self.__phases = ['init', 'final', 'run']

    def addMethod(self, phase, subrt):
        if phase not in self.__phases:
            raise NoneProperValueError('FakeVariable add method the phases')
        if phase not in self.__method:
            self.method[phase] = []
        self.__method[phase].append(subrt)

    @property
    def grid(self):
        return self.__grid

    @property
    def name(self):
        return self.__varName

    @property
    def varType(self):
        return self.__varType

    @property
    def method(self):
        return self.__method

class FakeModel(CoupleEntity):

    def __init__(self, name):
        super(FakeModel, self).__init__(name=name, _type='FakeModel')
        self.__deps = []
        self.__variables = {}
        self.__variablesType = ['attrVect']
        self.__flds = []
        #self.__fldsType = ['states','fluxes', 'fields']

    def addVariables(self, var):
        if var.varType not in self.__variablesType:
            raise NoneProperValueError('Fake Model set var.varType not proper')
        self.__variables[var.name] = var

    def addDeps(self, dep):
        self.__deps.append(dep)

    @property
    def deps(self):
        return self.__deps

    @deps.setter
    def deps(self, _deps):
        self.__deps = _deps

    @property
    def variables(self):
        return self.__variables

    @property
    def flds(self):
        return self.__flds
 
    @flds.setter
    def flds(self, _flds):
        self.__flds = _flds
       
