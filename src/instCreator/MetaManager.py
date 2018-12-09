#!/usr/bin/python
#encoding:utf-8
#
#  metaManager manage code location, Macro, and so on
#
#
#
import sys
sys.path.append('../parser')
import os
from instParser import InstParser
from configParser import ConfigParser
#from MetaParser import
class CodeDesc:
    def __init__(self, name, loc, templateName):
        self.name = name
        self.loc = loc
        self.templateName = templateName

codePathDict = {}
codePathDict['baseCpl.F90']    = CodeDesc('baseCpl.F90',     '/baseCpl/src/models/cpl', 'baseCpl_Template.F90')
codePathDict['base_field.F90']  = CodeDesc('base_field.F90', '/baseCpl/src/esm',        'baseField_Template.F90')
codePathDict['deploy_mod.F90'] = CodeDesc('deploy_mod.F90',  '/baseCpl/src/procManage', 'deploymod_Template.F90')
codePathDict['global_var.F90'] = CodeDesc('global_var.F90',  '/baseCpl/src/data_def',   'globalVar_Template.F90')
codePathDict['manage.F90']     = CodeDesc('manage.F90',      '/baseCpl/src/procManage', 'procM_Template.F90')
codePathDict['search_set.py']  = CodeDesc('search_set.py',   '/src/template',           'searchSet_Template.py')
codePathDict['timeCesm.F90']   = CodeDesc('timeCesm.F90',    '/baseCpl/src/timeManage', 'timeCesm_Template.F90')
codePathDict['time_def.F90']   = CodeDesc('time_def.F90',    '/baseCpl/src/data_def',   'timeDef_Template.F90')
codeGenList = [key for key in codePathDict]

class MetaManager:
    codePathDict = codePathDict
    codeGenList = codeGenList
    def __init__(self,absPath):
        self.absPath = absPath
        self.instPath= "" 
        self.nmlfile = ""
        self.confPath = ""
        self.dataPath = ""
        self.dataNml = ""
        self.datarc = ""
        self.inputPath = ""
        # init codePathDict absPath
       
        for key in MetaManager.codeGenList:
            MetaManager.codePathDict[key].loc = self.absPath + MetaManager.codePathDict[key].loc
        

    def setConfigMeta(self, confPath):
        confParser = ConfigParser(confPath)
        confParser.parse()
        confParser.writeNml()
        self.nmlfile = confParser.nmlfile

    def setMacroMeta(self, instSetupPath):
        instParser = InstParser(instSetupPath)
        instParser.parse()
        self.instPath = instParser.instPath
        self.confPath = instParser.confPath
        self.dataPath = instParser.dataPath
        self.dataNml = instParser.datanml
        self.datarc = instParser.datarc
        self.inputPath = instParser.inputPath
