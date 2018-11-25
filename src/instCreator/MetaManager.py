#!/usr/bin/python
#encoding:utf-8
#
#  metaManager manage code location, Macro, and so on
#
#
#
import sys
sys.path.append('../parser')
from instParser import InstParser
from configParser import ConfigParser
#from MetaParser import
class CodeDesc:
    def __init__(self, name, loc, templateName):
        self.name = name
        self.loc = loc
        self.templateName = templateName

codePathDict = {}
codePathDict['baseCpl.F90']    = CodeDesc('','','baseCpl_Template.F90')
codePathDict['baseField.F90']  = CodeDesc('','','baseField_Template.F90')
codePathDict['deploy_mod.F90'] = CodeDesc('','', 'deploymod_Template.F90')
codePathDict['global_var.F90'] = CodeDesc('','','globalVar_Template.F90')
codePathDict['manage.F90']     = CodeDesc('','','procM_Template.F90')
codePathDict['search_set.py']  = CodeDesc('','','searchSet_Template.py')
codePathDict['timeCesm.F90']   = CodeDesc('','','timeCesm_Template.F90')
codePathDict['time_def.F90']   = CodeDesc('','','timeDef_Template.F90')

class MetaManager:
    codePathDict = codePathDict
    def __init__(self):
        self.absPath = ""
        self.instPath= "" 
        self.nmlfile = ""
        self.confPath = ""
        self.dataPath = ""
        self.dataNml = ""

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
