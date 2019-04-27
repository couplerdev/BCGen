#!/usr/bin/python
#
#   strategy pattern for a model manager
#
import sys, os
sys.path.append('../ErrorHandle')
from ErrorHandle import NoneProperValueError
import xml.etree.ElementTree as ET
from xml.dom.minidom import Document


innerModelSets = "../../composing/innerModels.xml"

# pair {model name, model loc}
modelSetsDict = {'cam':'../../composing/camDesc.xml'}

def getModelSetDict():
    tree = ET.parse(innerModelSets)
    models = tree.find('models')    
    for model in models:
        modelName = model.find('name').text
        modelLoc = model.find('location').text
        modelSetsDict[modelName]=modelLoc

getModelSetDict()

class NormalModel:

    # need error handling
    def __init__(self, modelName):
        self.modelLoc = modelSetsDict[modelName]  
        self.modelName = modelName

    def createModelInst():
        pass     

class ConfModel:
    def __init__(self, modelName, fileDesc, metaManager):
       self.modelName = modelName
       self.fileDesc = fileDesc
       self.metaManager = metaManager
       print 'in conf____',self.fileDesc

    def createModelInst(self):
        tree = ET.parse(self.fileDesc)
        # confModel
        packageDir = tree.find('packageDir').text
        conf = tree.find('configure')
        confScripts = conf.find("script").text or ''
        confScripts = packageDir+"/"+confScripts
        workDir = packageDir+"/"+conf.find("workDir").text
        args = ""
        argsIn = conf.find('args')
        if argsIn != None:
            for argIn in argsIn:
                arg = argIn.text
                args+=arg+" "
        cmdConf = confScripts+"   "+args
        currDir = os.getcwd()
        os.chdir(workDir)
        os.system(cmdConf)
        os.chdir(currDir)
        # build namelist
        
        bldNml = tree.find("build-namelist")
        bldNmlScripts = bldNml.find("script").text or ' '
        args = ""
        argsIn = bldNml.find('args')
        if argsIn != None:
            for argIn in argsIn:
                arg = " "
                arg = argIn.text or 'none'
                argOpts = " "
                if "type" in argIn.attrib:
                    if argIn.attrib["type"]=="dataloc":
	                argOpts = self.metaManager.inputPath
                    elif argIn.attrib["type"]=="outloc":
                        argOpts = self.metaManager.instPath+"/conf"
                    else:
                        raise NoneProperValueError("in ConfModel.createModelInst:")
                args += arg + " "+argOpts +" "
        os.chdir(workDir)
        cmdBld = bldNmlScripts + "   "+ args
        print cmdBld
        os.system(cmdBld)
        os.chdir(currDir) 
        #copy to target dict
        
       

class ModelManager:
    def __init__(self, dct):
        self.dct = dct

    def createModelInst(self, modelType, fileName="None", modelName="atm"):
        if modelType == 0:
            model = NormalModel(modelName)
            model.createModelInst()
        elif modelType == 1:
            if fileName == "None":
                raise NoneProperValueError("in ModelManager.creatModelInst when modelType ==1:")
            model = ConfModel(modelName, fileName)
            model.createModelInst()
        else:
            raise NoneProperValueError("in ModelManager.createModel when modelType not find:")

if __name__ == "__main__":
    from MetaManager import MetaManager
    modelName = "cam"
    fileName = modelSetsDict[modelName]
    metaManager = MetaManager('../../')
    model = ConfModel(modelName, fileName, metaManager)
    model.createModelInst()
