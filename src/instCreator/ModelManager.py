#!/usr/bin/python
#
#   strategy pattern for a model manager
#
from ErrorHandling import NoModelTypeError
import xml.etree.ElementTree as ET
from xml.dom.minidom import Document




innerModelSets = "../../composing/innerModels.xml"

# pair {model name, model loc}
modelSetsDict = {}

def getModelSetDict():
    tree = ET.parse(self.xmlFile)
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
    def __init__(self, modelName, fileDesc):
       self.modelName = self.modelName
       self.fileDesc = fileDesc

    def createModelInst(self):
        tree = ET.parse(self.fileDesc)
        # confModel
        conf = tree.find('configure')
        confScripts = conf.find("script").text
        args = ""
        argsIn = conf.find('args')
        for argIn in argsIn:
            arg = argIn.find('arg').text
            args+=arg+" "
        cmdConf = confScript+"   "+args
        os.system(cmdConf)
        # build namelist
        bldNml = tree.find("build-namelist")
        bldNmlScripts = bldNml.find("script").text
        args = ""
        argsIn = conf.find('args')
        for argIn in argsIn:
            arg = argIn.find('arg').text
            args += arg+" "
        cmdBld = bldNml + "   "+ args
        os.system(cmdBld)
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
                raise NotProperValueError("in ModelManager.creatModelInst when modelType ==1:")
            model = ConfModel(modelName, fileName)
            model.createModelInst()
        else:
            raise NoModelTypeError("modelType not supported")


