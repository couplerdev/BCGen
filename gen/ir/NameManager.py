#
#       Name Manager Module
# TODO dict to check whether exists conflict names
# TODO using ir generate names: model name, attrVect, gsMap, sMat, if these name unset
#    
#  reversion history:	
#        2018,3,21        alex add the module
#!/usr/bin/python
from ir import AttrVect
from ir import Model
from ir import Mapper
  


class NameManager:
    __slots__ = ['__attrVectDict','__mapperDict', '__gsMapDict', '_sMatDict',\
                     '__modelNameDict']

    def __init__(self):
        self.__attrVectDict = {}
        self.__mapperDict = {}
        self.__gsMapDict = {}
        self.__sMatDict = {}
        self.__modelDict = {}
	
    
    def CheckAttrVect(self, attrVect):
        myName = ""
        if attrVect.name != "":
            if self.__attrVectDict.has_key(attrVect.name):
                raise ValueError("user define attrVect name conflict!")
            else:
                self.__attrVectDict[attrVect.name]=1
                myName = attrVect.name
        else:
            myName = attrVect.src + "2" + attrVect.dst + "_" + attrVect.grid + attrVect.pes
            if self.__attrVectDict.has_key(myName):
                # TODO may add some way to generate different name, but I think we should 
                # TODO just raise error
                raise ValueError("generate same attrVect name, please check your config")
            else:
                self.__attrVectDict[attrVect.name]=1
        return myName
		
    def CheckModelName(self, Model):
        myName = ""
        if Model.name != "":
            if self.__modelDict.has_key(model.name):
                raise ValueError("user define model name conflict")
            else:
                self.__modelDict[Model.name]=1
                myName = Model.name
        else:
            myName = model.name
            if self.__modelDict.has_key(myName):
                # TODO
                # TODO
                raise ValueError("generate same model name, please check your config")
            else:
                self.__modelDict[Model.name]=1
        return myName

    def CheckMapperName(self, Mapper):
                
