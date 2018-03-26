#
#       Name Manager Module
# TODO dict to check whether exists conflict names
# TODO using ir generate names: model name, attrVect, gsMap, sMat, if these name unset
#    
#  reversion history:	
#        2018,3,21        alex add the module
#!/usr/bin/python
from ir import AttrVect, mapper, gsMap, sMat, Model  


class NameManager:
    __slots__ = ['__attrVectDict','__mapperDict', '__gsMapDict', '_sMatDict',\
                     '__modelNameDict']

    def __init__(self):
        self.__attrVectDict = {}
        self.__mapperDict = {}
        self.__gsMapDict = {}
        self.__sMatDict = {}
        self.__modelDict = {}
	self.__checkObject = [self.__attrVectDict, self.__modelDict, self.__gsMapDict, \
                              self.__sMatDict, self.__mapperDict]

    def CheckName(self, obj):
        if obj.type == "AttrVect":
            return CheckObjVect(obj,0)
        elif obj.type == "Model":
            return CheckObjVect(obj,1)
        elif obj.type == "gsMap":
            return CheckObjVect(obj,2)
        elif obj.type == "sMat":
            return CheckObjVect(obj,3)
        else:
            return CheckObjVect(obj,4)


    def CheckObjVect(self, obj, index):
        myName = ""
        if obj.name != "":
            if self.__checkObject[index].has_key(obj.name):
                errorInfo = "user define " +obj.type +"name conflict!"
                raise ValueError(errorInfo)
            else:
                self.__checkObject[index][obj.name]=1
                myName = obj.name
        else:
            myName = GetName(obj, obj.type)
            if self.__checkObject[index].has_key(myName)
                errorInfo = "generate same "+ obj.name + "name. please check your config"
                raise ValueError(errorInfo)
            else:
                self.__checkObject[index][obj.name]=1
        return myName    
		
    def GetName(obj, objType):
        name = ""
        if objType == 'AttrVect':
            name = obj.src + "2" + obj.dst + "_" + obj.grid + obj.pes
        elif objType == "Model":
            name = obj.name
        elif objType == "gsMap":
            name = "gsMap_" + self.grid + self.pes
        elif objType == "Mapper":
            mapType = "C"
            if objType.mapType != "rearr":
                mapType = "S"
            name = "Mapper_"+ mapType + objType.src + "2" +objType.dst 
        else:
            name = obj.name
        return name     
                
