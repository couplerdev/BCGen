#
#       Name Manager Module
# TODO dict to check whether exists conflict names
# TODO using ir generate names: model name, attrVect, gsMap, sMat, if these name unset
#    
#  reversion history:	
#        2018,3,21        alex: add the module
#        2018,3,26        alex: finish v0.0
#        2018,3,27        alex: add FindName to check whether a name is defined
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
        self.__totalDict = 5
	self.__checkObject = [self.__attrVectDict, self.__modelDict, self.__gsMapDict, \
                              self.__sMatDict, self.__mapperDict]

    def CheckName(self, obj):
        if obj.type == "AttrVect":
            return self.__CheckObjVect(obj,0)
        elif obj.type == "Model":
            return self.__CheckObjVect(obj,1)
        elif obj.type == "gsMap":
            return self.__CheckObjVect(obj,2)
        elif obj.type == "sMat":
            return self.__CheckObjVect(obj,3)
        else:
            return self.__CheckObjVect(obj,4)


    def __CheckObjVect(self, obj, index):
        myName = ""
        if obj.name != "":
            if self.__checkObject[index].has_key(obj.name):
                errorInfo = "user define " +obj.type +"name conflict!"
                raise ValueError(errorInfo)
            else:
                self.__checkObject[index][obj.name]=1
                myName = obj.name
        else:
            myName = self.GetName(obj, obj.type)
            if self.__checkObject[index].has_key(myName)
                errorInfo = "generate same "+ obj.name + "name. please check your config"
                raise ValueError(errorInfo)
            else:
                self.__checkObject[index][obj.name]=1
        return myName    
		
    def GetName(self,obj, objType):
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

    def FindName(self, obj):
        if obj.type == "AttrVect":
            return __FindObject(obj,0)                  
        else:
            return False

    def __FindObject(self, obj, index):
         if index<0 .or. index >= self.__totalDict:
             raise ValueError("index out of range")
         if self.__checkObject[index].has_key(obj.name):
             return True
         else:
             return False
