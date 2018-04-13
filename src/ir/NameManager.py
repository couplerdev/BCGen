#
#       Name Manager Module: only used for manage the name of CoupleEntities , the identity of subroutine name 
#   are guaranteed by CoupleEntities because their names are formed by CoupleEntities at present. In next version
#   we will add support for subroutine name check because in some case user may need some names that are not formed#   by the form of modelxmodel 
# TODO using ir generate names: model name, attrVect, gsMap, sMat, if these name unset
#    
#  reversion history:	
#        2018,3,21        alex: add the module
#        2018,3,26        alex: finish v0.0
#        2018,3,27        alex: add FindName to check whether a name is defined
#!/usr/bin/python
from ir import AttrVect, Mapper, GsMap, Model  


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

    def CheckName(self, objName, objType):
        if objType == "AttrVect":
            return self.__CheckObjVect(objName,objType,0)
        elif objType == "Model":
            return self.__CheckObjVect(objName,objType,1)
        elif objType == "gsMap":
            return self.__CheckObjVect(objName,objType,2)
        elif objType == "sMat":
            return self.__CheckObjVect(objName,objType,3)
        else:
            return self.__CheckObjVect(objName,objType,4)


    def __CheckObjVect(self, objName, objType, index):
        myName = ""
        if objName != "":
            if self.__checkObject[index].has_key(objName):
                errorInfo = "user define " +objType +"name conflict!"
                raise ValueError(errorInfo)
            else:
                self.__checkObject[index][objName]=1
                myName = objName
        else:
            raise ValueError("check \"\" name")
        return False    
		
    def GetName(self,obj, objType):
        name = ""
        if objType == 'AttrVect':
            name = obj.src + "2" + obj.dst + "_" + obj.grid + obj.pes
        elif objType == "Model":
            if obj.name == "":
                raise ValueError("Model name not provided")
            name = obj.name
        elif objType == "GsMap":
            name = "gsMap_" + obj.grid + obj.pes
        elif objType == "Mapper":
            mapType = "C"
            if obj.mapType != "rearr":
                mapType = "S"
            name = "Mapper_"+ mapType + obj.src + "2" +obj.dst 
        else:
            raise TypeError("Undefined type")
        if self.CheckName(name, objType):
            raise ValueError("name conflict")  
        return name     

    def FindName(self, obj):
        if obj.type == "AttrVect":
            return self.__FindObject(obj,0)                  
        else:
            return False

    def __FindObject(self, obj, index):
         if index<0 or index >= self.__totalDict:
             raise ValueError("index out of range")
         if self.__checkObject[index].has_key(obj.name):
             return True
         else:
             return False
