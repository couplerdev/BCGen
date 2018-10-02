#
#    Intermediate representation : include Subroutine class and child class ModelSubroutine
#    class MergeSubroutine; CoupleEntity Class, its child class Model, AttrVect, sMat, Mapper,
#    gsMap, parser  parses xml and generates the intermediate representation 
#
#    reversion history:
#        2018,3,3              alex: add the module
#        2018,3,27             alex: finish version v0.0
#!/usr/bin/python

# intermediate representation
import sys
sys.path.append("../ErrorHandle")
from ErrorHandle import *
from Datatype import *

class Subroutine(object):
    __slots__=['__subroutineName','__argList', '__lineCharacter']
    def __init__(self, subroutineName="func",argList=[], lineCharacter=50):
	self.__subroutineName = subroutineName
	self.__argList = argList
        self.__lineCharacter = lineCharacter

    @property
    def name(self):
        return self.__subroutineName

    @property
    def argList(self):
        return self.__argList
    @argList.setter
    def argList(self, argListValue):
        self.__argList = argListValue

    def append(self, arg):
        if not isinstance(arg, str):
            raise  TypeError("arg not a string")
        print type(self.__argList)
        self.__argList.append(arg)

    def toString(self, subroutineName, argList):
	string = subroutineName
        string += "("
        lenString = len(string)
        lenSpace = lenString
        for arg in argList:
            argStr = arg + ", "
            string += argStr
            lenString += len(argStr)
            if lenString > self.__lineCharacter:
                lenString =lenSpace
                string += "&\n" + lenSpace*' '
        #print string
        if string[-2:] == "&\n":
            string = string[:-4] + ")"
        else:
            string = string[:-2]
            string += ")"
        return string 
           
class MergeSubroutine(Subroutine):
    __slots__=["__name","default","__argList","__subroutineName", "__atype", "__lineCharacter"]	
    def __init__(self, subroutineName="mrg", pattern=True, name=""):
        super(MergeSubroutine, self).__init__()
        self.__name = name
        self.__subroutineName = str(self.__name)
        self.__argList = []
        self.__atype = "Mrg"
        self.__lineCharacter = 50

    @property
    def name(self): 
        return self.__subroutineName
 
    @property
    def atype(self):
        return self.__atype
    #def append(self, arg):
    #    super(MergerSubroutine, self).append(arg)

    #def toString(self):
    #    return super(MergeSubroutine, self).toString(self.__subroutineName, self.__argList)

class ModelSubroutine(Subroutine):

    __slots__=["__name","default","__argList","__wrapper", "__subroutineName", "__type"]
    def __init__(self, pattern=True, name="", wrapper="mct", subroutineName="init"):
        super(ModelSubroutine, self).__init__()
        self.__name = name
        self.__wrapper = wrapper  # identify the wrapper API
        self.__type = subroutineName
        self.__subroutineName = self.__name + self.__type+ "_" + self.__wrapper
        self.default = pattern
	self.__argList = []		

    @property
    def subroutineName(self):
        return self.__subroutineName
    @subroutineName.setter
    def subroutineName(self, name):
        if isinstance(name, str):
            self.__subroutineName = name
        else:
            raise TypeError("name not str type")

    @property
    def name(self):
	return self.__name

    @name.setter
    def name(self, name):
	if isinstance(name, str):
	    self.__name = name
	else:
	    raise TypeError("name must be str type")
        self.__subroutineName = self.__name + self.__type + "_" + self.__wrapper

    @property
    def argList(self):
	return self.__argList

    @argList.setter
    def argList(self, _list):
	if isinstance(__argList, list):
	    self.__argList = _list
	else:
	    raise TypeError("_list must be list type")
   
    def append(self, arg):
        self.__argList.append(arg)
 
    def toString(self):
        return super(ModelSubroutine,self).toString(self.__subroutineName, self.__argList)

#   a bug: init with name not checked by NameManager
#   present solution: init phase name not allowed ?
class MapperMethod():
    __slots__=["__initStr", "__runStr", "__stype", "__mapper"]
    def __init__(self, mapper, stype="offline"):
        self.__initStr = ""
        self.__runStr = ""
        self.__stype = stype
        self.__mapper = mapper

    def setInit(self, argList):
	if self.__stype == "offline":
	    subroutine = "mapper_spmat_init"
            arg1 = mapper%name
        else:
            pass
 
    def setRun(self, argList):
        if self._stype == "offline":
            subroutine = "mapper_comp_map"
            arg1 = mapper%name
        else:
            pass
	    	        

     	
