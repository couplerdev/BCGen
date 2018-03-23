#!/usr/bin/python

# intermediate representation

class Subroutine:
    __slots__=['__subroutineName','__argList']
    def __init__(self, subroutineName="func",argList=[] ):
	self.__subroutineName = subroutineName
	self.__argList = argList
        self.__lineCharacter = 50       
 
    @property
    def argList(self):
        return self.__argList
    @argList.setter
    def argList(self, argListValue):
        self.__argList = argListValue

    def toString(self):
	string = self.__subroutineName + "("
        lenString = len(string)
        lenSpace = lenString
        for arg in self.__argList:
            argStr = arg + ", "
            string += argStr
            lenString += len(argStr)
            if lenString > self.__lineCharacter:
                lenString =lineSpace
                string += "&\n" + lenSpace*' '
        if string[-2:] == "&\n":
            string = string[:-4] + ")"
        else:
            string = string[:-2] + ")"
        return string 
           
	

class ModelSubroutine(subroutine):

    __slots__=["__name","default","__argList","__wrapper", "__subroutineName"]
    def __init__(self, pattern=True, name="", wrapper="mct", subroutineName="init"):
        self.__name = name
        self.__wrapper = wrapper  # identify the wrapper API
        self.__subroutineName = self.__name + subroutineName+ "_" + self.__wrapper
	self.default = pattern
	self.__argList = []		

    @property
    def name(self):
	return self.__name

    @name.setter
    def name(self, name):
	if isinstance(name, str):
	    self.__name = model_name
	else
	    raise TypeError("model_name must be str type")

    @property
    def argList(self):
	return self.__argList

    @argList.setter
    def argList(self, _list):
	if isinstance(__argList, list):
	    self.__argList = _list
	else
	    raise TypeError("_list must be list type")
	
    
# this only used for intermediate attrVect? no!!!
class AttrVect:
    __slots__ = ['lsize', '__field', '__name', '__nx', '__ny', '__atype', \
                 '__src', '__dst', '__grid', '__pes', '__manager']
    def __init__(self, lsize=0, field="", name="", nx=0, ny=0, src="", dst="", grid="",pes=""):
        self.lsize = lsize
        self.__field = field
        self.__name = name
        self.__nx = nx
        self.__ny = ny
        self.__src = src
        self.__dst = dst
        self.__grid = grid
        self.__pes = pes
        if (self.__grid == self.__src) .or. (self.__grid == self.__dst):
            self.__atype = 0 #"rearr"
        else:
            self.__atype = 1 #"smat"

    @property
    def field(self):
	return self.__field        
    @field.setter
    def field(self, fieldValue):
	self.__field = fieldValue
    @property
    def name(self):
        if self.__name = "":
            self.__name = getManagerName()
	return self.__name
    @name.setter
    def name(self, nameValue):
        self.__name = nameValue
    @property
    def src(self):
	return self.__src
    @src.setter
    def src(self, srcValue):
	self.__src = srcValue
    @property
    def dst(self):
        return self.__dst
    @dst.setter
    def dst(self, dstValue):
        self.__dst = dstValue
    @property
    def grid(self):
        return self.__grid
    @grid.setter
    def grid(self, gridValue):
        self.__grid = gridValue
    @property
    def pes(self):
        return self.__pes
    @pes.setter
    def pes(self, pesValue):
        self.__pes = pesValue         
    @property
    def atype(self):
        return self.__atype
    
    def bindToManager(self, manager):
        self.__manager = manager

    def getManageName(self):
        return self.__manager.AddAttrVect(self)
        


class Model:
    __slots__ = ['__model_name','__model_init','__model_run','__model_final', '__manager']
    def __init__(self):
	self.__name=""
	self.__model_init = ModelSubroutine() #optional?
	self.__model_run = ModelSubroutine()		
	self.__model_final = ModelSubroutine()

    @property
    def model_init(self):
	return self.__model_init
    
    @model_init.setter
    def model_init(self, init_subroutine):
	self.__model_init = init_subroutine
	
    @property
    def model_run(self):
	return self.__model_run

    @model_run.setter
    def model_run(self, run_subroutine):
	self.__model_run = run_subroutine

    @property
    def model_final(self):
	return self.__model_final

    @model_final.setter
    def model_final(self, final_subroutine):
	self.__model_final = final_subroutine
	
    @property
    def name(self):
	return self.__name

    @name.setter
    def name(self,name)
	self.__name = name

    def bindToManger(self, manager):
        self.__manager = manager


#
#   Mapper intermediate representation
#   subroutine ?
#
class Mapper:
    __slots__ = ["__mapType", "__src", "__dst", "__name"]
    def __init__(self, src, dst, name="", mapType="copy"):
        self.__name = name
        self.__mapType = mapType
        self.__src = src
	self.__dst = dst
	
    @property
    def name(self):
        return self.__name

    @property
    def src(self):
        return self.__src
    @src.setter
    def src(self, srcValue):
	if isinstance(srcValue, type(Model)):
	    self.__src = srcValue
	else
	    raise TypeError("src must be Model type")
  
    @property
    def dst(self):
        return self.__dst
    @dst.setter
    def dst(self, dstValue):
        if isinstance(dstValue, type(Model)):
            self.__dst = dstValue
        else
            raise TypeError("dst must be Model type")

class Gsmap:
    __slots__=['__gird', '__pes', '__manager']
    


class ir:
    def __init__():
		
    def code():
	pass

