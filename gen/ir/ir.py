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


class Subroutine(object):
    __slots__=['__subroutineName','__argList', '__lineCharacter']
    def __init__(self, subroutineName="func",argList=[], lineCharacter=50):
	self.__subroutineName = subroutineName
	self.__argList = argList
        self.__lineCharacter = lineCharacter
 
    @property
    def argList(self):
        return self.__argList
    @argList.setter
    def argList(self, argListValue):
        self.__argList = argListValue

    def append(self, arg):
        print 'haha', arg
        if not isinstance(arg, str):
            raise  TypeError("arg not a string")
        print type(self.__argList)
        self.__argList.append(arg)

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
           
class MergeSubroutine(Subroutine):
    __slots__=["__name","default","__argList","__subroutineName"]	
    def __init__(self, pattern=True, name=""):
        self.__name = name
        self.__subroutineName = "mrg_" + self.__name
        self.__argList = []

    #def append(self, arg):
    #    super(MergerSubroutine, self).append(arg)

    #def toString(self):
    #    return super(MergeSubroutine, self).toString()

class ModelSubroutine(Subroutine):

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
	    self.__name = name
	else:
	    raise TypeError("name must be str type")

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
 
    #def toString(self):
    #    return super(ModelSubroutine,self).toString()
	
class CoupleEntity(object):
    __slots__ = ['__name','__manager','__type', "__bind"]
    def __init__(self, name="", _type="Entity"):
         self.__name = name    
         self.__type = _type
         self.__bind = False

    def BindToManager(self, manager):
        self.__manager = manager
        self.__bind = True    

    @property
    def name(self):
        if not self.__bind:
            raise BindError("not Bind Entities")
        if self.__name == "":
            self.__name = self.__manager.CheckName(self)
        return self.__name
    @name.setter
    def name(self, nameValue):
        if not self.__bind:
            raise BindError("not Bind Entities")
        self.__name = nameValue
        self.__name = self.__manager.CheckName(self)

    @property
    def type(self):
        return self.__type

# this only used for intermediate attrVect? no!!!
class AttrVect(CoupleEntity):
    __slots__ = ['lsize', '__field', '__name', '__nx', '__ny', '__atype', \
                 '__src', '__dst', '__grid', '__pes', '__manager']
    def __init__(self, lsize=0, field="", name="", nx=0, ny=0, src="", dst="", grid="",pes=""):
        super(AttrVect, self).__init__(name, "AttrVect")
        self.lsize = lsize
        self.__field = field
        self.__name = name
        self.__nx = nx
        self.__ny = ny
        self.__src = src
        self.__dst = dst
        self.__grid = grid
        self.__pes = pes
        if (self.__grid == self.__src) or (self.__grid == self.__dst):
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
        

class Model(CoupleEntity):
    __slots__ = ['__name','__model_init','__model_run','__model_final',\
                 '__manager', '__type', '__attrVects','__gsMaps', '__mappers']
    def __init__(self,name=""):
	super(Model, self).__init__(name, "Model")
	self.__model_init = ModelSubroutine() #optional?
	self.__model_run = ModelSubroutine()		
	self.__model_final = ModelSubroutine()
        self.__attrVects = []   # a2x_aa x2a_aa, a2x_ax, x2a_ax     
        self.__gsMaps = []
        self.__mappers = []       

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

    def append(self, obj):
        if obj.type == "AttrVect":
            self.__attrVects.append(obj)
        elif obj.type == "Mapper":
            self.__mappers.append(obj)
        elif obj.type == "GsMap":
            self.__gsMaps.append(obj)
        else:
            print obj.type
            raise TypeError("no such type!!!")

#
#   Mapper intermediate representation
#   subroutine ?
#
class Mapper(CoupleEntity):
    __slots__ = ["__mapType", "__src", "__dst", "__name","__type"] # do we need init subroutine object?
    def __init__(self, src, dst, name="", mapType="copy"):
        super(Mapper, self).__init__(name, "Mapper")
        self.__name = name
        self.__mapType = mapType
        self.__src = src
	self.__dst = dst

    @property
    def src(self):
        return self.__src
    @src.setter
    def src(self, srcValue):
	if isinstance(srcValue, type(Model)):
	    self.__src = srcValue
	else:
	    raise TypeError("src must be Model type")
  
    @property
    def dst(self):
        return self.__dst
    @dst.setter
    def dst(self, dstValue):
        if isinstance(dstValue, type(Model)):
            self.__dst = dstValue
        else:
            raise TypeError("dst must be Model type")

class GsMap(CoupleEntity):
    __slots__=['__name','__grid', '__pes', '__manager','__type','__bind']
    def __init__(self, name="",grid="", pes=""):
        super(GsMap,self).__init__(name, "GsMap")
        self.__grid = grid
        self.__pes = pes

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


class sMat(CoupleEntity):
    __slots__  = ["__name", "__type","__bind","__manager" ]
    def __init__(self, name=""):
        super(sMat, self).__init__(name, "sMat")
        


