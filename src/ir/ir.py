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
class CoupleEntity(object):
    __slots__ = ['__name','__manager','__type', "__bind","__nameSet"]
    def __init__(self, name="",_type="Entity"):
         self.__name = name
         self.__type = _type
         self.__bind = False
         self.__nameSet = False

    def BindToManager(self, manager):
        self.__manager = manager
        self.__bind = True  
        
    def nameGenerate(self):
        if not self.__bind:
            raise BindError("not Bind Entities")
        if not self.__nameSet: 
            if self.__name == "":
                self.__name = self.__manager.GetName(self, self.__type)
            else:
                duplicate = self.__manager.CheckName(self.__name, self.__type)
                if duplicate:
                    raise ValueError("name conflict")
            self.__nameSet = True

    @property
    def name(self):
        if not self.__bind:
            raise BindError("not Bind Entities")
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
    def __init__(self, lsize=0, field="", nx=0, ny=0, src="", dst="", grid="",pes="",name=""):
        super(AttrVect, self).__init__(name=name,_type="AttrVect")
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
    def nx(self):
        return self.__nx

    @property
    def ny(self):
        return self.__ny        

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
                 '__manager', '__type', '__attrVects','__gsMaps', '__mappers',\
                 '__gSize']
    def __init__(self,name="", gSize=8):
	super(Model, self).__init__(name=name,_type="Model")
	self.__model_init = ModelSubroutine() #optional?
	self.__model_run = ModelSubroutine()		
	self.__model_final = ModelSubroutine()

        self.__attrVects = {}   # a2x_aa x2a_aa, a2x_ax, x2a_ax     
        self.__gsMaps = {}
        self.__mappers = {}       
        self.__name = name
        self.__gSize = gSize

### debug region
   
    @property
    def gSize(self):
        return self.__gSize

    @property
    def attrVects(self):
        return self.__attrVects

    @property
    def gsMaps(self):
        return self.__gsMaps

    @property
    def mappers(self):
        return self.__mappers

### debug region
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
            key = "c2x_cc"
            if obj.src == "x":
                if obj.pes == "x":
                    key = "x2c_cx"
                else:
                    key = "x2c_cc"
            else:
                if obj.pes == "x":
                    key = "c2x_cx"
                else: 
                    key = "c2x_cc"
            self.__attrVects[key] = obj
        elif obj.type == "Mapper":
            self.__mappers[obj.direction] = obj
        elif obj.type == "GsMap":
            if obj.pes == "x": 
                self.__gsMaps["cpl"] = obj
            else: 
                self.__gsMaps["comp"] = obj
        else:
            print obj.type
            raise TypeError("no such type!!!")

#
#   Mapper intermediate representation
#   subroutine ?
#
class Mapper(CoupleEntity):
    __slots__ = ["__mapType", "__srcAttrVect", "__dstAttrVect", "__name",\
                 "__type", "__srcGsMap", "__dstGsMap","__direction"]
    def __init__(self, srcAttrVect, dstAttrVect, srcGsMap="", dstGsMap="", \
                 mapType="copy", name=""):
        super(Mapper, self).__init__(name=name,_type="Mapper")
        self.__name = name
        self.__mapType = mapType
        self.__srcAttrVect = srcAttrVect
	self.__dstAttrVect = dstAttrVect
        self.__srcGsMap = srcGsMap
        self.__dstGsMap = dstGsMap
        if srcAttrVect.src == 'x':
            self.__direction = "x2c"
        else: 
            self.__direction = "c2x"
        self.__type = "Mapper"
        
    @property
    def direction(self):
        return self.__direction
    
    @property
    def srcGsMap(self):
        return self.__srcGsMap

    #@property
    #def name(self):
    #    return self.__name

    @property
    def dstGsMap(self):
        return self.__dstGsMap

    @property
    def srcAttrVect(self): 
        return self.__srcAttrVect

    @property
    def dstAttrVect(self):
        return self.__dstAttrVect
   
    @property
    def mapType(self):
        return self.__mapType 
 
    @property
    def atype(self):
        return self.__type
   
class GsMap(CoupleEntity):
    __slots__=['__name','__grid', '__pes', '__manager','__type','__bind']
    def __init__(self,grid="", pes="", name=""):
        super(GsMap,self).__init__(name=name, _type="GsMap")
        self.__grid = grid
        self.__pes = pes
        self.__name = name

    #@property
    #def name(self):
    #    return self.__name

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

class AttrVectCpl(AttrVect):
    __slots__=["__mapperName", "__field","__grid","__srcAttrVect",\
               "__name", "__type", "__nameSet", "__manager", "__bind", \
               "__srcModelName", "__dstModelName","__srcModel", "__dstModel"]
    def __init__(self, srcAttrVect, mapper, grid, field=""):
        name = ""
        self.__name = ""
        super(AttrVectCpl, self).__init__(name=name)
        self.__srcAttrVect = srcAttrVect
        self.__mapperName = mapper
        self.__field = field
        self.__grid = grid
        self.__nameSet = False
        self.__bind = False
        self.__type = "AttrVect"
        self.__srcModelName = self.__srcAttrVect.grid
        self.__dstModelName = grid
        self.__srcModel = Model()
        self.__dstModel = Model()

    def BindToManager(self, manager):
        self.__manager = manager
        self.__bind = True  

    def nameGenerate(self):
        if not self.__bind:
            raise BindError("not bind entities") 
        if not self.__nameSet:
            self.__name = self.__srcAttrVect.src + "2" + self.__srcAttrVect.dst + \
                          "_" + self.__grid + "x"
            duplicate =  self.__manager.CheckName(self.__name, self.__type)
            if duplicate:
                raise ValueError("name conflict!!!")
            self.__nameSet = True
        self.__srcModel = self.__manager.register.modelDict[self.__srcModelName] 
        self.__dstModel = self.__manager.register.modelDict[self.__dstModelName]
           
    @property
    def name(self):
        return self.__name

    @property
    def mapperName(self):
        return self.__mapperName
    
    @property
    def field(self):
        return self.__field

    @property
    def srcName(self):
        return self.__srcAttrVect.name

    @property
    def srcModel(self):
        return self.__srcModel
   
    @property
    def dstModel(self):
        return self.__dstModel
