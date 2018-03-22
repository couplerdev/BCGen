#!/usr/bin/python

# intermediate representation

class Subroutine:
    __slots__=['sub_name','__argList']
    def __init__(self):
	self.sub_name=""
	self.__argList = []

    @sub_name.setter
    def setName(self, name):
	self.sub_name = name

    def toString(self):
	pass
	

class ModelSubroutine(subroutine):

    __slots__=["__model_name","default","__argList"]
    def __init__(self, pattern=True, model_name=""):
        self.__model_name = model_name
	self.default = pattern
	self.__argList = []		

    @property
    def model_name(self):
	return self.__model_name

    @model_name.setter
    def model_name(self, model_name):
	if isinstance(model_name, str):
	    self.__model_name = model_name
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
	
			

class AttrVect:
    __slots__ = ['lsize', '__field', '__name', '__nx', '__ny']
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

    @property
    def field(self):
	return self.__field        
    @field.setter
    def field(self, fieldValue):
	self.__field = fieldValue
    @property
    def name(self):
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


class Model:
    __slots__ = ['__model_name','__model_init','__model_run','__model_final']
    def __init__(self):
	self.__model_name=""
	self.__model_init = model_subroutine() #optional?
	self.__model_run = model_subroutine()		
	self.__model_final = model_subroutine()

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
	return self.__model_name

    @model_name.setter
    def name(self,name)
	self.__model_name = name

#
#   Mapper intermediate representation
#   subroutine ?
#
class Mapper:
    __slots__ = ["__mapType", "__src", "__dst"]
    def __init__(self, src, dst, mapType="copy"):
        self.__mapType = mapType
        self.__src = src
	self.__dst = dst

    def 


class ir:
    def __init__():
		
    def code():
	pass

