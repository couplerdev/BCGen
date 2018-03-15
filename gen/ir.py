#!/usr/bin/python

# intermediate representation

class subroutine:
	__slots__=['sub_name','__argList']
	def __init__(self):
		self.sub_name=""
		self.__argList = []

	@__sub_name.setter
	def setName(self, name):
		self.sub_name = name

	def toString(self):
		pass
	
		

class model_subroutine(subroutine):
	def __init__(self):
		self.__argList = []		

	def set(self):
		pass
	
	def toSting(self):
		pass

class attrVect:
	__slots__ = ['lsize', 'field', '__name__']
	def __init__(self):
		self.lsize = 0
		self.__field = ""
		self.__name = ""

	@field.setter
	def setField(self, field):
		self.__field = field

class model:
	__slots__ = ['model_name','model_init','model_run','model_final']
	def __init__(self):
		self.__model_name=""
		self.__model_init = model_subroutine()
		self.__model_run = model_subroutine()		
		self.__model_final = model_subroutine()

	@property
	def model_init(self):
		return self.__model_init
    
    	@__model_init.setter
	def setModel_init(self, init_sbrt):
		self.__model_init = init_sbrt
	
	@property
	def model_run(self):
		return self.__model_run

	@__model_run.setter
	def setModel_run(self, run_sbrt):
		self.__model_run = run_sbrt

	@property
	def model_final(self):
		return self.__model_final

  	@__model_final.setter
	def setModel_final(self, final_sbrt):
		self.__model_final = final_sbrt
	
	@__model_name.setter
	def name(self,name)
		self.__model_name = name

class ir:
	def __init__():
		
	def code():
		pass

