#!/usr/bin/python

#import ir
# parser: parse xml to generate intermediate representation
import xml.etree.ElementTree as ET
import ir 



class parser:
	def __init__(self):
		self.__models__ = []
		
	def load(self,filename):
		tree = ET.parse(filename)
		root = tree.getroot()

	def models_parse(self):
		root = self.load('models.xml')
		mdl_parser = model_parser()
		for child in root:
			mdl_parser.setRoot(child)
			model = mdl.model()
			self.__models__.append(model)
	def deploy_parse(self):
		pass
	
	def coupler_parse(self):
		pass

class check:
	def __init__():
		pass

class subroutine_parser:
	__slots__=['__root', '__subrt']
	def __init__(self):
		self.__subrt = model_subroutine()

	def setRoot(self, root):
		self.__root = root

	def subroutine_parse(self):
		self.__subrt =  model_subroutine()
		av1 = attrVect(root.find('input'))
		av2 = attrVect(root.find('output'))
		self.__subrt.appendList(av1)
		self.__subrt.appendList(av2)

	@property
	def subrt(self):
		self.subroutine_parse()
		return self.__subrt
		
class model_parser:
	__slots__=['__root', '__model']
	def __init__(self):
		

	def model_parse(self):
		self.model = ir.model()
	 	name = root.find('name').text
		self.model.name(name)
		subrt = subroutine_parser()
		subrt.setRoot(root.find('init'))
		self.model.setModel_init(subrt.subrt())
		subrt.setRoot(root.find('run'))
		self.model.setModel_run(subrt.subrt())
		subrt.setRoot(root.find('final'))
		self.model.setModel_final(subrt.subrt())
	
	@property
	def model(self):
		self.model_parse()
		return self.__model

			
