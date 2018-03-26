#
#    This Parser module parse xml file to intermediate representation
#
# reversion history
#      2018.3.1    alex: add the module
#      2018.3.26   alex: modify 
#!/usr/bin/python

#import ir
# parser: parse xml to generate intermediate representation
import xml.etree.ElementTree as ET
from ir import Model, AttrVect, Mapper, GsMap
from ir import ModelSubroutine



class Parser:
    def __init__(self):
        self.__models = []
		
    def load(self,filename):
        tree = ET.parse(filename)
        root = tree.getroot()

    def modelsParse(self):
        root = self.load('models.xml')
        modelParser = ModelParser()
        for child in root:
            modelParser = ModelParser()
            modelParser.setRoot(child)
            model = modelParser.model
            self.__models.append(model)

    def deployParse(self):
        pass

    def schedule(self):
        #sort seq
        pass
	
    def couplerParse(self):
        pass

    def parse(self):
        pass

#
#    ModelParser uses SubroutineParser to parse the subroutine 
#
class SubroutineParser:
    __slots__=['__root', '__subroutine', '__']
    def __init__(self):
        self.__subroutine = ModelSubroutine()
        self.__parsed = False
        self.__root = ""

    def setRoot(self, root):
        self.__root = root

    def subroutineParse(self):
                if self.__root == "":
                    raise UnSetError("self.__root not set! Try setRoot method")
		self.__subroutine =  ModelSubroutine()
		for child in self.__root:
                    if child.tag == "name":
                        self.__subroutine.name = child.text
                    elif child.tag == "arg":
                        self.__subroutine.append(child.text)
                    else:
                        raise NoTagError("no such tag"+child.tag)

    @property
    def subroutine(self):
        self.subroutine_parse()
        return self.__subrt
		
class ModelParser:
    __slots__=['__root', '__model','__attrVects', '__gsMaps','__mappers','__name']
    def __init__(self, root=""):
        self.__root = root
        self.__name = ""
        self.__attrVects = []
        self.__mappers = []
        self.__gsMaps = []

    def setRoot(self, root):
        self.__root = root
	
    def __setGsMap(self):
        srcGsMap = GsMap(name=self.__name, grid=self.__name, pes=self.__name)
        dstGsMap = GsMap(name=self.__name, grid=self.__name, pes="x")
        self.__gsMap.append(srcGsMap)
        self.__gsMap.append(dstGsMap)

    def __setAttrVect(self):
        pass # some change may be here
  
    def __setMapper(self):
        srcMapper = Mapper(self.__name, "x", mapType="rearr")
        dstMapper = Mapper("x", self.__name, mapType="rearr")
        self.__mappers.append(srcMapper)
        self.__mappers.append(dstMapper)

    def modelParse(self):
        if self.__root = "":
            raise UnSetError("self.__root not set! Try setRoot method!") 
        name = root.find('name').text
        self.model = ir.model(name=name)
        subroutine = subroutineParser()
        subroutine.setRoot(root.find('init'))   ## need ErrorHandle
        self.model.modelInit(soubroutine.subroutine())
        subroutine.setRoot(root.find('run'))
        self.model.modelRun(subroutine.subroutine())
        subroutine.setRoot(root.find('final'))
        self.model.modelFinal(subroutine.subroutine())
        self.__setMapper()
        self.__setAttrVect()
        self.__setGsMap()
	
    @property
    def model(self):
        self.modelParse()
        return self.__model

#
#  This class uses to parse couple.xml 
#
class AttrVectParser:
    __slots__ = ['__root']
    def __init__(self):
        pass
    def setRoot(self, root):
        self.__root = root
    
    def attrVectParse(self):
        pass

class ScheduleParser:
    pass

class DeployParser:
    pass			
