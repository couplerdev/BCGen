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
import sys
sys.path.append('../ir')
from ir import Model, AttrVect, Mapper, GsMap
from ir import ModelSubroutine
sys.path.append('../ErrorHandle')
from ErrorHandle import *

DEBUG = 1

class Parser:
    def __init__(self):
        self.__models = {}
        self.__attrVectCouple = {}
        self.__subroutine = {}
        self.__sMapper = {}

    @property
    def models(self):
        return self.__models

    def load(self,filename):
        tree = ET.parse(filename)
        root = tree.getroot()
        return root

    def modelsParse(self):
        root = self.load('../../composing/models.xml')
        modelParser = ModelParser()
        for child in root:
            modelParser.setRoot(child)
            model = modelParser.model
            self.__models[model.name] = model

    def deployParse(self):
        pass

    def schedule(self):
        #sort seq
        pass
	
    def coupleAttrVectParse(self):
        root = self.load('coupler.xml')
        avParser = CoupleAttrVectParser()
        for child in root:
            avParser.setRoot(child)
            attrVect = avParser.attrVect
            self.__attrVect.append(attrVect)

    def parse(self):
        self.modelsParse()
        if DEBUG == 1:
            print 'model parsed'
        self.coupleAttrVectParse()
        if DEBUG == 1:
            print 'couple AttrVect parsed'

    def append(self, obj):
        if obj.atype == 'AttrVect':
            self.__attrVectCouple[obj.name] = obj
        elif obj.atype == 'Model':
            self.__model[obj.name] = obj
        elif obj.atype == 'Mrg':
            self.__subroutine[obj.name] = obj
        elif obj.atype == "Mapper":
            self.__sMapper[obj.name] = obj
        else:
            raise TypeError("Undefine atype of obj")

#
#    ModelParser uses SubroutineParser to parse the subroutine 
#
class SubroutineParser:
    __slots__=['__root', '__subroutine', '__isParsed']
    def __init__(self):
        self.__subroutine = ModelSubroutine()
        self.__isParsed = False
        self.__root = ""

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False

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
                        raise NoTagError("No such tag "+child.tag)
                self.__isParsed = True

    @property
    def subroutine(self):
        if self.__isParsed == False:
            self.subroutineParse()
        return self.__subroutine
		
class ModelParser:
    __slots__=['__root', '__model', '__name', '__isParsed', \
               '__lsize','__nx','__ny','__field']
    def __init__(self, root="",lsize=0,nx=0,ny=0,field=""):
        self.__root = root
        self.__name = ""
        self.__isParsed = False
        self.__lsize = lsize
        self.__nx = nx
        self.__ny = ny
        self.__field = field
        self.__model = Model(self.__name)

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False
	
    def __setGsMap(self):
        srcGsMap = GsMap(name=self.__name, grid=self.__name, pes=self.__name)
        dstGsMap = GsMap(name=self.__name, grid=self.__name, pes="x")
        self.__model.append(srcGsMap)
        self.__model.append(dstGsMap)

    # get attrVect that local in model
    def __setAttrVect(self):
        comp2x_aa = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src=self.__name, dst="x", grid=self.__name, pes=self.__name)
        x2comp_aa = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src="x", dst=self.__name, grid=self.__name, pes=self.__name)
        comp2x_ax = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src=self.__name, dst="x", grid=self.__name, pes="x")
        x2comp_ax = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src="x", dst=self.__name, grid=self.__name, pes="x")
        self.__model.append(comp2x_aa)
        self.__model.append(x2comp_aa)
        self.__model.append(comp2x_ax)
        self.__model.append(x2comp_ax)
  
    def __setMapper(self):
        srcMapper = Mapper(self.__name, "x", mapType="rearr")
        dstMapper = Mapper("x", self.__name, mapType="rearr")
        self.__model.append(srcMapper)
        self.__model.append(dstMapper)

    def modelParse(self):
        if self.__root == "":
            raise UnSetError("self.__root not set! Try setRoot method!") 
        name = self.__root.find('name').text
        root = self.__root
        self.model = Model(name=name)

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find('init'))   ## need ErrorHandle
        self.model.model_init = subroutine.subroutine
        subroutine.setRoot(root.find('run'))
        self.model.model_run = subroutine.subroutine
        subroutine.setRoot(root.find('final'))
        self.model.model_final = subroutine.subroutine

        root = root.find('attrVect')
        #if root.find('name')? how to handle optional 
        self.__lsize = root.find("lsize").text
        self.__nx = root.find("nx").text
        self.__ny = root.find("ny").text
        self.__field = root.find("field").text
        self.__setMapper()
        self.__setAttrVect()
        self.__setGsMap()
        self.__isParsed = True
	
    @property
    def model(self):
        if not self.__isParsed:
            self.modelParse()
        return self.__model

#
#  This class used to parse couple.xml 
#
class CouplerParser: ###!!!!
    __slots__ = ['__root', '__attrVect', '__isParsed', '__NameManager']
    def __init__(self):
        self.__root = ""
        self.__isParsed = False

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False

    def bindToNameManager(self, nameManager):
        self.__NameManager = nameManager   

    def couplerParse(self, parser):
        if self.__root == "":
            raise UnSetError("self.__root not set! Please try setRoot method")
        for child in root:
            if child.find("name")!= None and child.find("name")!= None:
                name = child.find("name").text
                av = AttrVect(name=name)
                if not self.__nameManager.findName(av):
                    raise ConfigError("try to mrg to a unexist attrVect")
            srcs = child.find("srcs")
            for src in srcs:
                name = src.find("name").text
                field = src.find("field").text
                attrVect = AttrVect(name=name, field=field)
                if not self.__nameManager.findName(attrVect):
                    parser.append(attrVect)
                mapperRoot = src.find("mapper")
                mapperName = mapperRoot.find('name')
                srvAttrVect = mapperRoot.find("src")
                mapper = Mapper(src=srcAttrVect,dst=name,name=mapperName)
                parser.append(mapper)
            mrg = child.find("mrg")
            name = mrg.find("name")
            argListroot = mrg.find("argList")
            argList = []
            for arg in argListroot:
                 argList.append(arg.text)
            mrg = MrgSubroutine(name=name, argList=argList)
            parser.append(mrg)
        

class ScheduleParser:
    pass

class DeployParser:
    pass			
