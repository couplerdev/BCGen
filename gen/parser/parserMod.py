#
#    This Parser module parse xml file to intermediate representation
#
# reversion history
#      2018.3.1    alex: add the module
#      2018.3.26   alex: modify 
#      2018.4.3    alex: fixed some bugs
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
from NameManager import *

DEBUG = 1

class Parser:
    def __init__(self, couplerFile="../../composing/coupler.xml",  modelFile="../../composing/models.xml", \
                 scheduleFile="../composing/schedule.xml"):
        self.__NameManager = NameManager()
        self.__models = {}
        self.__attrVectCouple = {}
        self.__subroutine = {}
        self.__sMapper = {}
        self.__couplerFile = couplerFile
        self.__modelFile = modelFile
        self.__scheduleFile = scheduleFile

    @property
    def models(self):
        return self.__models

    @property
    def attrVectCouple(self):
        return self.__attrVectCouple

    @property
    def subroutine(self):
        return self.__subroutine
        
    @property
    def sMapper(self):
        return self.__sMapper     

    def load(self,filename):
        tree = ET.parse(filename)
        root = tree.getroot()
        return root

    def modelsParse(self):
        root = self.load(self.__modelFile)
        modelParser = ModelParser(self.__NameManager)
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
        root = self.load(self.__couplerFile)
        avParser = CouplerParser(self.__NameManager)  ## not implemented now
        for child in root:
            avParser.setRoot(child)
            avParser.couplerParse(self)
            attrVect = avParser.attrVect
            #print attrVect.name, attrVect.atype
            self.__attrVectCouple.append(attrVect)

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
                        self.__subroutine.subroutineName = child.text
                    elif child.tag == "arg":
                        self.__subroutine.append(child.text)
                    else:
                        raise NoTagError("No such tag "+child.tag)
                self.__isParsed = True
                #print self.__subroutine.subroutineName

    @property
    def subroutine(self):
        if self.__isParsed == False:
            self.subroutineParse()
        return self.__subroutine
		
class ModelParser:
    __slots__=['__root', '__model', '__isParsed', '__name',\
               '__lsize','__nx','__ny','__field','__NameManager']
    def __init__(self, NameManager,root="",lsize=0,nx=0,ny=0,field=""):
        self.__root = root
        self.__isParsed = False
        self.__lsize = lsize
        self.__nx = nx
        self.__ny = ny
        self.__field = field
        self.__name = ""
        self.__NameManager = NameManager

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False
	
    def __setGsMap(self):
        srcGsMap = GsMap(grid=self.__name, pes=self.__name)
        dstGsMap = GsMap(grid=self.__name, pes="x")
        srcGsMap.BindToManager(self.__NameManager)
        dstGsMap.BindToManager(self.__NameManager)
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
        comp2x_aa.BindToManager(self.__NameManager)
        x2comp_aa.BindToManager(self.__NameManager)
        comp2x_ax.BindToManager(self.__NameManager)
        x2comp_ax.BindToManager(self.__NameManager)
        self.__model.append(comp2x_aa)
        self.__model.append(x2comp_aa)
        self.__model.append(comp2x_ax)
        self.__model.append(x2comp_ax)
  
    def __setMapper(self):
        srcMapper = Mapper(self.__name, "x", mapType="rearr")
        dstMapper = Mapper("x", self.__name, mapType="rearr")
        srcMapper.BindToManager(self.__NameManager)
        dstMapper.BindToManager(self.__NameManager)
        self.__model.append(srcMapper)
        self.__model.append(dstMapper)

    def modelParse(self):
        if self.__root == "":
            raise UnSetError("self.__root not set! Try setRoot method!") 
        name = self.__root.find('name').text
        root = self.__root
        self.__name = name
        self.__model = Model(name=name)
        self.__model.BindToManager(self.__NameManager)

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find('init'))   ## need ErrorHandle
        self.__model.model_init = subroutine.subroutine
        subroutine.setRoot(root.find('run'))
        self.__model.model_run = subroutine.subroutine
        subroutine.setRoot(root.find('final'))
        self.__model.model_final = subroutine.subroutine

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
    def __init__(self, nameManager):
        self.__root = ""
        self.__isParsed = False
        self.__NameManager = nameManager
        self.__attrVect = AttrVect()    
    
    @property
    def attrVect(self):
        return self.__attrVect

    @attrVect.setter
    def attrVect(self, attrVectValue):
        if not isinstance(type(attrVect),attrVectValue):
            raise TypeError("attrVectValue not attrVect type")
        self.__attrVect = attrVectValue

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False

    def couplerParse(self, parser):
        if self.__root == "":
            raise UnSetError("self.__root not set! Please try setRoot method")
        root = self.__root
        if root.find("name")!= None:
            name = root.find("name").text
            print name
            av = AttrVect(name=name)
            self.__attrVect = av
            av.BindToManager(self.__NameManager)
            if not self.__NameManager.FindName(av):
                raise ConfigError("try to mrg to a unexist attrVect")
        if root.find("srcs") != None:
            srcs = root.find("srcs")
            for src in srcs:
                attrVectName = src.find("attrVect").text
                field = src.find("field").text
                attrVect = AttrVect(name=name, field=field)
                attrVect.BindToManager(self.__NameManager)
                if not self.__NameManager.FindName(attrVect):
                    parser.append(attrVect)
                mapperRoot = src.find("mapper")
                mapperName = mapperRoot.find('name')
                srvAttrVect = mapperRoot.find("src")
                mapper = Mapper(src=srcAttrVect,dst=name,name=mapperName)
                parser.append(mapper)
        if root.find("mrg") != None:
            mrg = root.find("mrg")
            name = ""
            argList = []
            if mrg.find("name") != None:
                name = mrg.find("name")
            else:
                name = "mrg"
            if mrg.find("args") != None:
                argListRoot = mrg.find("args") 
                for arg in argListRoot:
                    argList.append(arg.text)
            else:
                argList = []
            mrg = MrgSubroutine(name=name, argList=argList)
            parser.append(mrg)
        else:
            print "no mrg"
            #parser.append(mrg)   ## bugy
        

class ScheduleParser:
    pass

class DeployParser:
    pass			
