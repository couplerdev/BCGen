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
from ir import Model, AttrVect, Mapper, GsMap, AttrVectCpl
from ir import ModelSubroutine, MergeSubroutine
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
        self.__subroutine = {}   ## mrg subroutine
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
        
    def addDict(self, attrVect, name):
        if not self.__attrVectCouple.has_key(name):
            self.__attrVectCouple[name] = []
        self.__attrVectCouple[name].append(attrVect)
 
    def visitByName(self, name):
        for model in self.__models:
            for av in self.__models[model].attrVects:
                avName =  self.__models[model].attrVects[av].name
                if avName == name:
                    return self.__models[model].attrVects[av]
        return None               

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
            self.__NameManager.register.modelDict[model.name] = model

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
            mrg = avParser.mergeSubroutine
            #print attrVect.name, attrVect.atyp
            self.__subroutine[mrg.name] = mrg

    def parse(self):
        self.modelsParse()
        if DEBUG == 1:
            print 'model parsed'
        self.coupleAttrVectParse()
        if DEBUG == 1:
            print 'couple AttrVect parsed'

    def append(self, obj):
        if obj.atype == 'Model':
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
        srcGsMap.nameGenerate()
        dstGsMap.BindToManager(self.__NameManager)
        dstGsMap.nameGenerate()
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
        comp2x_aa.nameGenerate()
        x2comp_aa.BindToManager(self.__NameManager)
        x2comp_aa.nameGenerate()
        comp2x_ax.BindToManager(self.__NameManager)
        comp2x_ax.nameGenerate()
        x2comp_ax.BindToManager(self.__NameManager)
        x2comp_ax.nameGenerate()
        self.__model.append(comp2x_aa)
        self.__model.append(x2comp_aa)
        self.__model.append(comp2x_ax)
        self.__model.append(x2comp_ax)
  
    def __setMapper(self):
        if len(self.__model.attrVects) != 4:
            raise ValueError("call __setAttrVect first!") 
        if len(self.__model.gsMaps) != 2:
            raise ValueError("call __setGsMap first!")
        srcMapper = Mapper(self.__model.attrVects["c2x_cc"], self.__model.attrVects["c2x_cx"], \
                           self.__model.gsMaps["comp"].name, self.__model.gsMaps["cpl"].name, \
                           mapType="rearr")
        dstMapper = Mapper(self.__model.attrVects["x2c_cc"], self.__model.attrVects["x2c_cx"], \
                           self.__model.gsMaps["comp"].name, self.__model.gsMaps["cpl"].name,  \
                           mapType="rearr")
        srcMapper.BindToManager(self.__NameManager)
        srcMapper.nameGenerate()
        dstMapper.BindToManager(self.__NameManager)
        dstMapper.nameGenerate()
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
        self.__setAttrVect()
        self.__setGsMap()
        self.__setMapper()
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
        self.__mergeSubroutine = MergeSubroutine()
    
    @property
    def attrVect(self):
        return self.__attrVect

    @attrVect.setter
    def attrVect(self, attrVectValue):
        if not isinstance(type(attrVect),attrVectValue):
            raise TypeError("attrVectValue not attrVect type")
        self.__attrVect = attrVectValue
    
    @property
    def mergeSubroutine(self):
        return self.__mergeSubroutine

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False

    def couplerParse(self, parser):
        if self.__root == "":
            raise UnSetError("self.__root not set! Please try setRoot method")
        root = self.__root
        if root.find("name")!= None:
            name = root.find("name").text
            av = AttrVect(name=name)
            self.__attrVect = av
            av.BindToManager(self.__NameManager)
            if not self.__NameManager.FindName(av):
                raise ConfigError("try to mrg to a unexist attrVect")
        if root.find("srcs") != None:
            srcs = root.find("srcs")
            if root.find('model') == None:
                raise UnsetError("need model be set in composing")
            grid = root.find('model').text
            for src in srcs:
                srcAttrVectName = src.find("attrVect").text
                srcAttrVect = parser.visitByName(srcAttrVectName)
                if srcAttrVect == None:
                    raise AttributeError("no such attrVect")
                field = src.find("field").text
                mapperName = src.find("mapper").text
                attrVect = AttrVectCpl(srcAttrVect, mapperName, grid, field=field)
                attrVect.BindToManager(self.__NameManager)
                attrVect.nameGenerate()
                if self.__NameManager.FindName(attrVect):
                    parser.addDict(attrVect, name)
                mapper = Mapper(srcAttrVect,attrVect, mapType="sMat",name=mapperName)
                mapper.BindToManager(self.__NameManager)
                mapper.nameGenerate()
                parser.append(mapper)
        if root.find("mrg") != None:
            mrg = root.find("mrg")
            name = ""
            args = []
            if mrg.find("name") != None:
                name = mrg.find("name").text
            else:
                name = "mrg"+'_'
            merge = MergeSubroutine(name=name) 
            if mrg.find("args") != None:  # if undefined args using default mod
                argListRoot = mrg.find("args") 
                for arg in argListRoot:
                    args.append(arg.text)
            else:
                args = []
            merge.argList = args
            self.__mergeSubroutine = merge
        else:
            print "no mrg"
            #parser.append(mrg)   ## bugy
        

class ScheduleParser:
    pass

class DeployParser:
    pass			
