#!/use/bin/python
#coding:utf-8
#    This Parser module parse xml file to intermediate representation
#
# reversion history
#      2018.3.1    alex: add the module
#      2018.3.26   alex: modify 
#      2018.4.3    alex: fixed some bugs
#      2019.4.1    alex: add fraction
#import ir
# parser: parse xml to generate intermediate representation
import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys
sys.path.append('../ir')
from ir import Model, AttrVect, Mapper, GsMap, AttrVectCpl
from ir import ModelSubroutine, MergeSubroutine, Subroutine
from FractionIr import Fraction, FractionMapper

from Datatype import *
sys.path.append('../ErrorHandle')
from ErrorHandle import *
from NameManager import *
from runCodeParser import SubroutineNode, SeqRun, Node
from codeWrapper import CodeWrapper, toString
from setupParser import Setup
from fieldManager import FieldMeta, FieldManager
from fakeModelParser import FakeModelParser


DEBUG = 1

class Parser():
    def __init__(self, setup=True, rest=False, hist=False,fileSpec={}):
        self.__couplerFile="../../composing/coupler.xml"
        self.__modelFile="../../composing/models.xml"
        self.__scheduleFile="../../composing/schedule.xml"
        self.__deployFile="../../composing/deploy.xml"
        self.__fieldFile="../../composing/field.xml"
        self.__setupFile="../../composing/setup.xml"
        self.__regriddingFile="../../composing/regriddingFile.xml"
        self.__fractionFile="../../composing/fractionSet.xml"
        self.__fakeModelFile = "../../composing/fakeModel.xml"   

        if len(fileSpec) != 0 :
            self.__couplerFile = fileSpec['coupler.xml']
            self.__modelFile = fileSpec['models.xml']
            self.__deployFile = fileSpec['deploy.xml']
            self.__fieldFile = fileSpec['field.xml']
            self.__setupFile = fileSpec['setup.xml']
            self.__regriddingFile = fileSpec['regriddingFile.xml']
            self.__fractionFile = fileSpec['fractionSet.xml']
            self.__fakeModelFile = fileSpec['fakeModel.xml']

        self.__NameManager = NameManager()
        self.__models = {}
        self.__attrVectCouple = {}
        self.__subroutine = {}   ## mrg subroutine
           
        self.__sMapper = {}
        self.__deployDistribution = {} # format {id: [first, last, stride]}
        self.__setupModels = {}
        self.__setupFakeModels = {}
        self.__enable_setup = setup
        if setup:
            # setup generate couple with fraction
            setup = Setup(fileName=self.__setupFile)
            setup.setupParse()
            setup.genXml()
            couplerFile = setup.couplerFile
            self.__setupModels = setup.model 
            self.__setupFakeModels = setup.fakeModel
        #self.__couplerFile = couplerFile
        #self.__fieldFile = fieldFile
        #self.__modelFile = modelFile
        #self.__scheduleFile = scheduleFile
        #self.__deployFile = deployFile
        self.__fractions = {}
        self.__seqRun = SeqRun()
        self.fldManager = FieldManager()
        self.runSubroutine = []
        self.__fldDict = {}
        self.__fldMetaDict = {}
        # util optional
        self.__hist = hist
        self.__rest = rest
        # for second phase init
        self.__secondPhaseInitSubrt = {}
        # for fake model
        self.__fakeModels = {}

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
    def deploy(self):
        return self.__deployDistribution

    @property
    def fractions(self):
        return self.__fractions
   
    @property
    def fldDict(self):
        return self.__fldDict

    @property
    def sMapper(self):
        return self.__sMapper
 
    @property
    def fldMetaDict(self):
        return self.__fldMetaDict

    @property
    def fakeModels(self):
        return self.__fakeModels

    def addDistribution(self, deployList, ID):
        self.__deployDistribution[ID]=deployList
        
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
    # bug may happend when modelFile not include relative setup file
        root = self.load(self.__modelFile)
        modelParser = ModelParser(self.__NameManager, self.__seqRun)
        #fakeModelParser = FakeModelParser(self.__seqRun)
        index = 1
        modelCount = 0
        for child in root:
            if self.__enable_setup:
                modelName = child.find('name').text
                modelVersion = child.find('version').text
                if modelName not in self.__setupModels:
                    continue
                elif self.__setupModels[modelName] != modelVersion:
                    continue
            modelParser.setRoot(child)
            model = modelParser.model
            model.ID = index
            index = index + 1
            modelCount = modelCount + 1
            self.__models[model.name] = model
            self.__NameManager.register.modelDict[model.name] = model
                    
        if self.__enable_setup and modelCount != len(self.__setupModels):
            print modelCount
            raise ComposingError("invalid "+self.__modelFile+\
                    " for some model(s) not supported in this file when setup.xml set them")
    
        #for child in root:    
        #    if 'type' in child.attrib and child.attrib['type'] == 'fake' \
        #       and child.find('name').text in self.__setupFakeModels:
        #        fakeModelParser.setRoot(child)
        #        fakeModel = fakeModelParser.model
        #        for dep in fakeModel.deps:
        #            if dep not in self.models:
        #                raise NotProperConfigError('in fakeModel deps')
        #        self.__fakeModels[fakeModel.name]  = fakeModel
                
    def fakeModelParse(self):
        root = self.load(self.__fakeModelFile)
        for child in root:
            fakeModelParser = FakeModelParser(self.__NameManager, self.__seqRun)
            fakeModelParser.setRoot(child)
            fakeModel = fakeModelParser.model
            for dep in fakeModel.deps:
                if dep not in self.models:
                    raise NotProperConfigError('in fakeModelParse') 
            self.__fakeModels[fakeModel.name] = fakeModel

    def deployParse(self):
        root = self.load(self.__deployFile)
        deployParser = DeployParser(self.__NameManager)
        deployParser.setRoot(root)
        deployParser.deployParse(self)

    def schedule(self):
        #sort seq
        pass
        
    def coupleAttrVectParse(self):
        root = self.load(self.__couplerFile)
        avParser = CouplerParser(self.__NameManager, self.__seqRun)  ## not implemented now
        for child in root:
            avParser.setRoot(child)
            avParser.couplerParse(self)
            mrg = avParser.mergeSubroutine
            #print attrVect.name, attrVect.atyp
            self.__subroutine[mrg.name] = mrg
            if avParser.fraction != None:
                self.__fractions[avParser.fraction.name]=avParser.fraction

    def fieldParse(self):
        for model in self.__models:
            self.fldManager.addModel(model)
        self.fldManager.queryBuild(self.__fieldFile)
        for model in self.__models:
            totalFlds = {}
            for fld in self.__models[model].fields:
                fldList = fld.split('_')
                fldPrex = fldList[0]+'_'+fldList[1]
                if not fldPrex in totalFlds:
                   totalFlds[fldPrex] = []
                totalFlds[fldPrex].append(fld)
                self.__models[model].fields[fld] = self.fldManager.fldsQuery[fld] 
                if not fld in self.fldManager.fldsQuery:
                     raise NotFoundError('fld not found')
                fldStr = self.fldManager.fldsQuery[fld]
                self.fldDict[fld] = fldStr
                fldList = fldStr.split(':') if fldStr else []
                for f in fldList:
                    if f in self.fldManager.fieldQuery:
                        self.fldMetaDict[f] = self.fldManager.fieldQuery[f]
            for fld in totalFlds:
                fldStr = ""
                for f in totalFlds[fld]:
                    fldStr+=self.fldDict[f]+":"
                fldStr = fldStr[:-1]
                self.fldDict[fld] = fldStr
                self.__models[model].myFields[fld]=fldStr
        for model in self.__fakeModels:
            for fld in self.__fakeModels[model].flds:
                fldStr = self.fldManager.fldsQuery[fld] 
                self.fldDict[fld] = fldStr
        # 特殊处理的dom，之后再加入不特殊处理的吧

    def parse(self):
        self.modelsParse()
        if DEBUG == 1:
            print '...................model parsed...................'
        self.coupleAttrVectParse()
        if DEBUG == 1:
            print '..............couple AttrVect parsed..............'
        self.fakeModelParse()
        if DEBUG == 1:
            print '..............fake Model parsed...................'
        self.deployParse()
        if DEBUG == 1:
            print '..................deploy parsed...................'
        
        #for node in self.__seqRun.graph.Nodes:
        #    sbr = self.__seqRun.graph.Nodes[node]
        #    print node, sbr.inEdge            


        self.runSubroutine = self.__seqRun.topology()
        # add rest and hist subroutine here 
        # this is a temp solution
        utilSubroutineNodeList = []
        if self.__rest:
            cw = CodeWrapper()
            restStr = "base_rest_write(metaData, EClock_drv)" 
            cw.appendStr(restStr)
            restBlock = cw.codeBlock("cpl", "cpl",flag="restart") 
            restNode = SubroutineNode("rest",strFormat=restBlock)
            addNode = Node("rest", data=restNode)
            utilSubroutineNodeList.append(addNode)
        if self.__hist:
            cw = CodeWrapper()
            histStr = "base_hist_write(metaData, EClock_drv)"
            cw.appendStr(histStr)
            histBlock = cw.codeBlock("cpl","cpl", flag="hist")
            histNode = SubroutineNode("hist", strFormat=histBlock)
            addNode = Node("hist", data=histNode)
            utilSubroutineNodeList.append(addNode)
        self.runSubroutine.append(utilSubroutineNodeList)
        
        # parse field
        self.fieldParse()

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
        self.inArgs = []
        self.outArgs = []
        self.subroutineNode = None

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
            elif child.tag == "in_args":
                root = self.__root.find("in_args")
                for sub in root:
                    if sub.tag == "arg":
                        self.inArgs.append(sub.text)
            elif child.tag == "out_args":
                root = self.__root.find("out_args")
                for sub in root:
                    if sub.tag == "arg":
                        self.outArgs.append(sub.text)
            else:
                raise NoTagError("No such tag "+child.tag)
        self.__isParsed = True
        #print self.__subroutine.subroutineName

    def appendArgs(self, args):
        for arg in args:
            self.__subroutine.append(arg)
        #for arg in args:
        #    self.__subroutine.argList.append(arg)

    def getSubroutineNode(self, model='', phase=-1, inArgs=[], outArgs=[], strFormat=""):
        if len(self.inArgs)!=0 and len(self.outArgs)!=0:
            self.subroutineNode = SubroutineNode(self.__subroutine.subroutineName, \
                                   model, phase=phase, inputArg=self.inArgs, \
                                   outputArg=self.outArgs, strFormat=strFormat)
        elif len(inArgs)!=0 and len(outArgs)!=0:
            self.subroutineNode = SubroutineNode(self.__subroutine.subroutineName, \
                                   model, phase=phase, inputArg=self.inArgs, \
                                   outputArg=self.outArgs, strFormat=strFormat)
        else:
            raise UnSetError("inArgs or outArgs not set")                                  
        return self.subroutineNode

    @property
    def subroutine(self):
        if self.__isParsed == False:
            self.subroutineParse()
        return self.__subroutine
                
class ModelParser:
    __slots__=['__root', '__model', '__isParsed', '__name',\
               '__gsize','__nx','__ny','__field','__NameManager']
    def __init__(self, NameManager, seqRun, root="",lsize=0,nx=0,ny=0,field="", gsize=0):
        self.__root = root
        self.__isParsed = False
        self.__gsize = gsize
        self.__nx = nx
        self.__ny = ny
        self.__field = field
        self.__name = ""
        self.__NameManager = NameManager
        self.SeqRun = seqRun 

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
        self.srcGsMap = srcGsMap
        self.dstGsMap = dstGsMap

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
        srcMapper = Mapper(self.__model.attrVects["x2c_cc"], self.__model.attrVects["x2c_cx"], \
                           self.__model.gsMaps["comp"].name, self.__model.gsMaps["cpl"].name, \
                           mapType="rearr")
        dstMapper = Mapper(self.__model.attrVects["c2x_cc"], self.__model.attrVects["c2x_cx"], \
                           self.__model.gsMaps["comp"].name, self.__model.gsMaps["cpl"].name,  \
                           mapType="rearr")
        srcMapper.BindToManager(self.__NameManager)
        srcMapper.nameGenerate()
        dstMapper.BindToManager(self.__NameManager)
        dstMapper.nameGenerate()
        self.__model.append(srcMapper)
        self.__model.append(dstMapper)
        self.srcMapper = srcMapper
        self.dstMapper = dstMapper

## set time modi 8/11
    def __setTime(self):
        root = self.__root.find('time')
        base_root = root.find('base')
        y = 0
        m = 0
        d = 0
        h = 0
        if base_root.find('y')!=None:
            y = base_root.find('y').text
        if base_root.find('m')!=None:
            m = base_root.find('m').text
        if base_root.find('d')!=None:
            d = base_root.find('d').text
        if base_root.find('h')!=None:
            h = base_root.find('h').text
        base = Base(y, m, d, h)         

        interval_root = root.find('interval')
        m = 0 
        d = 0
        h = 0
        minute = 0
        sec = 0
        if interval_root.find('m')!=None and interval_root.find('m').text!=None:
            m = interval_root.find('m').text
        if interval_root.find('d')!=None and interval_root.find('d').text!=None:
            d = interval_root.find('d').text
        if interval_root.find('h')!=None and interval_root.find('h').text!=None:
            h = interval_root.find('h').text
        if interval_root.find('minute')!=None and interval_root.find('minute').text!=None:
            minute = interval_root.find('minute').text
        if interval_root.find('sec')!=None and interval_root.find('sec').text!=None:
            sec = interval_root.find('sec').text
        #interval  = Interval(m, d, h, minute, sec)
        self.__model.Time = {"m":m,"d":d,"h":h,"minute":minute,"sec":sec}
    def __setDomain(self):
        '''
        root = self.__root
        domain_root = root.find('domain')
        field = ""
        if domain_root.find('field') != None:
            field = domain_root.find('field').text
        if domain_root.find('path') == None:
            raise UnsetError("domain data path not set!")
        path  = domain_root.find('path').text
        self.__domain = Domain(field, path)
        '''
        self.__model.domain['m'] = "domain_"+self.__name+self.__name
        self.__model.domain['x'] = "domain_"+self.__name+'x'

    def __setSubroutine(self):    # must be the last to be set
        root =self.__root.find("method")
        grid = self.__name
        args = ["metaData%"+grid, "EClock_"+grid,self.__model.attrVects["x2c_cc"].name, \
              self.__model.attrVects["c2x_cc"].name, "ierr=ierr"]

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find("init"))
        self.__model.model_init = subroutine.subroutine
        self.__model.model_init.argList = args

        args = ["metaData%"+grid ,"EClock_"+grid, self.__model.attrVects["x2c_cc"].name, \
                self.__model.attrVects["c2x_cc"].name, "ierr=ierr"]
        subroutine = SubroutineParser()
        subroutine.setRoot(root.find("run"))
        subroutine.appendArgs(args)
        subroutineModel = subroutine.subroutine
        self.__model.model_run = subroutineModel
        string = toString(subroutineModel.subroutineName, args)
        cw = CodeWrapper()
        cw.appendStr(string)
        strFormat = cw.codeBlock(grid, grid,flag= "comp")
        #print toString(subroutine.subroutine.subroutineName, args)
        self.SeqRun.addSubroutine(subroutine.getSubroutineNode(model=grid, phase=2, strFormat=strFormat))

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find("final"))
        subroutine.appendArgs(args)
        self.__model.model_final = subroutine.subroutine

        ### implementation detemine subroutine generating
        ###1 for x2a_ax to x2a_aa: this are all standard subroutine mapper_comp_comm, mappers \
        ###  are rearranger
        mapper_name = "mapper_comp_map"
        msg_tag = "msgtag=103"  # need modi
        ierr = "ierr=ierr"
        argList = ["metaData%"+self.srcMapper.name, self.__model.attrVects["x2c_cx"].name, \
               self.__model.attrVects["x2c_cc"].name, msg_tag, ierr]
        subroutine = Subroutine(subroutineName=mapper_name, argList=argList)
        in_args = []
        out_args = []
        in_args.append(self.__model.attrVects["x2c_cx"].name)
        out_args.append(self.__model.attrVects["x2c_cc"].name)
        string = toString(mapper_name, argList)
        cw = CodeWrapper()
        cw.appendStr(string)
        strFormat = cw.codeBlock(grid, grid+"2cpl", flag="comp")
        subroutineNode = SubroutineNode(mapper_name, model=grid, phase=1, inputArg=in_args, \
                                      outputArg=out_args, strFormat=strFormat)
        self.SeqRun.addSubroutine(subroutineNode)

        argList = ["metaData%"+self.dstMapper.name, self.__model.attrVects["c2x_cc"].name, \
               self.__model.attrVects["c2x_cx"].name, msg_tag, ierr]
        subroutine = Subroutine(mapper_name, argList=argList)
        in_args = []
        out_args = []
        in_args.append(self.__model.attrVects["c2x_cc"].name)
        out_args.append(self.__model.attrVects["c2x_cx"].name)
        string = toString(mapper_name, argList)
        cw = CodeWrapper()
        cw.appendStr(string)
        strFormat = cw.codeBlock(grid, grid+"2cpl", flag="comp")
        subroutineNode = SubroutineNode(mapper_name, model=grid, phase=3, inputArg=in_args, \
                                       outputArg=out_args, strFormat=strFormat)
        self.SeqRun.addSubroutine(subroutineNode)

    def __setField(self):
        fldStr = self.__root.find('attrVect').find('field').text
        
        fldList = fldStr.split(',')
        for fld in fldList:
            self.__model.fields[fld] = ''
        fldStrs = self.__root.find('domain').find('field').text
        fldList = fldStrs.split(',')
        for fld in fldList:
            self.__model.fields[fld] = ''

    def modelParse(self):
        if self.__root == "":
            raise UnSetError("self.__root not set! Try setRoot method!") 
        name = self.__root.find('name').text
        root = self.__root
        self.__name = name
        self.__model = Model(name=name)
        self.__model.BindToManager(self.__NameManager)
   
        # set metaFile
        metaFile = self.__root.find("metaFile").text
        self.__model.metaFile = metaFile

        version = self.__root.find("version").text
        self.__model.version = version
  
        src = self.__root.find("src").text
        self.__model.src = src

        #self.__model.interval = root.find('interval').text
        root = root.find('attrVect')
        #if root.find('name')? how to handle optional 
        self.__gsize = root.find("gsize").text
        self.__nx = root.find("nx").text
        self.__ny = root.find("ny").text
        self.__field = root.find("field").text
        if self.__gsize == 0:
            self.__gsize = self.__nx*self.__ny
        self.__model.gSize = self.__gsize
        self.__setAttrVect()
        self.__setGsMap()
        #self.__setTime()
        self.__setDomain()
        self.__setMapper()
        self.__setSubroutine()
        self.__setField()
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
    __slots__ = ['__root', '__attrVect', '__isParsed', '__NameManager', '__fraction']
    def __init__(self, nameManager, seqRun):
        self.__root = ""
        self.__isParsed = False
        self.__NameManager = nameManager
        self.__attrVect = AttrVect()    
        self.__mergeSubroutine = MergeSubroutine()
        self.__fraction = None
        self.seqRun = seqRun
    
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

    @property
    def fraction(self):
        return self.__fraction


    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False

    def couplerParse(self, parser):
        if self.__root == "":
            raise UnSetError("self.__root not set! Please try setRoot method")
        root = self.__root
        modelGrid = root.find("model").text
        if root.find("name")!= None:
            name = root.find("name").text
            av = AttrVect(name=name)
            self.__attrVect = av
            av.BindToManager(self.__NameManager)
            if not self.__NameManager.FindName(av):
                raise ConfigError("try to mrg to a unexist attrVect")
        
        # fraction parse
        if root.find('fraction')!=None:
            fractionNode = root.find('fraction')
            name = "fraction_"+modelGrid
            fraclist = fractionNode.find('fraclist').text
            initName = fractionNode.find('init').text
            updateName = fractionNode.find('update').text
            mapperMethod = "mapper_comp_map"
            fracs = fractionNode.find('fracs')
            fraction = Fraction(name, modelGrid, fraclist, initName, updateName)
            for frac in fracs:
                smapper = frac.find('mapper').text
                smapper = "metaData%"+smapper
                fracLocal = frac.find('name').text
                srcGrid = frac.find('model').text
                fracMapper = FractionMapper(smapper, mapperMethod, srcGrid,\
                                            modelGrid, fracLocal)
                fraction.append(fracMapper)
            self.__fraction = fraction
            

        if root.find("srcs") != None:
            srcs = root.find("srcs")
            if root.find('model') == None:
                raise UnsetError("need model be set in composing")
            grid = root.find('model').text
            for src in srcs:
                srcAttrVectName = src.find("attrVect").text
                print srcAttrVectName
                srcAttrVect = parser.visitByName(srcAttrVectName)
                if srcAttrVect == None:
                    raise AttributeError("no such attrVect {}".format(srcAttrVectName))
                field = src.find("field").text  # shall be optional
                ## mapper parse with method now
                mapperRoot = src.find("mapper")
                mapperName = mapperRoot.find("name").text  #latter shall be generated
                mapperType = mapperRoot.find("type").text
                mapperFile = "\""+mapperRoot.find("w_file").text+"\""
                attrVect = AttrVectCpl(srcAttrVect, mapperName, grid, field=field,\
                             mapperType=mapperType, mapperFile=mapperFile)
                attrVect.BindToManager(self.__NameManager)
                attrVect.nameGenerate()
                if self.__NameManager.FindName(attrVect):
                    parser.addDict(attrVect, attrVect.name)
                mapper = Mapper(srcAttrVect,attrVect, mapType="sMat",name=mapperName)
                mapper.BindToManager(self.__NameManager)
                mapper.nameGenerate()
      
                ## parse mapper method
                mapType = mapperRoot.find("type").text
                if mapType == "offline":
                    filePath = mapperRoot.find("w_file").text
                    methodRoot = mapperRoot.find("method")   # implementation added here, 
                    phaseIdx = 0                             # latter or sooner we will add
                    prefix = "metaData%"
                    tags  = 100+3
                    for method in methodRoot:
                        methodName = method.find("name").text
                        #field = method.find("field").text
                        in_args = []
                        for arg in method.find("in_args"):
                            in_args.append(arg.text)
                        out_args = []
                        for arg in method.find("out_args"):
                            out_args.append(arg.text)
                        if phaseIdx == 0:
                            argList = ["metaData", prefix+mapperName, prefix+"cplid",prefix+grid+"_gsize",\
                                      prefix+attrVect.grid+"_gsize", "gsmap_"+grid, "gsmap_"+attrVect.grid]
                            mapper.method.setInit(mapperName, argList)
                        elif phaseIdx == 1:
                            field_arg = "field=\""+field+"\""
                            msg = "msgtag="+str(tags)
                            argList = [prefix+mapperName, srcAttrVect.name, attrVect.name, msg, field_arg, "ierr=ierr"]
                            tags+= 1
                            mapper.method.setRun(mapperName, argList)
                            string = toString(methodName, argList)
                            cw = CodeWrapper()
                            cw.appendStr(string)
                            strFormat = cw.codeBlock(grid, grid+"2cpl",flag="comp")
                            subroutine = SubroutineNode(methodName, model=grid, phase=4, inputArg=in_args, \
                                                    outputArg=out_args, strFormat=strFormat)
                            self.seqRun.addSubroutine(subroutine)
                        phaseIdx+=1
                      
                    in_args = []
                    out_args = []
                    subroutine = SubroutineNode("", model=grid, phase=1,inputArg=in_args, outputArg=out_args)
                else:
                    mapper.method = MapperMethod()
                parser.append(mapper)

        ''' 
        if root.find("fraction") != None:
            fraction = root.find("fraction")
            fraction_name = fraction.find('name').text
            self.__fraction = Fraction(fraction_name)
            args = fraction.find("args")
            for arg in args:
                arg = arg.text
                self.__fraction.append(arg)
        '''

        if root.find("mrg") != None:
            mrg = root.find("mrg")
            name = ""
            args = []
            if mrg.find("name") != None:
                name = mrg.find("name").text
            else:
                name = "mrg"+'_'
            merge = MergeSubroutine(name=name) 
            in_args = []
            if mrg.find("in_args")!=None:
                inArg = mrg.find("in_args")
                for arg in inArg:
                     in_args.append(arg.text)
            out_args = []
            if mrg.find("out_args")!=None:
                outArg = mrg.find("out_args")
                for arg in outArg:
                     out_args.append(arg.text)
            args = ["metaData"]
            for arg in in_args:
                 args.append(arg)
            for arg in out_args:
                 args.append(arg)
            merge.argList = args
            self.__mergeSubroutine = merge
            string = toString(name, args)
            cw = CodeWrapper()
            cw.appendStr(string)
            strFormat=cw.codeBlock(modelGrid, modelGrid+"2cpl",flag="comp")
            subroutine =  SubroutineNode(name, model=modelGrid, phase=5,inputArg=in_args, outputArg=out_args,\
                                        strFormat=strFormat)
            self.seqRun.addSubroutine(subroutine)
        else:
            print "no mrg"
            #parser.append(mrg)   ## bugy
        

class ScheduleParser:
    pass

class DeployParser:
    __slots__ = ["__root", "__NameManager", "__isParsed"]        
    def __init__(self, nameManager):
        self.__root = ""
        self.__isParsed = False
        self.__NameManager = nameManager

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False

    def deployParse(self, parser):
        if self.__root=="":
            raise UnsetError("self.__root not set! Please try setRoot method!")
        root = self.__root
        if root.find("cpl") != None:
            cpl = root.find("cpl")
            first = cpl.find("first").text
            last  = cpl.find("last").text
            stride = cpl.find("stride").text
            deployList = [first , last, stride]
            parser.addDistribution(deployList, 1)
        else:
            raise ComposingError("cpl not composed see in composing/deploy.xml")
        models = root.find("models")
        for model in models:
            if model.find("name").text in parser.models:
                name = model.find("name").text
                first = model.find("first").text
                last = model.find("last").text
                stride = model.find("stride").text
                ID = parser.models[name].ID
                ID = ID + 1
                deployList = [first, last, stride]
                parser.addDistribution(deployList, ID)

