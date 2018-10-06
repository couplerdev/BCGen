#!/use/bon/python
#coding:utf-8
#    This Parser module parse xml file to intermediate representation
#
# reversion history
#      2018.3.1    alex: add the module
#      2018.3.26   alex: modify 
#      2018.4.3    alex: fixed some bugs
#import ir
# parser: parse xml to generate intermediate representation
import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys
sys.path.append('../ir')
from ir import Model, AttrVect, Mapper, GsMap, AttrVectCpl, Fraction
from ir import ModelSubroutine, MergeSubroutine, Subroutine
from Datatype import *
sys.path.append('../ErrorHandle')
from ErrorHandle import *
from NameManager import *
from runCodeParser import SubroutineNode, SeqRun
from codeWrapper import CodeWrapper, toString

DEBUG = 1

class Parser():
    def __init__(self, couplerFile="../../composing/coupler.xml",  modelFile="../../composing/models.xml", \
                 scheduleFile="../composing/schedule.xml", deployFile="../../composing/deploy.xml", \
                 setup=True):
        self.__NameManager = NameManager()
        self.__models = {}
        self.__attrVectCouple = {}
        self.__subroutine = {}   ## mrg subroutine
           
        self.__sMapper = {}
        self.__deployDistribution = {} # format {id: [first, last, stride]}
        self.__setupModels = []
        self.__enable_setup = setup
        if setup:
            setup = Setup()
            setup.setupParse()
            setup.genXml()
            couplerFile = setup.couplerFile
            self.__setupModels = setup.model
        self.__couplerFile = couplerFile
        self.__modelFile = modelFile
        self.__scheduleFile = scheduleFile
        self.__deployFile = deployFile
        self.__fractions = {}
        self.__seqRun = SeqRun()
        self.runSubroutine = []

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
        root = self.load(self.__modelFile)
        modelParser = ModelParser(self.__NameManager, self.__seqRun)
        #index = 1
        for child in root:
            modelParser.setRoot(child)
            model = modelParser.model
            #model.ID = index
            #index = index + 1
            self.__models[model.name] = model
            self.__NameManager.register.modelDict[model.name] = model
        models = {}
        index = 1
        for model in self.__models:
            if self.__enable_setup:
                if model in self.__setupModels:
                    models[model] = self.__models[model] 
                    self.__models[model].ID = index
                    index = index+1
            else:
                models[model] = self.__models[model]
                self.__models[model].ID = index
                index = index+1
        self.__models = models  
      
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

    def parse(self):
        self.modelsParse()
        if DEBUG == 1:
            print '...................model parsed...................'
        self.coupleAttrVectParse()
        if DEBUG == 1:
            print '..............couple AttrVect parsed..............'
        self.deployParse()
        if DEBUG == 1:
            print '..................deploy parsed...................'
        
        #for node in self.__seqRun.graph.Nodes:
        #    sbr = self.__seqRun.graph.Nodes[node]
        #    print node, sbr.inEdge            


        self.runSubroutine = self.__seqRun.topology()
        
        for node in self.__seqRun.graph.Nodes:
            sbr = self.__seqRun.graph.Nodes[node]

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
        self.__subroutine.argList = args
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
        if interval_root.find('m')!=None:
	    m = interval_root.find('m').text
        if interval_root.find('d')!=None:
	    d = interval_root.find('d').text
        if interval_root.find('h')!=None:
	    h = interval_root.find('h').text
        if interval_root.find('minute')!=None:
	    minute = interval_root.find('minute').text
        if interval_root.find('sec')!=None:
	    sec = interval_root.find('sec').text
        #interval  = Interval(m, d, h, minute, sec)
        self.__model.Time = {"m":m,"d":d,"h":h,"minute":minute,"sec":sec}
        
    def __setDomain(self):
        root = self.__root
        domain_root = root.find('domain')
        field = ""
        if domain_root.find('field') != None:
            field = domain_root.find('field').text
        if domain_root.find('path') == None:
	    raise UnsetError("domain data path not set!")
        path  = domain_root.find('path').text
        self.__domain = Domain(field, path)

    def __setSubroutine(self):    # must be the last to be set
        root =self.__root.find("method")
        grid = self.__name
        args = ["my_proc","EClock", self.__model.attrVects["x2c_cc"].name, \
              self.__model.attrVects["c2x_cc"].name,"ierr"]

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find("init"))
        subroutine.appendArgs(args)
        self.__model.model_init = subroutine.subroutine

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find("run"))
        subroutine.appendArgs(args)
        subroutineModel = subroutine.subroutine
        self.__model.model_run = subroutineModel
        string = toString(subroutineModel.subroutineName, args)
        cw = CodeWrapper(grid, grid)
        cw.appendStr(string)
        strFormat = cw.getStr()
        #print toString(subroutine.subroutine.subroutineName, args)
        self.SeqRun.addSubroutine(subroutine.getSubroutineNode(model=grid, phase=2, strFormat=strFormat))

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find("final"))
        subroutine.appendArgs(args)
        self.__model.model_final = subroutine.subroutine

        ### implementation detemine subroutine generating
        ###1 for x2a_ax to x2a_aa: this are all standard subroutine mapper_comp_comm, mappers \
        ###  are rearranger
        mapper_name = "mapper_comp_name"
        msg_tag = "msg_tag"
        ierr = "ierr"
        argList = [self.srcMapper.name, self.__model.attrVects["x2c_cx"].name, \
               self.__model.attrVects["x2c_cc"].name, msg_tag, ierr]
        subroutine = Subroutine(subroutineName=mapper_name, argList=argList)
        in_args = []
        out_args = []
        in_args.append(self.__model.attrVects["x2c_cx"].name)
        out_args.append(self.__model.attrVects["x2c_cc"].name)
        string = toString(mapper_name, argList)
        cw = CodeWrapper(grid, grid+"2cpl")
        cw.appendStr(string)
        strFormat = cw.getStr()
        subroutineNode = SubroutineNode(mapper_name, model=grid, phase=1, inputArg=in_args, \
                                      outputArg=out_args, strFormat=strFormat)
        self.SeqRun.addSubroutine(subroutineNode)

        argList = [self.srcMapper.name, self.__model.attrVects["c2x_cc"].name, \
               self.__model.attrVects["c2x_cx"].name, msg_tag, ierr]
        subroutine = Subroutine(mapper_name, argList=argList)
        in_args = []
        out_args = []
        in_args.append(self.__model.attrVects["c2x_cc"].name)
        out_args.append(self.__model.attrVects["c2x_cx"].name)
        cw = CodeWrapper(grid, grid+"2cpl")
        cw.appendStr(string)
        strFormat = cw.getStr()
        subroutineNode = SubroutineNode(mapper_name, model=grid, phase=3, inputArg=in_args, \
                                       outputArg=out_args, strFormat=strFormat)
        self.SeqRun.addSubroutine(subroutineNode)



    def modelParse(self):
        if self.__root == "":
            raise UnSetError("self.__root not set! Try setRoot method!") 
        name = self.__root.find('name').text
        root = self.__root
        self.__name = name
        self.__model = Model(name=name)
        self.__model.BindToManager(self.__NameManager)

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
        self.__setTime()
        self.__setDomain()
        self.__model.domain = name+'_grid_domain'
        self.__setMapper()
        self.__setSubroutine()
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
                field = src.find("field").text  # shall be optional
                ## mapper parse with method now
                mapperRoot = src.find("mapper")
                mapperName = mapperRoot.find("name").text  #latter shall be generated
                attrVect = AttrVectCpl(srcAttrVect, mapperName, grid, field=field)
                attrVect.BindToManager(self.__NameManager)
                attrVect.nameGenerate()
                if self.__NameManager.FindName(attrVect):
                    parser.addDict(attrVect, name)
                mapper = Mapper(srcAttrVect,attrVect, mapType="sMat",name=mapperName)
                mapper.BindToManager(self.__NameManager)
                mapper.nameGenerate()
      
                ## parse mapper method
                mapType = mapperRoot.find("type").text
                if mapType == "offline":
                    filePath = mapperRoot.find("w_file").text
                    methodRoot = mapperRoot.find("method")   # implementation added here, 
                    phaseIdx = 0                             # latter or sooner we will add
                    prefix = "my_proc%"
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
                            argList = ["my_proc", prefix+mapperName, prefix+"cplid",prefix+grid+"_gsize",\
                                      prefix+attrVect.grid+"_gsize", "gsmap_"+grid, "gsmap_"+attrVect.grid]
                            mapper.method.setInit(mapperName, argList)
                        elif phaseIdx == 1:
                            field_arg = "field=\""+field+"\""
                            argList = [prefix+mapperName, srcAttrVect.name, attrVect.name, tags, field_arg, "ierr"]
                            tags+= 1
                            mapper.method.setRun(mapperName, argList)
                            string = toString(methodName, argList)
                            cw = CodeWrapper(grid, grid+"2cpl")
                            cw.appendStr(string)
                            strFormat = cw.getStr()
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
            args = ["my_proc"]
            for arg in in_args:
                 args.append(arg)
            for arg in out_args:
                 args.append(arg)
            merge.argList = args
            self.__mergeSubroutine = merge
            string = toString(name, args)
            cw = CodeWrapper("cpl", "cpl")
            cw.appendStr(string)
            strFormat=cw.getStr()
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

class Setup:
    __slots__=["__root","__isParsed","__couple"]
    def __init__(self, fileName='../../composing/setup.xml'):
        tree = ET.parse(fileName)
        self.__root = tree.getroot()
        self.__isParsed = False
        self.__couple = []
        self.__coupleFile = './coupler.xml'
        self.__model = []

    def setupParse(self):
        for child in self.__root:
            modelName = child.find('name').text
            self.__model.append(modelName)
            if child.find('input')!=None and child.find('input').text !=None:
                attrVect = {}
                attrVect['model'] = modelName
                attrVect['name'] = modelName+'2x_'+modelName+'x'
                attrVect['src'] = []
                dstAvList = []
                srcAv = []
                #设置src的字典
                for src in child.find('input'):
                    srcModel = src.attrib['name']
                    srcField = src.text
                    srcAttrVect = srcModel+'2'+'x_'+srcModel+'x'
                    srcDict={}
                    srcDict['attrVect'] = srcAttrVect
                    srcDict['field'] = srcField
                    srcSmat = {}
                    srcSmatName = "mapper_Smat"+srcModel+"2"+modelName
                    dstAttrVect = srcModel+'2'+'x_'+modelName+'x'
                    dstAvList.append(dstAttrVect)
                    w_file = ""
                    map_type = "offline"
                    method = {}
                    phase_0 = {}
                    phase_0["name"] = "smat_init"
                    phase_0["in_args"] = []
                    phase_0["out_args"] = []
                    phase = {}
                    phaseName = "mapper_comp_comm"
                    phase["name"] = phaseName
                    args=srcAttrVect
                    in_args = []
                    in_args.append(args)
                    args = dstAttrVect
                    out_args = []
                    out_args.append(args)
                    phase["in_args"]=in_args
                    phase["out_args"]=out_args
                    method = []
                    method.append(phase_0)
                    method.append(phase)
                    srcSmat["name"] = srcSmatName
                    srcSmat["field"] = srcField   # assume srcField are same
                    srcSmat["w_file"] = w_file
                    srcSmat["method"]  = method
                    srcSmat["type"] = map_type
                    srcDict['mapper'] = srcSmat
                    
                    #print srcDict
                    srcAv.append(srcDict)
                attrVect['src']=srcAv
                mrg = {}
                mrg['name']='mrg_x2'+modelName
                mrg['out_args'] = []
                mrg['in_args'] = []
                dstAv = 'x2'+modelName+'_'+modelName+'x'
                mrg['out_args'].append(dstAv)
                for av in dstAvList:
                    #mrg['args'].append(src['attrVect'])
                    mrg['in_args'].append(av)
                mrg['in_args'].append(attrVect['name'])
                attrVect['mrg'] = mrg
                self.__couple.append(attrVect)
                #mrg['args'].append('fraction')
    def dictDom(self,doc, k, v):
        key = doc.createElement(k)
        value = doc.createTextNode(v)
        key.appendChild(value)
        return key

    def genXml(self):
        doc = Document()
        root = doc.createElement('coupler')
        #doc.appendChild(root)
        for avDict in self.__couple:
            attrVect = doc.createElement('attrVect')
            name = self.dictDom(doc,'name',avDict['name'])
            model = self.dictDom(doc, 'model', avDict['model'])
            attrVect.appendChild(name)
            attrVect.appendChild(model)
            srcs = doc.createElement('srcs')
            for src in avDict['src']:
                srcNode = doc.createElement('src')
                name = self.dictDom(doc, 'attrVect', src['attrVect'])
                field = ""
                try:
                    field = self.dictDom(doc, 'field', src['field'])
                except:
                    pass
                ##set mapper
                mapperNode = doc.createElement("mapper")
                mapper_name = self.dictDom(doc, 'name', src['mapper']['name'])
                mapper_type = self.dictDom(doc, 'type', src['mapper']['type'])
                mapper_w_file = self.dictDom(doc, 'w_file', src['mapper']['w_file'])
                mapperMethod = doc.createElement("method")

                for phase in src['mapper']['method']:
                    phaseNode=  doc.createElement("phase")
                    phase_name = self.dictDom(doc,'name', phase['name'])
                    in_args = None
                    if phase['in_args'] == []:
                        in_args = self.dictDom(doc, 'in_args', '')
                    else:
                        in_args = doc.createElement("in_args")
                        for in_arg in phase['in_args']:
                            arg = self.dictDom(doc, 'arg', in_arg)
                            in_args.appendChild(arg)
                    out_args = None
                    if phase['out_args'] == []:
                        out_args = self.dictDom(doc, 'out_args', '')
                    else:
                        out_args = doc.createElement("out_args")
                        for out_arg in phase['out_args']:
                            arg = self.dictDom(doc, 'arg', out_arg)
                            out_args.appendChild(arg)
                    phaseNode.appendChild(phase_name)
                    phaseNode.appendChild(in_args)
                    phaseNode.appendChild(out_args)
                    mapperMethod.appendChild(phaseNode)
                mapperNode.appendChild(mapper_name)
                mapperNode.appendChild(mapper_type)
                mapperNode.appendChild(mapper_w_file)
                mapperNode.appendChild(mapperMethod)    
                srcNode.appendChild(name)
                srcNode.appendChild(field)
                srcNode.appendChild(mapperNode)
                srcs.appendChild(srcNode)
            attrVect.appendChild(srcs)
            mrg = avDict['mrg']
            mrgNode = doc.createElement('mrg')
            name = self.dictDom(doc, 'name', mrg['name'])
            mrgNode.appendChild(name)
            in_args = doc.createElement('in_args')
            for arg in mrg['in_args']:
                argDom = self.dictDom(doc, 'arg',arg)
                in_args.appendChild(argDom)
            mrgNode.appendChild(in_args)
            out_args = doc.createElement('out_args')
            for arg in mrg['out_args']:
                argDom = self.dictDom(doc, 'arg', arg)
                out_args.appendChild(argDom)
            mrgNode.appendChild(out_args)
            attrVect.appendChild(mrgNode)
            root.appendChild(attrVect)
        doc.appendChild(root)
        f = open(self.__coupleFile,'w')
        doc.writexml(f, indent='\t',newl='\n',addindent='\t',encoding='utf-8')
    
    @property
    def couple(self):
        return self.__couple

    @property
    def couplerFile(self):
        return self.__coupleFile

    @property
    def model(self):
        return self.__model
