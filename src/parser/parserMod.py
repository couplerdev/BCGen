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
from xml.dom.minidom import Document
import sys
sys.path.append('../ir')
from ir import Model, AttrVect, Mapper, GsMap, AttrVectCpl, Fraction
from ir import ModelSubroutine, MergeSubroutine
from Datatype import *
sys.path.append('../ErrorHandle')
from ErrorHandle import *
from NameManager import *
from runCodeParser import SubroutineNode, SeqRun


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
        self.__seqRun = seqRun()

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
            if model in self.__setupModels:
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
        self.inArg = []
        self.outArg = []
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
                                self.inArg.append(sub.text)
                    elif child.tag == "out_args":
                        root = self.__root.finf("in_args")
                        
                    else:
                        raise NoTagError("No such tag "+child.tag)
                self.__isParsed = True
                #print self.__subroutine.subroutineName
    
    def appendArgs(self, args):
        for arg in args:
            self.__subroutine.append(arg)
           

    def getSubroutineNode(self, model='', phase=-1, inArgs=[], outArgs=[]):
        if len(self.inArgs)!=0 and len(self.outArgs)!=0:
	    self.subroutineNode = Subroutine(self.__subroutine.subroutineName, \
                                   model, phase=phase, inArgs=self.inArgs, \
                                   outArgs=self.outArgs)
        else if len(inArgs)!=0 and len(outArgs)!=0:
            self.subroutineNode = Subroutine(self.__subroutine.subroutineName, \
                                   model, phase=phase, inArgs=self.inArgs, \
                                   outArgs=self.outArgs)
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
        self.seqRun = seqRun 

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
        interval  = Interval(m, d, h, minute, sec)
        self.__model.Time = Time(base, interval)
        
    def __setDomain(self):
        domain_root = root.find('domain')
        field = ""
        if domain_root.find('field') != None:
            field = domain_root.find('field').text
        if domain_root.find('path') == None:
	    raise UnsetError("domain data path not set!")
        path  = domain_root.find('path').text
        self.__domain = Domain(field, path)

    def modelParse(self):
        if self.__root == "":
            raise UnSetError("self.__root not set! Try setRoot method!") 
        name = self.__root.find('name').text
        root = self.__root
        self.__name = name
        self.__model = Model(name=name)
        self.__model.BindToManager(self.__NameManager)

        args = []                              #设置model内置方法的参数，主要是与现在的
        args.append(self.__model.attrVects["x2c_cc"])   #相关的，公共的生成，
        args.append(self.__model.attrVects["c2x_cc"])   #之后会把这块独立出来
        args.append(self.__model.attrVects["comp"])
        subroutine = SubroutineParser()
        subroutine.setRoot(root.find('init'))   ## need ErrorHandle
        subroutine.appendArg(args)
        self.__model.model_init = subroutine.subroutine
        #self.seqRun.addSubroutine(subroutine.getSubroutineNode(self.__name))
        
        subroutine.setRoot(root.find('run'))
        subroutine.appendArgs(args)
        self.__model.model_run = subroutine.subroutine
        run = root.find("run")
        in_args = []
        in_argRoot = run.find("in_arg")
        for arg in in_argRoot:
            in_args.append(arg.text)
        out_args = []
        out_argRoot = run.find("out_arg")
        for arg in in_argRoot:
            out_args.append(arg.text)
        self.seqRun.addSubroutine(subroutine.getSubroutineNode(model=self.__name, phase=2,inArgs=in_args,outArg out_args ))
        
        #### implementation detemine subroutine generating
        ###1 for x2a_ax to x2a_aa : this are all standard subroutine mapper_comp_comm, mappers are
        ### rearranger
        mapper_name =  "mapper_comp_comm"
        msg_tag = "msg_tag"
        ierr = "ierr"
        argList = []
        argList.append(self.srcMapper.name, self.__model.attrVects["x2c_cx"].name,\
                self.__model.attrVects["x2c_cc"].name, msg_tag, ierr)
        subroutine = Subroutine(subroutineName=mapper_name, argList)
        in_args = []
        out_args = []
        in_args.append(self.__model.attrVect["x2c_cx"].name)
        out_args.append(self.__model.attrVect["x2c_cc"].name)
        subroutineNode = SubroutineNode(subroutineName, model=self.__name, phase=1, \
                                      inArgs=in_args, outArgs=out_args)
        self.SeqRun.addSubroutine(subroutineNode)

             

        subroutine.setRoot(root.find('final'))
        subroutine.appendArgs(args)
        self.__model.model_final = subroutine.subroutine
        #self.seqRun.addSubroutine(subroutine.getSubroutineNode(self.__name))

        self.__model.interval = root.find('interval').text
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
                print av.name
                raise ConfigError("try to mrg to a unexist attrVect")
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
f                       field = method.find("field").text
                        in_args = []
                        for arg in methodRoot.find("in_args"):
                            in_args.append(arg.text)
                        out_args = []
                        for arg in methodRoot.find("out_args"):
                            out_args.append(arg.text)
                        if phaseIdx == 0:
                            argList = ["my_proc", prefix+mapperName, prefix+"cplid",prefix+grid+"_gsize",\
                                      prefix+attrVect.grid+"_gsize", "gsmap_"+grid, "gsmap_"+attrVect.grid]
                            mapper.method.setInit(mapperName, argList)
                        else if phaseIdx == 1:
                            argList = [prefix+mapperName, srcAttrVect.name, attrVect.name, tags, field, "ierr"]
                            tags+= 1
                            mapper.method.setRun(mapperName, argList)
                            subroutine = Subroutine(methodName, model=grid, phase=3, inputArg=in_args, \
                                                    outputArg=out_args)
                            self.reqRun.addSubroutine(subroutine)
                        phaseIdx+=1
                      
                    in_args = []
                    out_args = []
                    subroutine = Subroutine("", model=grid, phase=1,inputArg=in_args, outputArg=out_args)
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
            subroutine =  Subroutine(name, model=modelGrid, phase=5,in_args, out_args)
            self.SeqRun.addSubroutine(subroutine)
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
        print parser.models
        for model in models:
            if model.find("name").text in parser.models:
                name = model.find("name").text
                print name
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
                srcAv = []
                for src in child.find('input'):
                    srcModel = src.attrib['name']
                    srcField = src.text
                    srcAttrVect = srcModel+'2'+'x_'+srcModel+'x'
                    srcSmat = "mapper_SMat"+srcModel+"2"+modelName
                    srcDict={}
                    srcDict['name'] = srcModel
                    srcDict['attrVect'] = srcAttrVect
                    srcDict['field'] = srcField
                    srcDict['mapper'] = srcSmat
                    #print srcDict
                    srcAv.append(srcDict)
                attrVect['src']=srcAv
                mrg = {}
                mrg['name']='mrg_x2'+modelName
                mrg['args']=[]
                mrg['args'].append('my_proc')
                dstAv = 'x2'+modelName+'_'+modelName+'x'
                mrg['args'].append(dstAv)
                for src in srcAv:
                    #mrg['args'].append(src['attrVect'])
                    srcName = src['name']
                    aV = srcName+'2x_'+modelName+'x'
                    mrg['args'].append(aV)
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
            print 'haha'
            attrVect = doc.createElement('attrVect')
            name = self.dictDom(doc,'name',avDict['name'])
            model = self.dictDom(doc, 'model', avDict['model'])
            attrVect.appendChild(name)
            attrVect.appendChild(model)
            srcs = doc.createElement('srcs')
            for src in avDict['src']:
                srcNode = doc.createElement('src')
                name = self.dictDom(doc, 'attrVect', src['attrVect'])
                field = self.dictDom(doc, 'field', src['field'])
                mapper = self.dictDom(doc, 'mapper', src['mapper'])
                srcNode.appendChild(name)
                srcNode.appendChild(field)
                srcNode.appendChild(mapper)
                srcs.appendChild(srcNode)
            attrVect.appendChild(srcs)
            mrg = avDict['mrg']
            mrgNode = doc.createElement('mrg')
            name = self.dictDom(doc, 'name', mrg['name'])
            mrgNode.appendChild(name)
            args = doc.createElement('args')
            for arg in mrg['args']:
                argDom = self.dictDom(doc, 'arg',arg)
                args.appendChild(argDom)
            mrgNode.appendChild(args)
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
