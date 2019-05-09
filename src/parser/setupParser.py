#!/use/bin/python
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
from Datatype import *
from regriddingManager import *
from fractionManager import FractionManager

class Setup:
    __slots__=["__root","__isParsed","__couple"]
    def __init__(self, fileName='../../composing/setup.xml',\
      regridFile='../../composing/regriddingFile.xml', fractionFile='../../composing/fractionSet.xml'):
        tree = ET.parse(fileName)
        self.__root = tree.getroot()
        self.__isParsed = False
        self.__couple = []
        self.__coupleFile = './coupler.xml'
        self.__model = {}
        self.__regridDataLoc = RegriddingDataMrg()
        self.__fractionDataLoc = FractionManager()
        self.__regridDataLoc.initByFile(regridFile)
        self.__fractionDataLoc.initByFile(fractionFile)
        self.__fakeModel = {}

    def setupParse(self):
        modelRoot = self.__root.find('models')
        resDict = {}
        for child in modelRoot:
            if 'type' not in child.attrib:
                res =child.find('res').text
                modelName = child.find('name').text
                resDict[modelName] = res
        for child in modelRoot:
            if 'type' in child.attrib and child.attrib['type'] == 'fake':
                name = child.find('name').text
                #version = child.find('version').text
                fakeModel = {'name':name}
                self.__fakeModel[name]=fakeModel
                continue
            modelName = child.find('name').text
            self.__model[modelName] = child.find('version').text
            res = child.find('res').text
            resDict[modelName] = res
            ### compute frac relationship frac init , update
            fracType  = child.find('frac').text
            fracTypeList = fracType.split(':')
            updateFlag = False
            if 'update' in child.find('frac').attrib and \
              child.find('frac').attrib['update']=="true":
                updateFlag = True
            frac = ""
            for fracType in fracTypeList:
                frac = self.__fractionDataLoc.query(modelName, types=fracType)+":"
            frac=frac[:-1]
            fracInit = self.__fractionDataLoc.query(modelName, frac=frac,stat=1)
            fraclist = frac
            fracUpdate = ""
            if updateFlag:
                fracUpdate = self.__fractionDataLoc.query(modelName, frac=frac, stat=2)

            # parse the srcs
            if child.find('input')!=None and child.find('input').text !=None:
                attrVect = {}
                attrVect['model'] = modelName
                attrVect['name'] = modelName+'2x_'+modelName+'x'
                attrVect['src'] = []
                attrVect['fraction'] = {}
                attrVect['fraction']['init']=fracInit
                attrVect['fraction']['update']=fracUpdate
                dstAvList = []
                srcAv = []
                otherMrgArgs = [] # for fakeModel merge
                #设置src的字典
                for src in child.find('input'):
                    if 'type' in src.attrib and src.attrib['type']=='var':
                        mrgArg = src.find('var').text
                        otherMrgArgs.append(mrgArg)
                        continue
                    srcModel = src.attrib['name']
                    srcField = src.find('field').text
                    w_file = ""
                    map_type = "offline"
                    smatType = src.find('field').attrib['type']
                    srcRes = resDict[srcModel]
                    if smatType == "none":
                        map_type = "online"
                    else:
                        if srcRes == res:
                            w_file = "samegrid"
                        else:
                            w_file = self.__regridDataLoc.query(srcModel, srcRes, modelName, res, smatType )

                    srcAttrVect = srcModel+'2'+'x_'+srcModel+'x'
                    srcDict={}
                    srcDict['attrVect'] = srcAttrVect
                    srcDict['field'] = srcField
                    srcSmat = {}
                    srcSmatName = "mapper_Smat"+srcModel+"2"+modelName
                    dstAttrVect = srcModel+'2'+'x_'+modelName+'x'
                    dstAvList.append(dstAttrVect)
                    method = {}
                    phase_0 = {}
                    phase_0["name"] = "smat_init"
                    phase_0["in_args"] = []
                    phase_0["out_args"] = []
                    phase = {}
                    phaseName = "mapper_comp_map"
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
                    
                    # src frac cacultate
                    fracTypes = src.find('field').attrib['frac']
                    fracTypeList = fracTypes.split(":")
                    srcFracList = ""
                    for fracType in fracTypeList:
                        srcFrac = self.__fractionDataLoc.query(srcModel, types=fracType)
                        fraclist +=":"+srcFrac
                        srcFracList +=srcFrac+":"
                    if "fracs" not in attrVect['fraction']:
                        attrVect['fraction']['fracs'] = []
                    srcFrac = {"name":srcFracList[:-1],"mapper":srcSmatName,"model":srcModel}
                    attrVect["fraction"]['fracs'].append(srcFrac)
                     
                    #print srcDict
                    srcAv.append(srcDict)
                attrVect['src']=srcAv
                attrVect['fraction']['fraclist'] = fraclist
                mrg = {}
                mrg['name']='mrg_x2'+modelName
                mrg['out_args'] = []
                mrg['in_args'] = []
                dstAv = 'x2'+modelName+'_'+modelName+'x'
                mrg['out_args'].append(dstAv)
                for av in dstAvList:
                    #mrg['args'].append(src['attrVect'])
                    mrg['in_args'].append(av)
                for av in otherMrgArgs:
                    mrg['in_args'].append(av)
                mrg['in_args'].append(attrVect['name'])
                mrg['in_args'].append("fraction_"+modelName)
                attrVect['mrg'] = mrg
                
                self.__couple.append(attrVect)
                #mrg['args'].append('fraction')
    def dictDom(self,doc, k, v):
        print(k, v)
        key = doc.createElement(k)
        value = doc.createTextNode(v)
        key.appendChild(value)
        return key

    def genXml(self):
    
    #    生成coupler.xml
    
        doc = Document()
        root = doc.createElement('coupler')
        #doc.appendChild(root)
        for avDict in self.__couple:
            attrVect = doc.createElement('attrVect')
            name = self.dictDom(doc,'name',avDict['name'])
            model = self.dictDom(doc, 'model', avDict['model'])
            attrVect.appendChild(name)
            attrVect.appendChild(model)
              
            # create fraction dom
            fraction = doc.createElement('fraction')
            fracList = self.dictDom(doc, 'fraclist', avDict['fraction']['fraclist'])
            init = self.dictDom(doc, 'init', avDict['fraction']['init'])
            update = self.dictDom(doc, 'update', avDict['fraction']['update'])
            fracs = doc.createElement('fracs')
            print avDict['fraction']
            for frac in avDict['fraction']['fracs']:
                fracNode = doc.createElement('frac')
                nameNode = self.dictDom(doc,'name', frac['name'])
                mapperNode = self.dictDom(doc,'mapper', frac['mapper'])
                modelNode = self.dictDom(doc, 'model', frac['model'])
                fracNode.appendChild(nameNode)
                fracNode.appendChild(mapperNode)
                fracNode.appendChild(modelNode)
                fracs.appendChild(fracNode)
            fraction.appendChild(fracList)
	    fraction.appendChild(init)
            fraction.appendChild(update)
            fraction.appendChild(fracs)
            attrVect.appendChild(fraction)
            
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
        print 'write'
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

    @property
    def fakeModel(self):
        return self.__fakeModel
