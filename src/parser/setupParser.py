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
                    srcField = src.find('field').text
                    w_file = ""
                    map_type = "offline"
                    try:
                    	w_file = src.find('file').text
                    except:
                        map_type = "online"
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
