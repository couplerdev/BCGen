#coding:utf-8

import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys
sys.path.append('../ir')
from argTranslator import ArgTranslator
from ir import Model, AttrVect, Mapper, GsMap, AttrVect
from ir import Subroutine
from NameManager import NameManager
from FakeModelIr import FakeModel, FakeVariables
from FractionIr import Fraction
from ErrorHandle import *
from runCodeParser import SubroutineNode, SeqRun, Node
from codeWrapper import CodeWrapper, toString


class SubroutineParserV:
    def __init__(self, ):
        self.__subroutine = None
        self.__isParsed = False
        self.__root = None
        self.inArgs = []
        self.outArgs = []
        self.subroutineNode = None
   
    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False
        self.__subroutine = None
        self.inArgs = []
        self.outArgs = []
        self.subroutineNode = None    

    def appendArgs(self, args):
        for arg in args:
            self.__subroutine.append(arg)

    def subroutineParse(self):
        if self.__root == None:
            raise UnSetError("SubroutineParserV root not set")
        self.__subroutine = Subroutine(argList=[])
        for child in self.__root:
            if child.tag == "name":
                self.__subroutine.name = child.text
            elif child.tag == "args":
                argsNode = self.__root.find('args')
                #args = []
                argTranslator = ArgTranslator()
                for arg in argsNode:
                    argVar = argTranslator.translate(arg)
                    self.__subroutine.append(argVar)
            elif child.tag == "in_args":
                root = self.__root.find("in_args")
                for sub in root:
                    if sub.tag == "arg":
                        arg = argTranslator.translate(sub)
                        self.inArgs.append(arg)
            elif child.tag == "out_args":
                root = self.__root.find("out_args")
                for sub in root:
                    if sub.tag == "arg":
                        arg = argTranslator.translate(sub)
                        self.outArgs.append(arg)
            else:
                raise NoTagError("No Such tag"+child.tag)
        self.__isParsed = True
        

    def getSubroutineNode(self, model='', phase=-1, inArgs=[], outArgs=[], strFormat=""):
        if len(self.inArgs)!=0 or len(self.outArgs)!=0:
            self.subroutineNode = SubroutineNode(self.__subroutine.name, \
                                   model, phase=phase, inputArg=self.inArgs, \
                                   outputArg=self.outArgs, strFormat=strFormat)
        elif len(inArgs)!=0 or len(outArgs)!=0:
            self.subroutineNode = SubroutineNode(self.__subroutine.name, \
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

class FakeModelParser:
    
    def __init__(self, nameManager, seqRun, root=None):
        self.__seqRun = seqRun
        self.__NameManager = nameManager
        self.__root = root
        self.__isParsed = False
        self.__fakeModel = None
        self.__deps = []
        
    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False
        

    def modelParse(self):
        if self.__root == None:
            raise UnSetError("self.__root not set! Try setRoot of FakeModelParser")
        modelName = self.__root.find('name').text
        self.__fakeModel = FakeModel(modelName)
        self.__fakeModel.BindToManager(self.__NameManager)
        self.__fakeModel.nameGenerate()
        deps = self.__root.find('deps').text.split(":")
        flds = self.__root.find('fields').text.split(",")
        self.__fakeModel.deps = deps
        self.__fakeModel.flds = flds
        argTranslator = ArgTranslator()
        variables = self.__root.find('variables')
        #subroutineParser = SubroutineParserV()
        for variable in variables:
            varType = variable.attrib['type']
            varName = variable.find('name').text
            grid = variable.find('grid').text
            varObj = FakeVariables(varType, varName, grid)
            method = variable.find('method')
            init = method.find('init') 
            for phase in init:
                subroutineParser = SubroutineParserV()
                subroutineParser.setRoot(phase)
                subrt = subroutineParser.subroutine
                if subrt.name == "mapper_comp_map":
                    subrt.append('msgtag=113')
                    subrt.append('ierr=ierr')
                varObj.addMethod('init', subrt)
            run = method.find('run')
            step = 0
            for phase in run:
                subroutineParser = SubroutineParserV()
                subroutineParser.setRoot(phase)
                subrt = subroutineParser.subroutine
                if subrt.name == "mapper_comp_map":
                    subrt.append("msgtag=113")
                    subrt.append('ierr=ierr')
                varObj.addMethod('run', subrt)
                string = toString(subrt.name, subrt.argList)
                cw = CodeWrapper()
                cw.appendStr(string)
                strFormat = cw.codeBlock("cpl","cpl",flag="cpl")
                subrtNode = subroutineParser.getSubroutineNode(model="cpl", phase=-1,\
                                     strFormat=strFormat)
                self.__seqRun.addSubroutine(subrtNode)
            # create Node        
            self.__fakeModel.addVariables(varObj) 
    @property
    def model(self):
        if not self.__isParsed:
            self.modelParse()
        return self.__fakeModel

