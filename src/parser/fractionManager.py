#coding:utf-8
# this is used to manage the fractionSet.xml
# there are several API for query and compute 
# the relative frac fraction name and method
# the fraction name is not verified for the lack
# of my time presently
# reversion hist:
# 2019/4/1 Alex
import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys
sys.path.append('../ErrorHandle')
from ErrorHandle import *

class FractionManager:
    def __init__(self):
        self.fracDict = {}
        self.initDict = {}
        self.updateDict = {}
        self.__root = None

    def initByFile(self, fileName):
        tree = ET.parse(fileName)
        self.__root = tree.getroot()
        self.dataDict = {}
        fracRoot = self.__root.find('field')
        for frac in fracRoot:
            model = frac.attrib['model']
            types = frac.attrib['type']
            name = frac.text
            if model not in self.dataDict:
                self.dataDict[model] = []
            self.dataDict[model].append((types,name))
        initRoot = self.__root.find('method').find('init')
        for sub in initRoot:
            model = sub.attrib['model']
            name = sub.find('name').text
            frac = sub.find('frac').text
            if model not in self.initDict:
                self.initDict[model] = []
            self.initDict[model].append((frac,name))
        updateRoot = self.__root.find('method').find('update')
        for sub in updateRoot:
            model = sub.attrib['model']
            name = sub.find('name').text
            frac = sub.find('frac').text
            if model not in self.updateDict:
                self.updateDict[model] = [] 
            self.updateDict[model].append((frac, name))

    def query(self,model,types="",frac="", stat=0):
        if types!="":
            fracList = self.dataDict[model] 
            for frac in fracList:
                if frac[0] == types:
                    return frac[1]
        if frac!="":
            subList = []
            if stat==1:
                subList = self.initDict[model]
            else:
                subList = self.updateDict[model]
            for sub in subList:
                if sub[0] == frac:
                    return sub[1]
        return ""
        

