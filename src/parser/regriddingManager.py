#coding:utf-8
# this parser module parse the xml file to get the weights 
# file for relavent components
# see in regriddingFile.xml
import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys
sys.path.append('../ErrorHandle')
from ErrorHandle import *

class RegriddingDataMrg:
    def __init__(self):
        self.dataDict = {}
        self.__root = None

    def initByFile(self, fileName):
        tree = ET.parse(fileName)
        self.__root = tree.getroot()
        self.dataDict = {}
        for child in self.__root:
            model = child.attrib['model']
            res = child.attrib['res']
            if model not in self.dataDict:
                self.dataDict[model] = {}
            if res not in self.dataDict[model]:
                self.dataDict[model][res] = []
            for targetWgt in child:
                targetModel = targetWgt.attrib['model']
                targetRes   = targetWgt.attrib['res']
                wgts = {"model":"","res":"","state":"none","flux":"none"}
                wgts["model"] = targetModel
                wgts["res"] = targetRes
                for wgtFile in targetWgt:
                    if wgtFile.attrib["type"] == "state":
                        wgts["state"] = wgtFile.text
                    elif wgtFile.attrib["type"] == "flux":
                        wgts["flux"] = wgtFile.text
                self.dataDict[model][res].append(wgts)

    def query(self, srcModel, srcRes, targetModel, targetRes, t):
        errMsg = "in RegriddingFile parsing,"+srcModel+','+srcRes
        if srcModel not in self.dataDict:
            raise NoneProperValueError(errMsg) 
        if srcRes not in self.dataDict[srcModel]:
            raise NoneProperValueError(errMsg)
        wgts = self.dataDict[srcModel][srcRes]
        found  = False
        ret = ""
        for wgt in wgts:
           if targetModel == wgt["model"] and targetRes == wgt["res"]:
               if t=="state" and wgt["state"] != "none":
                   ret = wgt["state"]
                   found = True
               elif t=="flux" and wgt["flux"] != "none":
                   ret = wgt["state"]
                   found = True
        if not found:
            raise NoneProperValueError(errMsg)
        return ret
