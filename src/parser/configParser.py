#!/usr/bin/python
#coding:utf-8
#  This module parse xml that generate config ,nml file
# and help parser to determine some var in global_var
# reversion history 
#     2018.11.1    alex: add the module
import xml.etree.ElementTree as ET
import sys
sys.path.append('../ir')
from configType import TimeConfig

class ConfigParser:
    nmlfile = 'nmlfile'
    nmlTimeHead = '&time_args'
    def __init__(self, confFile):
        self.confFile = confFile
        self.conf = TimeConfig()

    def parse(self):
        tree = ET.parse(self.confFile)
        root = tree.getroot().find('time')
        for key in self.conf.timeConf:
            if key=='comps':
                subRoot = root.find(key)
                for child in subRoot:
                    k = child.tag
                    val = child.text
                    self.conf.timeConf[key][k]=val
            else:
                val = root.find(key).text
                valType = root.find(key).attrib['type']
                valTuple = (val, valType)
                self.conf.timeConf[key]=valTuple

    def writeNml(self):
        with open(ConfigParser.nmlfile, 'a') as f:
            f.write(ConfigParser.nmlTimeHead+"\n")
            for key in self.conf.timeConf:
                if key=='comps':
                    for k in self.conf.timeConf[key]:
                        strs = k+" = "+self.conf.timeConf[key][k]+"\n"
                        f.write(strs)
                else:
                    valStr = ''
                    if self.conf.timeConf[key][1]=="string":
                        valStr="'"+self.conf.timeConf[key][0]+"'"
                    else:
                        valStr = self.conf.timeConf[key][0]
                    strs = key +" = "+valStr+"\n"
                    f.write(strs) 
            f.write("/\n")  

if __name__ == "__main__":
    confParser = ConfigParser("option.xml")
    confParser.parse()
    confParser.writeNml()
