#!/usr/bin/python
#encoding:utf-8
# This module parser the instSetup.xml
# to create new case
#
#

import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys
sys.path.append('../ir')
#from DataType import *

class InstParser:
    def __init__(self, xmlFile):
        self.xmlFile = xmlFile
        self.confPath = ""
        self.instPath = ""
        self.nmlfile = ""
        self.restartfile = ""
        self.dataPath = ""
        self.datanml = ""

    def parse(self):
        tree = ET.parse(self.xmlFile)
        macro = tree.find('macro')
        self.instPath = macro.find('instPath').text
        self.confPath = macro.find('confPath').text
        
        fileLoc = tree.find('file_location')
        self.nmlfile = fileLoc.find('nmlfile').text
        self.restart_file = fileLoc.find('restart_file').text

        dataLoc = tree.find('data_location')
        self.dataPath = dataLoc.find('data_path').text
        self.datanml = dataLoc.find('data_nml').text
       
