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

bcroot = os.environ.get('BCROOT')
if (bcroot == None or bcroot.strip() == '') :
        dirname, filename = os.path.split(os.path.abspath(__file__))
        bcroot = os.path.abspath(dirname + "/../../")
else :
        bcroot = os.path.abspath(bcroot.strip())

class InstParser:
    def __init__(self, xmlFile):
        bcroot = os.environ.get('BCROOT')
        if (bcroot == None or bcroot.strip() == '') :
            dirname, filename = os.path.split(os.path.abspath(__file__))
            bcroot = os.path.abspath(dirname + "/../../")
        else :
            bcroot = os.path.abspath(bcroot.strip())
        self.xmlFile = xmlFile
        self.confPath = ""
        self.instPath = ""
        self.nmlfile = ""
        self.restartfile = ""
        self.dataPath = ""
        self.datanml = ""
        self.datarc = ""
        self.inputPath = ""

    def parse(self):
        tree = ET.parse(self.xmlFile)
        macro = tree.find('macro')
        self.instPath = macro.find('instPath').text
	self.instPath = self.instPath.replace('$(BCROOT)', bcroot)
        self.confPath = macro.find('confPath').text
	self.confPath = self.confPath.replace('$(BCROOT)', bcroot)
        
        fileLoc = tree.find('file_location')
        self.nmlfile = fileLoc.find('nmlfile').text
	self.nmlfile = self.nmlfile.replace('$(BCROOT)', bcroot)
        self.restart_file = fileLoc.find('restart_file').text

        dataLoc = tree.find('data_location')
        self.dataPath = dataLoc.find('data_path').text
	self.dataPath = self.dataPath.replace('$(BCROOT)', bcroot)
        self.datanml = dataLoc.find('data_nml').text
	self.datanml = self.datanml.replace('$(BCROOT)', bcroot)
        self.datarc = dataLoc.find('data_rc').text
	self.datarc = self.datarc.replace('$(BCROOT)', bcroot)
 
        inputLoc = tree.find('input')
        self.inputPath = inputLoc.find('input_path').text
	self.inputPath = self.inputPath.replace('$(BCROOT)', bcroot)
       
