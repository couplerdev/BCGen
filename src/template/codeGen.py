#!/usr/bin/python

################################################################################
#
#   read the ir to instantialize the template, the template is formed by the xml
#   every root has a templateManager, e.g. mapper_C class
#
################################################################################

from Cheetah.Template import Template
#import xml.tree.ElementTree as ET

class codeGenerator:
	def __init__(self, template, target_file):
		self.nameList={}
		self.template = template
		self.target_file = target_file

	def addList(self, name, _list):
		self.nameList[name]=_list

	def addItem(self, name, item):
		self.nameList[name]=item
       
	def generate(self):
		fileDescribe = open(self.template,'rb')
		templateString = fileDescribe.read()
		output = Template(templateString, searchList=[self.nameList])
		fileOutput = open(self.target_file, 'w')
		fileOutput.write(str(output))
		fileOutput.close()
		




