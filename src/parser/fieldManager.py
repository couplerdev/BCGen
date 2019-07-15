#!/usr/bin/python
import sys, os
import xml.etree.ElementTree as ET

class FieldMeta:
    def __init__(self, shortname, longname, stdname, units):
        self.shortname = shortname
        self.longname = longname
        self.stdname = stdname
        self.units = units


class FieldManager:
    __instance = None
    def __new__(cls, *args, **kwargs):
        if cls.__instance is None:
            cls.__instance = super(FieldManager, cls).__new__(cls, *args, **kwargs)
        return cls.__instance

    def __init__(self):
        self.fldsQuery = {}  # var:[]
        self.fieldQuery = {}  #shortname:FieldMeta
        self.model = {} # model and others
        self.model['dom'] = 1

    def addModel(self, model):
        self.model[model] = 1
 
    def queryShortname(self, fldsVar):
        return self.fldsQuery[fldsVar] 

    def queryField(self, shortName):
        return self.fieldQuery[shortName]

    def queryBuild(self, fieldXml):
        tree = ET.parse(fieldXml)
        root = tree.getroot()
        fields = root.find('fields')
        fldDict = {}
        for child in fields:
            name = child.attrib['name']
            key = name.split('_')[1]
            val = []
            if key=='dom': 
                self.fldsQuery[name] = child.find('field').text
                val = self.fldsQuery[name].split(':')
                if os.environ.get('VERBOSE') == 'true'  :
                    print name,val
            elif key[0:2]=='x2':
                model = key[2:]
                if model in self.model:
                    self.fldsQuery[name]  = child.find('field').text or ''
                    val = self.fldsQuery[name].split(':')
            elif key[-2:]=='2x':
                model = key[:-2]
                if model in self.model:
                    self.fldsQuery[name] = child.find('field').text or ''
                    val = self.fldsQuery[name].split(':')
            elif key=="xao":
                self.fldsQuery[name] = child.find('field').text
                var = self.fldsQuery[name].split(':')
            if val != []:
                for v in val:
                    fldDict[v] = 1
        fldMeta = root.find('fldMeta')
        for name in self.fldsQuery:
            val = self.fldsQuery[name]
            #idx = 50
            #while idx<len(val):
            #    val = val[:idx-1]+"&\n"+val[idx-1:]
            #    idx+=50
            self.fldsQuery[name] = val
        for child in fldMeta:
            sname = child.find('shortname').text
            if sname in fldDict:
                lname = child.find('longname').text
                stdname = child.find('stdname').text
                units = child.find('units').text
                fm = FieldMeta(sname, lname, stdname, units)
                self.fieldQuery[sname] = fm
    def printf(self):
        if os.environ.get('VERBOSE') == 'true'  :
            print self.fldsQuery
            print self.fieldQuery
        
if __name__ == "__main__":
    fm = FieldManager()
    fm.addModel('atm')
    fm.queryBuild('field.xml')
    lname = fm.queryShortname("flds_x2atm_fields")
    print len(lname)
    #fm.printf()
