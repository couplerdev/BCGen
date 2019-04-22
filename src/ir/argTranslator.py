#encoding:utf-8
#!/usr/bin/python
import sys
sys.path.append('../ErrorHandle')
from ErrorHandle import *
import xml.etree.ElementTree as ET
from xml.dom.minidom import Document


class Arg:

    def __init__(self, argType='none', model='none',translate=True,\
                 content='none', matType='S',src='none', dst='none', pes='none'):
        self.__argType = argType
        self.__model = model
        self.__content = content
        self.__translate = translate
        self.__matType = matType
        self.__src = src
        self.__dst = dst
        self.__pes = pes

    @property
    def argType(self):
        return self.__argType

    @property
    def pes(self):
        return self.__pes
    
    @property
    def model(self):
        return self.__model
	
    @property
    def translate(self):
        return self.__translate

    @property
    def content(self):
        return self.__content

    @property
    def src(self):
        return self.__src
  
    @property
    def dst(self):
        return self.__dst

    @property
    def matType(self):
        return self.__matType

class CompTranslator:

    def __init__(self):
        pass

    def translate(self, arg):
        return "metaData%"+arg.model

class FieldTranslator:
    def __init__(self):
        pass

    def translate(self, arg):
        return "metaData%"+arg.content


class FractionTranslator:
    def __init__(self):
        pass
    def translate(self, arg):
        return "fraction_"+arg.model

class MapperTranslator:
    def __init__(self):
        pass
    def translate(self, arg):
        return "metaData%mapper_"+arg.matType+'mat'+arg.src+"2"+arg.dst

class AttrVectTranslator:
    def __init__(self):
        pass

    def translate(self, arg):
        src = 'x'
        dst = 'x'
        if arg.src != 'cpl':
            src = arg.src
        else:
            dst = arg.dst
        name = src+"2"+dst+"_"+arg.model+"x"
        return name

class DomainTranslator:
    def __init__(self):
        pass

    def translate(self, arg):
        pes = 'x'
        if arg.pes != 'cpl':
            pes = arg.model
        return "domain_"+ arg.model+pes

class GsMapTranslator:
    def __init__(self):
        pass

    def translate(self, arg):
        pes = 'x'
        if arg.pes != 'cpl':
            pes = arg.model 
        return "gsMap_"+arg.model+pes

class MpicomTranslator:
    def __init__(self):
        pass

    def translate(self, arg):
        pes = 'x'
        if arg.pes == 'cpl':
            if arg.model == 'global':
                return "metaData%mpi_glocomm"
            elif arg.model == 'cpl':
                return "metaData%mpi_cpl" 
            return 'metaData%mpi_model'+arg.model+"2cpl"
        else:
            return "metaData%mpi_model"+arg.model
            

class ArgTranslator:

    def __init__(self, version=1):
        self.__transDict = {}
        if version == 0:
            self.initVersion0()
        else:
            self.initVersion1()

    def initVersion1(self):
        mapperTranslator = MapperTranslator()
        fractionTranslator = FractionTranslator()
        compTranslator = CompTranslator()
        attrVectTranslator = AttrVectTranslator()
        fieldTranslator = FieldTranslator()
        gsmapTranslator = GsMapTranslator()
        mpiTranslator = MpicomTranslator()
        domainTranslator = DomainTranslator()
        self.__transDict['comp'] = compTranslator
        self.__transDict['fraction'] = fractionTranslator
        self.__transDict['mapper'] = mapperTranslator
        self.__transDict['attrVect'] = attrVectTranslator
        self.__transDict['field'] = fieldTranslator
        self.__transDict['gsmap'] = gsmapTranslator
        self.__transDict['mpicom'] = mpiTranslator
        self.__transDict['domain'] = domainTranslator

    def translateFromArg(self, arg):    
        if not arg.translate:
            return arg.content
        else:
            if arg.argType not in self.__transDict:
                raise NoneProperValueError('in ArgTranslator:'+',arg.argType:'+arg.argType)
            else:
                return self.__transDict[arg.argType].translate(arg)

    def translateFromXml(self, arg):
        argType = 'none'
        model = 'none'
        translate = True
        content = "none"
        matType = 'S'
        src = 'none'
        dst = 'none'
        pes = 'none'
        if len(arg.attrib) == 0 :
            content = arg.text
            translate = False
        else:
            if 'model' in arg.attrib:
                model = arg.attrib['model']
            if 'src' in arg.attrib:
                src = arg.attrib['src']
            if 'dst' in arg.attrib:
                dst = arg.attrib['dst']
            if 'matType' in arg.attrib:
                matType = arg.attrib['matType']
            if 'field' in arg.attrib:
                content = arg.attrib['field']
            if 'pes' in arg.attrib:
                pes = arg.attrib['pes']
            argType = arg.text
        return self.translateFromArg(Arg(argType=argType, model=model, translate=translate,\
                            content=content, matType=matType, src=src, dst=dst, pes=pes))

    def translate(self, arg):
        if isinstance(arg, Arg):
            return self.translateFromArg(arg)
        else:
            return self.translateFromXml(arg)    # not safe 
       

if __name__ == "__main__":
    print 'this is a test'
    at = ArgTranslator()
    arg = Arg(argType='comp',model='atm')
    arg1 = Arg(content='xao_ax',translate=False)
    arg2 = Arg(argType='fraction', model='ocn')
    arg3 = Arg(argType='mapper', src='atm',dst='ocn')
    print at.translate(arg)
    print at.translate(arg1)
    print at.translate(arg2)
    print at.translate(arg3)
