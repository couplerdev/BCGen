#!/usr/bin/python
#coding:utf-8
#
#   the instCreator create an inst for esm
#
import sys, os
sys.path.append('../parser')
sys.path.append('../template')
sys.path.append('../ir')
from codeGen import codeGenerator
from parserMod import Parser
from ir import Model
from configType import TimeConfig
from MetaManager import MetaManager
from instParser import InstParser

'''
codeMapper work as a batch code generator:
get a mapper: templateFile, codeFile, [cfgs_list]

'''
class TempConfig:
    def __init__(self, template, codeFile, cfgs):
        self.template = template
        self.codeFile = codeFile
        if type(cfgs) != type({}):
            raise TypeError("cfgs not list") 
        self.cfgs = cfgs
        

class CodeMapper:
    def __init__(self, mappers=[]):
        self.mappers = mappers

    def addTempConf(self, tempConf):
        self.mapper.append(tempConf)

    def genCode(self):
        for spec in self.mappers:
            code = codeGenerator(spec.template, spec.codeFile)
            for cfg in spec.cfgs:
                code.addList(cfg, spec.cfgs[cfg])
            code.generate()


def get_SMat_relation(attrVects):
    model_names = []
    model_SMats = {}
   
    for av in attrVects:
        dst_model = attrVects[av][0].dstModel
        model_name = dst_model.name
        model_gsmap_name = attrVects[av][0].srcModel.gsMaps['cpl'].name
        model_SMats[model_name] = {}
        model_SMats[model_name]['src'] = av
        model_SMats[model_name]['dst'] = []
        model_SMats[model_name]['gm'] = model_gsmap_name
        model_names.append(model_name)

    for av in attrVects:
        for src_x_dst_x_av in attrVects[av]:
            src_model = src_x_dst_x_av.srcModel
            dst_model = src_x_dst_x_av.dstModel
            src_model_name = src_model.name
            dst_model_name = dst_model.name
            model_gsmap_name = src_model.gsMaps['cpl'].name
            dst_gsmap_name = dst_model.gsMaps['cpl'].name
            dst_field = src_x_dst_x_av.field

            dst_info = {
		'dst_model_name': dst_model_name,
                'dst_av': src_x_dst_x_av,
                'dst_gm': dst_gsmap_name,
                'dst_mapper': src_x_dst_x_av.mapperName,
                'dst_field': dst_field,
                'w_file': src_x_dst_x_av.mapperFile,
		'smat_size':3
            } 
            if src_model_name not in model_SMats:
                model_SMats[src_model_name] = {}
                model_SMats[src_model_name]['src'] = src_x_dst_x_av.srcName
                model_SMats[src_model_name]['dst'] = []
                model_gsmap_name = dst_model.gsMaps['cpl'].name
                model_SMats[src_model_name]['gm'] = model_gsmap_name
            model_SMats[src_model_name]['dst'].append(dst_info)
    return model_SMats



class InstCreator:
    couplerCodePath='../../baseCpl'
    def __init__(self):
        self.metaManager = MetaManager()        
        self.parser = None     
        self.confXmlPath = {}
        self.setDefaultXmlPath()

    def setDefaultXmlPath(self):
        self.absPath = os.getcwd()
        prefix = self.absPath+"/../../composing/"
        self.confXmlPath['instSetup.xml'] = prefix+"instSetup.xml"
        self.confXmlPath['setup.xml'] = prefix+"setup.xml"
        self.confXmlPath['coupler.xml'] = prefix+"coupler.xml"
        self.confXmlPath['models.xml'] = prefix+"models.xml"
        self.confXmlPath['deploy.xml'] = prefix+"deploy.xml"
        self.confXmlPath['field.xml'] = prefix+"field.xml"
        self.confXmlPath['option.xml'] = prefix+"option.xml"
        self.confXmlPath['instSetup.xml'] = prefix+"instSetup.xml"

    def setXmlPath(self, name, val):
        if name not in self.confXmlPath:
            raise NoKeyError("no such key in confXmlPath")
        self.confXmlPath[name] = val

    def getIr(self):
        setupPath = self.confXmlPath['setup.xml']
        couplerPath = self.confXmlPath['coupler.xml']
        modelsPath = self.confXmlPath['models.xml']
        deployPath = self.confXmlPath['deploy.xml']
        fieldPath =  self.confXmlPath['field.xml']
        optionPath = self.confXmlPath['option.xml']
        instSetupPath = self.confXmlPath['instSetup.xml']
        self.parser = Parser(couplerFile=couplerPath, modelFile=modelsPath, \
                             deployFile=deployPath, fieldFile=fieldPath, \
                             setupFile=setupPath,setup=True)
        self.parser.parse()
        self.metaManager.setConfigMeta(optionPath)
        self.metaManager.setMacroMeta(instSetupPath)

        self.proc_cfgs = [self.parser.models[m] for m in self.parser.models]
        self.field_cfgs = [self.parser.fldMetaDict[m] for m in self.parser.fldMetaDict] 
        self.fieldVar_cfgs = self.parser.fldDict
        self.merge_subroutines = [self.parser.subroutine[m] for m in self.parser.subroutine]
        self.subrt_cfgs = [node.data.strFormat for list_ in self.parser.runSubroutine \
                           for node in list_]
        self.fraction_cfgs = self.parser.fractions
        self.deploy_cfgs = self.parser.deploy
        self.merge_Cfgs = get_SMat_relation(self.parser.attrVectCouple)

    def codeGenerate(self):
        if self.parser == None:
            raise NotSetError('parser not set in InstCreator!!!')
        searchTmp = TmpConfig('searchSet_Template.py', "search_set.py",\
                              {'models':proc_cfgs,'merge_cfgs':merge_cfgs})
        searchGen = codeMapper([searchTmp])
        searchGen.genCode()
        from search_set import *
        proc_cfgs = self.proc_cfgs
        deploy_cfgs = self.deploy_cfgs
        merge_cfgs = self.merge_cfgs
        fieldVar_cfgs = self.fieldVar_cfgs
        field_cfgs = self.field_cfgs
        subrt_cfgs = self.subrt_cfgs
        merge_subroutines = self.merge_subroutines
        fraction_cfgs = self.fraction_cfgs 
        manageTmp = TempConfig("procM_Template.F90", "manage.F90",{"proc_cfgs":proc_cfgs})
        deployTmp = TempConfig("deploymod_Template.F90", "deploy_mod.F90",\
                              {'proc_cfgs':proc_cfgs,'deploy_cfgs':deploy_cfgs})
        timeDefTmp = TempConfig("timeDef_Template.F90","time_def.F90",{"proc_cfgs":proc_cfgs})
        timeCesmTmp = TempConfig("timeCesm_Template.F90", "global_var.F90", {"proc_cfgs":proc_cfgs})
        globalTmp  = TempConfig("globalVar_Template.F90","global_var.F90",{"proc_cfgs":proc_cfgs,\
                                                          "merge_cfgs":merge_cfgs,"fieldVar_cfgs":fieldVar_cfgs})
        fieldTmp = TempConfig("baseField_Template.F90","base_field_F90", {"field_cfgs":field_cfgs,\
                                                                          "fieldVar_cfgs":fieldVar_cfgs})
        baseCplTmp = TempConfig("baseCpl_Template.F90","baseCpl.F90",\
                              {"proc_cfgs":proc_cfgs, "merge_subroutines":merge_subroutines,\
                               "merge_cfgs":merge_cfgs,"model_cfgs":model_cfgs, \
                               "subrt_cfgs":subrt_cfgs,'fraction_cfgs':fraction_cfgs})

    def createMakefile(self):
        # build prerequists libbcpl.a
        currDir = os.getcwd()
        os.chdir(InstCreator.couplerCodePath)
        cmdBuild = 'make'
        os.system(cmdBuild)
        os.chdir(currDir)
        # mv libbcpl.a to lib
        
        cmdMv = 'cp '+InstCreator.couplerCodePath+'/lib/libbcpl.a '+self.metaManager.instPath+'/lib'
        os.system(cmdMv)
        # mv required comp togather with its Makefile (may be modified) to models
        for model in self.proc_cfgs:
            name = model.name
            modelDir = InstCreator.couplerCodePath+"/src/models/"+name
            cmdCpComp = 'cp '+modelDir+"/* "+self.metaManager.instPath+"/models/"+name  
            os.system(cmdCpComp)
        # mv Cpl comp to models 
        cplDir = InstCreator.couplerCodePath+"/src/models/cpl"
        cmdCpCpl = 'cp '+cplDir+"/* "+self.metaManager.instPath+"/models/cpl"
        os.system(cmdCpCpl)
        # build Makefile from template
        
        # mv Makefile

    def instCreate(self):
        self.getIr()
        instPath = self.metaManager.instPath
        confPath = self.metaManager.confPath
        os.mkdir(instPath) 
        confPath= instPath+"/"+confPath
        srcPath = instPath+"/src"
        libPath = instPath+"/lib"
        includePath = instPath+"/include"
        binPath = instPath+"/bin"
        os.mkdir(confPath)
        os.mkdir(srcPath)
        os.mkdir(libPath)
        os.mkdir(includePath)
        os.mkdir(binPath)

        # copy src include mod and lib .a .o to instDir
        BCGenPath = "../../baseCpl/"
        copyIncludeCmd = "cp -r "+BCGenPath+"include/* "+includePath
        copyLibCmd  = "cp -r "+BCGenPath+"lib/* "+libPath
        os.system(copyIncludeCmd)
        os.system(copyLibCmd)
        
        # create models dir & copy relavent code and Makefile
        modelsPath = instPath+"/models/"
        os.mkdir(modelsPath)
        for model in self.proc_cfgs:
            modelDir = modelsPath+model.name
            os.mkdir(modelDir)
        cplDir = modelsPath+"/cpl"
        os.mkdir(cplDir)
        
        # copy all sorts of conf file to conf dir
        copyNmlCmd = "cp "+self.metaManager.nmlfile+" "+confPath
        os.system(copyNmlCmd)
        self.createMakefile()

if __name__ == "__main__":
    instCreator = InstCreator()
    instCreator.instCreate()
