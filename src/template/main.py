#!/usr/bin/python
#coding:utf-8
from codeGen import codeGenerator
import sys
sys.path.append('../parser')
from parserMod import Parser

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



if __name__ == "__main__":
    parser = Parser()
    parser.parse()
    proc_cfgs = [ parser.models[m] for m in parser.models]
    for m in parser.models:
        print parser.models[m].model_init.toString()
    merge_subroutines = [ parser.subroutine[m] for m in parser.subroutine]
    subrt_cfgs = [ node.data.strFormat for list_ in parser.runSubroutine \
                  for node in list_] # for run time cfgs
    fraction_cfgs = parser.fractions
    deploy_cfgs = parser.deploy
    merge_cfgs = get_SMat_relation(parser.attrVectCouple)

    searchTmp = TempConfig("searchSet_Template.py", "search_set.py",\
                          {'models':proc_cfgs, 'merge_cfgs': merge_cfgs})
    searchGen = CodeMapper([searchTmp])
    searchGen.genCode()
    from search_set import *
    manageTmp = TempConfig("procM_Template.F90", "manage.F90", {"proc_cfgs": proc_cfgs})

    deployTmp = TempConfig("deploymod_Template.F90", "deploy_mod.F90",\
                          {'proc_cfgs':proc_cfgs, "deploy_cfgs":deploy_cfgs}) 

    procdefTmp = TempConfig("procDef_Template.F90", "proc_def.F90",\
                          {"proc_cfgs":proc_cfgs, "merge_cfgs": merge_cfgs})

    timeMTmp = TempConfig("timeM_Template.F90", "timeM.F90",{"proc_cfgs":proc_cfgs})

    baseCplTmp = TempConfig("baseCpl_Template.F90","baseCpl.F90",\
                           {'proc_cfgs':proc_cfgs, 'merge_subroutines':merge_subroutines,\
                            'merge_cfgs':merge_cfgs, 'model_cfgs':model_cfgs,\
                            'subrt_cfgs':subrt_cfgs, 'fraction_cfgs':fraction_cfgs})
    confList = [searchTmp, manageTmp, deployTmp, procdefTmp,timeMTmp, baseCplTmp]

    codeGen = CodeMapper(confList)
    codeGen.genCode()
    
    
