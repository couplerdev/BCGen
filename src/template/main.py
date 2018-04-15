from codeGen import codeGenerator
from coupler_set import *
from model_set import *

proc_cfgs = [
    parser.models[m] for m in parser.models 
]

def get_SMat_relation(attrVects):
    model_names = []
    model_SMats = {}


    for av in attrVects:
        model_name = av[:1]
        model_SMats[model_name] = {}
        model_SMats[model_name]['src'] = av
        model_SMats[model_name]['dst'] = []
        # todo gsMap
        model_SMats[model_name]['gm'] = 'gsMap_' + model_name + "x"
        model_names.append(model_name)

    for av in attrVects:
        dst_model_name = av[:1]
        for src_x_dst_x_av in attrVects[av]:
            src_model_name = src_x_dst_x_av.name[:1]
            print(av, dst_model_name, src_x_dst_x_av.name,\
                    src_x_dst_x_av.mapper.name)
            # todo src_model_name dst_model_name
            dst_info = {
                'dst_model_name':dst_model_name,
                'dst_av':src_x_dst_x_av,
                'dst_gm':'gsMap_' + dst_model_name + 'x'
                    }
            model_SMats[src_model_name]['dst'].append(dst_info)
    return model_SMats

res = get_SMat_relation(parser.attrVectCouple)
print res

code = codeGenerator("searchSet_Template.py", "search_set.py")
code.addList('models',proc_cfgs)
code.generate()

from search_set import *

code = codeGenerator("procM_Template.F90", "manage.F90")
#TODO add 
code.addList('deploy_cfgs',deploy_cfgs)
code.addList('proc_cfgs',proc_cfgs)
code.generate()


code = codeGenerator("procDef_Template.F90", "proc_def.F90")
#TODO add 
code.addList('proc_cfgs',proc_cfgs)
#code.addList('sMat_cfgs',sMat_cfgs)
code.generate()


code = codeGenerator("baseCpl_Template.F90", "baseCpl.F90")
#TODO add 
code.addList('model_cfgs',model_cfgs)
code.addList('proc_cfgs',proc_cfgs)
code.addList('merge_cfgs',res)
code.generate()
