from codeGen import codeGenerator
from coupler_set import *
from model_set import *

proc_cfgs = [
    parser.models[m] for m in parser.models 
]

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
code.generate()
