from codeGen import codeGenerator
from search_set import *


code = codeGenerator("baseCpl_Template.F90", "baseCpl.F90")
#TODO add 
code.addList('model_cfgs',model_cfgs)
code.addList('proc_cfgs',proc_cfgs)
code.generate()

code = codeGenerator("procM_Template.F90", "manage.F90")
#TODO add 
code.addList('deploy_cfgs',deploy_cfgs)
code.generate()


code = codeGenerator("procDef_Template.F90", "proc_def.F90")
#TODO add 
code.addList('proc_cfgs',proc_cfgs)
code.generate()


