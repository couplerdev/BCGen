from codeGen import codeGenerator
from search_set import *


code = codeGenerator()
#TODO add 
code.addList('model_cfgs',model_cfgs)
code.generate()
