from codeGen import codeGenerator
from search_set import *


code = codeGenerator()
_list = ['a','b','c']
_text = ['d','b','j']
#TODO add 
code.addList('comp_set',_list)
code.addList('text_set',_text)
code.addList('model_cfgs',model_cfgs)
code.generate()
