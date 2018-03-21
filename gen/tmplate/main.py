from codeGen import codeGenerator


code = codeGenerator()
_list = ['a','b','c']
code.addList('comp_set',_list)
code.generate()
