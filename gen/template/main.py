from codeGen import codeGenerator


code = codeGenerator()
_list = ['a','b','c']
_text = ['d','b','j']
code.addList('comp_set',_list)
code.addList('text_set',_text)
code.generate()
