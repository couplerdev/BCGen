#
#    This module test the module parse, check all xml can work
#    especially we need test the wrong xml, and all sorts of setup
#    of xml
#
#  reversion history:
#      2018,3,26        alex: add module
#!/usr/bin/python
from parser import Parser

parser = Parser()
parser.modelsParse()
for model in parser.models:
    for av in parser.models[model].attrVects:
        print av.name
    #print parser.models[model].model_init.toString()
    for gsmap in parser.models[model].gsMaps:
        print gsmap.name
    for mapper in parser.models[model].mappers:
        print mapper.name
