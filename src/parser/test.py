#
#    This module test the module parse, check all xml can work
#    especially we need test the wrong xml, and all sorts of setup
#    of xml
#
#  reversion history:
#      2018,3,26        alex: add module
<<<<<<< HEAD
#!/usr/bin/python
from parser import Parser
=======
#!/usr/bin/python2
from parserMod import Parser
>>>>>>> master

parser = Parser()
parser.modelsParse()
for model in parser.models:
<<<<<<< HEAD
    for av in parser.models[model].attrVects:
        print av.name
    #print parser.models[model].model_init.toString()
    for gsmap in parser.models[model].gsMaps:
        print gsmap.name
    for mapper in parser.models[model].mappers:
        print mapper.name
=======
    print parser.models[model].model_init.toString()
#    print parser.models[model].model_run.toString()
#    print parser.models[model].model_final.toString()
    for av in parser.models[model].attrVects:
         print av.name, av.nx, av.field
#    for mapper in parser.models[model].mappers:
#        print mapper.name
>>>>>>> master
