#
#    This module test the module parse, check all xml can work
#    especially we need test the wrong xml, and all sorts of setup
#    of xml
#
#  reversion history:
#      2018,3,26        alex: add module
#!/usr/bin/python2
from parserMod import Parser

parser = Parser()
parser.parse()
#for model in parser.models:
#    print parser.models[model].model_init.toString()
#    print parser.models[model].model_run.toString()
#    print parser.models[model].model_final.toString()
#     for av in parser.models[model].attrVects:
#         print av.name, av.nx, av.field
#    for mapper in parser.models[model].mappers:
#        print mapper.name
