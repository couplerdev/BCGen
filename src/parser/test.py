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
parser.parse()
for model in parser.models:
<<<<<<< HEAD
    print '-------------------------------'
=======
<<<<<<< HEAD
    for av in parser.models[model].attrVects:
        print av.name
    #print parser.models[model].model_init.toString()
    for gsmap in parser.models[model].gsMaps:
        print gsmap.name
    for mapper in parser.models[model].mappers:
        print mapper.name
=======
>>>>>>> 08b2ff654cff4b68460fec055167eb3d8a61f5ab
    print parser.models[model].model_init.toString()
    print parser.models[model].model_run.toString()
    print parser.models[model].model_final.toString()
    for av in parser.models[model].attrVects:
<<<<<<< HEAD
        attrVect = parser.models[model].attrVects[av]
        print attrVect.name, attrVect.field, attrVect.nx
    for mapper in parser.models[model].mappers:
        print parser.models[model].mappers[mapper].name
    for gsMap in parser.models[model].gsMaps:
        print parser.models[model].gsMaps[gsMap].name
    print '--------------------------------'
print len(parser.attrVectCouple)
for av in parser.attrVectCouple:
    print parser.attrVectCouple[av].name, av

for sbr in parser.subroutine:
    subroutine = parser.subroutine[sbr]
    print subroutine.toString(subroutine.name, subroutine.argList)
=======
         print av.name, av.nx, av.field
#    for mapper in parser.models[model].mappers:
#        print mapper.name
>>>>>>> master
>>>>>>> 08b2ff654cff4b68460fec055167eb3d8a61f5ab
