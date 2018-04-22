#
#    This module test the module parse, check all xml can work
#    especially we need test the wrong xml, and all sorts of setup
#    of xml
#
#  reversion history:
#      2018,3,26        alex: add module
#!/usr/bin/python
from parserMod import Parser

parser = Parser()
parser.parse()
print '#############see how models works!'
for model in parser.models:
    print '-------------------------------'
    print parser.models[model].model_init.toString()
    print parser.models[model].model_run.toString()
    print parser.models[model].model_final.toString()
    for av in parser.models[model].attrVects:
        attrVect = parser.models[model].attrVects[av]
        print attrVect.name, attrVect.field, attrVect.nx
    for mapper in parser.models[model].mappers:
        print parser.models[model].mappers[mapper].name
    for gsMap in parser.models[model].gsMaps:
        print parser.models[model].gsMaps[gsMap].name
    print '--------------------------------'
print '##############see how attrVectCouple works!'
print parser.attrVectCouple
for av in parser.attrVectCouple:
    print "merge_dst_av:", av
    for attrVect in parser.attrVectCouple[av]:
        print attrVect.name, attrVect.mapperName, attrVect.field

print '##############see subroutine!'
for sbr in parser.subroutine:
    subroutine = parser.subroutine[sbr]
    print(sbr, subroutine)
    print subroutine.toString(subroutine.name, subroutine.argList)

print '##############deploy mod:'
for deploy in parser.deploy:
    print deploy, parser.deploy[deploy]
