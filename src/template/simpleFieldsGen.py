#!/usr/bin/env/python
#coding:utf-8

intent = 4*" "
prefix = intent+"character(CXX)     :: "

modStart = "module base_fields\n    implicit none\n"
modEnd = "end module base_fields"
mapDict = {'a':'atm','o':'ocn','i':'ice','l':'lnd','s':'sno','g':'glc','r':'rof','w':'wav'}
postfix = ['states','fluxes']
def check(string):
    if string[0:3]!='seq':
        return False
    checkList = string.split('=')[0].split('_')
    if not checkList[3] in postfix:
        return False
    if checkList[2][1]!='2':
        return False
    return True

def getName(prefix, name, fldType, direction):
    if direction==0:
        return prefix+"_"+name+"2x_"+fldType
    else:
        return prefix+"_"+"x2"+name+"_"+fldType

def getKey(key):
    k = key[0]
    direct = 0
    if k=='x':
        k = key[2]
        direct = 1
    return mapDict[k], direct

readFile = "variables"
writeFile = "base_fields.F90"

fr = open(readFile,'r')
fw = open(writeFile, 'w')
fw.write(modStart)
idx = 0
names = []
for line in fr:
    if not check(line):
        continue 
    splitList = line.split('=')
    nameVar = splitList[0].split('_')
    compName, direction = getKey(nameVar[2])
    shortName = "x2"+compName
    if direction == 0:
        shortName=compName+"2x"
    names.append(shortName)
    nameStr = getName('flds',compName, nameVar[3], direction)
    flds = ""
    fldsString = splitList[1].replace('[','').replace(']','').split(',')
    if fldsString == "":
        continue
    for fld in fldsString:
        fld = fld.replace('\'','')
        flds+=fld+":"
    flds = flds[:-1].replace(' ','').replace('\n','')
    flds = "\""+flds+"\""
    writeLine = prefix+nameStr+" = "+flds+"\n"
    fw.write(writeLine)

fw.write('\n\n')

for name in names:
    nameStr = "flds_"+name+"_fields"
    sub1 = "flds_"+name+"_states"
    sub2 = "flds_"+name+"_fluxes"
    writeLine = prefix+nameStr+" = "+sub1+" + "+sub2+"\n"
    fw.write(writeLine)
fw.write(modEnd)
fr.close()
fw.close()
