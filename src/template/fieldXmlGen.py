#!/usr/bin/python

import xml.etree.ElementTree as ET
from xml.dom.minidom import Document


dictMap = {'a':'atm','o':'ocn','g':'glc','l':'lnd','i':'ice','w':'wav','r':'rof','s':'sno'}

if __name__ == "__main__":
    doc =  Document()
    root = doc.createElement("fieldMeta")
    file1 = "variables"
    fieldsRoot = doc.createElement("fields")
    fldMeta = doc.createElement("fldMeta")
    with open(file1) as f:
        for line in f:
            if line[0:3]=="seq":
                lineList = line.split('=')
                nameStr = lineList[0].split('_')
                fldList = lineList[1].replace('[','').replace(']','').replace('\'','').split(',')
                fldsStr = ""
                for fld in fldList:
                    fldsStr+=fld.strip()+":"
                fldsStr = fldsStr[:-1]
                avName = ""
                fldName = "flds_dom_coord"
                if nameStr[2]!="dom":
                    if nameStr[2][0]=='x':
                        avName = "x2"+dictMap[nameStr[2][2]]
                    else:
                        avName = dictMap[nameStr[2][0]]+"2x"
                    fldName = "flds_"+avName+"_"+nameStr[3]
                fieldVar = doc.createElement('fieldVar')
                field = doc.createElement('field')
                fieldVal = doc.createTextNode(fldsStr.replace('_','').strip())
                field.appendChild(fieldVal)
                fieldVar.appendChild(field)
                fieldVar.setAttribute('name',fldName)
                fieldsRoot.appendChild(fieldVar)   
            elif line[0]=="{":
                line = line.replace('{','').replace('}','')
                lineList = line.split(',')
                nameVar = [pair.split(':')[1].replace('\'','') for pair in lineList]
                shortname = doc.createElement('shortname')
                var = doc.createTextNode(nameVar[0].strip())
                shortname.appendChild(var)
                longname = doc.createElement('longname')
                lname = "unknow"
                if nameVar[1].replace(' ','')!='':
                    lname = nameVar[1]
                var = doc.createTextNode(lname)
                longname.appendChild(var)
                stdname = doc.createElement('stdname')
                sname = 'unknow'
                if nameVar[2].replace(' ','')!='':
                    sname = nameVar[2]
                var = doc.createTextNode(sname)
                stdname.appendChild(var)
                units = doc.createElement('units')
                unitsName = nameVar[3].replace('\n','')
                uname = 'unknow'
                if unitsName.replace(' ','')!='':
                    uname = unitsName
                var = doc.createTextNode(uname)
                units.appendChild(var)
                fld = doc.createElement('fld')
                fld.appendChild(shortname) 
                fld.appendChild(longname)
                fld.appendChild(stdname)
                fld.appendChild(units)
                fldMeta.appendChild(fld)
          
    root.appendChild(fieldsRoot)
    root.appendChild(fldMeta)    
    doc.appendChild(root)
    with open('field.xml','w') as f:
        doc.writexml(f, indent='\t',newl='\n',addindent='\t',encoding='utf-8')
