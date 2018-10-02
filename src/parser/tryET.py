import xml.etree.ElementTree as ET
from xml.dom.minidom import Document


my_dict = {"doc":"1","cont":[{"ver":"1"},{"ver":"2"}]}

doc = Document()
root = doc.createElement('root')
doc.appendChild(root)

def dictDom(k, v):
    key = doc.createElement(k)
    value = doc.createTextNode(v)
    key.appendChild(value)
    return key
 
for (k,v) in my_dict.items():
    if k!='cont':
        root.appendChild(dictDom(k,v))
    else:
        cont = doc.createElement(k)
        for d in v:
            for k1 in d:
                v1 = d[k1]
                key = dictDom(k1,v1)
                cont.appendChild(key)
        root.appendChild(cont)
f = open('te.xml','w')
doc.writexml(f, indent='\t', newl='\n', addindent='\t', encoding='utf-8')
f.close()
