import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys, os

din_loc_root = '/data50T/CESM-data/inputdata'

srcFile = "../../composing/config_grid.xml"

tree = ET.parse(srcFile)
root = tree.getroot()

regridMap = [ ]
gridMap = { }
domainMap = [ ]

for gm in root.iter('gridmap') :
#	print '----------'
#	for k in gm.attrib :
#		print k, ' = ', gm.attrib[k]
#	print '---'
	for mapper in gm :
		tag = mapper.tag.lower()
		fr = tag[0:3]
		to = tag[4:7]
		sGrid = gm.attrib[fr+"_grid"]
		tGrid = gm.attrib[to+"_grid"]
		t = 'state'
		if tag[8] == 'f' :
			t = 'flux'
		elif tag[8] == 'v' :
			t = 'vector'
		elif tag[8] == 'r' :
			t = 'runoff'
#		print tag[0:3], '(', sGrid, ')->', tag[4:7], '(', tGrid, ') [', t, '] = ', mapper.text	
		found = False
		for item in regridMap :
			if item['src']['model'] == tag[0:3] and item['src']['res'] == sGrid and item['det']['model'] == tag[4:7] and item['det']['res'] == tGrid :
				item[t] = mapper.text.strip()
				found = True
		if not found :
			regridMap.append({ 'src' : {'model' : tag[0:3], 'res' : sGrid}, 'det' : {'model' : tag[4:7], 'res' : tGrid}, t : mapper.text.strip() })

# print regridMap

for g in root.iter('gridhorz') : 
	item = {'name' : g.attrib['name'] }
	if g.get('alias') :
		item['alias'] = g.get('alias')
	for child in g :
		if child.tag == 'nx' :
			item['nx'] = child.text.strip()
		if child.tag == 'ny' :
			item['ny'] = child.text.strip()
		if child.tag == 'desc' :
			item['desc'] = child.text.strip()
	gridMap[item['name']] = item
	if g.get('alias') :
		gridMap[g.get('alias')] = item

gridMap['fv4x5'] = gridMap['4x5']
gridMap['fv0.23x0.31'] = gridMap['0.23x0.31']
gridMap['fv0.47x0.63'] = gridMap['0.47x0.63']
gridMap['fv0.9x1.25'] = gridMap['0.9x1.25']
gridMap['fv1.9x2.5'] = gridMap['1.9x2.5']
gridMap['fv2.5x3.33'] = gridMap['2.5x3.33']
gridMap['fv10x15'] = gridMap['10x15']


# print gridMap

# print gridMap['reg']
# print gridMap['fv4x5']

for gd in root.iter('griddom') :
	item = {'grid' : gd.attrib['grid']}
	if gd.get('mask') :
		item['mask'] = gd.get('mask')
	for child in gd :
		item[child.tag.strip().lower()] = child.text.strip().replace('$DIN_LOC_ROOT', din_loc_root)
	domainMap.append(item)

print domainMap
