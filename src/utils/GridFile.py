#!/usr/bin python
#utf-8

import xml.etree.ElementTree as ET
from xml.dom.minidom import Document
import sys, os

class GridFile() :
	__slots__ = ['__grids', '__regridMap', '__domains']
	def __init__(self) :
		self.__grids = { }
		self.__regridMap = [ ]
		self.__domains = [ ]
		self.loaded = False

	@property
	def grids(self) : 
		return self.__grids
	
	@property
        def regridMap(self) :
		return self.__regridMap

	@property
        def domains(self) :
		return self.__domains

	def findGrid(self, name) :
		if name and self.loaded :
			return self.__grids[name]
		else :
			return None

	def findMapping(self, srcModel, srcRes, detModel, detRes, mappingType) :
		if not self.loaded :
			return None
		for gm in self.__regridMap :
			if gm['src']['model'] == srcModel and gm['src']['res'] == srcRes and gm['det']['model'] == detModel and gm['det']['res'] == detRes and mappingType in gm :
				return gm[mappingType]
		return None

	def findDomain(self, grid, mask, model) :
		if not self.loaded :
			return None
		for domain in self.__domains :
			key = model.lower() + '_domain_file'
			if domain['grid'] == grid and domain['mask'] == mask and key in domain :
				return domain[key]
		return None

	def load(self, filename = "../../composing/config_grid.xml", din_loc_root = '/data50T/CESM-data/inputdata') :
		tree = ET.parse(filename)
		root = tree.getroot()

		for gm in root.iter('gridmap') :
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
				found = False
				for item in self.__regridMap :
					if item['src']['model'] == tag[0:3] and item['src']['res'] == sGrid and item['det']['model'] == tag[4:7] and item['det']['res'] == tGrid :
						item[t] = mapper.text.strip()
						found = True
				if not found :
					self.__regridMap.append({ 'src' : {'model' : tag[0:3], 'res' : sGrid}, 'det' : {'model' : tag[4:7], 'res' : tGrid}, t : mapper.text.strip() })

		for g in root.iter('gridhorz') : 
			item = {'name' : g.attrib['name'], 'nx' : '0', 'ny' : '0' }
			if g.get('alias') :
				item['alias'] = g.get('alias')
			for child in g :
				if child.tag == 'nx' :
					item['nx'] = child.text.strip()
				if child.tag == 'ny' :
					item['ny'] = child.text.strip()
				if child.tag == 'desc' :
					item['desc'] = child.text.strip()
			item['gsize'] = str(int(item['nx']) * int(item['ny']))
			self.__grids[item['name']] = item
			if g.get('alias') :
				self.__grids[g.get('alias')] = item

		self.__grids['fv4x5'] = self.__grids['4x5']
		self.__grids['fv0.23x0.31'] = self.__grids['0.23x0.31']
		self.__grids['fv0.47x0.63'] = self.__grids['0.47x0.63']
		self.__grids['fv0.9x1.25'] = self.__grids['0.9x1.25']
		self.__grids['fv1.9x2.5'] = self.__grids['1.9x2.5']
		self.__grids['fv2.5x3.33'] = self.__grids['2.5x3.33']
		self.__grids['fv10x15'] = self.__grids['10x15']

		for gd in root.iter('griddom') :
			item = {'grid' : gd.attrib['grid']}
			if gd.get('mask') :
				item['mask'] = gd.get('mask')
			for child in gd :
				item[child.tag.strip().lower()] = child.text.strip().replace('$DIN_LOC_ROOT', din_loc_root)
			self.__domains.append(item)

		self.loaded = True

loadedGridFiles = [ ]

def getGridFile(filename = "../../composing/config_grid.xml", din_loc_root = '/data50T/CESM-data/inputdata') :
	for gf in loadedGridFiles :
		if gf['filename'] == filename and din_loc_root == gf['din_loc_root'] :
			return gf['gridFile']

	gf = GridFile()
	gf.load(filename, din_loc_root)
	loadedGridFiles.append({'filename' : filename, 'din_loc_root' : din_loc_root, 'gridFile' : gf})
	return gf

def findGrid(name, filename = "../../composing/config_grid.xml", din_loc_root = '/data50T/CESM-data/inputdata') :
	gf = getGridFile(filename, din_loc_root)
	if gf :
		return gf.findGrid(name)
	else :
		return None

def findMapping(srcModel, srcRes, detModel, detRes, mappingType, filename = "../../composing/config_grid.xml", din_loc_root = '/data50T/CESM-data/inputdata') :
	gf = getGridFile(filename, din_loc_root)
        if gf :
                return gf.findMapping(srcModel, srcRes, detModel, detRes, mappingType)
        else :
                return None

def findDomain(grid, mask, model, filename = "../../composing/config_grid.xml", din_loc_root = '/data50T/CESM-data/inputdata') :
	gf = getGridFile(filename, din_loc_root)
        if gf :
                return gf.findDomain(grid, mask, model)
        else :
                return None

def main() :
	print findGrid('gx3v7')
	print findGrid('fv4x5')
	print findDomain('4x5', 'gx3v7', 'atm')
	print findMapping('atm', '4x5', 'ocn', 'gx3v7', 'state')

if __name__ == '__main__':
	main()
