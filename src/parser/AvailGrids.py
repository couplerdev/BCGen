#!/usr/bin python
#utf-8

class AvailGrids():
	__slots__ = ['__cgrids']
	def __init__(self) : 
		self.__cgrids = { 
           "reg" : { "alias" : "", "desc" : "regional grid mask", "nx" : 0, "ny" : 0, "gsize" : 0 } ,
           "usgs" : { "alias" : "", "desc" : "USGS mask", "nx" : 0, "ny" : 0, "gsize" : 0 } ,
           "null" : { "alias" : "", "desc" : "null is no grid", "nx" : 0, "ny" : 0, "gsize" : 0 } ,
           "CLM_USRDAT" : { "alias" : "", "desc" : "user specified domain", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_numaIA" : { "alias" : "", "desc" : "1x1 Numa Iowa -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_brazil" : { "alias" : "", "desc" : "1x1 Brazil -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_smallvilleIA" : { "alias" : "", "desc" : "1x1 Smallville Iowa Crop Test Case -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_camdenNJ" : { "alias" : "", "desc" : "1x1 Camden New Jersey -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_mexicocityMEX" : { "alias" : "", "desc" : "1x1 Mexico City Mexico -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_vancouverCAN" : { "alias" : "", "desc" : "1x1 Vancouver Canada -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_tropicAtl" : { "alias" : "", "desc" : "1x1 Tropical Atlantic Test Case -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "1x1_urbanc_alpha" : { "alias" : "", "desc" : "1x1 Urban C Alpha Test Case -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "5x5_amazon" : { "alias" : "", "desc" : "5x5 Amazon regional case -- only valid for DATM/CLM compset", "nx" : 1, "ny" : 1, "gsize" : 1 } ,
           "360x720cru" : { "alias" : "", "desc" : "Exact half-degree CRUNCEP datm forcing grid with CRUNCEP land-mask -- only valid for DATM/CLM compset", "nx" : 720, "ny" : 360, "gsize" : 720*360 } ,
           "0.23x0.31" : { "alias" : "fv0.23x0.31", "desc" : "0.23x0.31 is FV 1/4-deg grid", "nx" : 1152, "ny" : 768, "gsize" : 1152*768 } ,
           "0.47x0.63" : { "alias" : "fv0.47x0.63", "desc" : "0.47x0.63 is FV 1/2-deg grid", "nx" : 576, "ny" : 384, "gsize" : 576*384 } ,
           "0.9x1.25" : { "alias" : "fv0.9x1.25", "desc" : "0.9x1.25 is FV 1-deg grid", "nx" : 288, "ny" : 192, "gsize" : 288*192 } ,
           "1.9x2.5" : { "alias" : "fv1.9x2.5", "desc" : "1.9x2.5 is FV 2-deg grid", "nx" : 144, "ny" : 96, "gsize" : 144*96 } ,
           "4x5" : { "alias" : "fv4x5", "desc" : "4x5 is FV 4-deg grid", "nx" : 72, "ny" : 46, "gsize" : 72*46 } ,
           "2.5x3.33" : { "alias" : "fv2.5x3.33", "desc" : "", "nx" : 108, "ny" : 72, "gsize" : 108*72 } ,
           "10x15" : { "alias" : "fv10x15", "desc" : "", "nx" : 24, "ny" : 19, "gsize" : 24*19 } ,
           "T341" : { "alias" : "512x1024", "desc" : "T341 is Gaussian grid", "nx" : 1024, "ny" : 512, "gsize" : 1024*512 } ,
           "T85" : { "alias" : "128x256", "desc" : "T85 is Gaussian grid", "nx" : 256, "ny" : 128, "gsize" : 256*128 } ,
           "T62" : { "alias" : "", "desc" : "T62 is Gaussian grid", "nx" : 192, "ny" : 69, "gsize" : 192*96 } ,
           "T42" : { "alias" : "64x128", "desc" : "T42 is Gaussian grid", "nx" : 128, "ny" : 64, "gsize" : 128*64 } ,
           "T31" : { "alias" : "48x96", "desc" : "T31 is Gaussian grid", "nx" : 96, "ny" : 48, "gsize" : 96*48 } ,
           "T21" : { "alias" : "32x64", "desc" : "T21 is Gaussian grid", "nx" : 64, "ny" : 32, "gsize" : 64*32 } ,
           "T05" : { "alias" : "8x16", "desc" : "T05 is Gaussian grid", "nx" : 16, "ny" : 8, "gsize" : 16*8 } ,
           "ne16np4" : { "alias" : "", "desc" : "ne16np4 is Spectral Elem 2-deg grid", "nx" : 13826, "ny" : 1, "gsize" : 13826*1 } ,
           "ne30np4" : { "alias" : "", "desc" : "ne30np4 is Spectral Elem 1-deg grid", "nx" : 48602, "ny" : 1, "gsize" : 48602*1 } ,
           "ne60np4" : { "alias" : "", "desc" : "ne60np4 is Spectral Elem 1/2-deg grid", "nx" : 194402, "ny" : 1, "gsize" : 194402*1 } ,
           "ne120np4" : { "alias" : "", "desc" : "ne120np4 is Spectral Elem 1/4-deg grid", "nx" : 777602, "ny" : 1, "gsize" : 777602*1 } ,
           "ne240np4" : { "alias" : "", "desc" : "ne240np4 is Spectral Elem 1/8-deg grid", "nx" : 3110402, "ny" : 1, "gsize" : 3110402*1 } ,
           "gx1v6" : { "alias" : "", "desc" : "gx1v6 is Greenland pole v6 1-deg grid", "nx" : 320, "ny" : 384, "gsize" : 320*384 } ,
           "gx3v7" : { "alias" : "", "desc" : "gx3v7 is Greenland pole v7 3-deg grid", "nx" : 100, "ny" : 116, "gsize" : 100*116 } ,
           "tx0.1v2" : { "alias" : "", "desc" : "tx0.1v2 is tripole v2 1/10-deg grid", "nx" : 3600, "ny" : 2400, "gsize" : 3600*2400 } ,
           "tx1v1" : { "alias" : "", "desc" : "tx1v1 is tripole v1 1-deg grid, testing proxy for high-res tripole ocean grids. do not use scientific experiments; use the T62_g16 resolution instead", "nx" : 360, "ny" : 240, "gsize" : 360*240 } ,
           "mpas120" : { "alias" : "", "desc" : "mpas120 is a MPAS ocean grid that is roughly 1 degree resolution", "nx" : 28574, "ny" : 1, "gsize" : 28574*1 } ,
           "rx1" : { "alias" : "", "desc" : "rx1 is 1 degree river routing grid", "nx" : 360, "ny" : 180, "gsize" : 360*180 } ,
           "r05" : { "alias" : "", "desc" : "r05 is 1/2 degree river routing grid", "nx" : 720, "ny" : 360, "gsize" : 720*360 } ,
           "r01" : { "alias" : "", "desc" : "r01 is 1/10 degree river routing grid", "nx" : 3600, "ny" : 1800, "gsize" : 3600*1800 } ,
           "mp120v1" : { "alias" : "", "desc" : "", "nx" : 28993, "ny" : 1, "gsize" : 28993*1 } ,
           "mp120r10v1" : { "alias" : "", "desc" : "", "nx" : 139734, "ny" : 1, "gsize" : 139734*1 } ,
           "ww3a" : { "alias" : "", "desc" : "WW3 90 x 50 global grid", "nx" : 90, "ny" : 50, "gsize" : 90*50 } 
					}

	@property
	def cgrids(self) :
		return self.__cgrids

	def findGrid(self, name) :
		name = name.strip()
		if name is None or name == '' :
			return None
		else :
			for n in self.__cgrids :
				if n == name :
					grid = self.__cgrids[n]
					grid['name'] = n
					return grid
				else :
					alias = self.__cgrids[n]['alias']
					for a in alias.split() :
						if a.strip() == name :
							grid = self.__cgrids[n]
							grid['name'] = n
							return grid
		return None

def findGrid(name) :
	g = AvailGrids()
	return g.findGrid(name)
