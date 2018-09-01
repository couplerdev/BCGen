

#
#    ModelParser uses SubroutineParser to parse the subroutine 
#
class SubroutineParser:
    __slots__=['__root', '__subroutine', '__isParsed']
    def __init__(self):
        self.__subroutine = ModelSubroutine()
        self.__isParsed = False
        self.__root = ""

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False

    def subroutineParse(self):
                if self.__root == "":
                    raise UnSetError("self.__root not set! Try setRoot method")
		self.__subroutine =  ModelSubroutine()
		for child in self.__root:
                    if child.tag == "name":
                        self.__subroutine.subroutineName = child.text
                    elif child.tag == "arg":
                        self.__subroutine.append(child.text)
                    else:
                        raise NoTagError("No such tag "+child.tag)
                self.__isParsed = True
                #print self.__subroutine.subroutineName

    @property
    def subroutine(self):
        if self.__isParsed == False:
            self.subroutineParse()
        return self.__subroutine
		
class ModelParser:
    __slots__=['__root', '__model', '__isParsed', '__name',\
               '__gsize','__nx','__ny','__field','__NameManager']
    def __init__(self, NameManager,root="",lsize=0,nx=0,ny=0,field="", gsize=0):
        self.__root = root
        self.__isParsed = False
        self.__gsize = gsize
        self.__nx = nx
        self.__ny = ny
        self.__field = field
        self.__name = ""
        self.__NameManager = NameManager

    def setRoot(self, root):
        self.__root = root
        self.__isParsed = False
	
    def __setGsMap(self):
        srcGsMap = GsMap(grid=self.__name, pes=self.__name)
        dstGsMap = GsMap(grid=self.__name, pes="x")
        srcGsMap.BindToManager(self.__NameManager)
        srcGsMap.nameGenerate()
        dstGsMap.BindToManager(self.__NameManager)
        dstGsMap.nameGenerate()
        self.__model.append(srcGsMap)
        self.__model.append(dstGsMap)


    # get attrVect that local in model
    def __setAttrVect(self):
        comp2x_aa = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src=self.__name, dst="x", grid=self.__name, pes=self.__name)
        x2comp_aa = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src="x", dst=self.__name, grid=self.__name, pes=self.__name)
        comp2x_ax = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src=self.__name, dst="x", grid=self.__name, pes="x")
        x2comp_ax = AttrVect(field=self.__field, nx=self.__nx, ny=self.__ny, \
                             src="x", dst=self.__name, grid=self.__name, pes="x")
        comp2x_aa.BindToManager(self.__NameManager)
        comp2x_aa.nameGenerate()
        x2comp_aa.BindToManager(self.__NameManager)
        x2comp_aa.nameGenerate()
        comp2x_ax.BindToManager(self.__NameManager)
        comp2x_ax.nameGenerate()
        x2comp_ax.BindToManager(self.__NameManager)
        x2comp_ax.nameGenerate()
        self.__model.append(comp2x_aa)
        self.__model.append(x2comp_aa)
        self.__model.append(comp2x_ax)
        self.__model.append(x2comp_ax)
  
    def __setMapper(self):
        if len(self.__model.attrVects) != 4:
            raise ValueError("call __setAttrVect first!") 
        if len(self.__model.gsMaps) != 2:
            raise ValueError("call __setGsMap first!")
        srcMapper = Mapper(self.__model.attrVects["c2x_cc"], self.__model.attrVects["c2x_cx"], \
                           self.__model.gsMaps["comp"].name, self.__model.gsMaps["cpl"].name, \
                           mapType="rearr")
        dstMapper = Mapper(self.__model.attrVects["x2c_cc"], self.__model.attrVects["x2c_cx"], \
                           self.__model.gsMaps["comp"].name, self.__model.gsMaps["cpl"].name,  \
                           mapType="rearr")
        srcMapper.BindToManager(self.__NameManager)
        srcMapper.nameGenerate()
        dstMapper.BindToManager(self.__NameManager)
        dstMapper.nameGenerate()
        self.__model.append(srcMapper)
        self.__model.append(dstMapper)
## set time modi 8/11
    def __setTime(self):
	root = self.__root.find('time')
	base_root = root.find('base')
	y = 0
	m = 0
	d = 0
	h = 0
	if base_root.find('y')!=None:
	    y = base_root.find('y').text
        if base_root.find('m')!=None:
	    m = base_root.find('m').text
        if base_root.find('d')!=None:
            d = base_root.find('d').text
        if base_root.find('h')!=None:
            h = base_root.find('h').text
        base = Base(y, m, d, h)         

        interval_root = root.find('interval')
        m = 0 
        d = 0
        h = 0
        minute = 0
        sec = 0
        if interval_root.find('m')!=None:
	    m = interval_root.find('m').text
        if interval_root.find('d')!=None:
	    d = interval_root.find('d').text
        if interval_root.find('h')!=None:
	    h = interval_root.find('h').text
        if interval_root.find('minute')!=None:
	    minute = interval_root.find('minute').text
        if interval_root.find('sec')!=None:
	    sec = interval_root.find('sec').text
        interval  = Interval(m, d, h, minute, sec)
        self.__model.Time = Time(base, interval)
        
    def __setDomain(self):
        domain_root = root.find('domain')
        field = ""
        if domain_root.find('field') != None:
            field = domain_root.find('field').text
        if domain_root.find('path') == None:
	    raise UnsetError("domain data path not set!")
        path  = domain_root.find('path').text
        self.__domain = Domain(field, path)

    def modelParse(self):
        if self.__root == "":
            raise UnSetError("self.__root not set! Try setRoot method!") 
        name = self.__root.find('name').text
        root = self.__root
        self.__name = name
        self.__model = Model(name=name)
        self.__model.BindToManager(self.__NameManager)

        subroutine = SubroutineParser()
        subroutine.setRoot(root.find('init'))   ## need ErrorHandle
        self.__model.model_init = subroutine.subroutine
        subroutine.setRoot(root.find('run'))
        self.__model.model_run = subroutine.subroutine
        subroutine.setRoot(root.find('final'))
        self.__model.model_final = subroutine.subroutine

        self.__model.interval = root.find('interval').text
        root = root.find('attrVect')
        #if root.find('name')? how to handle optional 
        self.__gsize = root.find("gsize").text
        self.__nx = root.find("nx").text
        self.__ny = root.find("ny").text
        self.__field = root.find("field").text
	if self.__gsize == 0:
            self.__gsize = self.__nx*self.__ny
        self.__model.gSize = self.__gsize
        self.__setAttrVect()
        self.__setGsMap()
        self.__setTime()
        self.__setDomain()
        self.__model.domain = name+'_grid_domain'
        self.__setMapper()
        self.__isParsed = True
	
    @property
    def model(self):
        if not self.__isParsed:
            self.modelParse()
        return self.__model
