#coding:utf-8
# this is the intermediation of fractions and interConnection among fractions
from ir import AttrVect, Subroutine, Mapper

class FractionMapper:
    def __init__(self, mapper, method, src, target, frac):
	self.mapper = mapper
        self.mapSubroutine = Subroutine(subroutineName=method)
        argList = [mapper, "fraction_"+src, "fraction_"+target,"field=\""+ frac+"\"", "ierr=ierr"]
        self.mapSubroutine.argList = argList

class Fraction(AttrVect):
     __slots__=["__name","__field","__init", "__update", "__fraclist","__ifupdate",\
                 "__model", "updateSubroutine", "mappers"]
     def __init__(self, fractionName, model, fraclist, subroutineName,\
                    updateSubroutine=""):
         self.__name = ""
         super(AttrVect, self).__init__(name=fractionName)
         self.__name = fractionName
         subName = subroutineName
         self.__model = model
         self.__init = Subroutine(subroutineName=subName, argList=[])
         self.__init.argList = ["domain_"+self.__model+"x", "fraction_"+self.__model, "\""+fraclist+"\""]
         self.__fraclist = fraclist
         self.updateSubroutine = None
         if updateSubroutine == "":
             self.update = False
             self.updateSubroutine = None
         else:
             self.updateSubroutine = Subroutine(subroutineName=updateSubroutine)
         self.mappers = []

     @property
     def init(self):
         return self.__init

     @property
     def name(self):
         return self.__name
    
     @property
     def fraclist(self):
         return self.__fraclist

     @property
     def model(self):
         return self.__model

     @property
     def fracs(self):
         return self.__fracs   

     def append(self, fracMapper):
         self.mappers.append(fracMapper)
