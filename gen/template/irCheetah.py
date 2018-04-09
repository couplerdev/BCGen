#
#    This module highly relys on the form of template
#
#  reversion history
#         2018,3,26      alex: add module 
#!/usr/bin/python


#   
#
#
class CheetahSearchList:
    __slots__ = ["searchList","comp_set"]

    def __init__(self):
        self.searchList = {}
        self.comp_set = [] 
        self.attrVect = [] # only used for init and set , in model subroutine , let model subroutine tackle
        self.gsmap = []
        self.mapper = []
        self.sMat = []

    @property
    def comp_set(self):
        return comp_set       

    @comp_set.setter
    def comp_set(self, models):
	pass

        
