#
#   ErrorHandle module
#
#  reversion history:
#      2018,3,26      alex: add module
#!/usr/bin/python

class UnsetError(Exception):
    def __init__(self, errMsg):
        super(UnsetError, self).__init__(errMsg)

class NoTagError(Exception):
    def __init__(self, errMsg):
        super(NoTagError, self).__init__(errMsg)

class BindError(Exception):
    def __init__(self, errMsg):
        super(BindError, self).__init__(errMsg)

class ComposingError(Exception):
    def __init__(self, errMsg):
        super(ComposingError, self).__init__(errMsg)