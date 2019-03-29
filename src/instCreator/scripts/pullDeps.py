import sys
import os
import argparse

class PullerArgsParser:
    def __init__(self):
        self.argParser = argparse.ArgumentParser()
        self.args = None
        self.argsParser.add_argument("-make","--remake",help="whether to remake",\
                                      action="store_true")

    def argSet(self):
        self.args = self.argParser.parse_args()


if __name__ == "__main__":
    pullArgsParser = PullerArgsParser()
    pullArgsParser.argSet()
    remake = pullArgsParser.args.remake
    f = open("../doc/descCase")
    baseCplDir = ""
    for l in f:
        kv = l.split(":")
        if kv[0] == "baseCplDir":
            baseCplDir = kv[1].strip('\n')
    f.close()
    currDir = os.getcwd()
    libDir = currDir+"/../lib"
    includeDir = currDir+"/../include"
    os.chdir(baseCplDir)
    if remake:
        cmd = "make"
        os.system(cmd)

    # cp lib to target
    cmd = "cp "+"./include/*.mod "+includeDir
    os.system(cmd)
    cmd = "cp "+"./include/*.h "+includeDir
    os.system(cmd)
    cmd = "cp "+"./lib/*.o "+libDir
    os.system(cmd)
    cmd = "cp "+"./lib/*.a "+libDir
    os.system(cmd)
    os.chdir(currDir)
    print "new lib updated!!!"
    
