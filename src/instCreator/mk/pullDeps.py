import sys
import os
f = open("../doc/descCase")
baseCplDir = ""
for l in f:
    kv = l.split(":")
    if kv[0] == "baseCplDir":
        baseCplDir = kv[1]

currDir = os.getcwd()
libDir = currDir+"/../lib"
includeDir = currDir+"/../include"
os.chdir(baseCplDir)
cmd = "make"
os.system(cmd)

# cp lib to target
cmd = "cp "+"./include/*.mod "+includeDir
os.system(cmd)
cmd = "cp "+"./include/*.h "+includeDir
os.system(cmd)
cmd = "cp "+"./include/*.o "+libDir
os.system(cmd)
cmd = "cp "+"./include/*.a "+libDir
os.system(cmd)
os.chdir(currDir)
print "new lib updated!!!"
    
