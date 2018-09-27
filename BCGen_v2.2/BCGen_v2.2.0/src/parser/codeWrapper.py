#!/usr/bin/python
#coding:utf-8


def toString(subroutine, args):
    string=""
    argString = ""
    for arg in args:
        argString+=arg+", "
    string=subroutine+"("+argString[:-2]+")"
    return string

class CodeWrapper:
    def __init__(self, model, model_cpl, intent=4):
        self.model = model
        self.model_cpl = model_cpl
        self.intent = intent*" "
        self.subroutine = []

    def appendStr(self, string):
        self.subroutine.append(string)

    def getStr(self):
        string = ""
        string+=self.intent+"if("+self.model+"_run)then\n"
        string+=2*self.intent+"if(my_proc%iamin_model"+self.model_cpl+")then\n"
        for sub in self.subroutine:
            string+=3*self.intent+"call "+sub+"\n"
        string+=2*self.intent+"end if\n"
        string+=self.intent+"end if\n\n"
        return string

if __name__=="__main__":
   cw = CodeWrapper("a", "a_cpl")
   cw.appendStr("a_run_mct(arg1, arg2)")
   cw.appendStr("MPI_Barrier(my_proc%a_comm)")
   print cw.getStr()
