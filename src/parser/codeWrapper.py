#!/usr/bin/python
#coding:utf-8
# using codeBlock inherence to support
# sorts of codeBlock generate

import sys

def toString(subroutine, args):
    string=""
    argString = ""
    for arg in args:
        argString+=str(arg)+", "
    string=subroutine+"("+argString[:-2]+")"
    return string


class CodeBlock(object):
    # default code block wrapper class

    def __init__(self, model, model_cpl, intent=4):
        self.model = model
        self.model_cpl = model_cpl
        self.intent = intent*' '
        self.subroutine  = []

    def appendStr(self, string):
        self.subroutine.append(string)
    
    def subroutineSet(self, stringList):
        self.subroutine = stringList

    def getStr(self):
        string = ""
        string+=self.intent+"if("+self.model+"_run)then\n"
        string+=2*self.intent+"if(metaData%iamin_model"+self.model_cpl+")then\n"
        for sub in self.subroutine:
            string+=3*self.intent+"call "+sub+"\n"
        string+=2*self.intent+"end if\n"
        string+=self.intent+"end if\n\n"
        return string

class CodeBlockUtil(CodeBlock):
    def __init__(self, model, model_cpl, intent=4, flag="restart"):
        super(CodeBlockUtil, self).__init__(model, model_cpl, intent)
        self.flag = flag

    def getStr(self):
	string = ""
	string+= self.intent+"if("+self.flag+"_run)then\n"
	string+= 2*self.intent+"if(metaData%iamin_model"+self.model_cpl+")then\n"
	string+= 3*self.intent+"call mpi_barrier(metaData%mpicom_"+self.model_cpl+",ierr)\n"
	for sub in self.subroutine:
	    string += 3*self.intent+"call "+sub+"\n"
	string+=2*self.intent+"end if\n"
	string+=self.intent+"end if\n\n"
	return string


class CodeWrapper:
      def __init__(self):
          self.subroutineList = []

      def appendStr(self, string):
          self.subroutineList.append(string)

      def codeBlock(self, model, model_cpl, intent=4, flag="comp", subroutines=[]):
          if flag=="comp":
              block = CodeBlock(model, model_cpl, intent)
              if len(subroutines)!=0:
                  block.subroutineSet(subroutines)
              else:
                  block.subroutineSet(self.subroutineList)
              return block.getStr()
          elif flag=="restart":
              block = CodeBlockUtil( model, model_cpl, intent=intent, flag="restart")
              if len(subroutines)!=0:
                  block.subroutineSet(subroutines)
              else:
                  block.subroutineSet(self.subroutineList)
              return block.getStr()
          elif flag=="hist":
              block = CodeBlockUtil(model, model_cpl, intent=intent, flag=flag)
              if len(subroutines)!=0:
                  block.subroutineSet(subroutines)
              else:
                  block.subroutineSet(self.subroutineList)
              return block.getStr()
          else:
              return "\n"




if __name__=="__main__":
   cw = CodeWrapper()
   cw.appendStr("seq_rest_mct(arg1, arg2)")
   cw.appendStr("MPI_Barrier(my_proc%a_comm)")
   print cw.codeBlock("cpl", "cpl",flag="hist")
