#!/usr/bin/python

class Node:
    def __init__(self, index, data=None):
        self.index = index
        self.inEdge = {}
        self.outEdge = {}
        self.data = data
        self.visited = False

class Edge:
    def __init__(self, inNode, outNode):
        self.inNode = inNode
        self.outNode = outNode
        self.name = str(inNode.index) + "_to_"+str(outNode.index)
        inNode.outEdge[self.name] = self
        outNode.inEdge[self.name] = self

class Graph:
    def __init__(self):
        self.Nodes = {}

    def addNode(self, node):
        self.Nodes[node.index]=node

    def startSet(self):
        ret = []
        for node in self.Nodes:
            if not self.Nodes[node].visited and len(self.Nodes[node].inEdge) == 0:
                ret.append(self.Nodes[node])
        return ret
        

class SubroutineNode:
    def __init__(self, name, model="", phase=-1, inputArg=[], outputArg=[], strFormat=""):
        self.originName = name
        self.model = model
        self.phase = phase
        self.inputArg = inputArg
        self.outputArg = outputArg
        self.name = name+ model + str(phase)    # inner mark name
        self.strFormat = strFormat

class SeqRun:
    def __init__(self, seq=[]):
        self.graph = Graph()
        self.seq = seq   # model sorted only name
        self.subroutineSeq = [] # result
        self.varOutTable = {}  # key = var, val = subroutine name
        self.varInTable = {}
        self.modelPhase = {} # dict {model,  [phase, name] }
        self.subroutineDict = {} # key = subroutine name, val = subroutine obj
        self.startPos = {}
        self.getSeq = False
        self.seqDict = {}

    def getSeqDict(self):
        idx = 0
        for s in self.seq:
            self.seqDict[s] = idx
            idx+=1

    def addSubroutine(self, subroutine):
        if self.subroutineDict.has_key(subroutine.name):
            return 
        self.graph.addNode(Node(subroutine.name, data=subroutine))
	self.subroutineDict[subroutine.name] = subroutine
        if subroutine.phase == 0:
            for arg in subroutine.inputArg:
                self.startPos[arg] = subroutine.name
        else:
            for arg in subroutine.inputArg:
                self.varInTable[arg] = subroutine.name
        for arg in subroutine.outputArg:                  
            self.varOutTable[arg] = subroutine.name
        if subroutine.phase != -1:                               # for phase info
            if not self.modelPhase.has_key(subroutine.model):
                self.modelPhase[subroutine.model] = []
            self.modelPhase[subroutine.model].append((subroutine.phase,subroutine.name))
		

        # if in a defined model then define edge phases
        # if in has attr depending then define an edge
        # in this way, we only define edge by a subroutine 
        # attr, subroutine need attr for model, and attr for 
        # input arg, out arg, this also define an edge, 
        # different models have an order, then the same phase
        # should have an edge
        # subroutine should provide : model_name, input_arg(list)
        # output_arg(list), phase/step(1,2,3,4), or none model_type 
        # subroutine

    def parseToGraph(self):
        for modelName in self.modelPhase:
            model = self.modelPhase[modelName]
            model.sort(key=lambda pair:pair[0])
            if len(model) <= 2:
                break
            else:
                for i in range(len(model)-1):
                    p1 = model[i][1]
                    p2 = model[i+1][1]
                    node1 = self.graph.Nodes[p1]
                    node2 = self.graph.Nodes[p2]
                    e = Edge(node1, node2)
                    node1.outEdge[e.name] = e
                    node2.inEdge[e.name] = e
       
        for arg in self.varOutTable:
            if self.varInTable.has_key(arg):
                p2 = self.varInTable[arg]
                p1 = self.varOutTable[arg]
                node1 = self.graph.Nodes[p1]
                node2 = self.graph.Nodes[p2]
                e = Edge(node1, node2)
                node1.outEdge[e.name] = e
                node2.inEdge[e.name] = e
                 

    def topology(self):
        g = self.graph
        seq = []
        while(True):
            ret = g.startSet()

            if len(ret) == 0:
                break
            seq.append(ret)
            for n in ret:
                n.visited = True
                for eName in n.outEdge:
                    outNode = n.outEdge[eName].outNode
                    g.Nodes[outNode.index].inEdge.pop(eName)

        if len(self.seq)>0:
            idx = 0
            for layer in seq:
                newLayer = []
                sortsTuple = []
                for s in layer:
                    if s.data.model=="":
                        sortsTuple.append((s,100))
                    else:
                        sortsTuple.append((s, self.seqDict[s.data.model]))
                sortsTuple.sort(key=lambda pair:pair[1]) 
                for p in sortsTuple:
                    newLayer.append(p[0])
                seq[idx] = newLayer
                idx+=1
        return seq
if __name__ == "__main__":
    s = 10*[None]
    s[0] = SubroutineNode("mapper_comp_comm", model="a",phase=0, inputArg=["x2a_ax"], outputArg=["x2a_aa"])
    s[1] = SubroutineNode("a_run_mct", model="a", phase=1, inputArg=["x2a_aa"], outputArg=["a2x_aa"])
    s[2] = SubroutineNode("mapper_comp_comm", model="a", phase=2, inputArg=["a2x_aa"], outputArg=["a2x_ax"])
    s[3] = SubroutineNode("mappr_comp_comm", model="a", phase=3, inputArg=["a2x_ax"], outputArg=["a2x_bx"])
    s[4] = SubroutineNode("mrg_a", inputArg=["b2x_ax", "a2x_ax"], outputArg=["x2a_ax"])
    s[5] = SubroutineNode("mapper_comp_comm", model="b", phase=0, inputArg=["x2b_bx"], outputArg=["x2b_bb"])
    s[6] = SubroutineNode("b_run_mct", model="b", phase=1, inputArg=["x2b_bb"], outputArg=["b2x_bb"])
    s[7] = SubroutineNode("mapper_comp_comm", model="b", phase=2, inputArg=["b2x_bb"], outputArg=["b2x_bx"])
    s[8] = SubroutineNode("mapper_comp_comm", model="b", phase=3, inputArg=["b2x_bx"], outputArg=["b2x_ax"])
    s[9] = SubroutineNode("mrg_b", inputArg=["b2x_bx", "a2x_bx"], outputArg=["x2b_bx"])

    order = ["a", "b"]
    sq = SeqRun(order)
    print type(sq)
    sq.getSeqDict()
    for i in range(10):
        sq.addSubroutine(s[i])

    sq.parseToGraph()
    seq = sq.topology()
    print len(seq)
    for layer in seq:
        for si in layer:
            print si.index
        print '.....'
