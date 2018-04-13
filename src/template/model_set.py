"""
For Test
"""
import sys
sys.path.append('../ir')
sys.path.append('../parser')
from ir import *
from av_set import *
from gsmap_set import *
from mapper_set import *
from parserMod import Parser

parser = Parser()
parser.parse()
model_a = parser.models['a']
model_b = parser.models['b']
model_c = parser.models['c']

#model_a = Model("a", 8) 
#model_a.append(a2x_aa)
#model_a.append(x2a_aa)
#model_a.append(a2x_ax)
#model_a.append(x2a_ax)
#model_a.append(gsMap_aa)
#model_a.append(gsMap_ax)
#model_a.append(mapper_Ca2x)
#model_a.append(mapper_Cx2a)
#
#model_b = Model("b", 12) 
#model_b.append(b2x_bb)
#model_b.append(x2b_bb)
#model_b.append(b2x_bx)
#model_b.append(x2b_bx)
#model_b.append(gsMap_bb)
#model_b.append(gsMap_bx)
#model_b.append(mapper_Cb2x)
#model_b.append(mapper_Cx2b)
#
#
#model_c = Model("c", 16) 
#model_c.append(c2x_cc)
#model_c.append(x2c_cc)
#model_c.append(c2x_cx)
#model_c.append(x2c_cx)
#model_c.append(gsMap_cc)
#model_c.append(gsMap_cx)
#model_c.append(mapper_Cc2x)
#model_c.append(mapper_Cx2c)




