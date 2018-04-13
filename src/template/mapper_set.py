"""
For Test
"""
import sys
sys.path.append('../ir')
from ir import *
from av_set import *

mapper_Ca2x = Mapper(a2x_aa, a2x_ax, name="mapper_Ca2x")
mapper_Cx2a = Mapper(x2a_ax, x2a_aa, name="mapper_Cx2a")

mapper_Cb2x = Mapper(b2x_bb, b2x_bx, name="mapper_Cb2x")
mapper_Cx2b = Mapper(x2b_bx, x2b_bb, name="mapper_Cx2b")

mapper_Cc2x = Mapper(c2x_cc, c2x_cx, name="mapper_Cc2x")
mapper_Cx2c = Mapper(x2c_cx, x2c_cc, name="mapper_Cx2c")



#mapper_SMatb2a = Mapper("c", "x", name="mapper_SMatb2a")
#mapper_SMatc2a = Mapper("c", "x", name="mapper_SMatc2a")
#
#
#mapper_SMata2b = Mapper("c", "x", name="mapper_SMata2b")
#mapper_SMatc2b = Mapper("c", "x", name="mapper_SMatc2b")
#
#mapper_SMata2c = Mapper("c", "x", name="mapper_SMata2c")
#mapper_SMatb2c = Mapper("c", "x", name="mapper_SMatb2c")
#
