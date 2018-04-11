"""
For Test
"""
import sys
sys.path.append('../ir')
from ir import *

mapper_Ca2x = Mapper("a", "x", name="mapper_Ca2x")
mapper_Cx2a = Mapper("x", "a", name="mapper_Cx2a")

mapper_Cb2x = Mapper("b", "x", name="mapper_Cb2x")
mapper_Cx2b = Mapper("x", "b", name="mapper_Cx2b")

mapper_Cc2x = Mapper("c", "x", name="mapper_Cc2x")
mapper_Cx2c = Mapper("x", "c", name="mapper_Cx2c")
