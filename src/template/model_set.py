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

