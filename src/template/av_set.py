"""
For Test
"""
import sys
sys.path.append('../ir')
from ir import *
a2x_aa = AttrVect(
    lsize=4,
    field="fieldi",
    name="a2x_aa"
)
x2a_aa = AttrVect(
    src="x",
    lsize=4,
    field="fieldi",
    name="x2a_aa"
)
a2x_ax = AttrVect(
    pes="x",
    lsize=4,
    field="fieldi",
    name="a2x_ax"
)
x2a_ax = AttrVect(
    pes="x",
    src="x",
    lsize=4,
    field="fieldi",
    name="x2a_ax"
)


b2x_bb = AttrVect(
    lsize=6,
    field="fieldi",
    name="b2x_bb"
)
x2b_bb = AttrVect(
    src="x",
    lsize=6,
    field="fieldi",
    name="x2b_bb"
)
b2x_bx = AttrVect(
    pes="x",
    lsize=6,
    field="fieldi",
    name="b2x_bx"
)
x2b_bx = AttrVect(
    pes="x",
    src="x",
    lsize=6,
    field="fieldi",
    name="x2b_bx"
)

c2x_cc = AttrVect(
    lsize=8,
    field="fieldi",
    name="c2x_cc"
)
x2c_cc = AttrVect(
    src="x",
    lsize=8,
    field="fieldi",
    name="x2c_cc"
)
c2x_cx = AttrVect(
    pes="x",
    lsize=8,
    field="fieldi",
    name="c2x_cx"
)
x2c_cx = AttrVect(
    pes="x",
    src="x",
    lsize=8,
    field="fieldi",
    name="x2c_cx"
)
