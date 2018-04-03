
model_c_cfg = { # Model M's cfg
'model_unique_name': 'c',
'mx_av_set' : { # Av between model M and Cpl
    'mx_mm':{
        'name': 'c2x_cc',
    },
    'mx_mx':{
        'name': 'c2x_cx',
    },   
    'xm_mm':{
        'name': 'x2c_cc',
    },   
    'xm_mx':{
        'name': 'x2c_cx',
    }   
},


'mn_av_set': [ # Av between Model M and Model N
    {
        'n_name': 'a',
        'n_rAv': 'c2x_ax',
        'n_rField': 'x',
        'n_gm': 'gsmap_ax',
        'transform_method': '',
    },
    {
        'n_name': 'b',
        'n_rAv': 'c2x_bx',
        'n_rField': 'x',
        'n_gm': 'gsmap_bx',
        'transform_method': '',
    },
],

'mx_gsmap_set':  { # gsMap of Model M
    'mx': {
        'name':'gsMap_cx'
    },
    'mm': {
        'name':'gsMap_cc'
    }
},

'mapper_set': [

]

}
model_b_cfg = {
'model_unique_name': 'b',
'mx_av_set' : {
    'mx_mm':{
        'name': 'b2x_bb',
    },
    'mx_mx':{
        'name': 'b2x_bx',
    },   
    'xm_mm':{
        'name': 'x2b_bb',
    },   
    'xm_mx':{
        'name': 'x2b_bx',
    }   
},

'mn_av_set': [
    {
        'n_name': 'a',
        'n_rAv': 'b2x_ax',
        'n_rField': 'x',
        'n_gm': 'gsmap_ax',
        'transform_method': '',
    },
    {
        'n_name': 'c',
        'n_rAv': 'b2x_cx',
        'n_rField': 'x',
        'n_gm': 'gsmap_cx',
        'transform_method': '',
    },
],

'mx_gsmap_set':  {
    'mx': {
        'name':'gsMap_bx'
    },
    'mm': {
        'name':'gsMap_bb'
    }
},

'mapper_set': [

]

}
model_a_cfg = {
'model_unique_name': 'a',
'mx_av_set' : {
    'mx_mm':{
        'name': 'a2x_aa',
    },
    'mx_mx':{
        'name': 'a2x_ax',
    },   
    'xm_mm':{
        'name': 'x2a_aa',
    },   
    'xm_mx':{
        'name': 'x2a_ax',
    }   
},


'mn_av_set': [
    {
        'n_name': 'b',
        'n_rAv': 'a2x_bx',
        'n_rField': 'x',
        'n_gm': 'gsmap_bx',
        'transform_method': '',
    },
    {
        'n_name': 'c',
        'n_rAv': 'a2x_cx',
        'n_rField': 'x',
        'n_gm': 'gsmap_cx',
        'transform_method': '',
    },
],

'mx_gsmap_set':  {
    'mx': {
        'name':'gsMap_ax'
    },
    'mm': {
        'name':'gsMap_aa'
    }
},

'mapper_set': [

]

}

model_cfgs = [
    model_a_cfg,
    model_b_cfg,
    model_c_cfg
]
