
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

    ],
    'subroutine': {
        'init_method': [
            {
                'method_name':'c_init_mct',
                'params':{
                    'my_proc':'my_proc', 
                    'ID':'my_proc%modelc_id',
                    'EClock':'EClock',
                    'gsMap_cc':'gsMap_cc', 
                    'c2x_cc':'c2x_cc', 
                    'x2c_cc':'x2c_cc', 
                    'ierr':'ierr'
                }
            }
        ],
        'run_method': {
            'run_phase1_method': [
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_Cx2c',
                        'src':'x2c_cx',
                        'dst':'x2c_cc', 
                        'msgtag':'100+10+2', 
                        'ierr':'ierr',
                        'rList':'',
                     }
                 }
            ],
            'run_phase2_method': [
                {
                    'method_name': 'c_run_mct',
                    'params':{
                        'my_proc':'my_proc',
                        'ID':'my_proc%modelc_id',
                        'EClock':'EClock', 
                        'c2x':'c2x_cc', 
                        'x2c':'x2c_cc',
                        'ierr':'ierr'
                    }
                }
            ],
            'run_phase3_method': [
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_Cc2x',
                        'src':'c2x_cc',
                        'dst':'c2x_cx', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'',
                    }
                },
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_SMatc2a',
                        'src':'c2x_cx',
                        'dst':'c2x_ax', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'x',
                    }
                },
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_SMatc2b',
                        'src':'c2x_cx',
                        'dst':'c2x_bx', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'x',
                    }
                },
            ]
        },
        'final_method':[
            {
                'method_name':'c_final_mct',
                'params':{
                }
            }
        ]
    }

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

    ],
    'subroutine': {
        'init_method': [
            {
                'method_name':'b_init_mct',
                'params':{
                    'my_proc':'my_proc', 
                    'ID':'my_proc%modelb_id',
                    'EClock':'EClock',
                    'gsMap_bb':'gsMap_bb', 
                    'b2x_bb':'b2x_bb', 
                    'x2b_bb':'x2b_bb', 
                    'ierr':'ierr'
                }
            }
        ],
        'run_method': {
            'run_phase1_method': [
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_Cx2b',
                        'src':'x2b_bx',
                        'dst':'x2b_bb', 
                        'msgtag':'100+10+2', 
                        'ierr':'ierr',
                        'rList':'',
                     }
                 }
            ],
            'run_phase2_method': [
                {
                    'method_name': 'b_run_mct',
                    'params':{
                        'my_proc':'my_proc',
                        'ID':'my_proc%modelb_id',
                        'EClock':'EClock', 
                        'b2x':'b2x_bb', 
                        'x2b':'x2b_bb',
                        'ierr':'ierr'
                    }
                }
            ],
            'run_phase3_method': [
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_Cb2x',
                        'src':'b2x_bb',
                        'dst':'b2x_bx', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'',
                    }
                },
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_SMatb2a',
                        'src':'b2x_bx',
                        'dst':'b2x_ax', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'x',
                    }
                },
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_SMatb2c',
                        'src':'b2x_bx',
                        'dst':'b2x_cx', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'x',
                    }
                },
            ]
        },
        'final_method':[
            {
                'method_name':'b_final_mct',
                'params':{
                }
            }
        ]
    }
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

    ],

    'subroutine': {
        'init_method': [
            {
                'method_name':'a_init_mct',
                'params':{
                    'my_proc':'my_proc', 
                    'ID':'my_proc%modela_id',
                    'EClock':'EClock',
                    'gsMap_aa':'gsMap_aa', 
                    'a2x_aa':'a2x_aa', 
                    'x2a_aa':'x2a_aa', 
                    'ierr':'ierr'
                }
            }
        ],
        'run_method':{
            'run_phase1_method': [
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_Cx2a',
                        'src':'x2a_ax',
                        'dst':'x2a_aa', 
                        'msgtag':'100+10+2', 
                        'ierr':'ierr',
                        'rList':'',
                     }
                 }
            ],
            'run_phase2_method': [
                {
                    'method_name': 'a_run_mct',
                    'params':{
                        'my_proc':'my_proc',
                        'ID':'my_proc%modela_id',
                        'EClock':'EClock', 
                        'a2x':'a2x_aa', 
                        'x2a':'x2a_aa',
                        'ierr':'ierr'
                    }
                }
            ],
            'run_phase3_method': [
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_Ca2x',
                        'src':'a2x_aa',
                        'dst':'a2x_ax', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'',
                    }
                },
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_SMata2b',
                        'src':'a2x_ax',
                        'dst':'a2x_bx', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'x',
                    }
                },
                {
                    'method_name': 'mapper_comp_map',
                    'params':{
                        'mapper':'my_proc%mapper_SMata2c',
                        'src':'a2x_ax',
                        'dst':'a2x_cx', 
                        'msgtag':'100+10+3', 
                        'ierr':'ierr',
                        'rList':'x',
                    }
                },
            ],
            'run_phase4_method': {
            },
        },
        'final_method':[
            {
                'method_name':'a_final_mct',
                'params':{
                }
            }
        ]
    }
}

model_cfgs = [
    model_a_cfg,
    model_b_cfg,
    model_c_cfg
]
