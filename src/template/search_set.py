class Temp:
    mix = False
    subroutine = []
    funcname = ""
    params = {
    
    }
    def __init__(self, funcname="", params="", mix=False, subroutine=[]):
        self.a = "333"
        self.subroutine = subroutine
        self.funcname = funcname
        self.params = params
        self.mix = mix
        self.space = 24*" "
    def getName(self):
        return "rr34"
    def getFuncFormat(self):
        res = []
        if self.mix:
            for routine in self.subroutine:
                res.append(routine.getFuncFormat())
        else:
            args = []
            for key in sorted(self.params):
                item = key + '=' + self.params[key] +'&\n'
                if key == 'rList':
                    item = key + '=\'' + params[key]+ '\'&\n' 
                args.append(str(item))
            args = (self.space+",").join(args)
            func_str = "call "+ self.funcname + "(" + args + self.space+")"
            #str_len = len(func_str) / 2
            #func_str = func_str[:str_len] + '&\n' + func_str[str_len:]

            res.append(func_str)
        return "\n".join(res)

method_name = 'a_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modela_id',
        'EClock':'EClock',
        'gsMap_aa':'gsMap_aa', 
        'a2x_aa':'a2x_aa', 
        'x2a_aa':'x2a_aa', 
        'ierr':'ierr'
    }
a_init = Temp(funcname=method_name, params=params)

method_name = 'lnd_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modellnd_id',
        'EClock':'EClock',
        'gsMap_lndlnd':'gsMap_lndlnd', 
        'lnd2x_lndlnd':'lnd2x_lndlnd', 
        'x2lnd_lndlnd':'x2lnd_lndlnd', 
        'ierr':'ierr'
    }
lnd_init = Temp(funcname=method_name, params=params)

method_name = 'b_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modelb_id',
        'EClock':'EClock',
        'gsMap_bb':'gsMap_bb', 
        'b2x_bb':'b2x_bb', 
        'x2b_bb':'x2b_bb', 
        'ierr':'ierr'
    }
b_init = Temp(funcname=method_name, params=params)

method_name = 'atm_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modelatm_id',
        'EClock':'EClock',
        'gsMap_atmatm':'gsMap_atmatm', 
        'atm2x_atmatm':'atm2x_atmatm', 
        'x2atm_atmatm':'x2atm_atmatm', 
        'ierr':'ierr'
    }
atm_init = Temp(funcname=method_name, params=params)

method_name = 'ocn_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modelocn_id',
        'EClock':'EClock',
        'gsMap_ocnocn':'gsMap_ocnocn', 
        'ocn2x_ocnocn':'ocn2x_ocnocn', 
        'x2ocn_ocnocn':'x2ocn_ocnocn', 
        'ierr':'ierr'
    }
ocn_init = Temp(funcname=method_name, params=params)


method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2a',
    'src':'x2a_ax',
    'dst':'x2a_aa', 
    'msgtag':'100+00+2', 
    'ierr':'ierr',
}
a_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'a_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modela_id',
    'EClock':'EClock', 
    'a2x':'a2x_aa', 
    'x2a':'x2a_aa',
    'ierr':'ierr'
}
a_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Ca2x',
    'src':'a2x_aa',
    'dst':'a2x_ax', 
    'msgtag':'100+00+3', 
    'ierr':'ierr',
}
a_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(a_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_SMata2ocn',
    'src':'a2x_ax',
    'dst':'a2x_ocnx', 
    'msgtag':'100+00+4', 
    'ierr':'ierr',
    'rList':'lat:lon',
}
a_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(a_run_phase3_2)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_SMata2b',
    'src':'a2x_ax',
    'dst':'a2x_bx', 
    'msgtag':'100+00+4', 
    'ierr':'ierr',
    'rList':'lat:lon',
}
a_run_phase3_3 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(a_run_phase3_3)

a_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2lnd',
    'src':'x2lnd_lndx',
    'dst':'x2lnd_lndlnd', 
    'msgtag':'100+10+2', 
    'ierr':'ierr',
}
lnd_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'lnd_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modellnd_id',
    'EClock':'EClock', 
    'lnd2x':'lnd2x_lndlnd', 
    'x2lnd':'x2lnd_lndlnd',
    'ierr':'ierr'
}
lnd_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Clnd2x',
    'src':'lnd2x_lndlnd',
    'dst':'lnd2x_lndx', 
    'msgtag':'100+10+3', 
    'ierr':'ierr',
}
lnd_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(lnd_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_SMatlnd2atm',
    'src':'lnd2x_lndx',
    'dst':'lnd2x_atmx', 
    'msgtag':'100+10+4', 
    'ierr':'ierr',
    'rList':'velo:cal',
}
lnd_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(lnd_run_phase3_2)

lnd_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2b',
    'src':'x2b_bx',
    'dst':'x2b_bb', 
    'msgtag':'100+20+2', 
    'ierr':'ierr',
}
b_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'b_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modelb_id',
    'EClock':'EClock', 
    'b2x':'b2x_bb', 
    'x2b':'x2b_bb',
    'ierr':'ierr'
}
b_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cb2x',
    'src':'b2x_bb',
    'dst':'b2x_bx', 
    'msgtag':'100+20+3', 
    'ierr':'ierr',
}
b_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(b_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_SMatb2a',
    'src':'b2x_bx',
    'dst':'b2x_ax', 
    'msgtag':'100+20+4', 
    'ierr':'ierr',
    'rList':'lat:lon',
}
b_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(b_run_phase3_2)

b_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2atm',
    'src':'x2atm_atmx',
    'dst':'x2atm_atmatm', 
    'msgtag':'100+30+2', 
    'ierr':'ierr',
}
atm_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'atm_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modelatm_id',
    'EClock':'EClock', 
    'atm2x':'atm2x_atmatm', 
    'x2atm':'x2atm_atmatm',
    'ierr':'ierr'
}
atm_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Catm2x',
    'src':'atm2x_atmatm',
    'dst':'atm2x_atmx', 
    'msgtag':'100+30+3', 
    'ierr':'ierr',
}
atm_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_1)


atm_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2ocn',
    'src':'x2ocn_ocnx',
    'dst':'x2ocn_ocnocn', 
    'msgtag':'100+40+2', 
    'ierr':'ierr',
}
ocn_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'ocn_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modelocn_id',
    'EClock':'EClock', 
    'ocn2x':'ocn2x_ocnocn', 
    'x2ocn':'x2ocn_ocnocn',
    'ierr':'ierr'
}
ocn_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cocn2x',
    'src':'ocn2x_ocnocn',
    'dst':'ocn2x_ocnx', 
    'msgtag':'100+40+3', 
    'ierr':'ierr',
}
ocn_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_SMatocn2atm',
    'src':'ocn2x_ocnx',
    'dst':'ocn2x_atmx', 
    'msgtag':'100+40+4', 
    'ierr':'ierr',
    'rList':'velo:cal',
}
ocn_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_2)

ocn_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []



model_a_cfg = { # Model M's cfg
'model_unique_name': 'a',
'model_unique_id': '1',
    'mx_av_set' : { # Av between model M and Cpl
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

    'mn_av_set': [ # Av between Model M and Model N

        {
            'n_name': 'ocn',
            'n_rAv': 'a2x_ocnx',
            'n_rField': 'lat:lon',
            'n_gm': 'gsMap_ocnx',
            'transform_method': '',
        },
        
        {
            'n_name': 'b',
            'n_rAv': 'a2x_bx',
            'n_rField': 'lat:lon',
            'n_gm': 'gsMap_bx',
            'transform_method': '',
        },
        
    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_ax'
        },
        'mm': {
            'name':'gsMap_aa'
        }
    },

    'subroutine': {
        'init_method': a_init,
        'run_method': {
            'run_phase1_method': a_run_phase1,
            'run_phase2_method': a_run_phase2,
            'run_phase3_method': a_run_phase3,
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

model_lnd_cfg = { # Model M's cfg
'model_unique_name': 'lnd',
'model_unique_id': '5',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': 'lnd2x_lndlnd',
        },
        'mx_mx':{
            'name': 'lnd2x_lndx',
        },   
        'xm_mm':{
            'name': 'x2lnd_lndlnd',
        },   
        'xm_mx':{
            'name': 'x2lnd_lndx',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

        {
            'n_name': 'atm',
            'n_rAv': 'lnd2x_atmx',
            'n_rField': 'velo:cal',
            'n_gm': 'gsMap_atmx',
            'transform_method': '',
        },
        
    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_lndx'
        },
        'mm': {
            'name':'gsMap_lndlnd'
        }
    },

    'subroutine': {
        'init_method': lnd_init,
        'run_method': {
            'run_phase1_method': lnd_run_phase1,
            'run_phase2_method': lnd_run_phase2,
            'run_phase3_method': lnd_run_phase3,
        },
        'final_method':[
            {
                'method_name':'lnd_final_mct',
                'params':{
                }
            }
        ]
    }

}

model_b_cfg = { # Model M's cfg
'model_unique_name': 'b',
'model_unique_id': '2',
    'mx_av_set' : { # Av between model M and Cpl
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

    'mn_av_set': [ # Av between Model M and Model N

        {
            'n_name': 'a',
            'n_rAv': 'b2x_ax',
            'n_rField': 'lat:lon',
            'n_gm': 'gsMap_ax',
            'transform_method': '',
        },
        
    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_bx'
        },
        'mm': {
            'name':'gsMap_bb'
        }
    },

    'subroutine': {
        'init_method': b_init,
        'run_method': {
            'run_phase1_method': b_run_phase1,
            'run_phase2_method': b_run_phase2,
            'run_phase3_method': b_run_phase3,
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

model_atm_cfg = { # Model M's cfg
'model_unique_name': 'atm',
'model_unique_id': '4',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': 'atm2x_atmatm',
        },
        'mx_mx':{
            'name': 'atm2x_atmx',
        },   
        'xm_mm':{
            'name': 'x2atm_atmatm',
        },   
        'xm_mx':{
            'name': 'x2atm_atmx',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_atmx'
        },
        'mm': {
            'name':'gsMap_atmatm'
        }
    },

    'subroutine': {
        'init_method': atm_init,
        'run_method': {
            'run_phase1_method': atm_run_phase1,
            'run_phase2_method': atm_run_phase2,
            'run_phase3_method': atm_run_phase3,
        },
        'final_method':[
            {
                'method_name':'atm_final_mct',
                'params':{
                }
            }
        ]
    }

}

model_ocn_cfg = { # Model M's cfg
'model_unique_name': 'ocn',
'model_unique_id': '3',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': 'ocn2x_ocnocn',
        },
        'mx_mx':{
            'name': 'ocn2x_ocnx',
        },   
        'xm_mm':{
            'name': 'x2ocn_ocnocn',
        },   
        'xm_mx':{
            'name': 'x2ocn_ocnx',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

        {
            'n_name': 'atm',
            'n_rAv': 'ocn2x_atmx',
            'n_rField': 'velo:cal',
            'n_gm': 'gsMap_atmx',
            'transform_method': '',
        },
        
    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_ocnx'
        },
        'mm': {
            'name':'gsMap_ocnocn'
        }
    },

    'subroutine': {
        'init_method': ocn_init,
        'run_method': {
            'run_phase1_method': ocn_run_phase1,
            'run_phase2_method': ocn_run_phase2,
            'run_phase3_method': ocn_run_phase3,
        },
        'final_method':[
            {
                'method_name':'ocn_final_mct',
                'params':{
                }
            }
        ]
    }

}


model_cfgs = [
    model_a_cfg,
    model_lnd_cfg,
    model_b_cfg,
    model_atm_cfg,
    model_ocn_cfg,
]


