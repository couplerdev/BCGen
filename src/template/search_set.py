import sys
sys.path.append('../ir')
sys.path.append('../parser')
from ir import ModelSubroutine
from codeWrapper import toString


class Temp:
    mix = False
    subroutine = []
    funcname = ""
    params = {
    
    }
    def __init__(self, funcname="", params="", mix=False, subroutine=[], strFmt=""):
        self.a = "333"
        self.subroutine = subroutine
        self.funcname = funcname
        self.params = params
        self.mix = mix
        self.space = 24*" "
        self.strFmt =  strFmt

    def getName(self):
        return "rr34"
  
    def getStrFormat(self):
	return 'call '+self.strFmt

    def getFuncFormat(self):
        res = []
        if self.mix:
            for routine in self.subroutine:
                res.append(routine.getFuncFormat())
        else:
            args = []
            l = len(self.params)
            indx = 0
            for key in sorted(self.params):
                indx = indx+1
                item = key + '=' + self.params[key] +'&\n'
                if key == 'rList':
                    item = key + '=\'' + self.params[key]+ '\'&\n' 
                if indx == l:
                    item = key + '=' + self.params[key]
                args.append(str(item))
            args = (self.space+",").join(args)
            func_str = "call "+ self.funcname + "(" + args +")"

            res.append(func_str)
        return "\n".join(res)

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
atm_init = Temp(funcname=method_name, params=params, strFmt='atm_init_mct(metaData%atm, EClock_atm, x2atm_atmatm, atm2x_atmatm, ierr=ierr)')

method_name = 'ice_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modelice_id',
        'EClock':'EClock',
        'gsMap_iceice':'gsMap_iceice', 
        'ice2x_iceice':'ice2x_iceice', 
        'x2ice_iceice':'x2ice_iceice', 
        'ierr':'ierr'
    }
ice_init = Temp(funcname=method_name, params=params, strFmt='ice_init_mct(metaData%ice, EClock_ice, x2ice_iceice, ice2x_iceice, ierr=ierr)')

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
ocn_init = Temp(funcname=method_name, params=params, strFmt='ocn_init_mct(metaData%ocn, EClock_ocn, x2ocn_ocnocn, ocn2x_ocnocn, ierr=ierr)')


method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2atm',
    'src':'x2atm_atmx',
    'dst':'x2atm_atmatm', 
    'msgtag':'100+00+2', 
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
    'msgtag':'100+00+3', 
    'ierr':'ierr',
}
atm_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_1)



method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatatm2ocn',
    'src':'atm2x_atmx',
    'dst':'atm2x_ocnx', 
    'msgtag':'100+00+4', 
    'ierr':'ierr',
    'rList':'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
}
atm_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_2)


atm_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2ice',
    'src':'x2ice_icex',
    'dst':'x2ice_iceice', 
    'msgtag':'100+10+2', 
    'ierr':'ierr',
}
ice_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'ice_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modelice_id',
    'EClock':'EClock', 
    'ice2x':'ice2x_iceice', 
    'x2ice':'x2ice_iceice',
    'ierr':'ierr'
}
ice_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cice2x',
    'src':'ice2x_iceice',
    'dst':'ice2x_icex', 
    'msgtag':'100+10+3', 
    'ierr':'ierr',
}
ice_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ice_run_phase3_1)





ice_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2ocn',
    'src':'x2ocn_ocnx',
    'dst':'x2ocn_ocnocn', 
    'msgtag':'100+20+2', 
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
    'msgtag':'100+20+3', 
    'ierr':'ierr',
}
ocn_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_1)



method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatocn2atm',
    'src':'ocn2x_ocnx',
    'dst':'ocn2x_atmx', 
    'msgtag':'100+20+4', 
    'ierr':'ierr',
    'rList':'So_t:So_s:So_u',
}
ocn_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_2)


ocn_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []



model_atm_cfg = { # Model M's cfg
'model_unique_name': 'atm',
'model_unique_id': '1',
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

        {
            'n_name': 'ocn',
            'n_rAv': 'atm2x_ocnx',
            'n_rField': 'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
            'n_gm': 'gsMap_ocnx',
            'transform_method': '',
        },
        
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

model_ice_cfg = { # Model M's cfg
'model_unique_name': 'ice',
'model_unique_id': '3',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': 'ice2x_iceice',
        },
        'mx_mx':{
            'name': 'ice2x_icex',
        },   
        'xm_mm':{
            'name': 'x2ice_iceice',
        },   
        'xm_mx':{
            'name': 'x2ice_icex',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_icex'
        },
        'mm': {
            'name':'gsMap_iceice'
        }
    },

    'subroutine': {
        'init_method': ice_init,
        'run_method': {
            'run_phase1_method': ice_run_phase1,
            'run_phase2_method': ice_run_phase2,
            'run_phase3_method': ice_run_phase3,
        },
        'final_method':[
            {
                'method_name':'ice_final_mct',
                'params':{
                }
            }
        ]
    }

}

model_ocn_cfg = { # Model M's cfg
'model_unique_name': 'ocn',
'model_unique_id': '2',
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
            'n_rField': 'So_t:So_s:So_u',
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
    model_atm_cfg,
    model_ice_cfg,
    model_ocn_cfg,
]


