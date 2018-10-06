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
            #str_len = len(func_str) / 2
            #func_str = func_str[:str_len] + '&\n' + func_str[str_len:]

            res.append(func_str)
        return "\n".join(res)

method_name = 'glc_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modelglc_id',
        'EClock':'EClock',
        'gsMap_glcglc':'gsMap_glcglc', 
        'glc2x_glcglc':'glc2x_glcglc', 
        'x2glc_glcglc':'x2glc_glcglc', 
        'ierr':'ierr'
    }
glc_init = Temp(funcname=method_name, params=params)

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
ice_init = Temp(funcname=method_name, params=params)

method_name = 'rof_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modelrof_id',
        'EClock':'EClock',
        'gsMap_rofrof':'gsMap_rofrof', 
        'rof2x_rofrof':'rof2x_rofrof', 
        'x2rof_rofrof':'x2rof_rofrof', 
        'ierr':'ierr'
    }
rof_init = Temp(funcname=method_name, params=params)

method_name = 'wav_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%modelwav_id',
        'EClock':'EClock',
        'gsMap_wavwav':'gsMap_wavwav', 
        'wav2x_wavwav':'wav2x_wavwav', 
        'x2wav_wavwav':'x2wav_wavwav', 
        'ierr':'ierr'
    }
wav_init = Temp(funcname=method_name, params=params)

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


method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2glc',
    'src':'x2glc_glcx',
    'dst':'x2glc_glcglc', 
    'msgtag':'100+00+2', 
    'ierr':'ierr',
}
glc_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'glc_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modelglc_id',
    'EClock':'EClock', 
    'glc2x':'glc2x_glcglc', 
    'x2glc':'x2glc_glcglc',
    'ierr':'ierr'
}
glc_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cglc2x',
    'src':'glc2x_glcglc',
    'dst':'glc2x_glcx', 
    'msgtag':'100+00+3', 
    'ierr':'ierr',
}
glc_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(glc_run_phase3_1)


glc_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2ocn',
    'src':'x2ocn_ocnx',
    'dst':'x2ocn_ocnocn', 
    'msgtag':'100+10+2', 
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
    'msgtag':'100+10+3', 
    'ierr':'ierr',
}
ocn_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatocn2wav',
    'src':'ocn2x_ocnx',
    'dst':'ocn2x_wavx', 
    'msgtag':'100+10+4', 
    'ierr':'ierr',
    'rList':'So_t:So_s:So_u',
}
ocn_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_2)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatocn2atm',
    'src':'ocn2x_ocnx',
    'dst':'ocn2x_atmx', 
    'msgtag':'100+10+4', 
    'ierr':'ierr',
    'rList':'So_t:So_s:So_u',
}
ocn_run_phase3_3 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_3)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatocn2ice',
    'src':'ocn2x_ocnx',
    'dst':'ocn2x_icex', 
    'msgtag':'100+10+4', 
    'ierr':'ierr',
    'rList':'So_t:So_s:So_u',
}
ocn_run_phase3_4 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ocn_run_phase3_4)

ocn_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2atm',
    'src':'x2atm_atmx',
    'dst':'x2atm_atmatm', 
    'msgtag':'100+20+2', 
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
    'msgtag':'100+20+3', 
    'ierr':'ierr',
}
atm_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatatm2wav',
    'src':'atm2x_atmx',
    'dst':'atm2x_wavx', 
    'msgtag':'100+20+4', 
    'ierr':'ierr',
    'rList':'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
}
atm_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_2)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatatm2ice',
    'src':'atm2x_atmx',
    'dst':'atm2x_icex', 
    'msgtag':'100+20+4', 
    'ierr':'ierr',
    'rList':'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
}
atm_run_phase3_3 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_3)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatatm2ocn',
    'src':'atm2x_atmx',
    'dst':'atm2x_ocnx', 
    'msgtag':'100+20+4', 
    'ierr':'ierr',
    'rList':'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
}
atm_run_phase3_4 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_4)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatatm2lnd',
    'src':'atm2x_atmx',
    'dst':'atm2x_lndx', 
    'msgtag':'100+20+4', 
    'ierr':'ierr',
    'rList':'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
}
atm_run_phase3_5 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(atm_run_phase3_5)

atm_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2ice',
    'src':'x2ice_icex',
    'dst':'x2ice_iceice', 
    'msgtag':'100+30+2', 
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
    'msgtag':'100+30+3', 
    'ierr':'ierr',
}
ice_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ice_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatice2wav',
    'src':'ice2x_icex',
    'dst':'ice2x_wavx', 
    'msgtag':'100+30+4', 
    'ierr':'ierr',
    'rList':'Si_avsdr:Si_anidr:Si_anidf',
}
ice_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ice_run_phase3_2)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatice2atm',
    'src':'ice2x_icex',
    'dst':'ice2x_atmx', 
    'msgtag':'100+30+4', 
    'ierr':'ierr',
    'rList':'Si_avsdr:Si_anidr:Si_anidf',
}
ice_run_phase3_3 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ice_run_phase3_3)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatice2ocn',
    'src':'ice2x_icex',
    'dst':'ice2x_ocnx', 
    'msgtag':'100+30+4', 
    'ierr':'ierr',
    'rList':'Si_avsdr:Si_anidr:Si_anidf',
}
ice_run_phase3_4 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(ice_run_phase3_4)

ice_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2rof',
    'src':'x2rof_rofx',
    'dst':'x2rof_rofrof', 
    'msgtag':'100+40+2', 
    'ierr':'ierr',
}
rof_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'rof_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modelrof_id',
    'EClock':'EClock', 
    'rof2x':'rof2x_rofrof', 
    'x2rof':'x2rof_rofrof',
    'ierr':'ierr'
}
rof_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Crof2x',
    'src':'rof2x_rofrof',
    'dst':'rof2x_rofx', 
    'msgtag':'100+40+3', 
    'ierr':'ierr',
}
rof_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(rof_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatrof2lnd',
    'src':'rof2x_rofx',
    'dst':'rof2x_lndx', 
    'msgtag':'100+40+4', 
    'ierr':'ierr',
    'rList':'Slrr_volr',
}
rof_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(rof_run_phase3_2)

rof_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2wav',
    'src':'x2wav_wavx',
    'dst':'x2wav_wavwav', 
    'msgtag':'100+50+2', 
    'ierr':'ierr',
}
wav_run_phase1 = Temp(funcname=method_name, params=params)

method_name = 'wav_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%modelwav_id',
    'EClock':'EClock', 
    'wav2x':'wav2x_wavwav', 
    'x2wav':'x2wav_wavwav',
    'ierr':'ierr'
}
wav_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cwav2x',
    'src':'wav2x_wavwav',
    'dst':'wav2x_wavx', 
    'msgtag':'100+50+3', 
    'ierr':'ierr',
}
wav_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(wav_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatwav2ocn',
    'src':'wav2x_wavx',
    'dst':'wav2x_ocnx', 
    'msgtag':'100+50+4', 
    'ierr':'ierr',
    'rList':'Sw_hstokes:Sw_lamult',
}
wav_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(wav_run_phase3_2)

wav_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%Mapper_Cx2lnd',
    'src':'x2lnd_lndx',
    'dst':'x2lnd_lndlnd', 
    'msgtag':'100+60+2', 
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
    'msgtag':'100+60+3', 
    'ierr':'ierr',
}
lnd_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(lnd_run_phase3_1)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatlnd2rof',
    'src':'lnd2x_lndx',
    'dst':'lnd2x_rofx', 
    'msgtag':'100+60+4', 
    'ierr':'ierr',
    'rList':'Sl_tref:Sl_qref:Sl_t',
}
lnd_run_phase3_2 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(lnd_run_phase3_2)
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_Smatlnd2atm',
    'src':'lnd2x_lndx',
    'dst':'lnd2x_atmx', 
    'msgtag':'100+60+4', 
    'ierr':'ierr',
    'rList':'Sl_tref:Sl_qref:Sl_t',
}
lnd_run_phase3_3 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(lnd_run_phase3_3)

lnd_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []



model_glc_cfg = { # Model M's cfg
'model_unique_name': 'glc',
'model_unique_id': '7',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': 'glc2x_glcglc',
        },
        'mx_mx':{
            'name': 'glc2x_glcx',
        },   
        'xm_mm':{
            'name': 'x2glc_glcglc',
        },   
        'xm_mx':{
            'name': 'x2glc_glcx',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_glcx'
        },
        'mm': {
            'name':'gsMap_glcglc'
        }
    },

    'subroutine': {
        'init_method': glc_init,
        'run_method': {
            'run_phase1_method': glc_run_phase1,
            'run_phase2_method': glc_run_phase2,
            'run_phase3_method': glc_run_phase3,
        },
        'final_method':[
            {
                'method_name':'glc_final_mct',
                'params':{
                }
            }
        ]
    }

}

model_ocn_cfg = { # Model M's cfg
'model_unique_name': 'ocn',
'model_unique_id': '1',
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
            'n_name': 'wav',
            'n_rAv': 'ocn2x_wavx',
            'n_rField': 'So_t:So_s:So_u',
            'n_gm': 'gsMap_wavx',
            'transform_method': '',
        },
        
        {
            'n_name': 'atm',
            'n_rAv': 'ocn2x_atmx',
            'n_rField': 'So_t:So_s:So_u',
            'n_gm': 'gsMap_atmx',
            'transform_method': '',
        },
        
        {
            'n_name': 'ice',
            'n_rAv': 'ocn2x_icex',
            'n_rField': 'So_t:So_s:So_u',
            'n_gm': 'gsMap_icex',
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

model_atm_cfg = { # Model M's cfg
'model_unique_name': 'atm',
'model_unique_id': '2',
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
            'n_name': 'wav',
            'n_rAv': 'atm2x_wavx',
            'n_rField': 'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
            'n_gm': 'gsMap_wavx',
            'transform_method': '',
        },
        
        {
            'n_name': 'ice',
            'n_rAv': 'atm2x_icex',
            'n_rField': 'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
            'n_gm': 'gsMap_icex',
            'transform_method': '',
        },
        
        {
            'n_name': 'ocn',
            'n_rAv': 'atm2x_ocnx',
            'n_rField': 'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
            'n_gm': 'gsMap_ocnx',
            'transform_method': '',
        },
        
        {
            'n_name': 'lnd',
            'n_rAv': 'atm2x_lndx',
            'n_rField': 'Sa_z:Sa_u:Sa_v:Sa_tbot:Sa_ptem',
            'n_gm': 'gsMap_lndx',
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
'model_unique_id': '4',
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

        {
            'n_name': 'wav',
            'n_rAv': 'ice2x_wavx',
            'n_rField': 'Si_avsdr:Si_anidr:Si_anidf',
            'n_gm': 'gsMap_wavx',
            'transform_method': '',
        },
        
        {
            'n_name': 'atm',
            'n_rAv': 'ice2x_atmx',
            'n_rField': 'Si_avsdr:Si_anidr:Si_anidf',
            'n_gm': 'gsMap_atmx',
            'transform_method': '',
        },
        
        {
            'n_name': 'ocn',
            'n_rAv': 'ice2x_ocnx',
            'n_rField': 'Si_avsdr:Si_anidr:Si_anidf',
            'n_gm': 'gsMap_ocnx',
            'transform_method': '',
        },
        
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

model_rof_cfg = { # Model M's cfg
'model_unique_name': 'rof',
'model_unique_id': '3',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': 'rof2x_rofrof',
        },
        'mx_mx':{
            'name': 'rof2x_rofx',
        },   
        'xm_mm':{
            'name': 'x2rof_rofrof',
        },   
        'xm_mx':{
            'name': 'x2rof_rofx',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

        {
            'n_name': 'lnd',
            'n_rAv': 'rof2x_lndx',
            'n_rField': 'Slrr_volr',
            'n_gm': 'gsMap_lndx',
            'transform_method': '',
        },
        
    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_rofx'
        },
        'mm': {
            'name':'gsMap_rofrof'
        }
    },

    'subroutine': {
        'init_method': rof_init,
        'run_method': {
            'run_phase1_method': rof_run_phase1,
            'run_phase2_method': rof_run_phase2,
            'run_phase3_method': rof_run_phase3,
        },
        'final_method':[
            {
                'method_name':'rof_final_mct',
                'params':{
                }
            }
        ]
    }

}

model_wav_cfg = { # Model M's cfg
'model_unique_name': 'wav',
'model_unique_id': '5',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': 'wav2x_wavwav',
        },
        'mx_mx':{
            'name': 'wav2x_wavx',
        },   
        'xm_mm':{
            'name': 'x2wav_wavwav',
        },   
        'xm_mx':{
            'name': 'x2wav_wavx',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

        {
            'n_name': 'ocn',
            'n_rAv': 'wav2x_ocnx',
            'n_rField': 'Sw_hstokes:Sw_lamult',
            'n_gm': 'gsMap_ocnx',
            'transform_method': '',
        },
        
    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'gsMap_wavx'
        },
        'mm': {
            'name':'gsMap_wavwav'
        }
    },

    'subroutine': {
        'init_method': wav_init,
        'run_method': {
            'run_phase1_method': wav_run_phase1,
            'run_phase2_method': wav_run_phase2,
            'run_phase3_method': wav_run_phase3,
        },
        'final_method':[
            {
                'method_name':'wav_final_mct',
                'params':{
                }
            }
        ]
    }

}

model_lnd_cfg = { # Model M's cfg
'model_unique_name': 'lnd',
'model_unique_id': '6',
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
            'n_name': 'rof',
            'n_rAv': 'lnd2x_rofx',
            'n_rField': 'Sl_tref:Sl_qref:Sl_t',
            'n_gm': 'gsMap_rofx',
            'transform_method': '',
        },
        
        {
            'n_name': 'atm',
            'n_rAv': 'lnd2x_atmx',
            'n_rField': 'Sl_tref:Sl_qref:Sl_t',
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


model_cfgs = [
    model_glc_cfg,
    model_ocn_cfg,
    model_atm_cfg,
    model_ice_cfg,
    model_rof_cfg,
    model_wav_cfg,
    model_lnd_cfg,
]


