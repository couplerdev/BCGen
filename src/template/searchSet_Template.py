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
            args = ",".join(args)
            func_str = "call "+ self.funcname + "(" + args + ")"
            #str_len = len(func_str) / 2
            #func_str = func_str[:str_len] + '&\n' + func_str[str_len:]

            res.append(func_str)
        return "\n".join(res)

#for $model in $models
    #set $model_name = $model.name
    #set $avs = $model.attrVects
    #set $c2x_cc = $avs['c2x_cc']
    #set $c2x_cx = $avs['c2x_cx']
    #set $x2c_cc = $avs['x2c_cc']
    #set $x2c_cx = $avs['x2c_cx']
    #set $mapper_c2x = $model.mappers['c2x']
    #set $mapper_x2c = $model.mappers['x2c']
method_name = '${model_name}_init_mct'
params = {
        'my_proc':'my_proc', 
        'ID':'my_proc%model${model_name}_id',
        'EClock':'EClock',
        'gsMap_${model_name}${model_name}':'gsMap_${model_name}${model_name}', 
        '${c2x_cc.name}':'${c2x_cc.name}', 
        '${x2c_cc.name}':'${x2c_cc.name}', 
        'ierr':'ierr'
    }
${model_name}_init = Temp(funcname=method_name, params=params)

#end for

#for $j, $model in enumerate($models)
    #set $model_name = $model.name
    #set $avs = $model.attrVects
    #set $c2x_cc = $avs['c2x_cc']
    #set $c2x_cx = $avs['c2x_cx']
    #set $x2c_cc = $avs['x2c_cc']
    #set $x2c_cx = $avs['x2c_cx']
    #set $comps = ['a', 'b', 'c']
    #set $mapper_c2x = $model.mappers['c2x']
    #set $mapper_x2c = $model.mappers['x2c']
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%${mapper_x2c.name}',
    'src':'${x2c_cx.name}',
    'dst':'${x2c_cc.name}', 
    'msgtag':'100+${j}0+2', 
    'ierr':'ierr',
    'rList':'',
}
${model_name}_run_phase1 = Temp(funcname=method_name, params=params)

method_name = '${model_name}_run_mct'
params = {
    'my_proc':'my_proc',
    'ID':'my_proc%model${model_name}_id',
    'EClock':'EClock', 
    '${model_name}2x':'${c2x_cc.name}', 
    'x2${model_name}':'${x2c_cc.name}',
    'ierr':'ierr'
}
${model_name}_run_phase2 = Temp(funcname=method_name, params=params)

sub_run_phase_3 = []
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%${mapper_c2x.name}',
    'src':'${c2x_cc.name}',
    'dst':'${c2x_cx.name}', 
    'msgtag':'100+${j}0+3', 
    'ierr':'ierr',
    'rList':'',
}
${model_name}_run_phase3_1 = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(${model_name}_run_phase3_1)

#if $merge_cfgs.has_key($model_name)
#for $i,$dst_info in enumerate($merge_cfgs[$model_name]['dst'])
    #set $d_av = $dst_info['dst_av']
    #set $av_mx_nx = $d_av.name
    #set $gm_nx = $dst_info['dst_gm']
    #set $dst_model_name = $dst_info['dst_model_name']
    #set $mapper_name = $dst_info['dst_mapper']
    #set $smat_size = $dst_info['smat_size']
    #set $run_phase_step = 2 + $i
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%${mapper_name}',
    'src':'${c2x_cx.name}',
    'dst':'${av_mx_nx}', 
    'msgtag':'100+${j}0+3', 
    'ierr':'ierr',
    'rList':'x',
}
${model_name}_run_phase3_${run_phase_step} = Temp(funcname=method_name, params=params)
sub_run_phase_3.append(${model_name}_run_phase3_${run_phase_step})
#end for
#end if

${model_name}_run_phase3 = Temp(subroutine=sub_run_phase_3,
             mix=True)
sub_run_phase_3 = []
#end for


#for $model in $models
    #set $model_name = $model.name
    #set $model_id = $model.ID
    #set $avs = $model.attrVects
    #set $gms = $model.gsMaps
    #set $gsMap_cx = $model.gsMaps['cpl']
    #set $gsMap_cc = $model.gsMaps['comp']
    #set $c2x_cc = $avs['c2x_cc']
    #set $c2x_cx = $avs['c2x_cx']
    #set $x2c_cc = $avs['x2c_cc']
    #set $x2c_cx = $avs['x2c_cx']
    #set $comps = ['a', 'b', 'c']
    #set $mapper_c2x = $model.mappers['c2x']
    #set $mapper_x2c = $model.mappers['x2c']

model_${model_name}_cfg = { # Model M's cfg
'model_unique_name': '${model_name}',
'model_unique_id': '${model_id}',
    'mx_av_set' : { # Av between model M and Cpl
        'mx_mm':{
            'name': '${c2x_cc.name}',
        },
        'mx_mx':{
            'name': '${c2x_cx.name}',
        },   
        'xm_mm':{
            'name': '${x2c_cc.name}',
        },   
        'xm_mx':{
            'name': '${x2c_cx.name}',
        }   
    },

    'mn_av_set': [ # Av between Model M and Model N

    #if $merge_cfgs.has_key($model_name)
    #for $dst_info in $merge_cfgs[$model_name]['dst']
        #set $d_av = $dst_info['dst_av']
        #set $av_mx_nx = $d_av.name
        #set $gm_nx = $dst_info['dst_gm']
        #set $dst_model_name = $dst_info['dst_model_name']
        #set $mapper_name = $dst_info['dst_mapper']
        #set $smat_size = $dst_info['smat_size']
        #set $dst_field = $dst_info['dst_field']
        {
            'n_name': '${dst_model_name}',
            'n_rAv': '${d_av.name}',
            'n_rField': '${dst_field}',
            'n_gm': '${gm_nx}',
            'transform_method': '',
        },
        
    #end for
    #end if
    ],


    'mx_gsmap_set':  { # gsMap of Model M
        'mx': {
            'name':'${gsMap_cx.name}'
        },
        'mm': {
            'name':'${gsMap_cc.name}'
        }
    },

    'subroutine': {
        'init_method': ${model_name}_init,
        'run_method': {
            'run_phase1_method': ${model_name}_run_phase1,
            'run_phase2_method': ${model_name}_run_phase2,
            'run_phase3_method': ${model_name}_run_phase3,
        },
        'final_method':[
            {
                'method_name':'${model_name}_final_mct',
                'params':{
                }
            }
        ]
    }

}
#end for


model_cfgs = [
#for $model in $models
    #set $model_name = $model.name
    model_${model_name}_cfg,
#end for
]


