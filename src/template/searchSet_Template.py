from model_set import *
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
            for key in self.params:
                item = key + '=' + self.params[key]
                if key == 'rList':
                    item = key + '=\'' + params[key]+ '\''
                args.append(str(item))
            args = ",".join(args)
            func_str = "call "+ self.funcname + "(" + args + ")"
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

#for $model in $models
    #set $model_name = $model.name
    #set $avs = $model.attrVects
    #set $c2x_cc = $avs['c2x_cc']
    #set $c2x_cx = $avs['c2x_cx']
    #set $x2c_cc = $avs['x2c_cc']
    #set $x2c_cx = $avs['x2c_cx']
    #set $comps = ['a', 'b', 'c']
    #set $others = [c for c in $comps if c != $model_name]
    #set $mapper_c2x = $model.mappers['c2x']
    #set $mapper_x2c = $model.mappers['x2c']
method_name='mapper_comp_map'
params = {
    'mapper':'my_proc%${mapper_x2c.name}',
    'src':'${x2c_cx.name}',
    'dst':'${x2c_cc.name}', 
    'msgtag':'100+10+2', 
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
method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%${mapper_c2x.name}',
    'src':'${c2x_cc.name}',
    'dst':'${c2x_cx.name}', 
    'msgtag':'100+10+3', 
    'ierr':'ierr',
    'rList':'',
}
${model_name}_run_phase3_1 = Temp(funcname=method_name, params=params)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_SMat${model_name}2${others[0]}',
    'src':'${c2x_cx.name}',
    'dst':'${model_name}2x_${others[0]}x', 
    'msgtag':'100+10+3', 
    'ierr':'ierr',
    'rList':'x',
}
${model_name}_run_phase3_2 = Temp(funcname=method_name, params=params)

method_name = 'mapper_comp_map'
params = {
    'mapper':'my_proc%mapper_SMat${model_name}2${others[1]}',
    'src':'${c2x_cx.name}',
    'dst':'${model_name}2x_${others[1]}x', 
    'msgtag':'100+10+3', 
    'ierr':'ierr',
    'rList':'x',
}
${model_name}_run_phase3_3 = Temp(funcname=method_name, params=params)
${model_name}_run_phase3 = Temp(subroutine=[
            ${model_name}_run_phase3_1,
            ${model_name}_run_phase3_2,
            ${model_name}_run_phase3_3], mix=True)

#end for


#for $model in $models
    #set $model_name = $model.name
    #set $avs = $model.attrVects
    #set $gms = $model.gsMaps
    #set $gsMap_cx = $model.gsMaps['cpl']
    #set $gsMap_cc = $model.gsMaps['comp']
    #set $c2x_cc = $avs['c2x_cc']
    #set $c2x_cx = $avs['c2x_cx']
    #set $x2c_cc = $avs['x2c_cc']
    #set $x2c_cx = $avs['x2c_cx']
    #set $comps = ['a', 'b', 'c']
    #set $others = [c for c in $comps if c != $model_name]
    #set $mapper_c2x = $model.mappers['c2x']
    #set $mapper_x2c = $model.mappers['x2c']

model_${model_name}_cfg = { # Model M's cfg
'model_unique_name': '${model_name}',
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
        {
            'n_name': '${others[0]}',
            'n_rAv': '${model_name}2x_${others[0]}x',
            'n_rField': 'x',
            'n_gm': 'gsmap_${others[0]}x',
            'transform_method': '',
        },
        {
            'n_name': '${others[1]}',
            'n_rAv': '${model_name}2x_${others[1]}x',
            'n_rField': 'x',
            'n_gm': 'gsmap_${others[1]}x',
            'transform_method': '',
        },
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
    model_a_cfg,
    model_b_cfg,
    model_c_cfg
]

deploy_cfgs = [
    model_a_cfg,
    model_b_cfg,
    model_c_cfg
]

