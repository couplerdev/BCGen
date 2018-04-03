# Documentation of IntermediateRepresentation

## classes in summary
There are two parents classes, others are derived from them: Subroutine and CoupleEntity. MergSubroutine and ModelSubroutine are basic requirements for users to define in XML. As for CoupleEntity, AttrVect, GsMap, Mapper, sMat(may be delete in a new version), Model. These CoupleEntity represent some infomation that is neccesary for couple. And Model shall have some attributes to include its relevant variables. More details shall be placed below.
## Subroutine class
### parent class Subroutine
Subroutine class is a class describe subroutine
```
Subroutine attributes: __subroutineName, name of this subroutine
                       __argList, list of subroutine list
                       __lineCharacter, how much character in one line
```
Subroutine methods
```
def append(arg): append an argment to __argList
def toString(): transform the instance to a string
```
### derived class MergSubroutine
MergSubroutine class is a class describe merg subroutine.The merg subroutine get dst attrVect and src attrVect, then use a given Mapper to mrg src attrVect to dst attrVect.
```
MergSubroutine attrbutes : __subroutineName
                           __argList
                           __lineCharacter
```
MergSubroutine methods
```
```
### derived class ModelSubroutine
ModelSubroutine class is a class describe model subroutine. In our definition, a model subroutine is a set of subroutine provided by component developers including model_init, model_run, model_final etc.
```
ModelSubroutine attributes: __name : name of this model
                            __subroutineName: the name of ModelSubroutine
                            __argList: list of the ModelSubroutine
                            default: the pattern of name generation: True by model name, False use user defined __subroutineName
                            __wrapper: the API used by this subroutine, now MCT
```
ModelSubroutine methods
```

```
## CoupleEntity class
### parent class CoupleEntity

### derived class Model

### derived class AttrVect

### derived class Mapper

### derived class GsMap
