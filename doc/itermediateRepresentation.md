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

CoupleEntity class implement some basic attribute and methods for Couple Entities and bound the behavior of Model, Mapper, etc

```
CoupleEntity attributes:self.__name : the name of this entity
                        self.__type : the type of this entity, used for check which func to call
                        self.__bind : whether this entity has bind to NameManager, if not, its                           name is not legal
```

CoupleEntity methods

```
def BindToManager(self,manager): bind this entity to a NameManager
```

### derived class Model

Model class describe the model. Model is a combination of model subroutine, relavent couple entities like attrVect.

```
Model attributes: self.__model_init : model init subroutine
       		      self.__model_run : model run subroutine
       		      self.__model_final : model final subroutine
       		      self.__attrVects : relevant attrVect, which only used for trans between                          coupler and used in model subroutine
       		      self.__gsMaps : relevant gsMap
       		      self.__mappers : relevant mappers, only the mapper_Cs are included
```

Model class methods

```
def append(self, obj): append obj to relevant obj list,(for example, attrVect to self.__attrVects)
```

### derived class AttrVect

### derived class Mapper

### derived class GsMap
