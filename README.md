# BCGen intro
## 1, introduction
We are implement a coupler generator for earth system model research. This project aims at flexibility and easy to use coupler. There are sorts of approach, such as CESM, MCT. These approaches have many problem, we call it as the trade-off between flexibility and easy to use. We are trying to implement a coupler with high flexibility and easy to use.
At present baseCpl v0.0 is just a toy coupler, in future work, we will wrap the components in CESM to test baseCpl, and add flux caculation and conserved quantity check.

## 2, code organization
In the top-level directory, we have baseCpl, the output code.
And user can compose their description of climate model application using the DSL we defined. Then user can use generator to generate their target code.

## 3, defined DSL
We have three XML composition files: coupler.xml define the attrVect needed for mrg attrVects from other components, model.xml define the components, schedule define the deployment of process and sequence of components if they are not run concurrently.
