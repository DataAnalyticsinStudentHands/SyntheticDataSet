# 500 Cities Project
These are functions to add health behaviors and outcomes from the Center for Disease Control's 500 Cities project to the synthetic dataset created by citymodels. 

The 500 Cities Project is the Center for Disease Control's model of the prevalence of different health behaviors and outcomes on the census tract level of 500 major U.S. cities. Data is from the Behavioral Risk Factor Surveillance System. More information can be found at https://www.cdc.gov/500cities/.

houstondata.csv was downloaded from the following link https://chronicdata.cdc.gov/500-Cities/500-Cities-Local-Data-for-Better-Health-2017-relea/6vp6-wxuq/data after the Geographical Level was set to Census Tract, State to Texas, and CityName to Houston. This data was then used with the functions in the R Script to add variables to the Houston Citymodels use case.

Functions are called in a similar manner to the functions for characteristics in the citymodels package. For example:

```R
model_individual_to_add_variable_to=getarthritis(county, tract, model_individual_to_add_variable_to,seed)
```

These functions are easily added to the simulation to begin with, otherwise to get a unique seed for sampling the user could use a the mapply function. For example:

```R
model_to_add_variable_to=mapply(getarthritis,model_to_add_variable_to$county, model_to_add_variable_to$tract,model_to_add_variable_to,row_model_to_add_variable_to)
```
