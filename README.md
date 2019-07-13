# Synthetic Data Set

This repository is the main development repo for the Synthetic Dataset project. The aim of this project is to build code and examples that help emulating certain aspects of mostly health realated data questions. Examples have been created to illustrate the approach and they are based on the City of Houston where the [DASH](http://dash.hnet.uh.edu/DASH/) team at the [University of Houston](http://www.uh.edu/) is located. 

## How to get started

The main.R file controls the flow for the simulation.

First the input data:

a) Census data (loaded either from existing RDS data files or generated through the Census API)
b) Housing stock data (loaded either from existing RDS files using Harris County Appraisal District data or generated through other sources)

is loaded and prepared for merging so that households can be formed. We simulate individuals with certain characterics from the census data information. Those individuals are forming households.
The housig stock data will provide locations where those households can be placed.

The result will be a dataset of simulated individuals.

In another step more variables (see Potential Expansions) can be added to the dataset using base cahracteristics for merging.

### Controlling aspects of the simulation

Inputs for the base simulations are mostly controlled through mappings which can be found inside the Mappings folder.

## Potential Expansions

Those are scripts for possible expansions of the project  to add health data variables for instance from the Center for Disease Control.




## Shiny Apps
Possible questions this kind of model could be used to investigate are in the Shiny Apps folder. Asthma App and Diabetes App are demonstrations of using the model to extrapolate some of the effects of public health interventions. Tiffany App, Nick App, and Adelle App are apps from our Modeling Metabolism students who were interested inpublic health health questions that may be modeled further at a later time.


