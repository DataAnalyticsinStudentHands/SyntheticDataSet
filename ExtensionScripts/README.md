# Potential Expansions
This folder is for use cases and potential expansions.

## Create possible Houston on UH cluster with citymodels
This folder contains the code for a use case in simulating Houston, as this takes a lot of computational resources, it was done on the University of Houston's computer cluster Opuntia. This was implemented with the R packages doParallel and foreach, as well as citymodels.

## 500 Cities Project
This folder contains code for augmenting the model with data from the Center for Disease Control's 500 Cities Project accessible here: https://www.cdc.gov/500cities/ . Examples of added variables include obesity, cancer (not including skin cances) diabetes or colonscopy use. 

## HUD
This folder is devoted to future expansions using data from the Housing and Urban development program. This is still in progress and the scripts are mainly for merging with data from the Harris County Appraisal District.

## aging functions to be
These scripts are place holders for functions that will be used to model a population's change in characteristics and migration over time.

## functions for looking at the validity of the dataset
These scripts were writted to look at how closely the simulated model resembles the data it was created from.

## hcadparcelstuff
This folder is for cleaning and structuring the hcad parcel shapefiles for use to merge with the Houston citymodels model

## merging with HCAD functions
These scripts are for merging simulated households from citymodels with shapefiles of homes from the Harris County Appraisal District.
