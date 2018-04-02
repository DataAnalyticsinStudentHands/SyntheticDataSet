# General Map App
This application maps some of the different characteristics simulated by Cenus Tract as well as the underlying data. The data for these characteristics were taken from the U.S. Census and 500 Cities Project. The app is hosted here: http://dash.hnet.uh.edu:3838/cmupchur/general_map_app/

The script formatfrequencydata.R creates tables for the frequency of characteristics for the model which is assumed to be called sampleset.RDS, and frequencies of characteristics for the underlying data which in this case is a series of csv files in the Inputs folder. Some characteristics were remined for simplicity's sake. Future developments of this map may move towards relying on the Census data mined from the census_data_API function or include scripts for mining data for some of the characteristics that could be more simply organized.

It then merges with a shape file for tracts in Harris County, these are stored in a file "Harris_tract_data.rds"

If the user already has the csv files needed in an inputs folder, a model saved as sampleset.RDS, and the appropriate tract files the script can just be sourced, then either the ui or server can be sourced to display the app.
