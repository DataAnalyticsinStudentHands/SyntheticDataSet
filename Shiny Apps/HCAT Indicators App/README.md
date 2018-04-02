# HCAT Indicators App

The Housing and Urban Development program developed Healthy Community Indicators to "evaluate factors contributing to community health." More information can be found here: https://www.huduser.gov/healthycommunities/node/160058 .

This app creates side by side comparisons of some HCAT Indicators for Houston from Census Data and the citymodels Possible Houston. Further notes on the methodology for these indicators can be found here: https://www.huduser.gov/healthycommunities/sites/default/files/public/HCAT%20Administrative%20Guide_March2016.pdf

The script formatfrequencydata.R creates tables for the frequency of characteristics for the model which is assumed to be called sampleset.RDS, and frequencies of characteristics for the underlying data which in this case is a series of csv files in the Inputs folder. Some characteristics were remined for simplicity's sake. Future developments of this map may move towards relying on the Census data mined from the census_data_API function or include scripts for mining data for some of the characteristics that could be more simply organized.

It then merges with a shape file for tracts in Harris County, these are stored in a file "Harris_tract_data.rds"

If the user already has the csv files needed in an inputs folder, a model saved as sampleset.RDS, and the appropriate tract files the script can just be sourced, then either the ui or server can be sourced to display the app.
