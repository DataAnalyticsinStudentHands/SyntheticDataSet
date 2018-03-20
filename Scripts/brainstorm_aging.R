
#Save data for two years
source("getcensusdata.R")
saveRDS(census_data_API(),"Census_data_2014.RDS")
saveRDS(census_data_API(base_url='http://api.census.gov/data/2015/acs5?'),"Census_data_2015.RDS")

#Clear environment
#Going to load part of the big dataset, subset a tract and lose the rest
rm(list=ls())
synthetic_data_set=readRDS("complete_sample_set.RDS")


