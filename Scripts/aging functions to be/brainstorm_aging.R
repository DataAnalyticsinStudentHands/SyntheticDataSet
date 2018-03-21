
#Save data for two years
source("getcensusdata.R")
saveRDS(census_data_API(),"Census_data_2014.RDS")
saveRDS(census_data_API(base_url='http://api.census.gov/data/2015/acs5?'),"Census_data_2015.RDS")

#Clear environment
#Going to load part of the big dataset, subset a tract and lose the rest
rm(list=ls())
synthetic_data_set=readRDS("complete_sample_set1.RDS")
synthetic_data_set=subset(synthetic_data_set,synthetic_data_set$tract==554403)

#load following year information
Census_data_following_year=readRDS("Census_data_2015.RDS")


ageoverpeopleandbeginsomulatingmovinginpeople<-function(state,county,tract,synthetic_data_set,seed,Census_data_following_year){
  
  #subset part of synthetic data set to work with
  synthetic_data_set=subset(synthetic_data_set,synthetic_data_set$state==state,synthetic_data_set$county==county,synthetic_data_set$tract==tract)
  
  #create table of existing ages in synthetic dataset
  old_ages_table=ftable(synthetic_data_set$age)
  old_ages_dataframe=as.data.frame.matrix(old_ages_table)
  colnames(old_ages_dataframe)=gsub(" ","_",unlist(attr(old_ages_table, "col.vars")))
  
  #Create table of next years ages
  
  
  
}