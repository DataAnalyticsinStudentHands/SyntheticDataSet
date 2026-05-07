setwd("~/Documents/SyntheticDataSet")
source('Sam/get_tools.R') #for valid_file_paths
library(stringr)
library(data.table)
library(sf)

maindir = "~/University\ Of\ Houston/Engaged\ Data\ Science\ -\ Data/" #Dan Studio
#maindir = "~/Documents/Sam_data/" #if need local
censusmapdir = paste0(maindir,"Census/Cartographic\ Boundary\ Files/") 
state = "48"
vintage = "2020"


#act as if a master join?
#bg_RDS <- valid_file_path(censusmapdir,vintage,state,county = "*","official","500k","block_group","combined")
#
#if(file.exists(bg_RDS)){
#  bg_census_map <- readRDS(bg_RDS)
#}else{
#  bg_census_map <- census_GIS_state_2020(census_GIS_state_2020)
#  saveRDS(bg_census_map,bg_RDS)
#}

#assign to available houses
