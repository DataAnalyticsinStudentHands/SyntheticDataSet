# Workflow controller for SAM creation
#source("BaseScripts/basesam.R")
library(tidyr)
library(dplyr)
library(stringr)
library(data.table)

#should change this so that state, county are part of the directory!

# before we get started, setup directories and parameters

##NEED TO BE RESTRUCTURED FOR EXPANDING TO OTHER LOCATIONS AND DATES
#maindir = "~/University Of Houston/Price, Daniel M - Social Network Hypergraphs/"
maindir = "~/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/" #Dan at home
#maindir = "~/Downloads/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/" #Dan at work
housingdir = paste0(maindir,"HCAD/")
houstondatadir = paste0(maindir,"HoustonCityData/") 
censusdir = paste0(maindir,"Census/") 
vintage = "2019"
housingStockFromRDS = TRUE 
#numberOfCores = 1
state = 48 #48 Texas; 22 Louisiana
county = 201 #8 county region: 201 Harris; 157 Fort Bend; 167 Galveston; 039 Brazoria; 071 Chambers; 291 Liberty; 339 Montgomery; 473 Waller ; other place FIPS are longer
tract = "*"
#seed = 135
#set.seed(seed = seed) #  have to reinvoke for each function call, but haven't decided where yet

#let's create SAM
#sam <- createBaseSAM(censusdir, housingdir, vintage, 
#                     housingStockFromRDS, 
#                     numberOfCores = numberOfCores, state = state, county = county, tract = tract)

#createBaseSAM will need to call exp_census_hh, prob. from within householdsGenerator.; then exp_census from within Individuals_generator
#current 8/2020 manual order: source Census_Data, workflow (ll. 3-24),
#Individuals_generator (expand_from_census), 
#householdsGenerator (expand_hh_from_census), partner_workers, 
#families_generator (expand_fam_from_census)
#HCAD_merge and HCAD_geo have not been functioned; all need error handling and sanity checks internally

# do some sanity checks and more columns
#if (sanityChecks(sam)) {
#  # Add column to add level to data
#  sam <- one_of(sam)
#  
#  # TODO move into HCAD preprocessing - adds cooordinates to the model based on the geometry column
#  sam <- add_lat_long(sam)
#  
#  # This converts all the columns in to the model to the appropriate class (either character or numeric)
#  sam <- convertColumnTypes(sam)
#  
#} else {
#  print("Did not pass sanity checks!")
#}

#add extension columns - NHANES etc.



#Save the final result
#saveRDS(sam, paste0(maindir,"complete_sam", Sys.Date(), ".RDS"))
