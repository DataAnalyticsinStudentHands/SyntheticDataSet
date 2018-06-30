#Master example

#Load previous and current year of HCAD data
#these shapefiles and text files can be downloaded from
#http://pdata.hcad.org/download/index.html
#They should be cleaned and organized appopriately using the prepareHCADparcels.R script 
#in the merging with HCAD functions folder
parcels_2014=readRDS("2014 HCAD stuff/valid_parcels_for_simulation.RDS")
parcels_2015=readRDS("2015 HCAD stuff/valid_parcels_for_simulation.RDS")

#Get vector of account numbers that moved out
source("find_ACCOUNT_numbers_that_moved_out.R")
ACCOUNTS_that_moved_out=find_ACCOUNT_numbers_that_moved_out(parcels_2014,parcels_2015)

#Remove shapefiles so there's more space for the dataset
rm(parcels_2014,parcels_2015)

#Load sample set
sample_set=readRDS("complete_sample_set2018-05-29.RDS")
sample_set=sample_set[,c(1:26,43:45)]

#If the age is still in brackets as created in citymodels, an individual numeric age may need to be sampled
source("get_age_from_brackets.R")
#Apply to set
sample_set$real_age=mapply(get_age_from_brackets,c(1:nrow(sample_set)),sample_set$age)

#Everyone should age one year
sample_set$real_age=sample_set$real_age+1

#Move people out due to HCAD
sample_set$Moved_Out=NA
sample_set$Moved_Out[sample_set$ACCOUNT %in% ACCOUNTS_that_moved_out]="Moved Out"

#Move out more people such as renters who wouldn't have caused a change in HCAD based off of Census data
#this must be done by tract
#Read in Census data
current_data_for_update=readRDS("Census_data_2015.RDS")
source("move_out_by_tract.R")

#get vector of tracts
tracts=unique(sample_set$tract)
#I cant run all the tracts on my laptop so we wil just run the first 200
tracts=tracts[1:5]

#Start dataframes
people_that_moved_out=data.frame()
people_still_living_in_the_tract=data.frame()
for(tract in tracts){
  sort_people=move_out_by_tract(sample_set,current_data_for_update,tract,1)
  
  people_that_moved_out=rbind(people_that_moved_out,sort_people$people_that_moved_out)
  people_still_living_in_the_tract=rbind(people_still_living_in_the_tract,sort_people$people_still_living_in_the_tract)
}

#Create Babies born to people still living in the tract
source("create_babies.R")
babies=data.frame()
#create readjusted people still living in the tract to account for changes in family size
people_still_living_in_the_tract2=data.frame()

for(tract in tracts){
  babies_and_families=create_babies(people_still_living_in_the_tract,current_data_for_update,tract,1)
  
  babies=rbind(babies,babies_and_families$babies)
  people_still_living_in_the_tract2=rbind(people_still_living_in_the_tract2,babies_and_families$people_still_living_in_the_tract)
}

#Move people into a tract
source("people_moving_in1.R")

people_moved_within_county = data.frame()
people_moved_in_from_out_of_county = data.frame()

for(tract in tracts){
  new_neighbors = peoplemovingin(current_data_for_update, tract, people_that_moved_out, people_still_living_in_the_tract, babies,1)
  
  people_moved_within_county = rbind(people_moved_within_county, new_neighbors$people_moved_within_county)
  people_moved_in_from_out_of_county = rbind(people_moved_in_from_out_of_county, new_neighbors$people_moved_in_from_out_of_county)
}
