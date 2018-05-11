
#Save data for two years
#source("getcensusdata.R")
#saveRDS(census_data_API(),"Census_data_2014.RDS")
#saveRDS(census_data_API(base_url='http://api.census.gov/data/2015/acs5?'),"Census_data_2015.RDS")

#Clear environment
#Going to load part of the big dataset, subset a tract and lose the rest
#rm(list=ls())
#synthetic_data_set=readRDS("complete_sample_set.RDS")
#synthetic_data_set=subset(synthetic_data_set,synthetic_data_set$tract==554403)
synthetic_data_set=readRDS("tract554403toworkwith.RDS")
synthetic_data_set$state=48

library(Hmisc)
#load following year information
Census_data_following_year=readRDS("Census_data_2015.RDS")

#Add useful rows to Census_data_following_year
Census_data_following_year$Under_5=rowSums(Census_data_following_year[c("black.boys.under.5","black.girls.under.5","amer.indian.alaskan.boys.under.5","amer.indian.alaskan.girls.under.5","asian.boys.under.5","asian.girls.under.5","islander.boys.under.5","islander.girls.under.5","other.race.boys.under.5","other.race.girls.under.5","multiracial.boys.under.5","multiracial.girls.under.5","white.boys.under.5","white.girls.under.5","hispanic.boys.under.5","hispanic.girls.under.5")])
Census_data_following_year$'5_to_9'=rowSums(Census_data_following_year[c("black.boys.5.to.9","black.girls.5.to.9","amer.indian.alaskan.boys.5.to.9","amer.indian.alaskan.girls.5.to.9","asian.boys.5.to.9","asian.girls.5.to.9","islander.boys.5.to.9","islander.girls.5.to.9","other.race.boys.5.to.9","other.race.girls.5.to.9","multiracial.boys.5.to.9","multiracial.girls.5.to.9","white.boys.5.to.9","white.girls.5.to.9","hispanic.boys.5.to.9","hispanic.girls.5.to.9")])
Census_data_following_year$'10_to_14'=rowSums(Census_data_following_year[c("black.boys.10.to.14","black.girls.10.to.14","amer.indian.alaskan.boys.10.to.14","amer.indian.alaskan.girls.10.to.14","asian.boys.10.to.14","asian.girls.10.to.14","islander.boys.10.to.14","islander.girls.10.to.14","other.race.boys.10.to.14","other.race.girls.10.to.14","multiracial.boys.10.to.14","multiracial.girls.10.to.14","white.boys.10.to.14","white.girls.10.to.14","hispanic.boys.10.to.14","hispanic.girls.10.to.14")])
Census_data_following_year$'15_to_17'=rowSums(Census_data_following_year[c("black.boys.15.to.17","black.girls.15.to.17","amer.indian.alaskan.boys.15.to.17","amer.indian.alaskan.girls.15.to.17","asian.boys.15.to.17","asian.girls.15.to.17","islander.boys.15.to.17","islander.girls.15.to.17","other.race.boys.15.to.17","other.race.girls.15.to.17","multiracial.boys.15.to.17","multiracial.girls.15.to.17","white.boys.15.to.17","white.girls.15.to.17","hispanic.boys.15.to.17","hispanic.girls.15.to.17")])
Census_data_following_year$'18_to_19'=rowSums(Census_data_following_year[c("black.men.18.to.19","black.women.18.to.19","amer.indian.alaskan.men.18.to.19","amer.indian.alaskan.women.18.to.19","asian.men.18.to.19","asian.women.18.to.19","islander.men.18.to.19","islander.women.18.to.19","other.race.men.18.to.19","other.race.women.18.to.19","multiracial.men.18.to.19","multiracial.women.18.to.19","white.men.18.to.19","white.women.18.to.19","hispanic.men.18.to.19","hispanic.women.18.to.19")])
Census_data_following_year$'20_to_24'=rowSums(Census_data_following_year[c("black.men.20.to.24","black.women.20.to.24","amer.indian.alaskan.men.20.to.24","amer.indian.alaskan.women.20.to.24","asian.men.20.to.24","asian.women.20.to.24","islander.men.20.to.24","islander.women.20.to.24","other.race.men.20.to.24","other.race.women.20.to.24","multiracial.men.20.to.24","multiracial.women.20.to.24","white.men.20.to.24","white.women.20.to.24","hispanic.men.20.to.24","hispanic.women.20.to.24")])
Census_data_following_year$'25_to_29'=rowSums(Census_data_following_year[c("black.men.25.to.29","black.women.25.to.29","amer.indian.alaskan.men.25.to.29","amer.indian.alaskan.women.25.to.29","asian.men.25.to.29","asian.women.25.to.29","islander.men.25.to.29","islander.women.25.to.29","other.race.men.25.to.29","other.race.women.25.to.29","multiracial.men.25.to.29","multiracial.women.25.to.29","white.men.25.to.29","white.women.25.to.29","hispanic.men.25.to.29","hispanic.women.25.to.29")])
Census_data_following_year$'30_to_34'=rowSums(Census_data_following_year[c("black.men.30.to.34","black.women.30.to.34","amer.indian.alaskan.men.30.to.34","amer.indian.alaskan.women.30.to.34","asian.men.30.to.34","asian.women.30.to.34","islander.men.30.to.34","islander.women.30.to.34","other.race.men.30.to.34","other.race.women.30.to.34","multiracial.men.30.to.34","multiracial.women.30.to.34","white.men.30.to.34","white.women.30.to.34","hispanic.men.30.to.34","hispanic.women.30.to.34")])
Census_data_following_year$'35_to_44'=rowSums(Census_data_following_year[c("black.men.35.to.44","black.women.35.to.44","amer.indian.alaskan.men.35.to.44","amer.indian.alaskan.women.35.to.44","asian.men.35.to.44","asian.women.35.to.44","islander.men.35.to.44","islander.women.35.to.44","other.race.men.35.to.44","other.race.women.35.to.44","multiracial.men.35.to.44","multiracial.women.35.to.44","white.men.35.to.44","white.women.35.to.44","hispanic.men.35.to.44","hispanic.women.35.to.44")])
Census_data_following_year$'45_to_54'=rowSums(Census_data_following_year[c("black.men.45.to.54","black.women.45.to.54","amer.indian.alaskan.men.45.to.54","amer.indian.alaskan.women.45.to.54","asian.men.45.to.54","asian.women.45.to.54","islander.men.45.to.54","islander.women.45.to.54","other.race.men.45.to.54","other.race.women.45.to.54","multiracial.men.45.to.54","multiracial.women.45.to.54","white.men.45.to.54","white.women.45.to.54","hispanic.men.45.to.54","hispanic.women.45.to.54")])
Census_data_following_year$'55_to_64'=rowSums(Census_data_following_year[c("black.men.55.to.64","black.women.55.to.64","amer.indian.alaskan.men.55.to.64","amer.indian.alaskan.women.55.to.64","asian.men.55.to.64","asian.women.55.to.64","islander.men.55.to.64","islander.women.55.to.64","other.race.men.55.to.64","other.race.women.55.to.64","multiracial.men.55.to.64","multiracial.women.55.to.64","white.men.55.to.64","white.women.55.to.64","hispanic.men.55.to.64","hispanic.women.55.to.64")])
Census_data_following_year$'65_to_74'=rowSums(Census_data_following_year[c("black.men.65.to.74","black.women.65.to.74","amer.indian.alaskan.men.65.to.74","amer.indian.alaskan.women.65.to.74","asian.men.65.to.74","asian.women.65.to.74","islander.men.65.to.74","islander.women.65.to.74","other.race.men.65.to.74","other.race.women.65.to.74","multiracial.men.65.to.74","multiracial.women.65.to.74","white.men.65.to.74","white.women.65.to.74","hispanic.men.65.to.74","hispanic.women.65.to.74")])
Census_data_following_year$'75_to_84'=rowSums(Census_data_following_year[c("black.men.75.to.84","black.women.75.to.84","amer.indian.alaskan.men.75.to.84","amer.indian.alaskan.women.75.to.84","asian.men.75.to.84","asian.women.75.to.84","islander.men.75.to.84","islander.women.75.to.84","other.race.men.75.to.84","other.race.women.75.to.84","multiracial.men.75.to.84","multiracial.women.75.to.84","white.men.75.to.84","white.women.75.to.84","hispanic.men.75.to.84","hispanic.women.75.to.84")])
Census_data_following_year$'Over_85'=rowSums(Census_data_following_year[c("black.men.over.85","black.women.over.85","amer.indian.alaskan.men.over.85","amer.indian.alaskan.women.over.85","asian.men.over.85","asian.women.over.85","islander.men.over.85","islander.women.over.85","other.race.men.over.85","other.race.women.over.85","multiracial.men.over.85","multiracial.women.over.85","white.men.over.85","white.women.over.85","hispanic.men.over.85","hispanic.women.over.85")])

Census_data_following_year$new_people_Under_5=rowSums(Census_data_following_year[c("moved.within.county.under.5","moved.within.state.under.5","out.of.state.under.5","out.of.nation.under.5")])
Census_data_following_year$new_people_5_to_17=rowSums(Census_data_following_year[c("moved.within.county.5.to.17","moved.within.state.5.to.17","out.of.state.5.to.17","out.of.nation.5.to.17")])
Census_data_following_year$new_people_18_to_19=rowSums(Census_data_following_year[c("moved.within.county.18.to.19","moved.within.state.18.to.19","out.of.state.18.to.19","out.of.nation.18.to.19")])
Census_data_following_year$new_people_20_to_24=rowSums(Census_data_following_year[c("moved.within.county.20.to.24","moved.within.state.20.to.24","out.of.state.20.to.24","out.of.nation.20.to.24")])
Census_data_following_year$new_people_25_to_29=rowSums(Census_data_following_year[c("moved.within.county.25.to.29","moved.within.state.25.to.29","out.of.state.25.to.29","out.of.nation.25.to.29")])
Census_data_following_year$new_people_30_to_34=rowSums(Census_data_following_year[c("moved.within.county.30.to.34","moved.within.state.30.to.34","out.of.state.30.to.34","out.of.nation.30.to.34")])
Census_data_following_year$new_people_35_to_39=rowSums(Census_data_following_year[c("moved.within.county.35.to.39","moved.within.state.35.to.39","out.of.state.35.to.39","out.of.nation.35.to.39")])
Census_data_following_year$new_people_40_to_44=rowSums(Census_data_following_year[c("moved.within.county.40.to.44","moved.within.state.40.to.44","out.of.state.40.to.44","out.of.nation.40.to.44")])
Census_data_following_year$new_people_45_to_49=rowSums(Census_data_following_year[c("moved.within.county.45.to.49","moved.within.state.45.to.49","out.of.state.45.to.49","out.of.nation.45.to.49")])
Census_data_following_year$new_people_50_to_54=rowSums(Census_data_following_year[c("moved.within.county.50.to.54","moved.within.state.50.to.54","out.of.state.50.to.54","out.of.nation.50.to.54")])
Census_data_following_year$new_people_55_to_59=rowSums(Census_data_following_year[c("moved.within.county.55.to.59","moved.within.state.55.to.59","out.of.state.55.to.59","out.of.nation.55.to.59")])
Census_data_following_year$new_people_60_to_64=rowSums(Census_data_following_year[c("moved.within.county.60.to.64","moved.within.state.60.to.64","out.of.state.60.to.64","out.of.nation.60.to.64")])
Census_data_following_year$new_people_65_to_69=rowSums(Census_data_following_year[c("moved.within.county.65.to.69","moved.within.state.65.to.69","out.of.state.65.to.69","out.of.nation.65.to.69")])
Census_data_following_year$new_people_70_to_74=rowSums(Census_data_following_year[c("moved.within.county.70.to.74","moved.within.state.70.to.74","out.of.state.70.to.74","out.of.nation.70.to.74")])
Census_data_following_year$new_people_over_75=rowSums(Census_data_following_year[c("moved.within.county.over.75","moved.within.state.over.75","out.of.state.over.75","out.of.nation.over.75")])

state=48
county=201
tract=554403

#http://stla.github.io/stlapblog/posts/Numextract.html for function below to make my life easier
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

ageoverpeopleandbeginsimulatingmovinginpeople<-function(state,county,tract,synthetic_data_set,seed,Census_data_following_year){
  
  #subset part of synthetic data set to work with
  synthetic_data_set=subset(synthetic_data_set,(synthetic_data_set$state==state&synthetic_data_set$county==county&synthetic_data_set$tract==tract))
  
  #create table of existing ages in synthetic dataset
  old_ages_table=ftable(synthetic_data_set$age)
  old_ages_dataframe=as.data.frame.matrix(old_ages_table)
  colnames(old_ages_dataframe)=gsub(" ","_",unlist(attr(old_ages_table, "col.vars")))
  
  #Create table of next years ages
  Census_data_following_year=Census_data_following_year[(Census_data_following_year$state==state & Census_data_following_year$county==county & Census_data_following_year$tract==tract),]
  
  #Add variable for new age
  synthetic_data_set$new_age=rep(NA,nrow(synthetic_data_set))
  
  #Keep old as is for now
  synthetic_data_set$new_age[synthetic_data_set$age=="Over 85"] <- "Over 85"
  synthetic_data_set$new_age[synthetic_data_set$age=="75 to 84"] <- "75 to 84"
  
  #Start looking at differences and begin aging
  differences=Census_data_following_year[c("state","county","tract")]
  differences$over_75=Census_data_following_year$same.house.over.75-old_ages_dataframe$Over_85-old_ages_dataframe$`75_to_84`
  #make indexes of people that can age up
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="65 to 74")
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$over_75 > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$over_75){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$over_75)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "75 to 84"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "65 to 74"
      #update value for difference
      differences$over_75_aged <- differences$over_75
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$over_75){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "75 to 84"
      #update value for difference
      differences$over_75_aged=length(indexes_of_potential_age_ups)
    }
  }
  if(differences$over_75 < 0){
    differences$over_75_aged=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"65 to 74"
  }
  
  #Next Age Set
  
  differences$`65_to_74`=Census_data_following_year$same.house.70.to.74+Census_data_following_year$same.house.65.to.69+differences$over_75_aged-old_ages_dataframe$`65_to_74`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="55 to 64")
  if(differences$`65_to_74` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`65_to_74`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`65_to_74`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "65 to 74"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "55 to 64"
      #update value for difference
      differences$`65_to_74_aged` <- differences$`65_to_74`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`65_to_74`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "65 to 74"
      #update value for difference
      differences$`65_to_74_aged` <- differences$`65_to_74`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`65_to_74` < 0){
    differences$`65_to_74_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"55 to 64"
  } 
  
  #Next Age Set
  
  differences$`55_to_64`=Census_data_following_year$same.house.60.to.64+Census_data_following_year$same.house.55.to.59+differences$`65_to_74_aged`-old_ages_dataframe$`55_to_64`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="45 to 54")
  if(differences$`55_to_64` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`55_to_64`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`55_to_64`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "55 to 64"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "45 to 54"
      #update value for difference
      differences$`55_to_64_aged` <- differences$`55_to_64`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`55_to_64`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "55 to 64"
      #update value for difference
      differences$`55_to_64_aged` <- differences$`55_to_64`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`55_to_64` < 0){
    differences$`55_to_64_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"45 to 54"
  }
  
  #Next Age Set
  
  differences$`45_to_54`=Census_data_following_year$same.house.50.to.54+Census_data_following_year$same.house.45.to.49+differences$`55_to_64_aged`-old_ages_dataframe$`45_to_54`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="35 to 44")
  if(differences$`45_to_54` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`45_to_54`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`45_to_54`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "45 to 54"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "35 to 44"
      #update value for difference
      differences$`45_to_54_aged` <- differences$`45_to_54`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`45_to_54`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "45 to 54"
      #update value for difference
      differences$`45_to_54_aged` <- differences$`45_to_54`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`45_to_54` < 0){
    differences$`45_to_54_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"35 to 44"
  }
  
  #Next Age Set
  
  differences$`35_to_44`=Census_data_following_year$same.house.40.to.44+Census_data_following_year$same.house.35.to.39+differences$`45_to_54_aged`-old_ages_dataframe$`35_to_44`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="30 to 34")
  if(differences$`35_to_44` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`35_to_44`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`35_to_44`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "35 to 44"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "30 to 34"
      #update value for difference
      differences$`35_to_44_aged` <- differences$`35_to_44`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`35_to_44`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "35 to 44"
      #update value for difference
      differences$`35_to_44_aged` <- differences$`35_to_44`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`35_to_44` < 0){
    differences$`35_to_44_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"30 to 34"
  }
  
  
  #Next Age Set
  
  differences$`30_to_34`=Census_data_following_year$same.house.30.to.34+differences$`35_to_44_aged`-old_ages_dataframe$`30_to_34`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="25 to 29")
  if(differences$`30_to_34`> 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`30_to_34`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`30_to_34`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "30 to 34"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "25 to 29"
      #update value for difference
      differences$`30_to_34_aged` <- differences$`30_to_34`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`30_to_34`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "30 to 34"
      #update value for difference
      differences$`30_to_34_aged` <- differences$`30_to_34`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`30_to_34` < 0){
    differences$`30_to_34_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"25 to 29"
  }
  
  
  #Next Age Set
  
  differences$`25_to_29`=Census_data_following_year$same.house.25.to.29+differences$`30_to_34_aged`-old_ages_dataframe$`25_to_29`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="20 to 24")
  if(differences$`25_to_29` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`25_to_29`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`25_to_29`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "25 to 29"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "20 to 24"
      #update value for difference
      differences$`25_to_29_aged` <- differences$`25_to_29`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`25_to_29`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "25 to 29"
      #update value for difference
      differences$`25_to_29_aged` <- differences$`25_to_29`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`25_to_29` < 0){
    differences$`25_to_29_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"20 to 24"
  }  
  
  #Next Age Set
  
  differences$`20_to_24`=Census_data_following_year$same.house.20.to.24+differences$`25_to_29_aged`-old_ages_dataframe$`20_to_24`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="18 to 19")
  if(differences$`20_to_24` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`20_to_24`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`20_to_24`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "20 to 24"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "18 to 19"
      #update value for difference
      differences$`20_to_24_aged` <- differences$`20_to_24`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`20_to_24`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "20 to 24"
      #update value for difference
      differences$`20_to_24_aged` <- differences$`20_to_24`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`20_to_24` < 0){
    differences$`20_to_24_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"18 to 19"
  }
  
  
  
  #Next Age Set
  
  differences$`18_to_19`=Census_data_following_year$same.house.18.to.19+differences$`20_to_24_aged`-old_ages_dataframe$`18_to_19`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="15 to 17")
  if(differences$`18_to_19` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`18_to_19`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`18_to_19`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "18 to 19"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "15 to 17"
      #update value for difference
      differences$`18_to_19_aged` <- differences$`18_to_19`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`20_to_24`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "20 to 24"
      #update value for difference
      differences$`18_to_19_aged` <- differences$`18_to_19`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`18_to_19` < 0){
    differences$`18_to_19_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"15 to 17"
  }
  
  ##Dealing with sub age categories for now
  sub_category_ages <- which(synthetic_data_set$age=="10 to 14")
  synthetic_data_set$new_age[sub_category_ages]<-"10 to 14"
  sub_category_ages <- which(synthetic_data_set$age=="5 to 9")
  synthetic_data_set$new_age[sub_category_ages]<-"5 to 9"
  
  #Next Age Set
  
  differences$`5_to_17`=Census_data_following_year$same.house.5.to.17+differences$`18_to_19_aged`-old_ages_dataframe$`15_to_17`-old_ages_dataframe$`10_to_14`-old_ages_dataframe$`5_to_9`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  #make indexes
  indexes_of_potential_age_ups <- which(synthetic_data_set$age=="Under 5")
  if(differences$`5_to_17` > 0){
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`5_to_17`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`5_to_17`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "5 to 9"
      #everyone else can stay the same age
      synthetic_data_set$new_age[setdiff(indexes_of_potential_age_ups,people_to_age)] <- "Under 5"
      #update value for difference
      differences$`5_to_17_aged` <- differences$`5_to_17`
    }
    #if not age up everyone and update the difference
    if(length(indexes_of_potential_age_ups)<differences$`5_to_17`){
      #make everyone older
      synthetic_data_set$new_age[indexes_of_potential_age_ups] <- "5 to 9"
      #update value for difference
      differences$`5_to_17_aged` <- differences$`5_to_17`-length(indexes_of_potential_age_ups)
    }
  }
  if(differences$`5_to_17` < 0){
    differences$`5_to_17_aged`=0
    synthetic_data_set$new_age[indexes_of_potential_age_ups]<-"Under 5"
  }
  
  #Babbies!!!!!!!!!
  
  differences$`under_5`=Census_data_following_year$Under_5-Census_data_following_year$Under_5+differences$`5_to_17_aged`-old_ages_dataframe$`Under_5`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`under_5` > 0){
    number_of_babies=differences$'under_5'
    
    #figure out what race most should be
    #what race are our current babies
    current_babies=subset(synthetic_data_set,synthetic_data_set$new_age=="Under 5")
    current_babies$race_sex=paste0(current_babies$race,current_babies$sex)
    current_babies_race_sex=ftable(synthetic_data_set$race_sex)
    current_babies_race_sex_dataframe=as.data.frame.matrix(current_babies_race_sex)
    colnames(current_babies_race_sex_dataframe)=gsub("Male",".boys.under.5",unlist(attr(current_babies_race_sex, "col.vars")))
    colnames(current_babies_race_sex_dataframe)=gsub("Female",".girls.under.5",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("Black or African American","black",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("American Indian or Alaskan Native","amer.indian.alaskan",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("Asian","asian",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("Native Hawaiian or Other Pacific Islander","islander",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("Some Other Race","other",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("Two or More Races","multiracial",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("White","white",colnames(current_babies_race_sex_dataframe))
    colnames(current_babies_race_sex_dataframe)=gsub("Hispanic or Latino","hispanic",colnames(current_babies_race_sex_dataframe))
    colnames_needed=c("black.boys.under.5","black.girls.under.5","amer.indian.alaskan.boys.under.5","amer.indian.alaskan.girls.under.5","asian.boys.under.5","asian.girls.under.5","islander.boys.under.5","islander.girls.under.5","other.race.boys.under.5","other.race.girls.under.5","multiracial.boys.under.5","multiracial.girls.under.5","white.boys.under.5","white.girls.under.5","hispanic.boys.under.5","hispanic.girls.under.5")
    
    for (col in colnames_needed){
      if(!(paste0(col) %in% colnames(current_babies_race_sex_dataframe))){
        current_babies_race_sex_dataframe[(paste0(col))]=0
      }
    }
    
    #Finally now we can make a probability vector to sample babies race and find appropriate households for them to be born in
    baby_probability_vector=Census_data_following_year[colnames_needed]-current_babies_race_sex_dataframe[colnames_needed]
    #make negative numbers 0
    baby_probability_vector[baby_probability_vector<0]<-0
    
    #And finally make babies
    babies=data.frame()
    for(baby in 1:number_of_babies){
      baby=sample(colnames(baby_probability_vector),1,prob=baby_probability_vector)
      
      if(baby %in% c("black.boys.under.5","black.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$race=="Black or African American" & synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="Black or African American",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
      
      if(baby %in% c("amer.indian.alaskan.boys.under.5","amer.indian,alaskan.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$race=="American Indian or Alaskan Native" & synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="American Indian or Alaskan Native",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
      if(baby %in% c("asian.boys.under.5","asian.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$race=="Asian" & synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="Asian",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
      if(baby %in% c("islander.boys.under.5","islander.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$race=="Native Hawaiian or Other Pacific Islander" & synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="Some Other Race",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
      if(baby %in% c("other.boys.under.5","other.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$race=="Native Hawaiian or Other Pacific Islander" & synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="Some Other Race",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
      if(baby %in% c("white.boys.under.5","white.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$race=="White" & synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="White",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
      if(baby %in% c("hispanic.boys.under.5","hispanic.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$race=="Hispanic or Latino" & synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="Hispanic or Latino",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
      if(baby %in% c("multiracial.under.5","multiracial.girls.under.5")){
        find_women=subset(synthetic_data_set,(synthetic_data_set$sex=="Female" & !synthetic_data_set$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & synthetic_data_set$household.type!="Group Quarters"))
        babys_moms_number=sample(1:nrow(find_women),1)
        babys_household_id=find_women$householdID[babys_moms_number]
        #update household
        synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]<-synthetic_data_set$size[synthetic_data_set$householdID==paste0(babys_household_id)]+1
        #create row for baby
        babys_sex=ifelse(grepl("boys",baby),"Male",
                         ifelse(grepl("girls",baby),"Female",
                                "Something Went Wrong"))
        
        babys_row=data.frame(householdID=paste0(babys_household_id),race="Two or More Races",sex=babys_sex,member="Child")
        babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","size","number.of.vehicles","county","state","tract","household.income","health.insurance")]
        babys_row=cbind(babys_row,babys_household_variables)
        babies=rbind(babies,babys_row)
      }
    }
  }
  
  #synthetic_data_set$new_age[(is.na(synthetic_data_set$new_age))]=synthetic_data_set$age[(is.na(synthetic_data_set$new_age))]
  
  
  
  
  
  
  
  
  
  
  
  #Time to start sorting houeholds
  
  #make a variable in synthetic dataset to more easily table to work with Census data
  make_easier=function(household.type,size){
    type=ifelse(household.type=="Non-family","nonfamily",
                ifelse(household.type=="Alone","nonfamily",
                ifelse(household.type=="Group Quarters","group.quarters.population","family")))
    easier_to_table=paste0(type,".",size,".person.household")
    return(easier_to_table)
  }
  synthetic_data_set$household_and_size=mapply(make_easier,synthetic_data_set$household.type,synthetic_data_set$size)
  #create table of household sizes and family or not family in synthetic dataset
  old_households_table=ftable(synthetic_data_set$household_and_size)
  old_households_dataframe=as.data.frame.matrix(old_households_table)
  colnames(old_households_dataframe)=unlist(attr(old_households_table, "col.vars"))
  sizes=as.numeric(numextract(colnames(old_households_dataframe)))
  sizes[is.na(sizes)]=1
  old_households_dataframe=old_households_dataframe/sizes
  
  colnames_needed=c(paste0("family.",2:7,".person.household"),paste0("nonfamily.",1:7,".person.household"),"group.quarters.population")
  
  for (col in colnames_needed){
    if(!(paste0(col) %in% colnames(old_households_dataframe))){
      old_households_dataframe[(paste0(col))]=0
    }
  }
  
  Census_households=Census_data_following_year[c(paste0("family.",2:7,".person.household"),paste0("nonfamily.",1:7,".person.household"),"group.quarters.population")]
  households=c("nonfamily.7.person.household","family.7.person.household","nonfamily.6.person.household","family.6.person.household","nonfamily.5.person.household","family.5.person.household","nonfamily.4.person.household","family.4.person.household","nonfamily.3.person.household","family.3.person.household","nonfamily.2.person.household","family.2.person.household")
  
  differences_in_households=Census_households[households]-old_households_dataframe[households]
  
  #Remove or shrink households
  #starting with bigger households and moving down
  synthetic_data_set$left_population=NA
  
  for(hh in households){
    
    if(differences_in_households[paste0(hh)]<0){
      #Try to remove a household first because it's harder
      eligible_households=subset(synthetic_data_set,synthetic_data_set$household_and_size==hh)
      hhIDs=unique(eligible_households$householdID)
      
      for (hhID in hhIDs){
        #break as soon as we've removed enough households
        if(differences_in_households[paste0(hh)]==0){
          break
        }
        
        #see if houses have ages eligible to be removed from population
        a_household=subset(eligible_households,eligible_households$householdID==paste0(hhID))
        eligible_ages_to_move=colnames(differences[, colSums(differences < 0) > 0]) 
        eligible_ages_to_move=gsub("_"," ",eligible_ages_to_move)
        eligible_ages_to_move=gsub("under","Under",eligible_ages_to_move)
        eligible_ages_to_move=gsub("over","Over",eligible_ages_to_move)
        
        if( all(a_household$new_age %in% eligible_ages_to_move) ){
          #that household leaves
          synthetic_data_set$left_population[synthetic_data_set$householdID==a_household$householdID[1]]="left population"
          #eligible ages has to be updated
          for(update in unique(a_household$new_age)){
            update_by=sum(a_household$new_age==update)
            update_name=gsub(" ","_",update)
            update_name=gsub("Under","under",update_name)
            update_name=gsub("Over","over",update_name)
            differences[paste0(update_name)]=differences[paste0(update_name)]+update_by
          }
          #As well as number of households
          differences_in_households[paste0(hh)]=differences_in_households[paste0(hh)]+1
        }
      }
      
      #If we can't remove enough households remove individual people from households so households are smaller
      for (hhID in hhIDs){
        #break as soon as we've removed enough
        if(differences_in_households[paste0(hh)]==0){
          break
        }
        
        #see if houses have ages eligible to be removed from population
        a_household=subset(eligible_households,eligible_households$householdID==paste0(hhID))
        eligible_ages_to_move=colnames(differences[, colSums(differences < 0) > 0]) 
        eligible_ages_to_move=gsub("_"," ",eligible_ages_to_move)
        eligible_ages_to_move=gsub("under","Under",eligible_ages_to_move)
        eligible_ages_to_move=gsub("over","Over",eligible_ages_to_move)
        
        if( any(a_household$new_age %in% eligible_ages_to_move) ){
          #someone should leave
          who_can_leave=subset(a_household,a_household$new_age %in% eligible_ages_to_move)
          who_is_moving=sample(1:nrow(who_can_leave),1)
          who_is_moving=who_can_leave[who_is_moving,]
          synthetic_data_set$left_population[row.match(who_is_moving,synthetic_data_set)]="left population"
          #eligible ages has to be updated
          update_name=who_is_moving$new_age
          update_name=gsub(" ","_",update_name)
          update_name=gsub("Under","under",update_name)
          update_name=gsub("Over","over",update_name)
          differences[paste0(update_name)]=differences[paste0(update_name)]+1
          #As well as number of households
          differences_in_households[paste0(hh)]=differences_in_households[paste0(hh)]+1
          hh_number=as.numeric(numextract(paste0(hh)))
          other_household=gsub(paste0(hh_number),paste0(hh_number+1),paste0(hh))
          differences_in_households[paste0(other_household)]=differences_in_households[paste0(other_household)]-1
          
        }
      }
    }
  }
  
  #Begin moving people in
  
  


}    
