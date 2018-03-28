
#Save data for two years
source("getcensusdata.R")
saveRDS(census_data_API(),"Census_data_2014.RDS")
saveRDS(census_data_API(base_url='http://api.census.gov/data/2015/acs5?'),"Census_data_2015.RDS")

#Clear environment
#Going to load part of the big dataset, subset a tract and lose the rest
rm(list=ls())
synthetic_data_set=readRDS("complete_sample_set.RDS")
synthetic_data_set=subset(synthetic_data_set,synthetic_data_set$tract==554403)

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


ageoverpeopleandbeginsimulatingmovinginpeople<-function(state,county,tract,synthetic_data_set,seed,Census_data_following_year){
  
  #subset part of synthetic data set to work with
  synthetic_data_set=subset(synthetic_data_set,synthetic_data_set$state==state,synthetic_data_set$county==county,synthetic_data_set$tract==tract)
  
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
  differences$over_75=Census_data_following_year$Over_85+Census_data_following_year$`75_to_84`-Census_data_following_year$new_people_65_to_69-Census_data_following_year$new_people_70_to_74-old_ages_dataframe$Over_85-old_ages_dataframe$`75_to_84`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$over_75 > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="65 to 74")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$over_75){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$over_75)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "75 to 84"
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
  }
  
  #Next Age Set
  
  differences$`65_to_74`=Census_data_following_year$`65_to_74`-Census_data_following_year$new_people_70_to_74-Census_data_following_year$new_people_65_to_69+differences$over_75_aged-old_ages_dataframe$`65_to_74`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`65_to_74` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="55 to 64")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`65_to_74`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`65_to_74`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "65 to 74"
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
  } 
  
  #Next Age Set
  
  differences$`55_to_64`=Census_data_following_year$`55_to_64`-Census_data_following_year$new_people_60_to_64-Census_data_following_year$new_people_55_to_59+differences$`65_to_74_aged`-old_ages_dataframe$`55_to_64`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`55_to_64` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="45 to 54")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`55_to_64`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`55_to_64`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "55 to 64"
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
  }
  
  #Next Age Set
  
  differences$`45_to_54`=Census_data_following_year$`45_to_54`-Census_data_following_year$new_people_50_to_54-Census_data_following_year$new_people_45_to_49+differences$`55_to_64_aged`-old_ages_dataframe$`45_to_54`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`45_to_54` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="35 to 44")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`45_to_54`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`45_to_54`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "45 to 54"
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
  }
  
  #Next Age Set
  
  differences$`35_to_44`=Census_data_following_year$`35_to_44`-Census_data_following_year$new_people_40_to_44-Census_data_following_year$new_people_35_to_39+differences$`45_to_54_aged`-old_ages_dataframe$`35_to_44`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`35_to_44` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="30 to 34")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`35_to_44`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`35_to_44`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "35 to 44"
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
  }
  
  
  #Next Age Set
  
  differences$`30_to_34`=Census_data_following_year$`30_to_34`-Census_data_following_year$new_people_30_to_34+differences$`35_to_44_aged`-old_ages_dataframe$`30_to_34`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`30_to_34`> 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="25 to 29")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`30_to_34`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`30_to_34`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "30 to 34"
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
  }
  
  
  #Next Age Set
  
  differences$`25_to_29`=Census_data_following_year$`25_to_29`-Census_data_following_year$new_people_25_to_29+differences$`30_to_34_aged`-old_ages_dataframe$`25_to_29`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`25_to_29` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="20 to 24")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`25_to_29`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`25_to_29`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "25 to 29"
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
  }  
  
  #Next Age Set
  
  differences$`20_to_24`=Census_data_following_year$`20_to_24`-Census_data_following_year$new_people_20_to_24+differences$`25_to_29_aged`-old_ages_dataframe$`20_to_24`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`20_to_24` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="18 to 19")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`20_to_24`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`20_to_24`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "20 to 24"
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
  }
  
  
  
  #Next Age Set
  
  differences$`18_to_19`=Census_data_following_year$`18_to_19`-Census_data_following_year$new_people_18_to_19+differences$`20_to_24_aged`-old_ages_dataframe$`18_to_19`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`18_to_19` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="15 to 17")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`18_to_19`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`18_to_19`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "18 to 19"
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
  } 
  
  #Next Age Set
  
  differences$`5_to_17`=Census_data_following_year$`15_to_17`+Census_data_following_year$`10_to_14`+Census_data_following_year$`5_to_9`-Census_data_following_year$new_people_5_to_17+differences$`18_to_19_aged`-old_ages_dataframe$`15_to_17`-old_ages_dataframe$`10_to_14`-old_ages_dataframe$`5_to_9`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`5_to_17` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="Under 5")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`5_to_17`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`5_to_17`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "5 to 9"
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
  }
  
  #Babbies!!!!!!!!!
  
  differences$`under_5`=Census_data_following_year$Under_5-Census_data_following_year$Under_5+differences$`5_to_17_aged`-old_ages_dataframe$`Under_5`
  #if this number is positive age people from the age bracket below, if the number is negative leave alone for now
  if(differences$`under_5` > 0){
    #make indexes
    indexes_of_potential_age_ups <- which(synthetic_data_set$age=="Under 5")
    #make sure there are enough people to age up
    if(length(indexes_of_potential_age_ups)>differences$`5_to_17`){
      #sample indexes
      people_to_age=sample(indexes_of_potential_age_ups,differences$`5_to_17`)
      #make those people older
      synthetic_data_set$new_age[people_to_age] <- "5 to 9"
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