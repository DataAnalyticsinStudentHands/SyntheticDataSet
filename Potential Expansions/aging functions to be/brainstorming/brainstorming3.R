current_data_for_update=readRDS("Census_data_2015.RDS")

county201=subset(current_data_for_update,current_data_for_update$county=="201")

current_data_for_update=subset(current_data_for_update,current_data_for_update$tract=="555702")


#Add babies in people already living in the same house
#make sure size works 
people_still_living_in_the_tract$size=as.numeric(as.character(people_still_living_in_the_tract$size))
babies=data.frame()
#if this number is positive age people from the age bracket below, if the number is negative leave alone for now
if(differences$same.house.under.5 > 0 & sum(differences)>0){
  number_of_babies=differences$same.house.under.5
  
  #figure out what race most should be
  #what race are our current babies
  current_babies=subset(people_still_living_in_the_tract,people_still_living_in_the_tract$real_age<5)
  current_babies$race_sex=paste0(current_babies$race,current_babies$sex)
  current_babies_race_sex=ftable(current_babies$race_sex)
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
  baby_probability_vector=current_data_for_update[colnames_needed]-current_babies_race_sex_dataframe[colnames_needed]
  #make negative numbers 0
  baby_probability_vector[baby_probability_vector<0]<-0
  
  #And finally make babies
  for(baby in 1:number_of_babies){
    baby=sample(colnames(baby_probability_vector),1,prob=baby_probability_vector)
    
    if(baby %in% c("black.boys.under.5","black.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$race=="Black or African American" & people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7 & !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="Black or African American",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
    
    if(baby %in% c("amer.indian.alaskan.boys.under.5","amer.indian,alaskan.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$race=="American Indian or Alaskan Native" & people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7& !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="American Indian or Alaskan Native",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
    if(baby %in% c("asian.boys.under.5","asian.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$race=="Asian" & people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7& !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="Asian",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
    if(baby %in% c("islander.boys.under.5","islander.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$race=="Native Hawaiian or Other Pacific Islander" & people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7& !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="Some Other Race",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
    if(baby %in% c("other.boys.under.5","other.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$race=="Native Hawaiian or Other Pacific Islander" & people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7& !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="Some Other Race",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
    if(baby %in% c("white.boys.under.5","white.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$race=="White" & people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7& !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="White",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
    if(baby %in% c("hispanic.boys.under.5","hispanic.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$race=="Hispanic or Latino" & people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7& !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="Hispanic or Latino",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
    if(baby %in% c("multiracial.under.5","multiracial.girls.under.5")){
      find_women=subset(people_still_living_in_the_tract,(people_still_living_in_the_tract$sex=="Female" & !people_still_living_in_the_tract$age %in% c("under 5", "5 to 9", "10 to 14", "15 to 17") & people_still_living_in_the_tract$household.type!="Group Quarters" & people_still_living_in_the_tract$size!=7& !people_still_living_in_the_tract$householdID %in% babies$householdID))
      babys_moms_number=sample(1:nrow(find_women),1)
      babys_household_id=find_women$householdID[babys_moms_number]
      #update household
      people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]<-people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==paste0(babys_household_id)]+1
      #create row for baby
      babys_sex=ifelse(grepl("boys",baby),"Male",
                       ifelse(grepl("girls",baby),"Female",
                              "Something Went Wrong"))
      
      babys_row=data.frame(householdID=paste0(babys_household_id),race="Two or More Races",sex=babys_sex,member="Child",size=find_women$size[babys_moms_number]+1)
      babys_household_variables=find_women[babys_moms_number,c("ACCOUNT","household.type","number.of.vehicles","county","state","tract","household.income","health.insurance")]
      babys_row=cbind(babys_row,babys_household_variables)
      babies=rbind(babies,babys_row)
    }
  }
}
babies$age=1


