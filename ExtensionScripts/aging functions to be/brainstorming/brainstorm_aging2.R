parcels_2014=readRDS("2014 HCAD stuff/valid_parcels_for_simulation.RDS")
parcels_2015=readRDS("2015 HCAD stuff/valid_parcels_for_simulation.RDS")

parcels_2014$ACCOUNT=paste0(parcels_2014$ACCOUNT,"_",parcels_2014$BUILDING_NUMBER)
parcels_2015$ACCOUNT=paste0(parcels_2015$ACCOUNT,"_",parcels_2015$BUILDING_NUMBER)

buildings_no_longer_available=parcels_2014$ACCOUNT[which(!parcels_2014$ACCOUNT %in% parcels_2015$ACCOUNT)]
new_buildings=parcels_2015$ACCOUNT[which(!parcels_2015$ACCOUNT %in% parcels_2014$ACCOUNT)]

#Can I figure out changes in ownership?
old_owners=parcels_2014[c("ACCOUNT","CurrOwner")]
new_owners=parcels_2015[c("ACCOUNT","CurrOwner","BUILDING_STYLE_CODE")]
owners=merge(old_owners,new_owners,by="ACCOUNT")
owners$change_in_ownership=ifelse(as.character(owners$CurrOwner.x)==as.character(owners$CurrOwner.y),"Same Owner","Change in Ownership")
owners_changed=subset(owners,owners$change_in_ownership=="Change in Ownership")
#Really only care about residential places
owners_changed_residential=subset(owners_changed,owners_changed$BUILDING_STYLE_CODE %in% c("101","102","103","104","107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988"))

#Really only need account numbers and there's too much in memory
to_move_people_out=c(buildings_no_longer_available,owners_changed_residential$ACCOUNT)
rm(new_buildings,new_owners,parcels_2014,parcels_2015,old_owners,owners,owners_changed,owners_changed_residential)

#Load sample set
sample_set=readRDS("complete_sample_set2018-05-29.RDS")

#Make ages from age brackets
get_age_from_brackets<-function(seed,age_bracket){
  set.seed(seed)
  age=ifelse(age_bracket=="Under 5",sample(c(0:4),1),
             ifelse(age_bracket=="5 to 9",sample(c(5:9),1),
                    ifelse(age_bracket=="10 to 14",sample(c(10:14),1),
                           ifelse(age_bracket=="15 to 17",sample(c(15:17),1),
                                  ifelse(age_bracket=="18 to 19",sample(c(18:19),1),
                                         ifelse(age_bracket=="20 to 24",sample(c(20:24),1),
                                                ifelse(age_bracket=="25 to 29",sample(c(25:29),1),
                                                       ifelse(age_bracket=="30 to 34",sample(c(30:34),1),
                                                              ifelse(age_bracket=="35 to 44",sample(c(35:44),1),
                                                                     ifelse(age_bracket=="45 to 54",sample(c(45:54),1),
                                                                            ifelse(age_bracket=="55 to 64",sample(c(55:64),1),
                                                                                   ifelse(age_bracket=="65 to 74",sample(c(65:74),1),
                                                                                          ifelse(age_bracket=="75 to 84",sample(c(75:84),1),
                                                                                                 ifelse(age_bracket=="Over 85",sample(c(85:100),1),
                                                                                                        NA))))))))))))))
  return(age)
}

#Apply to set
sample_set$real_age=mapply(get_age_from_brackets,c(1:nrow(sample_set)),sample_set$age)

#Everyone got a year older
sample_set$real_age=sample_set$real_age+1

#Move people out
sample_set$Moved_Out=NA
sample_set$Moved_Out[sample_set$ACCOUNT %in% to_move_people_out]="Moved Out"

people_that_moved_out=subset(sample_set,sample_set$Moved_Out=="Moved Out")

#We're going to do this by tract
#subset a random tract to play with
a_tract_to_play_with=subset(sample_set,sample_set$tract=="555702")
#remove sample set for more memory
rm(sample_set)

#subset current people
people_still_living_in_the_tract=subset(a_tract_to_play_with,is.na(a_tract_to_play_with$Moved_Out))

#Read in current data
current_data_for_update=readRDS("Census_data_2015.RDS")
current_data_for_update=subset(current_data_for_update,current_data_for_update$county=="201"&current_data_for_update$tract=="555702")

#Are our numbers reasonablish
our_people=data.frame(county=201,tract=555702)
#our_people$under.5=sum(people_still_living_in_the_tract$real_age<5)#if this doesn't run something is wrong with how age was done so subset NAs in age
our_people$same.house.under.5=sum(people_still_living_in_the_tract$real_age<5,na.rm=TRUE)

our_people$same.house.5.to.17=sum(people_still_living_in_the_tract$real_age>=5&people_still_living_in_the_tract$real_age<=17,na.rm = TRUE)
our_people$same.house.18.to.19=sum(people_still_living_in_the_tract$real_age>=18&people_still_living_in_the_tract$real_age<=19,na.rm = TRUE)
our_people$same.house.20.to.24=sum(people_still_living_in_the_tract$real_age>=20&people_still_living_in_the_tract$real_age<=24,na.rm = TRUE)
our_people$same.house.25.to.29=sum(people_still_living_in_the_tract$real_age>=25&people_still_living_in_the_tract$real_age<=29,na.rm = TRUE)
our_people$same.house.30.to.34=sum(people_still_living_in_the_tract$real_age>=30&people_still_living_in_the_tract$real_age<=34,na.rm = TRUE)
our_people$same.house.35.to.39=sum(people_still_living_in_the_tract$real_age>=35&people_still_living_in_the_tract$real_age<=39,na.rm = TRUE)
our_people$same.house.40.to.44=sum(people_still_living_in_the_tract$real_age>=40&people_still_living_in_the_tract$real_age<=44,na.rm = TRUE)
our_people$same.house.45.to.49=sum(people_still_living_in_the_tract$real_age>=45&people_still_living_in_the_tract$real_age<=49,na.rm = TRUE)
our_people$same.house.50.to.54=sum(people_still_living_in_the_tract$real_age>=50&people_still_living_in_the_tract$real_age<=54,na.rm = TRUE)
our_people$same.house.55.to.59=sum(people_still_living_in_the_tract$real_age>=55&people_still_living_in_the_tract$real_age<=59,na.rm = TRUE)
our_people$same.house.60.to.64=sum(people_still_living_in_the_tract$real_age>=60&people_still_living_in_the_tract$real_age<=64,na.rm = TRUE)
our_people$same.house.65.to.69=sum(people_still_living_in_the_tract$real_age>=65&people_still_living_in_the_tract$real_age<=69,na.rm = TRUE)
our_people$same.house.70.to.74=sum(people_still_living_in_the_tract$real_age>=70&people_still_living_in_the_tract$real_age<=74,na.rm = TRUE)
our_people$same.house.over.75=sum(people_still_living_in_the_tract$real_age>=75,na.rm = TRUE)

variables_of_interest=c("same.house.under.5","same.house.5.to.17","same.house.18.to.19","same.house.20.to.24","same.house.25.to.29","same.house.30.to.34","same.house.35.to.39","same.house.40.to.44","same.house.45.to.49","same.house.50.to.54","same.house.55.to.59","same.house.60.to.64","same.house.65.to.69","same.house.70.to.74","same.house.over.75")

differences=current_data_for_update[variables_of_interest]-our_people[variables_of_interest]

#If we have too many people move people out until same amount of people
while(sum(differences)<0){
  if(differences$same.house.over.75>=0){
    indexes_potential_75_year_olds_to_move=which(people_still_living_in_the_tract$real_age>=75)
    #Move someone out
    who_moves_out=sample(indexes_potential_75_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.over.75=differences$same.house.over.75+1
  }
  if(differences$same.house.70.to.74>=0){
    indexes_potential_70_year_olds_to_move=which(people_still_living_in_the_tract$real_age<75&people_still_living_in_the_tract$real_age>=70)
    #Move someone out
    who_moves_out=sample(indexes_potential_70_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.70.to.74=differences$same.house.70.to.74+1
  }
  if(differences$same.house.65.to.69>=0){
    indexes_potential_65_year_olds_to_move=which(people_still_living_in_the_tract$real_age<70&people_still_living_in_the_tract$real_age>=65)
    #Move someone out
    who_moves_out=sample(indexes_potential_65_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.65.to.69=differences$same.house.65.to.69+1
  }
  if(differences$same.house.60.to.64>=0){
    indexes_potential_60_year_olds_to_move=which(people_still_living_in_the_tract$real_age<65&people_still_living_in_the_tract$real_age>=60)
    #Move someone out
    who_moves_out=sample(indexes_potential_60_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.60.to.64=differences$same.house.60.to.64+1
  }
  if(differences$same.house.55.to.59>=0){
    indexes_potential_55_year_olds_to_move=which(people_still_living_in_the_tract$real_age<60&people_still_living_in_the_tract$real_age>=55)
    #Move someone out
    who_moves_out=sample(indexes_potential_55_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.55.to.59=differences$same.house.55.to.59+1
  }
  if(differences$same.house.50.to.54>=0){
    indexes_potential_50_year_olds_to_move=which(people_still_living_in_the_tract$real_age<55&people_still_living_in_the_tract$real_age>=50)
    #Move someone out
    who_moves_out=sample(indexes_potential_50_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.50.to.54=differences$same.house.50.to.54+1
  }
  if(differences$same.house.45.to.49>=0){
    indexes_potential_45_year_olds_to_move=which(people_still_living_in_the_tract$real_age<50&people_still_living_in_the_tract$real_age>=45)
    #Move someone out
    who_moves_out=sample(indexes_potential_45_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.45.to.49=differences$same.house.45.to.49+1
  }
  if(differences$same.house.40.to.44>=0){
    indexes_potential_40_year_olds_to_move=which(people_still_living_in_the_tract$real_age<45&people_still_living_in_the_tract$real_age>=40)
    #Move someone out
    who_moves_out=sample(indexes_potential_40_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.40.to.44=differences$same.house.40.to.44+1
  }
  if(differences$same.house.35.to.39>=0){
    indexes_potential_35_year_olds_to_move=which(people_still_living_in_the_tract$real_age<40&people_still_living_in_the_tract$real_age>=35)
    #Move someone out
    who_moves_out=sample(indexes_potential_35_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.35.to.39=differences$same.house.35.to.39+1
  }
  if(differences$same.house.30.to.34>=0){
    indexes_potential_30_year_olds_to_move=which(people_still_living_in_the_tract$real_age<35&people_still_living_in_the_tract$real_age>=30)
    #Move someone out
    who_moves_out=sample(indexes_potential_30_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.30.to.34=differences$same.house.30.to.34+1
  }
  if(differences$same.house.25.to.29>=0){
    indexes_potential_25_year_olds_to_move=which(people_still_living_in_the_tract$real_age<30&people_still_living_in_the_tract$real_age>=25)
    #Move someone out
    who_moves_out=sample(indexes_potential_25_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.25.to.29=differences$same.house.25.to.29+1
  }
  if(differences$same.house.20.to.24>=0){
    indexes_potential_20_year_olds_to_move=which(people_still_living_in_the_tract$real_age<25&people_still_living_in_the_tract$real_age>=20)
    #Move someone out
    who_moves_out=sample(indexes_potential_20_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.20.to.24=differences$same.house.20.to.24+1
  }
  if(differences$same.house.18.to.19>=0){
    indexes_potential_18_year_olds_to_move=which(people_still_living_in_the_tract$real_age<20&people_still_living_in_the_tract$real_age>=18)
    #Move someone out
    who_moves_out=sample(indexes_potential_18_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.18.to.19=differences$same.house.18.to.19+1
  }
  if(differences$same.house.5.to.17>=0){
    indexes_potential_17_year_olds_to_move=which(people_still_living_in_the_tract$real_age<18&people_still_living_in_the_tract$real_age>=18)
    #Move someone out
    who_moves_out=sample(indexes_potential_17_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.5.to.17=differences$same.house.5.to.17+1
  }
  if(differences$same.house.under.5>=0){
    indexes_potential_5_year_olds_to_move=which(people_still_living_in_the_tract$real_age<=5)
    #Move someone out
    who_moves_out=sample(indexes_potential_5_year_olds_to_move,size=1)
    people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
    #update differences
    differences$same.house.under.5=differences$same.house.under.5+1
  }
  newly_moved_out=subset(people_still_living_in_the_tract,people_still_living_in_the_tract$Moved_Out=="Moved Out")
  people_that_moved_out=rbind(people_that_moved_out,newly_moved_out)
  people_still_living_in_the_tract=subset(people_still_living_in_the_tract,is.na(people_still_living_in_the_tract$Moved_Out))
}

