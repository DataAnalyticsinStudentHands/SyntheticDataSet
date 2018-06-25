#Move others out due to differences in Census data
#these are renters who would have moved out, but wouldn't have caused a change in the HCAD data
#this is done by tract

move_out_by_tract<-function(sample_set,Census_data,state,county,tract,seed){
  #We're going to do this by tract
  #subset a random tract to play with
  a_tract_to_play_with=subset(sample_set,sample_set$tract==tract&sample_set$state==state&sample_set$county==county)
  
  #subset current people
  people_still_living_in_the_tract=subset(a_tract_to_play_with,is.na(a_tract_to_play_with$Moved_Out))
  
  #Read in current data
  current_data_for_update=subset(Census_data,Census_data$county==county&Census_data$tract==tract&Census_data$state==state)
  
  #Are our numbers reasonablish
  our_people=data.frame(county=county,tract=tract)
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
    set.seed(seed)
    if(differences$same.house.over.75>=0){
      indexes_potential_75_year_olds_to_move=which(people_still_living_in_the_tract$real_age>=75)
      #Move someone out
      who_moves_out=sample(indexes_potential_75_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.over.75=differences$same.house.over.75+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.70.to.74>=0){
      indexes_potential_70_year_olds_to_move=which(people_still_living_in_the_tract$real_age<75&people_still_living_in_the_tract$real_age>=70)
      #Move someone out
      who_moves_out=sample(indexes_potential_70_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.70.to.74=differences$same.house.70.to.74+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.65.to.69>=0){
      indexes_potential_65_year_olds_to_move=which(people_still_living_in_the_tract$real_age<70&people_still_living_in_the_tract$real_age>=65)
      #Move someone out
      who_moves_out=sample(indexes_potential_65_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.65.to.69=differences$same.house.65.to.69+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.60.to.64>=0){
      indexes_potential_60_year_olds_to_move=which(people_still_living_in_the_tract$real_age<65&people_still_living_in_the_tract$real_age>=60)
      #Move someone out
      who_moves_out=sample(indexes_potential_60_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.60.to.64=differences$same.house.60.to.64+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.55.to.59>=0){
      indexes_potential_55_year_olds_to_move=which(people_still_living_in_the_tract$real_age<60&people_still_living_in_the_tract$real_age>=55)
      #Move someone out
      who_moves_out=sample(indexes_potential_55_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.55.to.59=differences$same.house.55.to.59+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.50.to.54>=0){
      indexes_potential_50_year_olds_to_move=which(people_still_living_in_the_tract$real_age<55&people_still_living_in_the_tract$real_age>=50)
      #Move someone out
      who_moves_out=sample(indexes_potential_50_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.50.to.54=differences$same.house.50.to.54+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.45.to.49>=0){
      indexes_potential_45_year_olds_to_move=which(people_still_living_in_the_tract$real_age<50&people_still_living_in_the_tract$real_age>=45)
      #Move someone out
      who_moves_out=sample(indexes_potential_45_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.45.to.49=differences$same.house.45.to.49+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.40.to.44>=0){
      indexes_potential_40_year_olds_to_move=which(people_still_living_in_the_tract$real_age<45&people_still_living_in_the_tract$real_age>=40)
      #Move someone out
      who_moves_out=sample(indexes_potential_40_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.40.to.44=differences$same.house.40.to.44+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.35.to.39>=0){
      indexes_potential_35_year_olds_to_move=which(people_still_living_in_the_tract$real_age<40&people_still_living_in_the_tract$real_age>=35)
      #Move someone out
      who_moves_out=sample(indexes_potential_35_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.35.to.39=differences$same.house.35.to.39+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.30.to.34>=0){
      indexes_potential_30_year_olds_to_move=which(people_still_living_in_the_tract$real_age<35&people_still_living_in_the_tract$real_age>=30)
      #Move someone out
      who_moves_out=sample(indexes_potential_30_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.30.to.34=differences$same.house.30.to.34+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.25.to.29>=0){
      indexes_potential_25_year_olds_to_move=which(people_still_living_in_the_tract$real_age<30&people_still_living_in_the_tract$real_age>=25)
      #Move someone out
      who_moves_out=sample(indexes_potential_25_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.25.to.29=differences$same.house.25.to.29+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.20.to.24>=0){
      indexes_potential_20_year_olds_to_move=which(people_still_living_in_the_tract$real_age<25&people_still_living_in_the_tract$real_age>=20)
      #Move someone out
      who_moves_out=sample(indexes_potential_20_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.20.to.24=differences$same.house.20.to.24+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.18.to.19>=0){
      indexes_potential_18_year_olds_to_move=which(people_still_living_in_the_tract$real_age<20&people_still_living_in_the_tract$real_age>=18)
      #Move someone out
      who_moves_out=sample(indexes_potential_18_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.18.to.19=differences$same.house.18.to.19+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.5.to.17>=0){
      indexes_potential_17_year_olds_to_move=which(people_still_living_in_the_tract$real_age<18&people_still_living_in_the_tract$real_age>=18)
      #Move someone out
      who_moves_out=sample(indexes_potential_17_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.5.to.17=differences$same.house.5.to.17+1
      #change seed
      seed=seed+1
    }
    if(differences$same.house.under.5>=0){
      indexes_potential_5_year_olds_to_move=which(people_still_living_in_the_tract$real_age<=5)
      #Move someone out
      who_moves_out=sample(indexes_potential_5_year_olds_to_move,size=1)
      people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
      #update differences
      differences$same.house.under.5=differences$same.house.under.5+1
      #change seed
      seed=seed+1
    }
    newly_moved_out=subset(people_still_living_in_the_tract,people_still_living_in_the_tract$Moved_Out=="Moved Out")
    people_that_moved_out=rbind(people_that_moved_out,newly_moved_out)
    people_still_living_in_the_tract=subset(people_still_living_in_the_tract,is.na(people_still_living_in_the_tract$Moved_Out))
  }
  return(list(people_still_living_in_the_tract,people_that_moved_out))
}