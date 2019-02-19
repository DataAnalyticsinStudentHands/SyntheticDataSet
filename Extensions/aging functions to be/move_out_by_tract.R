#Move others out due to differences in Census data
#these are renters who would have moved out, but wouldn't have caused a change in the HCAD data
#this is done by tract

move_out_by_tract<-function(sample_set,Census_data,tract,seed){
  #We're going to do this by tract
  #subset a random tract to play with
  a_tract_to_play_with=sample_set[sample_set$tract==tract,]
  #remove sample set because its a memory hog
  rm(sample_set)
  #subset current people
  people_still_living_in_the_tract=subset(a_tract_to_play_with,is.na(a_tract_to_play_with$Moved_Out))
  people_that_moved_out=subset(a_tract_to_play_with,a_tract_to_play_with$Moved_Out=="Moved Out")
  #Read in current data
  current_data_for_update=Census_data[Census_data$county==201&Census_data$tract==tract&Census_data$state==48,]
  
  #Are our numbers reasonablish
  our_people=data.frame(county=201,tract=tract)
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
  
  MinTable <- function(x){
    return(which.min(apply(x, MARGIN=2,min)))
  }
  
  #If we have too many people move people out until same amount of people
  while(sum(differences)<0){
    set.seed(seed)
    
    max_diff=MinTable(differences)
    
    min_age = switch(as.character(max_diff),
                     "2" = 5,"3" = 18, "4"=20, "5"=25, "6"=30, "7"=35,"8"=40,"9"=45,"10"=50,"11"=55,"12"=60,"13"=65,"14"=70,"15"=75,-1)
    max_age = switch(as.character(max_diff),
                     "1"=5,"2"=17,"3"=19,"4"=24,"5"=29,"6"=34,"7"=39,"8"=44,"9"=49,"10"=54,"11"=59,"12"=64,"13"=69,"14"=74,105)
    
    while(max_diff==MinTable(differences)){
      if(differences[max_diff]<0){
        indexes_potential_to_move=which(people_still_living_in_the_tract$real_age<as.numeric(max_age)&people_still_living_in_the_tract$real_age>=as.numeric(min_age))
        #Move someone out
        who_moves_out=sample(indexes_potential_to_move,size=1)
        people_still_living_in_the_tract$Moved_Out[who_moves_out]="Moved Out"
        #update household size if not group quarters
        if(people_still_living_in_the_tract$household.type[who_moves_out]!="Group Quarters"){
          hhID=people_still_living_in_the_tract$householdID[who_moves_out]
          size=as.numeric(people_still_living_in_the_tract$size[who_moves_out])+1
          people_still_living_in_the_tract$size[people_still_living_in_the_tract$householdID==hhID]=size
        }
        #update differences
        differences[max_diff]=differences[max_diff]+1
        #change seed
        seed=seed+1
      }
    }
    
    newly_moved_out=subset(people_still_living_in_the_tract,people_still_living_in_the_tract$Moved_Out=="Moved Out")
    people_that_moved_out=rbind(people_that_moved_out,newly_moved_out)
    people_still_living_in_the_tract=subset(people_still_living_in_the_tract,is.na(people_still_living_in_the_tract$Moved_Out))
  }
  return(list(people_still_living_in_the_tract=people_still_living_in_the_tract,people_that_moved_out=people_that_moved_out))
}
