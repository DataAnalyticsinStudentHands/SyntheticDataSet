#If we prioritized the Census data we have on people moving in

#Put them in household sizes that will make the model move toward the tabled household sizes

library(stringr)
peoplemovingin <-function(Census_data, tract, people_that_moved_out, people_still_living_in_the_tract, babies, seed){
  Census_data_following_year=Census_data[Census_data$county==201&Census_data$tract==tract&Census_data$state==48,]
  
  to_get_households=rbind(babies[c("householdID","size","household.type","age")],people_still_living_in_the_tract[c("householdID","size","household.type","age")])
  to_get_households$household_and_size=mapply(make_easier,to_get_households$household.type,to_get_households$size)
  #create table of household sizes and family or not family in synthetic dataset
  old_households_table=ftable(to_get_households$household_and_size)
  old_households_dataframe=as.data.frame.matrix(old_households_table)
  colnames(old_households_dataframe)=unlist(attr(old_households_table, "col.vars"))
  sizes=as.numeric(numextract(colnames(old_households_dataframe)))
  #sizes[is.na(sizes)]=1
  old_households_dataframe=old_households_dataframe/sizes
  
  colnames_needed=c(paste0("family.",2:7,".person.household"),paste0("nonfamily.",1:7,".person.household"),"group.quarters.population")
  
  for (col in colnames_needed){
    if(!(paste0(col) %in% colnames(old_households_dataframe))){
      old_households_dataframe[(paste0(col))]=0
    }
  }
  
  Census_households=Census_data_following_year[c(paste0("family.",2:7,".person.household"),paste0("nonfamily.",1:7,".person.household"),"group.quarters.population")]
  households=c("nonfamily.7.person.household","family.7.person.household","nonfamily.6.person.household","family.6.person.household","nonfamily.5.person.household","family.5.person.household","nonfamily.4.person.household","family.4.person.household","nonfamily.3.person.household","family.3.person.household","nonfamily.2.person.household","family.2.person.household","group.quarters.population")
  
  differences_in_households=Census_households[households]-old_households_dataframe[households]
  
  people_moved_within_state <- people_within_state(Census_data_following_year, tract)
  people_moved_out_of_state<- people_out_of_state(Census_data_following_year, tract)
  people_moved_out_of_nation <- people_out_of_nation(Census_data_following_year, tract)
  
  #Find households that match numbers for within county to move in
  householdIDs=unique(people_that_moved_out$householdID)
  
  people_that_moved_out$moving_bracket_age=get_brackets_from_age(people_that_moved_out$real_age)
  people_moved_within_county<-people_in_county(Census_data_following_year, people_that_moved_out, householdIDs)
  
  people_moved_in_from_out_of_county<-place_in_houses(people_moved_within_state, people_moved_out_of_state, people_moved_out_of_nation, differences_in_households, seed)
  
  return(list(people_moved_within_county=people_moved_within_county, people_moved_in_from_out_of_county=people_moved_in_from_out_of_county))
}

numextract <- function(string){
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

#make a variable in synthetic dataset to more easily table to work with Census data
make_easier=function(household.type,size){
  type=ifelse(household.type=="Non-family","nonfamily",
              ifelse(household.type=="Alone","nonfamily",
                     ifelse(household.type=="Group Quarters","group.quarters.population","family")))
  size[size=="Group Quarters"]=1
  easier_to_table=paste0(type,".",size,".person.household")
  return(easier_to_table)
}

people_within_state <-function(Census_data_next_year, tract){
  #Create people from out of county based on Census Data about the people moving out from out of county
  people_moved_within_state=data.frame()
  if(Census_data_next_year$moved.within.state.total>0){
    seeds=sample(1:100000000,Census_data_next_year$moved.within.state.total,replace = FALSE)
    
    for(seedy in seeds){
      set.seed(seedy)
      
      sex=Census_data_next_year[c("moved.within.state.men","moved.within.state.women")]
      code_sex=c("Male","Female")
      age=Census_data_next_year[c("moved.within.state.under.5","moved.within.state.5.to.17","moved.within.state.18.to.19",
                                  "moved.within.state.20.to.24","moved.within.state.25.to.29","moved.within.state.30.to.34",
                                  "moved.within.state.35.to.39","moved.within.state.40.to.44","moved.within.state.45.to.49",
                                  "moved.within.state.50.to.54","moved.within.state.55.to.59","moved.within.state.60.to.64",
                                  "moved.within.state.65.to.69","moved.within.state.70.to.74","moved.within.state.over.75")]
      code_age=c("Under 5","5 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
                 "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","Over 75")
      race=Census_data_next_year[c("moved.within.state.black","moved.within.state.amer.indian.alaskan","moved.within.state.asian","moved.within.state.islander","moved.within.state.other","moved.within.state.multiracial","moved.within.state.white","moved.within.state.hispanic")]
      code_race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
      
      person_who_moved_in=data.frame(sex=sample(code_sex,prob=sex,size = 1),
                                     age=sample(code_age,prob = age,size = 1),
                                     race=sample(code_race,prob = race,size = 1),
                                     moved_from = "Moved Within State",
                                     tract=tract,
                                     county=201,
                                     state=48)
      people_moved_within_state=rbind(people_moved_within_state,person_who_moved_in)
    }
    return(people_moved_within_state)
  }
}

people_out_of_state <-function(Census_data_next_year, tract){
  people_moved_out_of_state=data.frame()
  if(Census_data_next_year$out.of.state.total>0){
    seeds=sample(1:100000000,Census_data_next_year$out.of.state.total,replace = FALSE)
    
    for(seedy in seeds){
      set.seed(seedy)
      
      sex=Census_data_next_year[c("out.of.state.men","out.of.state.women")]
      code_sex=c("Male","Female")
      age=Census_data_next_year[c("out.of.state.under.5","out.of.state.5.to.17","out.of.state.18.to.19",
                                  "out.of.state.20.to.24","out.of.state.25.to.29","out.of.state.30.to.34",
                                  "out.of.state.35.to.39","out.of.state.40.to.44","out.of.state.45.to.49",
                                  "out.of.state.50.to.54","out.of.state.55.to.59","out.of.state.60.to.64",
                                  "out.of.state.65.to.69","out.of.state.70.to.74","out.of.state.over.75")]
      code_age=c("Under 5","5 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
                 "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","Over 75")
      race=Census_data_next_year[c("out.of.state.black","out.of.state.amer.indian.alaskan","out.of.state.asian","out.of.state.islander","out.of.state.other","out.of.state.multiracial","out.of.state.white","out.of.state.hispanic")]
      code_race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
      
      person_who_moved_in=data.frame(sex=sample(code_sex,prob=sex,size = 1),
                                     age=sample(code_age,prob = age,size = 1),
                                     race=sample(code_race,prob = race,size = 1),
                                     moved_from = "Moved Out of State",
                                     tract=tract,
                                     county=201,
                                     state=48)
      people_moved_out_of_state=rbind(people_moved_out_of_state,person_who_moved_in)
    }
    return(people_moved_out_of_state)
  }
}

people_out_of_nation <-function(Census_data_next_year, tract){
  people_moved_out_of_nation=data.frame()
  if(Census_data_next_year$out.of.nation.total>0){
    seeds=sample(1:100000000,Census_data_next_year$out.of.nation.total,replace = FALSE)
    
    for(seedy in seeds){
      set.seed(seedy)
      
      sex=Census_data_next_year[c("out.of.nation.men","out.of.nation.women")]
      code_sex=c("Male","Female")
      age=Census_data_next_year[c("out.of.nation.under.5","out.of.nation.5.to.17","out.of.nation.18.to.19",
                                  "out.of.nation.20.to.24","out.of.nation.25.to.29","out.of.nation.30.to.34",
                                  "out.of.nation.35.to.39","out.of.nation.40.to.44","out.of.nation.45.to.49",
                                  "out.of.nation.50.to.54","out.of.nation.55.to.59","out.of.nation.60.to.64",
                                  "out.of.nation.65.to.69","out.of.nation.70.to.74","out.of.nation.over.75")]
      code_age=c("Under 5","5 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
                 "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","Over 75")
      race=Census_data_next_year[c("out.of.nation.black","out.of.nation.amer.indian.alaskan","out.of.nation.asian","out.of.nation.islander","out.of.nation.other","out.of.nation.multiracial","out.of.nation.white","out.of.nation.hispanic")]
      code_race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
      
      person_who_moved_in=data.frame(sex=sample(code_sex,prob=sex,size = 1),
                                     age=sample(code_age,prob = age,size = 1),
                                     race=sample(code_race,prob = race,size = 1),
                                     moved_from = "Moved Out of Nation",
                                     tract=tract,
                                     county=201,
                                     state=48)
      people_moved_out_of_nation=rbind(people_moved_out_of_nation,person_who_moved_in)
    }
    return(people_moved_out_of_nation)
  }
  
}

#Make age brackets to match with move
get_brackets_from_age<-function(real_age){
  age_bracket=ifelse(real_age<5,"Under 5",
                     ifelse(real_age>=5&real_age<18,"5 to 17",
                            ifelse(real_age>=18&real_age<=19,"18 to 19",
                                   ifelse(real_age>=20&real_age<=24,"20 to 24",
                                          ifelse(real_age>=25&real_age<=29,"25 to 29",
                                                 ifelse(real_age>=30&real_age<=34,"30 to 34",
                                                        ifelse(real_age>=35&real_age<=39,"35 to 39",
                                                               ifelse(real_age>=40&real_age<=44,"40 to 44",
                                                                      ifelse(real_age>=45&real_age<=49,"45 to 49",
                                                                             ifelse(real_age>=50&real_age<=54,"50 to 54",
                                                                                    ifelse(real_age>=55&real_age<=59,"55 to 59",
                                                                                           ifelse(real_age>=60&real_age<=64,"60 to 64",
                                                                                                  ifelse(real_age>=65&real_age<=69,"65 to 69",
                                                                                                         ifelse(real_age>=70&real_age<=74,"70 to 74",
                                                                                                                ifelse(real_age>=75,"Over 75",
                                                                                                                       NA)))))))))))))))
  
  return(age_bracket)
}

people_in_county<-function(Census_data_next_year, people_that_moved_out, householdIDs){
  sex=Census_data_next_year[c("moved.within.county.men","moved.within.county.women")]
  colnames(sex)=c("Male","Female")
  age=Census_data_next_year[c("moved.within.county.under.5","moved.within.county.5.to.17","moved.within.county.18.to.19",
                              "moved.within.county.20.to.24","moved.within.county.25.to.29","moved.within.county.30.to.34",
                              "moved.within.county.35.to.39","moved.within.county.40.to.44","moved.within.county.45.to.49",
                              "moved.within.county.50.to.54","moved.within.county.55.to.59","moved.within.county.60.to.64",
                              "moved.within.county.65.to.69","moved.within.county.70.to.74","moved.within.county.over.75")]
  colnames(age)=c("Under 5","5 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
                  "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","Over 75")
  race=Census_data_next_year[c("moved.within.county.black","moved.within.county.amer.indian.alaskan","moved.within.county.asian","moved.within.county.islander","moved.within.county.other","moved.within.county.multiracial","moved.within.county.white","moved.within.county.hispanic")]
  colnames(race)=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
  
  total_movers_from_county=Census_data_next_year$moved.within.county.total
  
  #I screwed up on a prior dataset that Im working with now so I'm using this line which should no longer be necessary with the new one
  people_that_moved_out=subset(people_that_moved_out,!is.na(people_that_moved_out$sex)&!is.na(people_that_moved_out$race)&!is.na(people_that_moved_out$age))
  
  people_moved_within_county=data.frame()
  for(hh in householdIDs){
    household=subset(people_that_moved_out,people_that_moved_out$householdID==hh)
    
    #Table sex, age and race
    household_sex_table=ftable(household$sex)
    household_sex_dataframe=as.data.frame.matrix(household_sex_table)
    colnames(household_sex_dataframe)=unlist(attr(household_sex_table, "col.vars"))
    
    household_race_table=ftable(household$race)
    household_race_dataframe=as.data.frame.matrix(household_race_table)
    colnames(household_race_dataframe)=unlist(attr(household_race_table, "col.vars"))
    
    household_age_table=ftable(household$moving_bracket_age)
    household_age_dataframe=as.data.frame.matrix(household_age_table)
    colnames(household_age_dataframe)=unlist(attr(household_age_table, "col.vars"))
    
    #Check if a household could move into the tract
    if(all(sex[colnames(household_sex_dataframe)]-household_sex_dataframe>0)&
       all(race[colnames(household_race_dataframe)]-household_race_dataframe>0)&
       all(age[colnames(household_age_dataframe)]-household_age_dataframe>0)
    ){
      #Move into the tract
      people_moved_within_county=rbind(people_moved_within_county,household)
      #Remove as option from households
      householdIDs=householdIDs[!householdIDs==hh]
      #update total
      total_movers_from_county=total_movers_from_county-nrow(household)
      #update criteria
      sex[colnames(household_sex_dataframe)]=sex[colnames(household_sex_dataframe)]-household_sex_dataframe
      race[colnames(household_race_dataframe)]=race[colnames(household_race_dataframe)]-household_race_dataframe
      age[colnames(household_age_dataframe)]=age[colnames(household_age_dataframe)]-household_age_dataframe
    }
    
    if(total_movers_from_county<=0){break()}
  }
  return (people_moved_within_county)
}

get_age_from_moving_brackets<-function(seed,age_bracket){
  set.seed(seed)
  age=ifelse(age_bracket=="Under 5",sample(c(0:4),1),
             ifelse(age_bracket=="5 to 17",sample(c(5:17),1),
                    ifelse(age_bracket=="18 to 19",sample(c(18:19),1),
                           ifelse(age_bracket=="20 to 24",sample(c(20:24),1),
                                  ifelse(age_bracket=="25 to 29",sample(c(25:29),1),
                                         ifelse(age_bracket=="30 to 34",sample(c(30:34),1),
                                                ifelse(age_bracket=="35 to 39",sample(c(35:39),1),
                                                       ifelse(age_bracket=="40 to 44",sample(c(40:44),1),
                                                              ifelse(age_bracket=="45 to 49",sample(c(45:49),1),
                                                                     ifelse(age_bracket=="50 to 54",sample(c(50:54),1),
                                                                            ifelse(age_bracket=="55 to 59",sample(c(55:59),1),
                                                                                   ifelse(age_bracket=="60 to 64",sample(c(60:64),1),
                                                                                          ifelse(age_bracket=="65 to 69",sample(c(65:69),1),
                                                                                                 ifelse(age_bracket=="70 to 74",sample(c(70:74),1),
                                                                                                        ifelse(age_bracket=="Over 75",sample(c(75:100),1),
                                                                                                               NA)))))))))))))))
  return(age)
}

place_in_houses <- function(people_moved_within_state, people_moved_out_of_state, people_moved_out_of_nation, differences_in_households, seed){
#Now we need to put people in households

total_people_made=rbind(people_moved_within_state,people_moved_out_of_state,people_moved_out_of_nation)
#give them an identifier to index them later
total_people_made$identifier=c(1:nrow(total_people_made))
#Give them real ages

total_people_made$real_age=mapply(get_age_from_moving_brackets,c(1:nrow(total_people_made)),total_people_made$age)
#Get total households
total_households=sum(differences_in_households[differences_in_households>0])

#function I found on line
MaxTable <- function(x){
  dd <- unique(x)
  dd[which.max(tabulate(match(x,dd)))]
}

people_moved_in_from_out_of_county=data.frame()
while(nrow(total_people_made)>0 & total_households>0){
  #use the largest group of people first
  which_group_of_people_to_use=MaxTable(total_people_made$moved_from)
  use_me=total_people_made[total_people_made$moved_from==which_group_of_people_to_use,]
  #create households we're missing the most first
  #decide the size
  number_of_people_for_household=as.numeric(numextract(colnames(differences_in_households[which.max(differences_in_households)])))
  if(is.na(number_of_people_for_household)){
    #then its a group quarters person
    number_of_people_for_household=1
  }
  #set seed for sampling
  set.seed(seed)
  #Sample people to make up household
  new_household=sample(use_me$identifier,size=number_of_people_for_household)
  new_household=total_people_made[total_people_made$identifier %in% new_household,]
  new_household$householdID=rep(
    paste("201",tract,colnames(differences_in_households[which.max(differences_in_households)]),seed,"year",2015,sep="."),
    nrow(new_household))
  #make sure there is at least one adult
  if(!all(new_household$real_age<18)){
    people_moved_in_from_out_of_county=rbind(people_moved_in_from_out_of_county,new_household)
    #update number of households
    differences_in_households[which.max(differences_in_households)]=differences_in_households[which.max(differences_in_households)]-1
    #remove them from people made
    total_people_made=total_people_made[!(total_people_made$identifier %in% new_household$identifier), ]
    total_households=total_households-1
  }
  #increment seed
  seed=seed+1
}
return(people_moved_in_from_out_of_county)
}
