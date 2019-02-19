#prioritizing Census data on age race and gender
Census_data=current_data_for_update
#function to make a code so I can table it like its in the Census
make_easier=function(sex,race,age){
  race.for.code=ifelse(race=="Black or African American","black",
                       ifelse(race=="American Indian or Alaskan Native","amer.indian.alaskan",
                              ifelse(race=="Asian","asian",
                                     ifelse(race=="Native Hawaiian or Other Pacific Islander","islander",
                                            ifelse(race=="Some Other Race","other.race",
                                                   ifelse(race=="Two or More Races","multiracial",
                                                          ifelse(race=="White","white",
                                                                 ifelse(race=="Hispanic or Latino","hispanic",NA
                       ))))))))
  age.for.code=ifelse(age<5,"under.5",
                      ifelse(age>=5&age<=9,"5.to.9",
                             ifelse(age>=10&age<=14,"10.to.14",
                                    ifelse(age>=15&age<=17,"15.to.17",
                                           ifelse(age>=18&age<=19,"18.to.19",
                                                  ifelse(age>=20&age<=24,"20.to.24",
                                                         ifelse(age>=25&age<=29,"25.to.29",
                                                                ifelse(age>=30&age<=34,"30.to.34",
                                                                       ifelse(age>=35&age<=44,"35.to.44",
                                                                              ifelse(age>=45&age<=54,"45.to.54",
                                                                                     ifelse(age>=55&age<=64,"55.to.64",
                                                                                            ifelse(age>=65&age<=74,"65.to.74",
                                                                                                   ifelse(age>=75&age<=84,"75.to.84",
                                                                                                          ifelse(age>=75,"over.85",
                                                                                                                 NA))))))))))))))
  sex.for.code=ifelse(sex=="Female"&age<18,"girls",
                      ifelse(sex=="Female"&age>=18,"women",
                             ifelse(sex=="Male"&age<18,"boys",
                                    ifelse(sex=="Male"&age>=18,"men",NA))))

  final_code_for_table_reference=paste(race.for.code,sex.for.code,age.for.code,sep = ".")
  return(final_code_for_table_reference)
}

#Subset tract
a_tract_to_play_with=people_still_living_in_the_tract2[people_still_living_in_the_tract2$tract==tract,]
a_tract_to_play_with=a_tract_to_play_with[c("real_age","age","race","sex","householdID","household.type","size","tract")]
babies=babies[babies$tract==tract,]
babies$real_age=babies$age
a_tract_to_play_with=rbind(a_tract_to_play_with,babies[c("real_age","age","race","sex","householdID","household.type","size","tract")])
#Add code
a_tract_to_play_with$code_to_compare_race_age_gender=make_easier(a_tract_to_play_with$sex,a_tract_to_play_with$race,a_tract_to_play_with$real_age)
#Table by race age and gender
age_race_gender_tract=ftable(a_tract_to_play_with$code_to_compare_race_age_gender)
age_race_gender_tract_dataframe=as.data.frame.matrix(age_race_gender_tract)
colnames(age_race_gender_tract_dataframe)=unlist(attr(age_race_gender_tract, "col.vars"))

#Colnames needed
column_names_needed=c("black.boys.under.5",
                      "black.boys.5.to.9",
                      "black.boys.10.to.14",
                      "black.boys.15.to.17",
                      "black.men.18.to.19",
                      "black.men.20.to.24",
                      "black.men.25.to.29",
                      "black.men.30.to.34",
                      "black.men.35.to.44",
                      "black.men.45.to.54",
                      "black.men.55.to.64",
                      "black.men.65.to.74",
                      "black.men.75.to.84",
                      "black.men.over.85",
                      "black.girls.under.5",
                      "black.girls.5.to.9",
                      "black.girls.10.to.14",
                      "black.girls.15.to.17",
                      "black.women.18.to.19",
                      "black.women.20.to.24",
                      "black.women.25.to.29",
                      "black.women.30.to.34",
                      "black.women.35.to.44",
                      "black.women.45.to.54",
                      "black.women.55.to.64",
                      "black.women.65.to.74",
                      "black.women.75.to.84",
                      "black.women.over.85",
                      "amer.indian.alaskan.boys.under.5",
                      "amer.indian.alaskan.boys.5.to.9",
                      "amer.indian.alaskan.boys.10.to.14",
                      "amer.indian.alaskan.boys.15.to.17",
                      "amer.indian.alaskan.men.18.to.19",
                      "amer.indian.alaskan.men.20.to.24",
                      "amer.indian.alaskan.men.25.to.29",
                      "amer.indian.alaskan.men.30.to.34",
                      "amer.indian.alaskan.men.35.to.44",
                      "amer.indian.alaskan.men.45.to.54",
                      "amer.indian.alaskan.men.55.to.64",
                      "amer.indian.alaskan.men.65.to.74",
                      "amer.indian.alaskan.men.75.to.84",
                      "amer.indian.alaskan.men.over.85",
                      "amer.indian.alaskan.girls.under.5",
                      "amer.indian.alaskan.girls.5.to.9",
                      "amer.indian.alaskan.girls.10.to.14",
                      "amer.indian.alaskan.girls.15.to.17",
                      "amer.indian.alaskan.women.18.to.19",
                      "amer.indian.alaskan.women.20.to.24",
                      "amer.indian.alaskan.women.25.to.29",
                      "amer.indian.alaskan.women.30.to.34",
                      "amer.indian.alaskan.women.35.to.44",
                      "amer.indian.alaskan.women.45.to.54",
                      "amer.indian.alaskan.women.55.to.64",
                      "amer.indian.alaskan.women.65.to.74",
                      "amer.indian.alaskan.women.75.to.84",
                      "amer.indian.alaskan.women.over.85",
                      "asian.boys.under.5",
                      "asian.boys.5.to.9",
                      "asian.boys.10.to.14",
                      "asian.boys.15.to.17",
                      "asian.men.18.to.19",
                      "asian.men.20.to.24",
                      "asian.men.25.to.29",
                      "asian.men.30.to.34",
                      "asian.men.35.to.44",
                      "asian.men.45.to.54",
                      "asian.men.55.to.64",
                      "asian.men.65.to.74",
                      "asian.men.75.to.84",
                      "asian.men.over.85",
                      "asian.girls.under.5",
                      "asian.girls.5.to.9",
                      "asian.girls.10.to.14",
                      "asian.girls.15.to.17",
                      "asian.women.18.to.19",
                      "asian.women.20.to.24",
                      "asian.women.25.to.29",
                      "asian.women.30.to.34",
                      "asian.women.35.to.44",
                      "asian.women.45.to.54",
                      "asian.women.55.to.64",
                      "asian.women.65.to.74",
                      "asian.women.75.to.84",
                      "asian.women.over.85",
                      "islander.boys.under.5",
                      "islander.boys.5.to.9",
                      "islander.boys.10.to.14",
                      "islander.boys.15.to.17",
                      "islander.men.18.to.19",
                      "islander.men.20.to.24",
                      "islander.men.25.to.29",
                      "islander.men.30.to.34",
                      "islander.men.35.to.44",
                      "islander.men.45.to.54",
                      "islander.men.55.to.64",
                      "islander.men.65.to.74",
                      "islander.men.75.to.84",
                      "islander.men.over.85",
                      "islander.girls.under.5",
                      "islander.girls.5.to.9",
                      "islander.girls.10.to.14",
                      "islander.girls.15.to.17",
                      "islander.women.18.to.19",
                      "islander.women.20.to.24",
                      "islander.women.25.to.29",
                      "islander.women.30.to.34",
                      "islander.women.35.to.44",
                      "islander.women.45.to.54",
                      "islander.women.55.to.64",
                      "islander.women.65.to.74",
                      "islander.women.75.to.84",
                      "islander.women.over.85",
                      "other.race.boys.under.5",
                      "other.race.boys.5.to.9",
                      "other.race.boys.10.to.14",
                      "other.race.boys.15.to.17",
                      "other.race.men.18.to.19",
                      "other.race.men.20.to.24",
                      "other.race.men.25.to.29",
                      "other.race.men.30.to.34",
                      "other.race.men.35.to.44",
                      "other.race.men.45.to.54",
                      "other.race.men.55.to.64",
                      "other.race.men.65.to.74",
                      "other.race.men.75.to.84",
                      "other.race.men.over.85",
                      "other.race.girls.under.5",
                      "other.race.girls.5.to.9",
                      "other.race.girls.10.to.14",
                      "other.race.girls.15.to.17",
                      "other.race.women.18.to.19",
                      "other.race.women.20.to.24",
                      "other.race.women.25.to.29",
                      "other.race.women.30.to.34",
                      "other.race.women.35.to.44",
                      "other.race.women.45.to.54",
                      "other.race.women.55.to.64",
                      "other.race.women.65.to.74",
                      "other.race.women.75.to.84",
                      "other.race.women.over.85",
                      "multiracial.boys.under.5",
                      "multiracial.boys.5.to.9",
                      "multiracial.boys.10.to.14",
                      "multiracial.boys.15.to.17",
                      "multiracial.men.18.to.19",
                      "multiracial.men.20.to.24",
                      "multiracial.men.25.to.29",
                      "multiracial.men.30.to.34",
                      "multiracial.men.35.to.44",
                      "multiracial.men.45.to.54",
                      "multiracial.men.55.to.64",
                      "multiracial.men.65.to.74",
                      "multiracial.men.75.to.84",
                      "multiracial.men.over.85",
                      "multiracial.girls.under.5",
                      "multiracial.girls.5.to.9",
                      "multiracial.girls.10.to.14",
                      "multiracial.girls.15.to.17",
                      "multiracial.women.18.to.19",
                      "multiracial.women.20.to.24",
                      "multiracial.women.25.to.29",
                      "multiracial.women.30.to.34",
                      "multiracial.women.35.to.44",
                      "multiracial.women.45.to.54",
                      "multiracial.women.55.to.64",
                      "multiracial.women.65.to.74",
                      "multiracial.women.75.to.84",
                      "multiracial.women.over.85",
                      "white.boys.under.5",
                      "white.boys.5.to.9",
                      "white.boys.10.to.14",
                      "white.boys.15.to.17",
                      "white.men.18.to.19",
                      "white.men.20.to.24",
                      "white.men.25.to.29",
                      "white.men.30.to.34",
                      "white.men.35.to.44",
                      "white.men.45.to.54",
                      "white.men.55.to.64",
                      "white.men.65.to.74",
                      "white.men.75.to.84",
                      "white.men.over.85",
                      "white.girls.under.5",
                      "white.girls.5.to.9",
                      "white.girls.10.to.14",
                      "white.girls.15.to.17",
                      "white.women.18.to.19",
                      "white.women.20.to.24",
                      "white.women.25.to.29",
                      "white.women.30.to.34",
                      "white.women.35.to.44",
                      "white.women.45.to.54",
                      "white.women.55.to.64",
                      "white.women.65.to.74",
                      "white.women.75.to.84",
                      "white.women.over.85",
                      "hispanic.boys.under.5",
                      "hispanic.boys.5.to.9",
                      "hispanic.boys.10.to.14",
                      "hispanic.boys.15.to.17",
                      "hispanic.men.18.to.19",
                      "hispanic.men.20.to.24",
                      "hispanic.men.25.to.29",
                      "hispanic.men.30.to.34",
                      "hispanic.men.35.to.44",
                      "hispanic.men.45.to.54",
                      "hispanic.men.55.to.64",
                      "hispanic.men.65.to.74",
                      "hispanic.men.75.to.84",
                      "hispanic.men.over.85",
                      "hispanic.girls.under.5",
                      "hispanic.girls.5.to.9",
                      "hispanic.girls.10.to.14",
                      "hispanic.girls.15.to.17",
                      "hispanic.women.18.to.19",
                      "hispanic.women.20.to.24",
                      "hispanic.women.25.to.29",
                      "hispanic.women.30.to.34",
                      "hispanic.women.35.to.44",
                      "hispanic.women.45.to.54",
                      "hispanic.women.55.to.64",
                      "hispanic.women.65.to.74",
                      "hispanic.women.75.to.84",
                      "hispanic.women.over.85")

#Make sure we have all the columns we need
for (col in column_names_needed){
  if(!(paste0(col) %in% colnames(age_race_gender_tract_dataframe))){
    age_race_gender_tract_dataframe[(paste0(col))]=0
  }
}

#Use appropriate Census data
#Read in current data
current_data_for_update=Census_data[Census_data$county==201&Census_data$tract==tract&Census_data$state==48,]

#And finally get a vector for differences
differences=current_data_for_update[column_names_needed]-age_race_gender_tract_dataframe[column_names_needed]
total_people_to_move_or_make=sum(differences)

#Time to decide how many people came from where
where_are_yall_from=current_data_for_update[c("moved.within.county.total","moved.within.state.total","out.of.state.total","out.of.nation.total")]
where_are_yall_from=round(where_are_yall_from*total_people_to_move_or_make/sum(where_are_yall_from))

#Negative differences aren't useful at this point and will mess with sampling later
#make negative numbers 0
differences[differences<0]<-0

#We also want household types and sizes
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
#make a variable in synthetic dataset to more easily table to work with Census data
make_easier2=function(household.type,size){
  type=ifelse(household.type=="Non-family","nonfamily",
              ifelse(household.type=="Alone","nonfamily",
                     ifelse(household.type=="Group Quarters","group.quarters.population","family")))
  size[size=="Group Quarters"]=1
  easier_to_table=paste0(type,".",size,".person.household")
  return(easier_to_table)
}

a_tract_to_play_with$household_and_size=mapply(make_easier2,a_tract_to_play_with$household.type,a_tract_to_play_with$size)
#create table of household sizes and family or not family in synthetic dataset
old_households_table=ftable(a_tract_to_play_with$household_and_size)
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

Census_households=current_data_for_update[c(paste0("family.",2:7,".person.household"),paste0("nonfamily.",1:7,".person.household"),"group.quarters.population")]
households=c("nonfamily.7.person.household","family.7.person.household","nonfamily.6.person.household","family.6.person.household","nonfamily.5.person.household","family.5.person.household","nonfamily.4.person.household","family.4.person.household","nonfamily.3.person.household","family.3.person.household","nonfamily.2.person.household","family.2.person.household","group.quarters.population")

differences_in_households=Census_households[households]-old_households_dataframe[households]

#Alright time to people from within the county
#give them a code to make my life easier
people_that_moved_out$code_to_compare_race_age_gender=make_easier(people_that_moved_out$sex,people_that_moved_out$race,people_that_moved_out$real_age)
people_that_moved_out$code_to_compare_household_type_and_size=make_easier2(people_that_moved_out$household.type,people_that_moved_out$size)
#do it by households
householdIDs=unique(people_that_moved_out$householdID)

#Create data frame
people_moved_within_county=data.frame()
for(hh in householdIDs){
  household=subset(people_that_moved_out,people_that_moved_out$householdID==hh)
  
  #Table sex, age and race
  hh_table=ftable(household$code_to_compare_race_age_gender)
  hh_dataframe=as.data.frame.matrix(hh_table)
  colnames(hh_dataframe)=unlist(attr(hh_table, "col.vars"))
  
  
  #Check if a household could move into the tract
  if(all(differences[colnames(hh_dataframe)]-hh_dataframe>0)&
     (people_that_moved_out$code_to_compare_household_type_and_size %in% differences_in_households[,differences_in_households>0])
  ){
    #Move into the tract
    people_moved_within_county=rbind(people_moved_within_county,household)
    #Remove as option from households
    householdIDs=householdIDs[!householdIDs==hh]
    #update total
    where_are_yall_from$moved.within.county.total=where_are_yall_from$moved.within.county.total-nrow(household)
    #update criteria
    differences[colnames(hh_dataframe)]=differences[colnames(hh_dataframe)]-hh_dataframe
    differences_in_households[,household$code_to_compare_household_type_and_size[1]]=differences_in_households[,household$code_to_compare_household_type_and_size[1]]-1
  }
  
  if(where_are_yall_from$moved.within.county.total<=0){break()} 
}

#Time to start simulating people from other parts
probability_vector_for_race_age_gender=differences[,differences>0]
moved_within_state=data.frame(sexbyagecode=sample(colnames(probability_vector_for_race_age_gender),size=where_are_yall_from$moved.within.state.total,prob = probability_vector_for_race_age_gender,replace = TRUE),
                              moved_from=rep("Moved Within State",where_are_yall_from$moved.within.state.total),
                              tract=rep(tract,where_are_yall_from$moved.within.state.total),
                              county=rep(201,where_are_yall_from$moved.within.state.total),
                              state=rep(48,where_are_yall_from$moved.within.state.total))
out_of_state=data.frame(sexbyagecode=sample(colnames(probability_vector_for_race_age_gender),size=where_are_yall_from$out.of.state.total,prob = probability_vector_for_race_age_gender,replace = TRUE),
                        moved_from=rep("Moved Out of State",where_are_yall_from$out.of.state.total),
                        tract=rep(tract,where_are_yall_from$out.of.state.total),
                        county=rep(201,where_are_yall_from$out.of.state.total),
                        state=rep(48,where_are_yall_from$out.of.state.total))
out_of_nation=data.frame(sexbyagecode=sample(colnames(probability_vector_for_race_age_gender),size=where_are_yall_from$out.of.nation.total,prob = probability_vector_for_race_age_gender,replace = TRUE),
                         moved_from=rep("Moved Out of Nation",where_are_yall_from$out.of.nation.total),
                         tract=rep(tract,where_are_yall_from$out.of.nation.total),
                         county=rep(201,where_are_yall_from$out.of.nation.total),
                         state=rep(48,where_are_yall_from$out.of.nation.total))




races = c("black", "amer.indian.alaskan", "asian", "islander", "other.race", "multiracial", "white", "hispanic")

# Organize Census data by race and adult and children, as well as get the minimum number of necessary adults per race: 1 per household 2 for married couples
# boys and girls are 17 and under men and women are 18 and over
for(race in races){
  assign(paste0("total", race), sum(
    assign(paste0(race, "Boys"), Census_data[c(paste0(race, ".boys.under.5"), paste0(race, ".boys.5.to.9"), paste0(race, ".boys.10.to.14"), paste0(race, ".boys.15.to.17"))]),
    assign(paste0(race, "Men"), Census_data[c(paste0(race, ".men.18.to.19"), paste0(race, ".men.20.to.24"), paste0(race, ".men.25.to.29"), paste0(race, ".men.30.to.34"), paste0(race, ".men.35.to.44"), paste0(race, ".men.45.to.54"), paste0(race, ".men.55.to.64"), paste0(race, ".men.65.to.74"), paste0(race, ".men.75.to.84"), paste0(race, ".men.over.85"))]),
    assign(paste0(race, "Girls"), Census_data[c(paste0(race, ".girls.under.5"), paste0(race, ".girls.5.to.9"), paste0(race, ".girls.10.to.14"), paste0(race, ".girls.15.to.17"))]),
    assign(paste0(race, "Women"), Census_data[c(paste0(race, ".women.18.to.19"), paste0(race, ".women.20.to.24"), paste0(race, ".women.25.to.29"), paste0(race, ".women.30.to.34"), paste0(race, ".women.35.to.44"), paste0(race, ".women.45.to.54"), paste0(race, ".women.55.to.64"), paste0(race, ".women.65.to.74"), paste0(race, ".women.75.to.84"), paste0(race, ".women.over.85"))])))
}

library(broman)
getsex <- function(syntheticdataset){
  
  Female = colnames(cbind(blackWomen, blackGirls, amer.indian.alaskanWomen, amer.indian.alaskanGirls, asianWomen, asianGirls, islanderWomen, islanderGirls, other.raceWomen, other.raceGirls, multiracialWomen, multiracialGirls, whiteWomen, whiteGirls, hispanicWomen, hispanicGirls))
  
  sex = switchv(as.character(syntheticdataset$sexbyagecode %in% Female),
                "TRUE" = "Female",
                "Male")
  
  return(sex)
}

getage <- function(syntheticdataset){
  age=ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.under.5","amer.indian.alaskan.boys.under.5","asian.boys.under.5","islander.boys.under.5","other.race.boys.under.5","multiracial.boys.under.5","white.boys.under.5","hispanic.boys.under.5","black.girls.under.5","amer.indian.alaskan.girls.under.5","asian.girls.under.5","islander.girls.under.5","other.race.girls.under.5","multiracial.girls.under.5","white.girls.under.5","hispanic.girls.under.5")),"Under 5",
             ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.5.to.9","amer.indian.alaskan.boys.5.to.9","asian.boys.5.to.9","islander.boys.5.to.9","other.race.boys.5.to.9","multiracial.boys.5.to.9","white.boys.5.to.9","hispanic.boys.5.to.9","black.girls.5.to.9","amer.indian.alaskan.girls.5.to.9","asian.girls.5.to.9","islander.girls.5.to.9","other.race.girls.5.to.9","multiracial.girls.5.to.9","white.girls.5.to.9","hispanic.girls.5.to.9")),"5 to 9",
                    ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.10.to.14","amer.indian.alaskan.boys.10.to.14","asian.boys.10.to.14","islander.boys.10.to.14","other.race.boys.10.to.14","multiracial.boys.10.to.14","white.boys.10.to.14","hispanic.boys.10.to.14","black.girls.10.to.14","amer.indian.alaskan.girls.10.to.14","asian.girls.10.to.14","islander.girls.10.to.14","other.race.girls.10.to.14","multiracial.girls.10.to.14","white.girls.10.to.14","hispanic.girls.10.to.14")),"10 to 14",
                           ifelse((syntheticdataset$sexbyagecode %in% c("black.boys.15.to.17","amer.indian.alaskan.boys.15.to.17","asian.boys.15.to.17","islander.boys.15.to.17","other.race.boys.15.to.17","multiracial.boys.15.to.17","white.boys.15.to.17","hispanic.boys.15.to.17","black.girls.15.to.17","amer.indian.alaskan.girls.15.to.17","asian.girls.15.to.17","islander.girls.15.to.17","other.race.girls.15.to.17","multiracial.girls.15.to.17","white.girls.15.to.17","hispanic.girls.15.to.17")),"15 to 17",
                                  ifelse((syntheticdataset$sexbyagecode %in% c("black.men.18.to.19","amer.indian.alaskan.men.18.to.19","asian.men.18.to.19","islander.men.18.to.19","other.race.men.18.to.19","multiracial.men.18.to.19","white.men.18.to.19","hispanic.men.18.to.19","black.women.18.to.19","amer.indian.alaskan.women.18.to.19","asian.women.18.to.19","islander.women.18.to.19","other.race.women.18.to.19","multiracial.women.18.to.19","white.women.18.to.19","hispanic.women.18.to.19")),"18 to 19",
                                         ifelse((syntheticdataset$sexbyagecode %in% c("black.men.20.to.24","amer.indian.alaskan.men.20.to.24","asian.men.20.to.24","islander.men.20.to.24","other.race.men.20.to.24","multiracial.men.20.to.24","white.men.20.to.24","hispanic.men.20.to.24","black.women.20.to.24","amer.indian.alaskan.women.20.to.24","asian.women.20.to.24","islander.women.20.to.24","other.race.women.20.to.24","multiracial.women.20.to.24","white.women.20.to.24","hispanic.women.20.to.24")),"20 to 24",
                                                ifelse((syntheticdataset$sexbyagecode %in% c("black.men.25.to.29","amer.indian.alaskan.men.25.to.29","asian.men.25.to.29","islander.men.25.to.29","other.race.men.25.to.29","multiracial.men.25.to.29","white.men.25.to.29","hispanic.men.25.to.29","black.women.25.to.29","amer.indian.alaskan.women.25.to.29","asian.women.25.to.29","islander.women.25.to.29","other.race.women.25.to.29","multiracial.women.25.to.29","white.women.25.to.29","hispanic.women.25.to.29")),"25 to 29",
                                                       ifelse((syntheticdataset$sexbyagecode %in% c("black.men.30.to.34","amer.indian.alaskan.men.30.to.34","asian.men.30.to.34","islander.men.30.to.34","other.race.men.30.to.34","multiracial.men.30.to.34","white.men.30.to.34","hispanic.men.30.to.34","black.women.30.to.34","amer.indian.alaskan.women.30.to.34","asian.women.30.to.34","islander.women.30.to.34","other.race.women.30.to.34","multiracial.women.30.to.34","white.women.30.to.34","hispanic.women.30.to.34")),"30 to 34",
                                                              ifelse((syntheticdataset$sexbyagecode %in% c("black.men.35.to.44","amer.indian.alaskan.men.35.to.44","asian.men.35.to.44","islander.men.35.to.44","other.race.men.35.to.44","multiracial.men.35.to.44","white.men.35.to.44","hispanic.men.35.to.44","black.women.35.to.44","amer.indian.alaskan.women.35.to.44","asian.women.35.to.44","islander.women.35.to.44","other.race.women.35.to.44","multiracial.women.35.to.44","white.women.35.to.44","hispanic.women.35.to.44")),"35 to 44",
                                                                     ifelse((syntheticdataset$sexbyagecode %in% c("black.men.45.to.54","amer.indian.alaskan.men.45.to.54","asian.men.45.to.54","islander.men.45.to.54","other.race.men.45.to.54","multiracial.men.45.to.54","white.men.45.to.54","hispanic.men.45.to.54","black.women.45.to.54","amer.indian.alaskan.women.45.to.54","asian.women.45.to.54","islander.women.45.to.54","other.race.women.45.to.54","multiracial.women.45.to.54","white.women.45.to.54","hispanic.women.45.to.54")),"45 to 54",
                                                                            ifelse((syntheticdataset$sexbyagecode %in% c("black.men.55.to.64","amer.indian.alaskan.men.55.to.64","asian.men.55.to.64","islander.men.55.to.64","other.race.men.55.to.64","multiracial.men.55.to.64","white.men.55.to.64","hispanic.men.55.to.64","black.women.55.to.64","amer.indian.alaskan.women.55.to.64","asian.women.55.to.64","islander.women.55.to.64","other.race.women.55.to.64","multiracial.women.55.to.64","white.women.55.to.64","hispanic.women.55.to.64")),"55 to 64",
                                                                                   ifelse((syntheticdataset$sexbyagecode %in% c("black.men.65.to.74","amer.indian.alaskan.men.65.to.74","asian.men.65.to.74","islander.men.65.to.74","other.race.men.65.to.74","multiracial.men.65.to.74","white.men.65.to.74","hispanic.men.65.to.74","black.women.65.to.74","amer.indian.alaskan.women.65.to.74","asian.women.65.to.74","islander.women.65.to.74","other.race.women.65.to.74","multiracial.women.65.to.74","white.women.65.to.74","hispanic.women.65.to.74")),"65 to 74",
                                                                                          ifelse((syntheticdataset$sexbyagecode %in% c("black.men.75.to.84","amer.indian.alaskan.men.75.to.84","asian.men.75.to.84","islander.men.75.to.84","other.race.men.75.to.84","multiracial.men.75.to.84","white.men.75.to.84","hispanic.men.75.to.84","black.women.75.to.84","amer.indian.alaskan.women.75.to.84","asian.women.75.to.84","islander.women.75.to.84","other.race.women.75.to.84","multiracial.women.75.to.84","white.women.75.to.84","hispanic.women.75.to.84")),"75 to 84",
                                                                                                 ifelse((syntheticdataset$sexbyagecode %in% c("black.men.over.85","amer.indian.alaskan.men.over.85","asian.men.over.85","islander.men.over.85","other.race.men.over.85","multiracial.men.over.85","white.men.over.85","hispanic.men.over.85","black.women.over.85","amer.indian.alaskan.women.over.85","asian.women.over.85","islander.women.over.85","other.race.women.over.85","multiracial.women.over.85","white.women.over.85","hispanic.women.over.85")),"Over 85"
                                                                                                        ,NA))))))))))))))
  return(age)
}

# get race from sexageandrace code
getrace <- function(syntheticdataset){
  race = ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(blackWomen, blackGirls, blackMen, blackBoys))), "Black or African American",
                ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(amer.indian.alaskanWomen, amer.indian.alaskanGirls, amer.indian.alaskanMen, amer.indian.alaskanBoys))), "American Indian or Alaskan Native",
                       ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(asianWomen, asianGirls, asianMen, asianBoys))), "Asian",
                              ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(islanderWomen, islanderGirls, islanderMen, islanderBoys))), "Native Hawaiian or Other Pacific Islander",
                                     ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(other.raceWomen, other.raceGirls, other.raceMen, other.raceBoys))), "Some Other Race",
                                            ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(multiracialBoys, multiracialGirls, multiracialMen, multiracialWomen))), "Two or More Races",
                                                   ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(whiteWomen, whiteGirls, whiteMen, whiteBoys))), "White",
                                                          ifelse((syntheticdataset$sexbyagecode %in% colnames(cbind(hispanicWomen, hispanicGirls, hispanicMen, hispanicBoys))), "Hispanic or Latino", NA))))))))
  
  return(race)
}

moved_within_state$sex=getsex(moved_within_state)
moved_within_state$age=getage(moved_within_state)
moved_within_state$race=getrace(moved_within_state)

out_of_state$sex=getsex(out_of_state)
out_of_state$age=getage(out_of_state)
out_of_state$race=getrace(out_of_state)

out_of_nation$sex=getsex(out_of_nation)
out_of_nation$age=getage(out_of_nation)
out_of_nation$race=getrace(out_of_nation)

#Now we need to put people in households
total_people_made=rbind(moved_within_state,out_of_state,out_of_nation)
#give them an identifier to index them later
total_people_made$identifier=c(1:nrow(total_people_made))
#Give them real ages
source("get_age_from_brackets.R")
total_people_made$real_age=mapply(get_age_from_brackets,c(1:nrow(total_people_made)),total_people_made$age)
#Get total households
total_households=sum(differences_in_households[differences_in_households>0])

#function I found on line
MaxTable <- function(x){
  dd <- unique(x)
  dd[which.max(tabulate(match(x,dd)))]
}

people_moved_in_from_out_of_county=data.frame()
seed=1
while(nrow(total_people_made)>0 & total_households>0){
  #use the largest group of people first
  which_group_of_people_to_use=MaxTable(total_people_made$moved_from)
  use_me=total_people_made[total_people_made$moved_from==which_group_of_people_to_use,]
  #create households we're missing the most first
  #decide the size
  number_of_people_for_household=as.numeric(numextract(colnames(differences_in_households[which.max(differences_in_households)])))
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
  }
  #increment seed
  seed=seed+1
}
