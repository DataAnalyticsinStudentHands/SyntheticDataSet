#If we prioritized the Census data we have on people moving in

#Put them in household sizes that will make the model move toward the tabled household sizes
library(stringr)
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

#Create people from out of county based on Census Data about the people moving out from out of county
people_moved_within_state=data.frame()
if(Census_data_following_year$moved.within.state.total>0){
  seeds=sample(1:100000000,Census_data_following_year$moved.within.state.total,replace = FALSE)
  
  for(seedy in seeds){
    set.seed(seedy)
    
    sex=Census_data_following_year[c("moved.within.state.men","moved.within.state.women")]
    code_sex=c("Male","Female")
    age=Census_data_following_year[c("moved.within.state.under.5","moved.within.state.5.to.17","moved.within.state.18.to.19",
                                     "moved.within.state.20.to.24","moved.within.state.25.to.29","moved.within.state.30.to.34",
                                     "moved.within.state.35.to.39","moved.within.state.40.to.44","moved.within.state.45.to.49",
                                     "moved.within.state.50.to.54","moved.within.state.55.to.59","moved.within.state.60.to.64",
                                     "moved.within.state.65.to.69","moved.within.state.70.to.74","moved.within.state.over.75")]
    code_age=c("Under 5","5 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
               "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","Over 75")
    race=Census_data_following_year[c("moved.within.state.black","moved.within.state.amer.indian.alaskan","moved.within.state.asian","moved.within.state.islander","moved.within.state.other","moved.within.state.multiracial","moved.within.state.white","moved.within.state.hispanic")]
    code_race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
    
    person_who_moved_in=data.frame(sex=sample(code_sex,prob=sex,size = 1),
                                age=sample(code_age,prob = age,size = 1),
                                race=sample(code_race,prob = race,size = 1))
    people_moved_within_state=rbind(people_moved_within_state,person_who_moved_in)
  }
}
people_moved_out_of_state=data.frame()
if(Census_data_following_year$out.of.state.total>0){
  seeds=sample(1:100000000,Census_data_following_year$out.of.state.total,replace = FALSE)
  
  for(seedy in seeds){
    set.seed(seedy)
    
    sex=Census_data_following_year[c("out.of.state.men","out.of.state.women")]
    code_sex=c("Male","Female")
    age=Census_data_following_year[c("out.of.state.under.5","out.of.state.5.to.17","out.of.state.18.to.19",
                                     "out.of.state.20.to.24","out.of.state.25.to.29","out.of.state.30.to.34",
                                     "out.of.state.35.to.39","out.of.state.40.to.44","out.of.state.45.to.49",
                                     "out.of.state.50.to.54","out.of.state.55.to.59","out.of.state.60.to.64",
                                     "out.of.state.65.to.69","out.of.state.70.to.74","out.of.state.over.75")]
    code_age=c("Under 5","5 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
               "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","Over 75")
    race=Census_data_following_year[c("out.of.state.black","out.of.state.amer.indian.alaskan","out.of.state.asian","out.of.state.islander","out.of.state.other","out.of.state.multiracial","out.of.state.white","out.of.state.hispanic")]
    code_race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
    
    person_who_moved_in=data.frame(sex=sample(code_sex,prob=sex,size = 1),
                                   age=sample(code_age,prob = age,size = 1),
                                   race=sample(code_race,prob = race,size = 1))
    people_moved_out_of_state=rbind(people_moved_out_of_state,person_who_moved_in)
  }
}

people_moved_out_of_nation=data.frame()
if(Census_data_following_year$out.of.nation.total>0){
  seeds=sample(1:100000000,Census_data_following_year$out.of.nation.total,replace = FALSE)
  
  for(seedy in seeds){
    set.seed(seedy)
    
    sex=Census_data_following_year[c("out.of.nation.men","out.of.nation.women")]
    code_sex=c("Male","Female")
    age=Census_data_following_year[c("out.of.nation.under.5","out.of.nation.5.to.17","out.of.nation.18.to.19",
                                     "out.of.nation.20.to.24","out.of.nation.25.to.29","out.of.nation.30.to.34",
                                     "out.of.nation.35.to.39","out.of.nation.40.to.44","out.of.nation.45.to.49",
                                     "out.of.nation.50.to.54","out.of.nation.55.to.59","out.of.nation.60.to.64",
                                     "out.of.nation.65.to.69","out.of.nation.70.to.74","out.of.nation.over.75")]
    code_age=c("Under 5","5 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49",
               "50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","Over 75")
    race=Census_data_following_year[c("out.of.nation.black","out.of.nation.amer.indian.alaskan","out.of.nation.asian","out.of.nation.islander","out.of.nation.other","out.of.nation.multiracial","out.of.nation.white","out.of.nation.hispanic")]
    code_race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
    
    person_who_moved_in=data.frame(sex=sample(code_sex,prob=sex,size = 1),
                                   age=sample(code_age,prob = age,size = 1),
                                   race=sample(code_race,prob = race,size = 1))
    people_moved_out_of_nation=rbind(people_moved_out_of_nation,person_who_moved_in)
  }
}

