library(tidyr)
library(dplyr)
library(stringr)

#' createIndividuals
#'
#' This function simulates individual people based on census data (age, sex, race, maritial status)
#'
#' @return citizens A dataframe of simulated people.
createIndividuals <- function() {
  
  #Create or read in individual citizens
  if(citizensFromRDS) {
    # import saved citzens from RDS file
    citizens_data_file <- paste0(censusdir, vintage,"/Citizen_data.RDS")
    citizens <- readRDS(citizens_data_file)
    print(sprintf("Done reading citizens RDS from %s", citizens_data_file))
  } else {
    
    #get the census key
    censuskey <- readLines(paste0(censusdir, vintage, "/key"))
    
    #setup race codes https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
    acs_race_codes <- c("H","B","C","D","E","F","G","I","_") 
    
    #gather information from census data, group B01001 will give us gender, race, age
    sex_by_age_race_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B01001")
    
    #use information in label and name columns to create subgroups 
    sex_by_age_race_data <- sex_by_age_race_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% #clean up label 
      filter(substr(name,7,7) %in% acs_race_codes) %>% 
  #   gather(tract,number_sams,4:ncol(sex_by_age_race_data_from_census)) #%>%
      pivot_longer(4:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      filter(number_sams != 0) %>%
      mutate(race = substr(name,7,7)) %>%
      filter(str_detect(label, "!!")) %>%
      separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE) %>%  #NA in rows with only one
      rename(census_group_name = name) %>%
      select(-concept) %>%
      #want to use the totals for testing
      mutate(tract_2r_total = if_else(sex == "Estimate" & race == "G", as.integer(number_sams), as.integer(0)),
             tract_total = if_else(sex == "Estimate" & race == "_", as.integer(number_sams), as.integer(0)))%>%
      group_by(tract) %>%
      mutate(r2_total = sum(tract_2r_total),
             trac_total = sum(tract_total),
             new_numbers_sam = as.integer(number_sams * (1 - 3*(r2_total/(trac_total))))) %>% #fudging here
      select(-number_sams,-tract_2r_total,-tract_total,-r2_total) %>%
      rename(number_sams = new_numbers_sam,tract_total = trac_total) %>% 
      filter(!is.na(age_range) & sex!="Estimate" & race != "_" & number_sams!=0) %>%
      ungroup()

    #get marriage data
    marital_status_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B12002")
  
    marital_status_data <-  marital_status_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(substr(name,7,7) %in% acs_race_codes) %>%
      mutate(race = substr(name,7,7)) %>%
      pivot_longer(4:ncol(marital_status_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      filter(number_sams != 0) %>%
      separate(label, into = c("sex","part1", "part2", "part3", "part4"), sep = "!!", remove = F) %>%
      mutate(age_range = case_when(str_detect(part1, "year") ~ part1,
                                   str_detect(part2, "year") ~ part2,
                                   str_detect(part3, "year") ~ part3,
                                   str_detect(part4, "year") ~ part4)) %>%
      mutate(spouse_present = case_when(str_detect(part1, "spouse") ~ part1,
                                   str_detect(part2, "spouse") ~ part2,
                                   str_detect(part3, "spouse") ~ part3,
                                   str_detect(part4, "spouse") ~ part4)) %>%
      mutate(marital_status = part1) %>%
      rename(census_group_name = name) %>%
      select(-concept, -part1, -part2, -part3, -part4) %>%
      mutate(B_tract = if_else(sex == "Estimate" & race == "B", as.integer(number_sams), as.integer(0)),
             C_tract = if_else(sex == "Estimate" & race == "C", as.integer(number_sams), as.integer(0)),
             D_tract = if_else(sex == "Estimate" & race == "E", as.integer(number_sams), as.integer(0)),
             E_tract = if_else(sex == "Estimate" & race == "E", as.integer(number_sams), as.integer(0)),
             F_tract = if_else(sex == "Estimate" & race == "F", as.integer(number_sams), as.integer(0)),
             G_tract = if_else(sex == "Estimate" & race == "G", as.integer(number_sams), as.integer(0)),
             H_tract = if_else(sex == "Estimate" & race == "H", as.integer(number_sams), as.integer(0)),
             I_tract = if_else(sex == "Estimate" & race == "I", as.integer(number_sams), as.integer(0)),
             tract_total = if_else(sex == "Estimate" & race == "_", as.integer(number_sams), as.integer(0))) %>%
      group_by(tract) %>%
      mutate(B_total = sum(B_tract),
             C_total = sum(C_tract),
             D_total = sum(D_tract),
             E_total = sum(E_tract),
             F_total = sum(F_tract),
             G_total = sum(G_tract),
             H_total = sum(H_tract),
             I_total = sum(I_tract),
             trac_total = sum(tract_total),
             B = as.integer(number_sams *(B_total/trac_total)),
             C = as.integer(number_sams *(C_total/trac_total)),
             D = as.integer(number_sams *(D_total/trac_total)),
             E = as.integer(number_sams *(E_total/trac_total)),
             F = as.integer(number_sams *(F_total/trac_total)),
             G = as.integer(number_sams *(G_total/trac_total)),
             H = as.integer(number_sams *(H_total/trac_total)),
             I = as.integer(number_sams *(I_total/trac_total))
             ) %>%
      select(-ends_with("_total"),-ends_with("_tract")) %>%
      pivot_longer(c("B","C","D","E","F","G","H","I"),names_to = "race_m",values_to = "new_numbers_sam") %>%
      select(-number_sams, -race) %>%
      rename(number_sams = new_numbers_sam,race = race_m) %>%
      filter(!is.na(age_range) & sex!="Estimate" & number_sams!=0) %>%
      ungroup()
    
    base_married_join <- full_join(sex_by_age_race_data, marital_status_data,by=c("tract","sex","race","age_range"),suffix = c("_base", "_married"))  
    
    base_married <- base_married_join %>%
      mutate(number_sams= if_else(is.na(number_sams_married),number_sams_base,number_sams_married))
    
#make group quarters sam residents - sex by age (B26101) is all NA ; marital status (B26104) is all NA; mobility (B26109) is all NA; ed_status (B26109) is all NA
#very small numbers except in 210100 and 100000 (6099 and 1586) - assuming all in GC not living with spouse, but every other combo possible
#biggest thing to worry about is inmate population; other GC are not large, as far as I can tell
    group_quarters_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B26001")
    
    base_group_quarters_data <- group_quarters_data_from_census %>%
      pivot_longer(4:ncol(group_quarters_data_from_census),names_to = "tract", values_to = "number_sams_GQ") %>%
      full_join(.,base_married,by = "tract") %>%
      mutate(
             number_sams_GQ = 
               case_when(age_range=="18 and 19 years" ~ as.integer(as.integer(number_sams_GQ) * .02), #approximated from natl BOP - https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
                         age_range=="20 to 24 years" ~ as.integer(as.integer(number_sams_GQ) * .05),
                         age_range=="25 to 29 years" ~ as.integer(as.integer(number_sams_GQ) * .13),
                         age_range=="30 to 34 years" ~ as.integer(as.integer(number_sams_GQ) * .18),
                         age_range=="35 to 39 years" ~ as.integer(as.integer(number_sams_GQ) * .19),
                         age_range=="40 to 44 years" ~ as.integer(as.integer(number_sams_GQ) * .18),
                         age_range=="35 to 44 years" ~ as.integer(as.integer(number_sams_GQ) * .18), #not consistent use in marriage; fix by age later
                         age_range=="45 to 49 years" ~ as.integer(as.integer(number_sams_GQ) * .12),
                         age_range=="45 to 54 years" ~ as.integer(as.integer(number_sams_GQ) * .11),
                         age_range=="55 to 59 years" ~ as.integer(as.integer(number_sams_GQ) * .05),
                         age_range=="55 to 64 years" ~ as.integer(as.integer(number_sams_GQ) * .05),
                         age_range=="50 to 54 years" ~ as.integer(as.integer(number_sams_GQ) * .08))
      ) %>% 
      mutate(number_sams_GQ = 
               case_when(sex=="Male" ~ as.integer(number_sams_GQ * .93), #approximated from natl BOP - https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
                         sex=="Female" ~ as.integer(number_sams_GQ * .07)),
               percent_GQ = as.integer(number_sams_GQ/tract_total) #almost always 0
      ) %>%
      uncount(2,.id="group_quarter") %>%
      mutate(number_sams = 
               case_when(
                 group_quarter == 2 & is.na(number_sams_GQ) ~ as.integer(0),
                 group_quarter == 1 & is.na(number_sams_GQ) & is.na(number_sams_married) ~ as.integer(number_sams_base),
                 group_quarter == 1 & !is.na(number_sams_GQ) & is.na(number_sams_married) ~ as.integer(number_sams_GQ * (1-percent_GQ)),
                 group_quarter == 1 & !is.na(number_sams_GQ) & !is.na(number_sams_married) ~ as.integer(number_sams_married * (1-percent_GQ)),
                 group_quarter == 2 & !is.na(number_sams_GQ) & !is.na(number_sams_married) ~ as.integer(number_sams_married * percent_GQ),
                 group_quarter == 1 & !is.na(number_sams_GQ) & is.na(number_sams_married) ~ as.integer(number_sams_GQ *percent_GQ)
               )
              ) %>%
      filter(number_sams!=0)

      
  pregnancy_data_race_marriage_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B13002")
    
  pregnancy_data_race <- pregnancy_data_race_marriage_from_census %>%
    mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(substr(name,7,7) %in% acs_race_codes) %>%
      mutate(race = substr(name,7,7)) %>%
      pivot_longer(4:ncol(pregnancy_data_race_marriage_from_census),names_to = "tract", values_to = "number_sams") %>%
      filter(number_sams != 0 & lengths(str_split(label,"!!"))>1) %>% #still have estimate!!total, but not for did/didn't have birth in last 12months
      separate(label, into = c("birth","marital_status", "age_range"), sep = "!!", remove = F) %>%
      rename(census_group_name = name) %>%
      select(-concept) %>%
      mutate(B_tract = if_else(birth == "Estimate" & race == "B", as.integer(number_sams), as.integer(0)),
             C_tract = if_else(birth == "Estimate" & race == "C", as.integer(number_sams), as.integer(0)),
             D_tract = if_else(birth == "Estimate" & race == "E", as.integer(number_sams), as.integer(0)),
             E_tract = if_else(birth == "Estimate" & race == "E", as.integer(number_sams), as.integer(0)),
             F_tract = if_else(birth == "Estimate" & race == "F", as.integer(number_sams), as.integer(0)),
             G_tract = if_else(birth == "Estimate" & race == "G", as.integer(number_sams), as.integer(0)),
             H_tract = if_else(birth == "Estimate" & race == "H", as.integer(number_sams), as.integer(0)),
             I_tract = if_else(birth == "Estimate" & race == "I", as.integer(number_sams), as.integer(0)),
             tract_total = if_else(birth == "Estimate" & race == "_", as.integer(number_sams), as.integer(0))) %>%
      group_by(tract) %>%
      mutate(B_total = sum(B_tract),
             C_total = sum(C_tract),
             D_total = sum(D_tract),
             E_total = sum(E_tract),
             F_total = sum(F_tract),
             G_total = sum(G_tract),
             H_total = sum(H_tract),
             I_total = sum(I_tract),
             trac_total = sum(tract_total),
             B = as.integer(number_sams *(B_total/trac_total)),
             C = as.integer(number_sams *(C_total/trac_total)),
             D = as.integer(number_sams *(D_total/trac_total)),
             E = as.integer(number_sams *(E_total/trac_total)),
             F = as.integer(number_sams *(F_total/trac_total)),
             G = as.integer(number_sams *(G_total/trac_total)),
             H = as.integer(number_sams *(H_total/trac_total)),
             I = as.integer(number_sams *(I_total/trac_total))
      ) %>%
      select(-ends_with("_total"),-ends_with("_tract")) %>%
      pivot_longer(c("B","C","D","E","F","G","H","I"),names_to = "race_p",values_to = "new_numbers_sam") %>%
      select(-number_sams, -race) %>%
      rename(number_sams = new_numbers_sam,race = race_p) %>%
      filter(!is.na(age_range) & birth!="Estimate" & number_sams!=0) %>%
      ungroup()
    
    pregnancy_data_age_marriage_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B13002")
    
    pregnancy_data_age <- pregnancy_data_age_marriage_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(substr(name,7,7) %in% acs_race_codes) %>%
      mutate(race = substr(name,7,7)) %>%
      pivot_longer(4:ncol(pregnancy_data_age_marriage_from_census),names_to = "tract", values_to = "number_sams") %>%
      filter(number_sams != 0 & lengths(str_split(label,"!!"))>1) %>% #still have estimate!!total, but not for did/didn't have birth in last 12months
      separate(label, into = c("birth","marital_status", "age_range"), sep = "!!", remove = F) %>%
      rename(census_group_name = name) %>%
      select(-concept) %>%
      mutate(B_tract = if_else(birth == "Estimate" & race == "B", as.integer(number_sams), as.integer(0)),
             C_tract = if_else(birth == "Estimate" & race == "C", as.integer(number_sams), as.integer(0)),
             D_tract = if_else(birth == "Estimate" & race == "E", as.integer(number_sams), as.integer(0)),
             E_tract = if_else(birth == "Estimate" & race == "E", as.integer(number_sams), as.integer(0)),
             F_tract = if_else(birth == "Estimate" & race == "F", as.integer(number_sams), as.integer(0)),
             G_tract = if_else(birth == "Estimate" & race == "G", as.integer(number_sams), as.integer(0)),
             H_tract = if_else(birth == "Estimate" & race == "H", as.integer(number_sams), as.integer(0)),
             I_tract = if_else(birth == "Estimate" & race == "I", as.integer(number_sams), as.integer(0)),
             tract_total = if_else(birth == "Estimate" & race == "_", as.integer(number_sams), as.integer(0))) %>%
      group_by(tract) %>%
      mutate(B_total = sum(B_tract),
             C_total = sum(C_tract),
             D_total = sum(D_tract),
             E_total = sum(E_tract),
             F_total = sum(F_tract),
             G_total = sum(G_tract),
             H_total = sum(H_tract),
             I_total = sum(I_tract),
             trac_total = sum(tract_total),
             B = as.integer(number_sams *(B_total/trac_total)),
             C = as.integer(number_sams *(C_total/trac_total)),
             D = as.integer(number_sams *(D_total/trac_total)),
             E = as.integer(number_sams *(E_total/trac_total)),
             F = as.integer(number_sams *(F_total/trac_total)),
             G = as.integer(number_sams *(G_total/trac_total)),
             H = as.integer(number_sams *(H_total/trac_total)),
             I = as.integer(number_sams *(I_total/trac_total))
      ) %>%
      select(-ends_with("_total"),-ends_with("_tract")) %>%
      pivot_longer(c("B","C","D","E","F","G","H","I"),names_to = "race_p",values_to = "new_numbers_sam") %>%
      select(-number_sams, -race) %>%
      rename(number_sams = new_numbers_sam,race = race_p) %>%
      filter(!is.na(age_range) & birth!="Estimate" & number_sams!=0) %>%
      ungroup()
    
      
    
    #now expand on each row that doesn't have a total
    test <- uncount(base_group_quarters_data,base_group_quarters_data$number_sams,.id="ind_id") #have to do below for individual ids to work across all
    test2 <- uncount(base_married,base_married$number_sams,.id="ind_id") #test has 3,627,000 - have to think through a bit more....     
    
    
    #add individual ids
    sam_residents <- sams_ready %>%
      group_by(tract) %>%
      mutate(
        individual_id = paste0(tract,'_',rep(1:n())+1000000)
      ) %>%
      select(individual_id, everything())
    
   #test2 <- left_join(census_data,married_data,by=c("tract","sex","race","age_range"))
    #have to walk the rows in the full_expanded sam, and sample based on these same matches - that should be generalizable, though
    
    
      
    #saveRDS(citizen_data,paste0(censusDataDirectory,"citizen_data_7-25.RDS"))  #4,693,483 (4,653,000 official)

  }
  
  return(citizens)
}





#' Household Generator
#'
#' This function simulates people living in households.
#'
#' It calls other functions in the citymodels package in order to simulate characteristics for households and for each individual
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param number.of.people The number of people to simulate
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation
#' @return syntheticdataset A dataframe of simulated people living in households.

households_generator <- function(state, county, tract, seed, Census_data){
  
  # initialize data frame
  fullset = data.frame()
  
  # set seed
  set.seed(seed)
  
  # subset data for correct Census tract
  Census_data = Census_data[(Census_data$state == state) & (Census_data$tract == tract) & (Census_data$county == county),]
  
  # Get a probability vector for their family type: Married couple, Female householder only, or Male householder only
  familyHHtypes = Census_data[18:20]
  colnames(familyHHtypes) <- c("Married-couple family", "Male householder- no wife present", "Female householder- no husband present")
  familyHHtypes = familyHHtypes/rowSums(familyHHtypes)
  
  # Find number of families of size 2-7, nonfamilies of size 1-7, and group quarters
  number.of.people = Census_data$group.quarters.population
  HHs = Census_data[5:17]
  HHs=cbind(HHs,number.of.people)
  
  # If there are people living in the tract then call the create_households() function to simulate households
  if(sum(HHs) > 0){
    fullset = do.call(rbind,sapply(2:15, function(x){
      create_households(state, county, tract, Census_data, HHs[x-1], familyHHtypes, x)
    }))
    
    # The individual ID is just the household ID with a number starting from 10000 added to the beginning
    fullset$individualID = as.numeric(sapply(1:nrow(fullset), function(i) paste((9999+i), fullset[i,]$individualID, sep="",collapse="")))
  }
  
  # return data.frame with all households built
  return(fullset)
}

create_households <- function(state, county, tract, Census_data, census_col, family_type, family_size){
  if(census_col > 0){
    house_set = data.frame()
    
    # make a seed for each household
    family_HH_seeds = sample(1000000:6000000, as.numeric(census_col), replace = FALSE)
    
    # for each seed create a household
    house_set = as.data.frame(do.call(rbind, lapply(family_HH_seeds, function(seedy){ 
      # set seed
      set.seed(seedy)
      
      
      if(family_size < 8){                 # For Families size 2-7
        # sample Household Type: Married couple, Female Housheolder only, Male Householder Only
        HHtype = sample(colnames(family_type), size = 1, prob = family_type)
        
        # Create initial data frame
        if(HHtype == "Married-couple family"){
          partofset = data.frame(household.type = rep(HHtype, family_size), householder=c("Householder", rep("Non-householder", family_size - 1)), members = c("Husband","Wife", rep("NA", family_size - 2)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Male householder- no wife present"){
          partofset = data.frame(household.type = rep(HHtype, family_size), householder=c("Householder", rep("Non-householder", family_size - 1)), members = c("Male Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Female householder- no husband present"){
          partofset = data.frame(household.type = rep(HHtype, family_size),  householder=c("Householder", rep("Non-householder", family_size - 1)), members = c("Female Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
      }
      else{                             # For Non-Families size 1-7
        if(family_size == 8){           # For Non-Families size 1
          partofset = data.frame(household.type = "Alone",  householder="Householder", members = "Householder", size = 1)
        }
        else if(family_size > 8 & family_size != 15){            # For Non-Families size 2-7
          partofset = data.frame(household.type = rep("Non-family", family_size - 7),  householder=c("Householder", rep("Non-householder", family_size - 8)), members = c("Householder", rep("NA",(family_size - 8))), size = rep(family_size - 7, family_size -7))
        }
        else{                      # For Group Quarters
          partofset = data.frame(household.type="Group Quarters",householder="Householder",members="NA",size=1) #create initial data frame
        }
      }
      
      # Begin sampling characteristics of household (functions stored in other scripts)
      # The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
      
      # Build using Census Data
      
      # From individualfunctions.R script
      partofset = getindividualcharacteristics(partofset, seedy, Census_data)  # simulates sex, race, age, school.enrollment, education.attainment, employment, disability, nativity, citizenship, language, veteran.status, transport.method, travel.time
      
      if(family_size != 15){        # Use functions from householdfunctions.R for non group quarters
        partofset = getnumberofvehiclesforhouseholds(partofset, seedy, Census_data) # only dependent on size
        partofset = gethouseholdincome(partofset, seedy, Census_data) # independent -- samples are directly from census data
        partofset = gethouseholdhealthinsurance(partofset, seedy, Census_data) # dependent on income
      }
      else{                        # Use functions from groupquartersfunctions.R for group quarters
        partofset = getnumberofvehiclesforgroupquarters(partofset, seedy, Census_data) # depends on sex and employment
        partofset = getincomeforgroupquarters(partofset, seedy, Census_data) # independent  -- samples are directly from census data
        partofset = gethealthinsuranceforgroupquarters(partofset, seedy, Census_data) # depends on disability and age
      }
     
      partofset$state = rep(state,nrow(partofset))
      partofset$county = rep(county,nrow(partofset))
      partofset$tract = rep(tract,nrow(partofset))
      
      partofset$number.of.vehicles = as.numeric(partofset$number.of.vehicles)
      partofset$age=as.numeric(partofset$age)
      partofset$travel.time.to.work=as.numeric(partofset$travel.time.to.work)
      partofset$household.income=as.numeric(partofset$household.income)
      
      partofset$householdID = as.numeric(rep(paste(tract, seedy, sep="",collapse=""), nrow(partofset)))
      partofset$individualID = as.numeric(rep(paste(tract, seedy, sep="",collapse=""), nrow(partofset)))
      
      # Save new household with any previous households
      return(partofset)
    })))
 
    return(house_set)
  }
}


#' Simulate All Individual Characteristics
#'
#' This function uses data from the U.S. Census to simulate a variety of characteristics for an individual.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for some of the characteristics, such as sex, race, and, age, which are then used to simulate other dependent characteristics.
#' It then samples using the user inputed seed.
#'
#' @param syntheticdataset The individual simulated so far.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with all the characteristics for an individual.

getindividualcharacteristics <- function(syntheticdataset, seed, Census_data){
  # Set seed so sampling is random but repeatable
  set.seed(seed)
  
  # Samples for sex, race, and age 
  # Organize Census data by race and adult and children, as well as get the minimum number of necessary adults per race: 1 per household 2 for married couples
  # boys and girls are 17 and under men and women are 18 and over
  races = c("black", "american.indian.or.alaskan", "asian", "islander", "other.race", "multiracial", "white", "hispanic")
  
  sapply(races, function(race){
    assign(paste0(race, "Boys"), Census_data[startsWith(names(Census_data), paste0(race,".boys"))], envir = parent.frame(3))
    assign(paste0(race, "Men"), Census_data[startsWith(names(Census_data), paste0(race,".men"))], envir = parent.frame(3))
    assign(paste0(race, "Girls"), Census_data[startsWith(names(Census_data), paste0(race,".girls"))], envir = parent.frame(3))
    assign(paste0(race, "Women"), Census_data[startsWith(names(Census_data), paste0(race,".women"))], envir = parent.frame(3))
  })
  
  all.the.kids = cbind(blackBoys, blackGirls, american.indian.or.alaskanBoys, american.indian.or.alaskanGirls, asianBoys, asianGirls, islanderBoys, islanderGirls, other.raceBoys, other.raceGirls, multiracialBoys, multiracialGirls, whiteBoys, whiteGirls, hispanicBoys, hispanicGirls)
  all.the.adult.men = cbind(blackMen, american.indian.or.alaskanMen, asianMen, islanderMen, other.raceMen, multiracialMen, whiteMen, hispanicMen)
  all.the.adult.women = cbind(blackWomen, american.indian.or.alaskanWomen, asianWomen, islanderWomen, other.raceWomen, multiracialWomen, whiteWomen, hispanicWomen)
  totalhouseholders = sum(2 * Census_data$married.couple.families, Census_data$male.householders.no.wife, Census_data$female.householders.no.husband, Census_data$nonfamily.1.person.household, Census_data$nonfamily.2.person.household, Census_data$nonfamily.3.person.household, Census_data$nonfamily.4.person.household, Census_data$nonfamily.5.person.household, Census_data$nonfamily.6.person.household, Census_data$nonfamily.7.person.household)
  
  # Samples for school enrollment 
  age_groups = c(".women.5.to.9", ".women.10.to.14", ".women.15.to.17", ".women.18.to.19", ".women.20.to.24", ".women.25.to.34", ".women.over.35",
                 ".men.5.to.9", ".men.10.to.14", ".men.15.to.17", ".men.18.to.19", ".men.20.to.24", ".men.25.to.34", ".men.over.35")
  
  sapply(age_groups, function(group) assign(paste0("enrollment", group), Census_data[endsWith(names(Census_data), paste0("school", group))], envir = parent.frame(3)))
  
  # Samples for educational attainment
  age_groups = c(".women.18.24", ".women.25.34", ".women.35.44", ".women.45.64", ".women.over.65",
                 ".men.18.24", ".men.25.34", ".men.35.44", ".men.45.64", ".men.over.65")
  
  sapply(age_groups, function(group) assign(paste0("attainment", group), Census_data[endsWith(names(Census_data), paste0("education", group))], envir = parent.frame(3)))
  
  attainment_code = c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
  
  # Samples for employment
  age_groups = c(".women.16.19", ".women.20.21", ".women.22.24", ".women.25.29", ".women.30.34", ".women.35.44", ".women.45.54", ".women.55.59", ".women.60.61", ".women.62.64", ".women.65.69", ".women.70.74", ".women.over.75",
                 ".men.16.19", ".men.20.21", ".men.22.24", ".men.25.29", ".men.30.34", ".men.35.44", ".men.45.54", ".men.55.59", ".men.60.61", ".men.62.64", ".men.65.69", ".men.70.74", ".men.over.75")
  
  sapply(age_groups, function(group) assign(paste0("employment", group), Census_data[endsWith(names(Census_data), paste0("status", group))], envir = parent.frame(3)))
  
  employment.women.20.24 = employment.women.20.21 + employment.women.22.24
  employment.women.55.64 = employment.women.55.59 + employment.women.60.61 + employment.women.62.64
  employment.men.20.24 = employment.men.20.21 + employment.men.22.24
  employment.men.55.64 = employment.men.55.59 + employment.men.60.61 + employment.men.62.64
  employment.women.65.74 = employment.women.65.69 + employment.women.70.74
  employment.men.65.74 = employment.men.65.69 + employment.men.70.74
  
  employment_code1 = c("In Armed Forces","Employed","Unemployed","Not in labor force")
  employment_code2 = c("Employed","Unemployed","Not in labor force")
  
  # Samples for disability
  age_group = c("under18", "from18.64", "over65")
  sapply(age_group, function(group) assign(group, Census_data[startsWith(names(Census_data), group)], envir = parent.frame(3)))
  
  disability_code = c("With One Type of Disability", "With Two or More Types of Disabilities", "No Disabilities")
  
  # Samples for nativity and english speaking skills
  sapply(races, function(race) assign(paste0("language", race), Census_data[c(paste0(race, ".native.only.english"), paste0(race, ".native.other.language.english.well"), paste0(race, ".native.other.language.english.bad"), paste0(race, ".foreign.only.english"), paste0(race, ".foreign.other.language.english.well"), paste0(race, ".foreign.other.language.english.bad"))], envir = parent.frame(3)))
  
  #samples for citizenship and language at home
  native_english = c("native.5.17.english.well", "native.5.17.english.bad", "native.over18.english.well", "native.over18.english.bad")
  foreign_english = c("english.well", "english.bad")
  
  sapply(native_english, function(skill) assign(skill, Census_data[endsWith(names(Census_data), skill)], envir = parent.frame(3)))
  
  sapply(foreign_english, function(skill){
    assign(paste0("foreign.5.17.", skill), Census_data[c(paste0("spanish.foreign.5.17.naturalized.", skill), paste0("other.lang.foreign.5.17.naturalized.", skill), paste0("spanish.foreign.5.17.not.citizen.", skill), paste0("other.lang.foreign.5.17.not.citizen.", skill))], envir = parent.frame(3))
    assign(paste0("foreign.18.", skill), Census_data[c(paste0("spanish.foreign.18.naturalized.", skill), paste0("other.lang.foreign.18.naturalized.", skill), paste0("spanish.foreign.18.not.citizen.", skill), paste0("other.lang.foreign.18.not.citizen.", skill))], envir = parent.frame(3))
  })
  
  foreign.5.17onlyenglish=Census_data[startsWith(names(Census_data), "english.foreign.5.17")]
  foreign.18.onlyenglish=Census_data[startsWith(names(Census_data), "english.foreign.over18")]
  
  # Samples for veteran status
  age_groups = c("men.18.34", "men.35.54", "men.55.64", "men.65.74", "men.over75",
                 "women.18.34", "women.35.54", "women.55.64", "women.65.74", "women.over75")
  
  sapply(age_groups, function(group) assign(paste0("vets.", group), Census_data[c(paste0("veteran.", group), paste0("nonveteran.", group))], envir = parent.frame(3)))
  
  # Samples for means of transportation to work
  gender = c("men", "women")
  sapply(gender, function(g) assign(g, Census_data[startsWith(names(Census_data), g)], envir = parent.frame(3)))
  
  transport_code = c("drove alone","carpooled","public transportation","bicycle","walked","other","worked at home")
  
  # Samples for travel time to work
  transportation = c("drove.alone", "carpooled", "public.transport", "walked", "other.transport")
  sapply(transportation, function(transport) assign(transport, Census_data[startsWith(names(Census_data), transport)], envir = parent.frame(3)))
  
  travel_code = c("1 to 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes","25 to 29 minutes","30 to 34 minutes","35 to 44 minutes","45 to 59 minutes","60 to 100 minutes")
  
  # These are all the characteristics that will be simulated and added to the data set
  member = character()
  sex = character()
  bracket.age = character()
  age = numeric()
  race = character()
  school.enrollment = character()
  educational.attainment = character()
  employment = character()
  disability = character()
  nativity = character()
  English.speaking.skills = character()
  citizenship = character()
  Language.at.home = character()
  veteran.status = character()
  means.of.transportation.to.work = character()
  bracket.travel.time.to.work = character()
  travel.time.to.work = numeric()
  
  # The characterisitics for each individual in a household are simulated in this loop using all of the above varaiables
  syntheticdataset = cbind(syntheticdataset, as.data.frame(do.call(rbind, lapply(1:nrow(syntheticdataset), function(i){
    partialset = data.frame()
    
    # Householders and married couples were already created in the initial data frame, and these people must be adults.
    # To make sure adults and children are evenly balanced other members of households are first sampled as either children or adults
    # with the population reweighted by removing adult householders. The result is that all "NA" members or now a child or adult
    member = switch(as.character(syntheticdataset[i,]$members),
                    "NA" = sample(c("Child","Adult"), size = 1, prob = c(sum(all.the.kids)/(sum(all.the.kids,all.the.adult.women,all.the.adult.men) - (totalhouseholders)), (sum(all.the.adult.women,all.the.adult.men) - (totalhouseholders))/(sum(all.the.adult.men,all.the.adult.women,all.the.kids) - (totalhouseholders)))),
                    "Husband" = "Husband",
                    "Wife" = "Wife",
                    "Male Householder" = "Male Householder",
                    "Female Householder" = "Female Householder",
                    "Householder" = "Householder")
    
    # The sexbyagecode is string that includes the individuals race, sex, and age
    sexbyagecode = switch(member,
                          "Child" = sample(colnames(all.the.kids), size = 1, prob = c(all.the.kids/sum(all.the.kids))),
                          "Adult Man"=, "Male Householder"=, "Husband" = sample(colnames(all.the.adult.men), size = 1, prob = c(all.the.adult.men/sum(all.the.adult.men))),
                          "Adult Woman"=, "Female Householder"=, "Wife" = sample(colnames(all.the.adult.women), size = 1, prob = c(all.the.adult.women/sum(all.the.adult.women))),
                          "Householder"=, "Adult" = sample(colnames(cbind(all.the.adult.women,all.the.adult.men)), size = 1, prob = c(all.the.adult.women/sum(all.the.adult.women,all.the.adult.men), all.the.adult.men/sum(all.the.adult.women,all.the.adult.men))))
    
    # Race and sex are extracted from the sexbyagecode. Sex is the last word from the raceandsex list and race is the first part of the raceandsex list.
    raceandsex = gsub("[0-9]+[^0-9]*", "", sexbyagecode)
    race = gsub("\\.\\w+\\.+$", "", raceandsex)
    sex = switch(strsplit(gsub(paste0(race, "."), "", raceandsex), "\\.")[[1]],
                 "girls"=, "women" = "Female",
                 "Male")
    
    # bracket.age is extracted from the last part of the sexbyagecode and it is string that states the age range (Ex: "5.to.9"). 
    # This is included in the data set so that it is easier to simulate other characteristics that are dependent on age.
    # age is real number between the range stated in bracket.age
    bracket.age <- gsub("^([^0-9]+)", "", sexbyagecode)
    age_range <- as.numeric(unlist(strsplit(gsub("[^0-9]+", ".", bracket.age), "\\.")))
    age = sample(c(age_range[1]:age_range[2]),1)
    
    # school.enrollment is dependent on sex and bracket.age
    school.enrollment = switch(sex,
                               "Female" = switch(bracket.age,
                                                 "5.to.9" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.5.to.9/sum(enrollment.women.5.to.9)),
                                                 "10.to.14" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.10.to.14/sum(enrollment.women.10.to.14)),
                                                 "15.to.17" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.15.to.17/sum(enrollment.women.15.to.17)),
                                                 "18.to.19" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.18.to.19/sum(enrollment.women.18.to.19)),
                                                 "20.to.24" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.20.to.24/sum(enrollment.women.20.to.24)),
                                                 "25.to.29"=, "30.to.34" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.25.to.34/sum(enrollment.women.25.to.34)),
                                                 "35.to.44"=, "45.to.54"=, "55.to.64"=, "65.to.74"=, "75.to.84"=, "85.to.100" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.women.over.35/sum(enrollment.women.over.35)),
                                                 NA),
                               "Male" = switch(bracket.age,
                                               "5.to.9" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.5.to.9/sum(enrollment.men.5.to.9)),
                                               "10.to.14" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.10.to.14/sum(enrollment.men.10.to.14)),
                                               "15.to.17" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.15.to.17/sum(enrollment.men.15.to.17)),
                                               "18.to.19" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.18.to.19/sum(enrollment.men.18.to.19)),
                                               "20.to.24" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.20.to.24/sum(enrollment.men.20.to.24)),
                                               "25.to.29"=, "30.to.34" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.25.to.34/sum(enrollment.men.25.to.34)),
                                               "35.to.44"=, "45.to.54"=, "55.to.64"=, "65.to.74"=, "75.to.84"=, "85.to.100" = sample(c("Public School","Private School","Not Enrolled in School"), 1, prob = enrollment.men.over.35/sum(enrollment.men.over.35)),
                                               NA))
    
    # education.attainment is dependent on sex and bracket.age
    educational.attainment = switch(sex,
                                    "Male" = switch(bracket.age,
                                                    "18.to.19"=, "20.to.24" = sample(attainment_code, 1, prob = attainment.men.18.24/sum(attainment.men.18.24)),
                                                    "25.to.29"=, "30.to.34" = sample(attainment_code, 1, prob = attainment.men.25.34/sum(attainment.men.25.34)),
                                                    "35.to.44"= sample(attainment_code, 1, prob = attainment.men.35.44/sum(attainment.men.35.44)),
                                                    "45.to.54"=, "55.to.64" = sample(attainment_code, 1, prob = attainment.men.45.64/sum(attainment.men.45.64)),
                                                    "65.to.74"=, "75.to.84"=, "85.to.100" = sample(attainment_code, 1, prob = attainment.men.over.65/sum(attainment.men.over.65)),
                                                    NA),
                                    "Female" = switch(bracket.age,
                                                      "18.to.19"=, "20.to.24" = sample(attainment_code, 1, prob = attainment.women.18.24/sum(attainment.women.18.24)),
                                                      "25.to.29"=, "30.to.34" = sample(attainment_code, 1, prob = attainment.women.25.34/sum(attainment.women.25.34)),
                                                      "35.to.44"= sample(attainment_code, 1, prob = attainment.women.35.44/sum(attainment.women.35.44)),
                                                      "45.to.54"=, "55.to.64" = sample(attainment_code, 1, prob = attainment.women.45.64/sum(attainment.women.45.64)),
                                                      "65.to.74"=, "75.to.84"=, "85.to.100" = sample(attainment_code, 1, prob = attainment.women.over.65/sum(attainment.women.over.65)),
                                                      NA))
    
    # employment is dependent on sex and bracket.age
    employment = switch(sex,
                        "Male" = switch(bracket.age,
                                        "18.to.19" = sample(employment_code1,1,prob =employment.men.16.19/sum(employment.men.16.19)),
                                        "20.to.24" = sample(employment_code1,1,prob=employment.men.20.24/sum(employment.men.20.24)),
                                        "25.to.29"=sample(employment_code1,1,prob=employment.men.25.29/sum(employment.men.25.29)),
                                        "30.to.34" = sample(employment_code1,1,prob=employment.men.30.34/sum(employment.men.30.34)),
                                        "35.to.44"= sample(employment_code1,1,prob=employment.men.35.44/sum(employment.men.35.44)),
                                        "45.to.54"=sample(employment_code1,1,prob=employment.men.45.54/sum(employment.men.45.54)),
                                        "55.to.64" = sample(employment_code1,1,prob=employment.men.55.64/sum(employment.men.55.64)),
                                        "65.to.74"=sample(employment_code2,1,prob=employment.men.65.74/sum(employment.men.65.74)),
                                        "75.to.84"=, "85.to.100" = sample(employment_code2,1,prob=employment.men.over.75/sum(employment.men.over.75)),
                                        NA),
                        "Female" = switch(bracket.age,
                                          "18.to.19"=sample(employment_code1,1,prob=employment.women.16.19/sum(employment.women.16.19)),
                                          "20.to.24" = sample(employment_code1,1,prob=employment.women.20.24/sum(employment.women.20.24)),
                                          "25.to.29"=sample(employment_code1,1,prob=employment.women.25.29/sum(employment.women.25.29)),
                                          "30.to.34" = sample(employment_code1,1,prob=employment.women.30.34/sum(employment.women.30.34)),
                                          "35.to.44"=  sample(employment_code1,1,prob=employment.women.35.44/sum(employment.women.35.44)),
                                          "45.to.54"=sample(employment_code1,1,prob=employment.women.45.54/sum(employment.women.45.54)),
                                          "55.to.64" = sample(employment_code1,1,prob=employment.women.55.64/sum(employment.women.55.64)),
                                          "65.to.74"=sample(employment_code2,1,prob=employment.women.65.74/sum(employment.women.65.74)),
                                          "75.to.84"=, "85.to.100" =  sample(employment_code2,1,prob=employment.women.over.75/sum(employment.women.over.75)),
                                          NA))
    
    # disability is dependent on bracket.age
    disability = switch(bracket.age,
                        "0.to.4"=, "5.to.9"=, "10.to.14"=, "15.to.17" = sample(disability_code, 1, prob = under18/sum(under18)),
                        "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34"=, "35.to.44"=, "45.to.54"=, "55.to.64" = sample(disability_code, 1, prob = from18.64/sum(from18.64)),
                        "65.to.74"=, "75.to.84"=, "85.to.100" = sample(disability_code, 1, prob = over65/sum(over65)))
    
    # langandnat is dependent on bracket.age and race and returns a string stating the race, nativity and english speaking skills
    # nativity and english.speaking.skills are extracted from the string variable langandnat
    langandnat = switch(bracket.age,
                        "0.to.4" = NA,
                        switch(race,
                               "black" =sample(colnames(languageblack),1,prob=languageblack/sum(languageblack)),
                               "american.indian.or.alaskan" = sample(colnames(languageamerican.indian.or.alaskan),1,prob=languageamerican.indian.or.alaskan/sum(languageamerican.indian.or.alaskan)),
                               "asian" = sample(colnames(languageasian),1,prob=languageasian/sum(languageasian)),
                               "islander" = sample(colnames(languageislander),1,prob=languageislander/sum(languageislander)),
                               "other.race" = sample(colnames(languageother.race),1,prob=languageother.race/sum(languageother.race)),
                               "multiracial" = sample(colnames(languagemultiracial),1,prob=languagemultiracial/sum(languagemultiracial)),
                               "white" = sample(colnames(languagewhite),1,prob=languagewhite/sum(languagewhite)),
                               "hispanic" = sample(colnames(languagehispanic),1,prob=languagehispanic/sum(languagehispanic))))
    
    if(!is.na(langandnat)){
      langandnat = gsub(paste0(race, "."), "", langandnat)
      nativity = gsub("\\.+\\w+", "", langandnat)
      English.speaking.skills = paste(tail(strsplit(langandnat, "\\.")[[1]], 2), collapse = ".")
    }
    else{
      nativity = NA
      English.speaking.skills = NA
    }
    # citizenandlang is dependent on nativity and english speaking skills and returns a string stating the citizenship and language
    citizenandlang = switch(nativity,
                            "native" = switch(English.speaking.skills,
                                              "english.well" = sample(colnames(cbind(native.5.17.english.well, native.over18.english.well)), 1, prob = cbind(native.5.17.english.well/sum(native.5.17.english.well, native.over18.english.well), native.over18.english.well/sum(native.5.17.english.well, native.over18.english.well))),
                                              "english.bad" = sample(colnames(cbind(native.5.17.english.bad, native.over18.english.bad)), 1, prob = cbind(native.5.17.english.bad/sum(native.5.17.english.bad, native.over18.english.bad), native.over18.english.bad/sum(native.over18.english.bad, native.5.17.english.bad))),
                                              NA),
                            "foreign" = switch(English.speaking.skills,
                                               "only.english" = sample(colnames(cbind(foreign.5.17onlyenglish, foreign.18.onlyenglish)), 1, prob = cbind(foreign.5.17onlyenglish/sum(foreign.5.17onlyenglish, foreign.18.onlyenglish), foreign.18.onlyenglish/sum(foreign.18.onlyenglish, foreign.5.17onlyenglish))),
                                               "english.well" = sample(colnames(cbind(foreign.5.17.english.well, foreign.18.english.well)), 1, prob = cbind(foreign.5.17.english.well/sum(foreign.5.17.english.well, foreign.18.english.well), foreign.18.english.well/sum(foreign.18.english.well, foreign.5.17.english.well))),
                                               "english.bad" = sample(colnames(cbind(foreign.5.17.english.bad, foreign.18.english.bad)), 1, prob = cbind(foreign.5.17.english.bad/sum(foreign.5.17.english.bad, foreign.18.english.bad), foreign.18.english.bad/sum(foreign.18.english.bad, foreign.5.17.english.bad))),
                                               NA),
                            NA)
    
    # citizenship depends on citizenandlang
    citizenship = ifelse(nativity == "native", "Citizen",
                         ifelse(grepl("naturalized", citizenandlang), "Naturalized Citizen",
                                ifelse(grepl("not.citizen", citizenandlang), "Not a U.S. Citizen", NA)))
    
    # Language.at.home depends on citizenandlang
    Language.at.home = ifelse(English.speaking.skills == "only.english", "English",
                              ifelse(grepl("spanish", citizenandlang), "Speaks Spanish",
                                     ifelse(grepl("other.lang", citizenandlang), "Speaks Other Languages", NA)))
    
    # veteran.status depends on employment, sex, and bracket.age
    veteran.status = switch(as.character(employment),
                            "In Armed Forces" = "Nonveteran",
                            switch(sex,
                                   "Male" = switch(bracket.age,
                                                   "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34" = sample(c("Veteran","Nonveteran"),1,prob=vets.men.18.34/sum(vets.men.18.34)),
                                                   "35.to.44"=, "45.to.54"=sample(c("Veteran","Nonveteran"),1,prob=vets.men.35.54/sum(vets.men.35.54)),
                                                   "55.to.64" = sample(c("Veteran","Nonveteran"),1,prob=vets.men.55.64/sum(vets.men.55.64)),
                                                   "65.to.74"=sample(c("Veteran","Nonveteran"),1,prob=vets.men.65.74/sum(vets.men.65.74)),
                                                   "75.to.84"=, "85.to.100" = sample(c("Veteran","Nonveteran"),1,prob=vets.men.over75/sum(vets.men.over75)),
                                                   "Nonveteran"),
                                   "Female" = switch(bracket.age,
                                                     "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34" = sample(c("Veteran","Nonveteran"),1,prob=vets.women.18.34/sum(vets.women.18.34)),
                                                     "35.to.44"=, "45.to.54"=sample(c("Veteran","Nonveteran"),1,prob=vets.women.35.54/sum(vets.women.35.54)),
                                                     "55.to.64" = sample(c("Veteran","Nonveteran"),1,prob=vets.women.55.64/sum(vets.women.55.64)),
                                                     "65.to.74"=sample(c("Veteran","Nonveteran"),1,prob=vets.women.65.74/sum(vets.women.65.74)),
                                                     "75.to.84"=, "85.to.100" =  sample(c("Veteran","Nonveteran"),1,prob=vets.women.over75/sum(vets.women.over75)),
                                                     "Nonveteran")))
    
    # means.of.transportation.to.work depends on employment and sex
    means.of.transportation.to.work = switch(as.character(employment),
                                             "Employed" = switch(sex,
                                                                 "Male" = sample(transport_code,1,prob=men/sum(men)),
                                                                 sample(transport_code,1,prob=women/sum(women))),
                                             NA)
    
    # bracket.travel.time.to.work depends on means.of.transportation.to.work and returns a string with the time range
    # The numbers from the string are extracted and used to assign a real number between that range to travel.time.to.work
    bracket.travel.time.to.work = switch(as.character(means.of.transportation.to.work),
                                         "drove alone" = sample(travel_code,1,prob=drove.alone/sum(drove.alone)),
                                         "carpooled" = sample(travel_code,1,prob=carpooled/sum(carpooled)),
                                         "public transportation" = sample(travel_code,1,prob=public.transport/sum(public.transport)),
                                         "walked" = sample(travel_code,1,prob=walked/sum(walked)),
                                         "bicycle"=, "other" = sample(travel_code,1,prob=other.transport/sum(other.transport)),
                                         NA)
    
    if(!is.na(bracket.travel.time.to.work)){
      twonumbers <- strsplit(gsub("[^0-9]+", ".", bracket.travel.time.to.work), "\\.")
      time_range <- as.numeric(twonumbers[[1]])
      travel.time.to.work = as.numeric(sample(c(time_range[1]:time_range[2]), 1))
    }
    else{
      travel.time.to.work = NA
    }
    
    partialset = cbind(member, sex, bracket.age, age, race, school.enrollment, educational.attainment, employment, disability, nativity, English.speaking.skills, citizenship, Language.at.home, veteran.status, means.of.transportation.to.work, bracket.travel.time.to.work, travel.time.to.work)
    
    return(partialset)
  })), stringsAsFactors = F))
  
  syntheticdataset$members = NULL
  
  return(syntheticdataset)
}

