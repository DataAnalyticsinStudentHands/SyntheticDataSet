library(tidyr)
library(dplyr)
library(stringr)

#' createIndividuals
#'
#' This function simulates individual people based on census data (age, sex, race, as of April 1, vintage year).
#' If simulated data already exists, it will read it from an RDS file.
#'
#' @return sam_residents A dataframe of simulated people.
createIndividuals <- function() {
  
  sam_residents_data_file <- paste0(censusdir, vintage,"/Residents_data.RDS")
  #Create or read in individual sam residents
  if(file.exists(sam_residents_data_file)) {
    # import saved sam residents from RDS file
    sam_residents <- readRDS(sam_residents_data_file)
    print(sprintf("Done reading sam residents RDS from %s", sam_residents_data_file ))
  } else {
    
    #get the census key
    censuskey <- readLines(paste0(censusdir, vintage, "/key"))
    
    #setup race codes https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
    acs_race_codes <- c("A","B","C","D","E","F","G") #could collect all - add them up, without H and I, and you get the total! H is white alone, not hispanic and I is hispanic and if you add them up they don't equal white alone
    
    #gather information from census data, group B01001 will give us gender, race, age
    sex_by_age_race_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B01001")
    
    #use information in label and name columns to create subgroups 
    sex_by_age_race_data <- sex_by_age_race_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% #clean up label 
      pivot_longer(4:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams") %>%    
      mutate(race = substr(name,7,7)) %>%
      separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE) %>%  #NA in rows with only one
      rename(census_group_name = name) %>%
      filter(number_sams != 0, !is.na(age_range),race!="_",age_range!="Total") %>% 
      #use ages to assign a per-year percentage for each?      
      mutate(
        age_range = str_replace(age_range,"Under 5 years","0  to  5 years"), #have to do something funky...
        age_range = str_replace(age_range,"5 to 9 years","5  to  9 years"),
        age_range = str_replace(age_range,"18 and 19 years","18 to 19 years"),
        age_range = str_replace(age_range,"85 years and over","85 to 94 years"),  #have to norm it when calculating...
        first_age = as.numeric(substr(age_range,1,2)),
        last_age = as.numeric(substr(age_range,7,8)),
        age_range_length = last_age-first_age+1) %>%
      select(-concept) %>%
      group_by(tract, sex, age_range) %>% 
      mutate(
        A_tract_age = if_else(race=="A", as.integer(number_sams),as.integer(0)),
        B_tract_age = if_else(race=="B", as.integer(number_sams),as.integer(0)),
        C_tract_age = if_else(race=="C", as.integer(number_sams),as.integer(0)),
        D_tract_age = if_else(race=="D", as.integer(number_sams),as.integer(0)),
        E_tract_age = if_else(race=="E", as.integer(number_sams),as.integer(0)),
        F_tract_age = if_else(race=="F", as.integer(number_sams),as.integer(0)),
        G_tract_age = if_else(race=="G", as.integer(number_sams),as.integer(0)),
        H_tract_age = if_else(race=="H", as.integer(number_sams),as.integer(0)), 
        I_tract_age = if_else(race=="I", as.integer(number_sams),as.integer(0)), 
        white_hispanic = sum(A_tract_age)-((sum(H_tract_age)+(sum(F_tract_age)*.3)+(sum(G_tract_age)*.2)+(sum(B_tract_age)*.03))),
        #all very approximate - should be able to use percents of other races  
        hispanic_number = case_when(race=="A" & white_hispanic > 0 ~ as.integer(white_hispanic),
                                    race=="I" ~ as.integer(number_sams), 
                                    race=="F" ~ as.integer(number_sams*.92), 
                                    race=="B" ~ as.integer(number_sams*.2),  
                                    race=="G" ~ as.integer(number_sams*.75),
                                    TRUE ~ as.integer(0))
      ) %>% 
      ungroup() %>%
      uncount(2,.id="hispanic_id") %>%
      filter(race %in% acs_race_codes) %>%
#      select(-ends_with("_tract_age")) %>% 
      mutate(number_sams = case_when(hispanic_id==1 & hispanic_number > 0 ~ as.integer(number_sams) - as.integer(hispanic_number),
                                     hispanic_id==2 & hispanic_number > 0 ~ as.integer(hispanic_number),
                                     hispanic_id==2 & hispanic_number == 0 ~ as.integer(0),
                                     TRUE ~ as.integer(number_sams)),
             hispanic = if_else(hispanic_id == 2, TRUE, FALSE),
             tract_race_year = case_when( #this is the number per year in that age_range for that tract and that race
               race=="A" ~ A_tract_age/age_range_length, 
               race=="B" ~ B_tract_age/age_range_length,
               race=="C" ~ C_tract_age/age_range_length,
               race=="D" ~ D_tract_age/age_range_length,
               race=="E" ~ E_tract_age/age_range_length,
               race=="F" ~ F_tract_age/age_range_length,
               race=="G" ~ G_tract_age/age_range_length)
      ) %>%
      filter(number_sams!=0) %>%
      select(-hispanic_id,-hispanic_number,-white_hispanic,-ends_with("_tract_age"))
    
      sam_sex_race_age <- uncount(sex_by_age_race_data,number_sams,.id = "sams_id") %>% # should equal 4525519 per B10001 row 166 total
        rowwise() %>%
        mutate(age=as.numeric(sample(as.character(first_age:last_age),1,prob = rep(1/age_range_length,age_range_length),replace = FALSE)))
    
    #for other individual characteristics: get age_range to year; get percentages by race and age, sample with tract_race_year multiplied into probability?
     
     
     
    
    #get marriage data
    marital_status_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B12002")
    
    marital_status_data_named <-  marital_status_data_from_census %>%
      pivot_longer(4:ncol(marital_status_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      group_by(tract) %>%
      mutate(race = substr(name,7,7),  #has age_range or race, not both
             tract_pop = if_else(race=="_" & label == "Estimate!!Total",number_sams,0),
             label = str_remove_all(label,"Estimate!!Total!!"),
             total_tract_pop = sum(tract_pop)) %>%
      separate(label, into = c("sex", "part2", "part3", "part4","part5"), sep = "!!", remove = T) %>%  
      mutate(age_range = case_when(str_detect(part2, "year") ~ part2,
                                   str_detect(part3, "year") ~ part3,
                                   str_detect(part4, "year") ~ part4,
                                   str_detect(part5, "year") ~ part5),
             spouse_present = case_when(str_detect(part2, "spouse") ~ part2,
                                        str_detect(part3, "spouse") ~ part3,
                                        str_detect(part4, "spouse") ~ part4),
             marital_status = case_when(str_detect(part2,"married") ~ part2, 
                                        str_detect(part3,"married") ~ part3,
                                        str_detect(part4,"married") ~ part4,
                                        str_detect(part2,"Widowed") ~ part2,
                                        str_detect(part3,"Widowed") ~ part3,
                                        str_detect(part4,"Widowed") ~ part4,
                                        str_detect(part2,"Divorced") ~ part2,
                                        str_detect(part3,"Divorced") ~ part3,
                                        str_detect(part4,"Divorced") ~ part4
             )
      ) %>% 
      filter(!is.na(marital_status)) %>%
      select(-starts_with("part"),-concept,-tract_pop) 

    
    marital_status_data_age <- marital_status_data_named %>%
      filter(!is.na(age_range)) %>%
      mutate(
        age_range = str_replace(age_range,"18 and 19 years","18 to 19 years"),
        age_range = str_replace(age_range,"85 years and over","85 to 94 years"),  #have to norm it when calculating...
        first_age = as.numeric(substr(age_range,1,2)),
        last_age = as.numeric(substr(age_range,7,8)),
        age_range_length = last_age-first_age+1) %>%
      group_by(tract,sex,age_range,marital_status,spouse_present, .drop=T) %>%
      mutate(
        tract_marital_sex_age = sum(number_sams/total_tract_pop),
        tract_marital_sex_year = tract_marital_sex_age/age_range_length) %>%
      uncount(age_range_length,.id="age_") %>%
      mutate(age=first_age+age_) %>%
      ungroup() %>%
      select(-first_age,-last_age,-tract_marital_sex_age, -race, -age_range)
#for each age, it has: sex, spouse_present, marital_status    

    marital_status_data_race <- marital_status_data_named %>%
      group_by(tract,sex,race,marital_status,spouse_present, .drop=T) %>% 
      mutate(tract_marital_sex_race = number_sams/total_tract_pop) %>% # if_else(!is.na(age_range),number_sams,0),
      filter(race %in% acs_race_codes) %>%
      uncount(95,.id = "age") %>%
      ungroup() 
    
    marital_status_age_race_combinations <- left_join(marital_status_data_age,marital_status_data_race,by=c("tract","sex","age","marital_status","spouse_present"),suffix=c("_age","_race"))
      
    sam_marital_age_race <- left_join(sam_sex_race_age,marital_status_age_race_combinations,by=c("tract","sex","age"),suffix=c("_sam","_marital")) %>%
      test <- sam_marital_age_race %>%
      mutate(
        prob = tract_marital_sex_race*tract_marital_sex_year*total_tract_pop_race,
        cull = if_else(prob>0 & prob<1 & !is.na(prob),sample(c("keep","cut"),1,c(prob,1-prob),replace = FALSE),"cut")
        ) %>%
      filter(cull=="keep")
      

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

pregnancy_data <- pregnancy_data_race_marriage_from_census %>%
  mutate(sex="Female",label = str_remove_all(label,"Estimate!!Total!!")) %>% 
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

base_pregnant_join <- full_join(base_married, pregnancy_data,by=c("tract","sex","race","age_range","marital_status"),suffix = c("_base2", "_pregnant"))  

base_pregnant <- base_pregnant_join %>%
  mutate(number_sams= if_else(is.na(number_sams_pregnant),number_sams_base,number_sams_pregnant))

#now expand on each row that doesn't have a total
test <- uncount(base_group_quarters_data,base_group_quarters_data$number_sams,.id="ind_id") #have to do below for individual ids to work across all
test2 <- uncount(base_married,base_married$number_sams,.id="ind_id") #test has 3,627,000 - have to think through a bit more....     
test3 <- uncount(base_pregnant,base_pregnant$number_sams[1],.id="ind_id") ##invalid times argument????

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
  
  return(sam_residents)
}

#' createHouseholds
#'
#' This function forms housholds using the indivials information based on census data.
#' If simulated data already exists, it will read it from an RDS file.
#'
#' @return sam_residents A dataframe of simulated people updated with information about households
createHouseholds <- function(sam_residents) {
  
  #Create or read in individuals with household information
  if(householdsFromRDS) {
    # import saved citzens from RDS file
    households_data_file <- paste0(censusdir, vintage,"/Households_data.RDS")
    sam_residents <- readRDS(households_data_file)
    print(sprintf("Done reading individuals with households information RDS from %s", households_data_file))
  } else {
    
    #get the census key
    censuskey <- readLines(paste0(censusdir, vintage, "/key"))
    
    #gather information from census data,
    #Household - type by size - tells you if it's a family or non-family household, but no race - need to tidy the data
    household_type_size_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11016")
    #Household - multigenerational B11017 all NA
    # poverty - type - number of persons in HH
    household_poverty_people_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17013")
    #above just says how many people below or above poverty, but number of people and type of HH is given - something... match on family_type of Head of Household, but have to already have households
    #  type - relatives -just tells you if they live with additional non-relatives, and gives race
    household_relatives_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11002")
    #type by age of HH 
    household_age_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17017")
    #type by education of HH B17018
    household_educ_level_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17018")
    
    
    
    #gets per_capita by race per tract
    per_capita_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19301")
    
    #income median by race B19013
    race_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19013")
    
    #AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS FOR FAMILIES BY FAMILY TYPE -only below poverty... too complicated to unwind and explain
    agg_deficit_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17011")
    
    #by number of children and poverty B17023
    number_children_poverty_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17023")
    
    #by tenure B17019 (whether renter or not)
    household_tenure_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17019")
    
    #family data - family type by employment status B23007
    family_employment_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B23007")
    
    #foodstamps B22005 race of HH
    food_stamps_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B22005")
    
    #unmarried partner household by sex of partner B11009 (2015 - not sure who might be married now)
    unmarried_partner_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11009")
    
    #occupation by earnings B24121 /122 is male /123 is female /124 is total? ALL NAs
    occupation_earnings_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B24124")
  }
}



