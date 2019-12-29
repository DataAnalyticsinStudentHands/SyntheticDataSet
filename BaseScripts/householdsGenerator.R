library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
#library(rio) #may not need here - playing with it
library(FactoMineR)
library(doParallel)
library(foreach)
#for distance calculation ? may do as own calculation instead
#library(stats)

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
    
    #American community survey 1yr variables: https://api.census.gov/data/2018/acs/acs1/variables.html
    #American community survey 1yr variables: https://api.census.gov/data/2018/acs/acs5/variables.html
    
    #setup race codes https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
    acs_race_codes <- c("A","B","C","D","E","F","G") #could collect all - add them up, without H and I, and you get the total! H is white alone, not hispanic and I is hispanic and if you add them up they don't equal white alone
    
    #gather information from census data, group B01001 will give us gender, race, age
    
    
    
    
    pov_ratio_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05009") #count of kids with family income less than pov.
    
    moved_1yr_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07001")
    moved_1yr_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07003")
    moved_1yr_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07004")
    moved_1yr_citizen_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07007")
    moved_1yr_marital_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07008")
    moved_1yr_education_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07009")
    moved_1yr_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07010")
    transport_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08006")
    transport_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08101")
    transport_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08105")
    transport_language_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08113")
    transport_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08119")
    transport_occupation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08124")
    transport_industry_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08126")
    transport_time_work_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08134")
    when_go_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08011")
    time_to_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08012")
    vehicles_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08014")
    vehicles_workers_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08203")
    transport_tenure_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08137")
    vehicles_household_size_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08201")
    household_workers_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08202")
    kids_family_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09002")
    kids_family_type_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09005")
    kids_unmarried_partner_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09008")
    kids_SSI_household_type_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09010")
    kids_to_householder_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09018")
    kids_age_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10001")
    kids_respons_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10002")
    kids_respons_grands_time_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10050")
    kids_respons_grands_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10051")
    kids_respons_grands_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10057")
    kids_respons_grands_disability_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10052")
    kids_respons_grands_nativity_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10053")

    
    #housing - have to keep an idea of housing/household separate from individual counts...
    housing_units_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25001") #total number
    housing_occupancy_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25002") #gives vacant vs. occup
    housing_occup_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25003") #of occup, own or rent by race
    housing_units_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25032") #units in structure by race
    housing_occup_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25007") #of occup, own or rent by age
    housing_occup_date_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25026") #of occup, own or rent by move in date
    housing_occup_hhsize_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25009") #of occup, own or rent by household size
    housing_occup_hhtype_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25011") #of occup, own or rent by household type
    housing_occup_educ_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25013") #of occup, own or rent by educ attainment
    housing_occup_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25118") #of occup, own or rent by income
    housing_occup_rooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25020") #of occup, number of rooms
    housing_occup_bedrooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25042") #of occup, number of bedrooms
    housing_per_room_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25014") #ratio per tract occup per/room/race
    housing_per_room_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25015") #ratio per tract occup per/room/race
    mortgate_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25027") 
    #gross rent is contract plus estimate for utilities, etc.
    gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25063")
    contract_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25056")
    bedrooms_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25068")
    income_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25122")
    income_value_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25121")
    #stopped before health insurance - B27001
    
    test <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B26101")
    
    
    
    #sample instead of percentage
    #using acs_race_codes you get correct total number - so have to think through assigning latino heritage, etc.
    #sex by age and race gets you a base by tract - everything else will be matched to it in some way
    #key is to get the sample so that it's the same size as the thing matching, by whatever piece you want to match 
    sex_by_age_race_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B01001")
    sex_by_age_race_data <- sex_by_age_race_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% #clean up label 
      pivot_longer(4:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams") %>%    
      mutate(race = substr(name,7,7),
             white = if_else(race=="A",1,0),  #add these for PCA dimensions #white includes latino and hispanic, as do parts of other categories
             black = if_else(race=="B",1,0),
             american_indian = if_else(race=="C",1,0),
             asian = if_else(race=="D",1,0),
             pacific_islander = if_else(race=="E",1,0),
             other_race = if_else(race=="F",1,0),
             bi_racial = if_else(race=="G",1,0)
      ) %>%
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
      ##do uncount
    
    #expand into sam
    sam_sex_race_age <- uncount(sex_by_age_race_data,number_sams,.id = "sams_id") %>% # should equal 4525519 per B10001 row 166 total in 2017; 4602523 in 2018;
      rowwise() %>%
      mutate(
        age=as.numeric(sample(as.character(first_age:last_age),1,prob = rep(1/age_range_length,age_range_length),replace = FALSE))
      )
    
    #may have better way of dealing with age numbers...
    #add age_range to work for merge with marital data, uses data.table to make this fast
    sam_sex_race_age_DT <- as.data.table(sam_sex_race_age)
    sam_sex_race_age_DT[, age_range_marital := c("not of age",
                                                 "15 to 17 years",
                                                 "18 to 19 years",
                                                 "20 to 24 years",
                                                 "25 to 29 years",
                                                 "30 to 34 years",
                                                 "35 to 39 years",
                                                 "40 to 44 years",
                                                 "45 to 49 years",
                                                 "50 to 54 years",
                                                 "55 to 59 years",
                                                 "60 to 64 years",
                                                 "65 to 74 years",
                                                 "75 to 84 years",
                                                 "85 to 104 years")[1 +
                                                                      1 * (age >= 15 & age <= 17) + 
                                                                      2 * (age >= 18 & age <= 19) +
                                                                      3 * (age >= 20 & age <= 24) +
                                                                      4 * (age >= 25 & age <= 29) +
                                                                      5 * (age >= 30 & age <= 34) +
                                                                      6 * (age >= 35 & age <= 39) +
                                                                      7 * (age >= 40 & age <= 44) +
                                                                      8 * (age >= 45 & age <= 49) +
                                                                      9 * (age >= 50 & age <= 54) +
                                                                      10 * (age >= 55 & age <= 59) +
                                                                      11 * (age >= 60 & age <= 64) +
                                                                      12 * (age >= 65 & age <= 74) +
                                                                      13 * (age >= 75 & age <= 84) +
                                                                      14 * (age >= 85 & age <= 104) ]
                        ]
    sam_sex_race_age <- sam_sex_race_age_DT
    
    #ugh get 4,194,975 in 2017 - so missing 330,554 - some in 7 or more 41220 in GQ? has right size for number of households total!!!
    #has right number of householders / households
    household_type_size_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11016") 
    household_type_size_data <- household_type_size_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_size_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","number_in_family"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(number_in_family)) %>%
      mutate(nonfamily = if_else(str_detect(family,"Nonfamily"),TRUE,FALSE),
             numeric_in_family = as.numeric(substr(number_in_family,1,1))) %>%
      uncount(number_sams,.id = "family_id",.remove = TRUE) %>% #has right size for number of households total!!! maybe family / non-family mixed don't add in??
      uncount(numeric_in_family,.id="num_family_id",.remove = FALSE)  
    
    #this gives exact number (4525519)
    household_type_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09019") 
    household_type_relation_data <- household_type_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("group_or_hh","family_or_non","relative","family_role","living_alone"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
             group_quarters = if_else(str_detect(group_or_hh,"group"),TRUE,FALSE),
             nonfamily = if_else(str_detect(family_or_non,"nonfamily"),TRUE,FALSE),
             #sex = if_else(str_detect(family_role,"ale"),if_else(!is.na(living_alone),family_role,'del'),'none'),
             sex = if_else(str_detect(family_role,"ale"),family_role,'none'),
             sex = if_else(is.na(sex),'none',sex),
             family_role = if_else(group_quarters,"in_group_quarters", #you can't define family_role in series...(except that is.na)
                if_else(relative=="Child" | relative=="Nonrelatives",if_else(is.na(family_role),'del',family_role),
                if_else(relative=="Householder",relative,family_role))),
             family_role = if_else(is.na(family_role),relative,family_role),
             del = if_else(sex == "Male" | sex == "Female" & is.na(living_alone),TRUE,if_else((sex == "Male" | sex == "Female") & nonfamily,TRUE,FALSE)),
             del = if_else(family_role=="del",TRUE,del)
             ) %>% #want each role to have 786 before uncount
      filter(!del) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "role_id") #not sure why it needed as.numeric this time, but still works on filter above...
    
    #adults and kids gets you right(ish) total (20,000, depending on using seniors from inside adults or separately)
    household_seniors_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09020")
    household_seniors_relation_data <- household_seniors_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_seniors_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("group_or_hh","family_or_non","relative","family_role","living_alone"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        group_quarters = if_else(str_detect(group_or_hh,"group"),TRUE,FALSE),
        nonfamily = if_else(str_detect(family_or_non,"nonfamily"),TRUE,FALSE),
        #sex = if_else(str_detect(family_role,"ale"),if_else(!is.na(living_alone),family_role,'del'),'none'),
        sex = if_else(str_detect(family_role,"ale"),family_role,'none'),
        sex = if_else(is.na(sex),'none',sex),
        family_role = if_else(group_quarters,"in_group_quarters", #you can't define family_role in series...(except that is.na)
                              if_else(relative=="Child" | relative=="Nonrelatives",if_else(is.na(family_role),'del',family_role),
                                      if_else(relative=="Householder",relative,family_role))),
        family_role = if_else(is.na(family_role),relative,family_role),
        del = if_else(sex == "Male" | sex == "Female" & is.na(living_alone),TRUE,if_else((sex == "Male" | sex == "Female") & nonfamily,TRUE,FALSE)),
        del = if_else(family_role=="del",TRUE,del)
      ) %>% #want each role to have 786 before uncount
      filter(!del) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "sr_role_id")
    
    #this gets you seniors, too - slightly different totals from one with just seniors, for some reason
    household_adults_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09021")
    household_adults_relation_data <- household_adults_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_adults_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("age_range","relation_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(relation_hh)) %>%
      uncount(as.numeric(number_sams),.id = "family_id",.remove = TRUE) 
    
    #gives by race - seems superior to own_kids dataset
    household_related_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11004")
    household_related_kids_data <- household_related_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_related_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_or_non","family_role","related_kids","age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        race = substr(name,7,7),
        family_role = if_else(family_role=="Other family",related_kids,family_role),
        related_kids = if_else(family_role=="Other family",age,related_kids),
        age = if_else(family_role=="Other family",
                      if_else(str_detect(related_kids,"No related"),related_kids,
                              if_else(str_detect(family_role,"No related"),family_role,age)),
                      if_else(str_detect(family_role,"No related"),family_role,related_kids))
      ) %>% #want each role to have 786 before uncount
      filter(!is.na(age)) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "related_kids_id")
    
    household_type_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11001") #vgl. B25006??
    household_type_race_data <- household_type_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_or_non","family_type","family_role"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        race = substr(name,7,7),
        family_role = if_else(family_type=="Other family",family_role,family_type)
      ) %>% 
      filter(!is.na(family_role)) %>%
      filter(number_sams > 0 & race %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "hh_type_race_id")  #gives 1562813, which is same as householders in household_type_relation_data
    
#doesn't seem to add anything more than above
    household_related_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11002") 
    
    
    household_own_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11003") 
    household_own_kids_data <- household_own_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_own_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_type","family_role","own_kids","age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        own_kids = if_else(family_type=="Other family",own_kids,family_role),
        family_role = if_else(family_type=="Other family",family_type,family_role),
        age = if_else(family_type=="Other family",
                      if_else(str_detect(family_role,"No own"),family_role,
                              if_else(str_detect(own_kids,"No own"),own_kids,age)),
                      if_else(str_detect(family_role,"No own"),family_role,own_kids))
      ) %>% 
      filter(!is.na(age)) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "hh_own_kids_id") #gives 1420205 - which is 140k short, but could add with related kids?
    
    #just gives numbers of households with seniors - could randomly assign among those with householders of certain types?
    #gives a total of 4279825 - not quite 300k missing? Sure that nothing is wrong on my side....
    household_type_seniors_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11007")
    household_type_seniors_data <- household_type_seniors_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_seniors_from_census),names_to = "tract", values_to = "number_sams") %>% 
      mutate(sr_present = if_else(str_detect(label,"no people 65"),FALSE,TRUE)) %>%
      uncount(as.numeric(number_sams),.id = "sr_present_id",.remove = TRUE)
    
    #definitely use with sample - gives unmarried partners, straight and same-sex, and has others in amount to make for married - all within non-family households? 
    household_type_partners_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11009") #only unmarried but same sex included
    household_type_partners_data <- household_type_partners_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Unmarried-partner households") %>%
      pivot_longer(4:ncol(household_type_partners_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("unmarried","partner_type"), sep = "!!", remove = F, convert = FALSE) %>%
      uncount(as.numeric(number_sams),.id = "partner_id",.remove = TRUE)
    
    #tells only if household is in single structure or complex - also worth adding, and gets correct number of 1562813
    household_type_units_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11011")
    household_type_units_data <- household_type_units_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>% 
      filter(label != "Nonfamily households") %>%
      pivot_longer(4:ncol(household_type_units_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_type","family_role","structs","num_structures"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        num_structures = if_else(family_role=="Other family",num_structures,if_else(family_type=="Nonfamily households",family_role,structs)),
        family_role = if_else(family_role=="Other family",structs,if_else(family_type=="Nonfamily households",family_type,family_role))
      ) %>% 
      filter(!is.na(num_structures)) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "hh_units_id")

#didn't finish list - go back to start of marital, right after B11011
    
    
    #can we join original place to the hispanic data? 
    #combine all place_born_data into one, then match to sex_by_age
    #unique(place_born) - "Born in state of residence" "Born in other state in the United States" "Native; born outside the United States"   "Foreign born" 
    origin_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05006") #has PR at bottom
    origin_data <- origin_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(origin_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("origin_continent","origin_area","origin_region","origin_country"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(origin_country = if_else(is.na(origin_country),if_else(str_detect(origin_area,"n.e.c") | 
                                       origin_area=="Fiji",origin_area,origin_region),origin_country)) %>%
      filter(number_sams > 0 & !is.na(origin_country)) %>%
      uncount(as.numeric(number_sams),.id = "origin_id")
    
    place_born_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06001")
    #correct if do by either age or by category, but 465645 short if do by agesxcategories!! all missing from foreign born, under age - foreign born by total is right
    #could redistribute foreign_born by age category and total foreign_born?? excel B06001 work shows calculations.
    #means that this cannot be used without checking for other years / counties
    place_born_age_data <- place_born_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","age_range"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(as.numeric(number_sams) > 0 & !is.na(age_range)) %>%
      mutate(number_sams = if_else(place_born=="Foreign born",round(number_sams*1.6564,digits = 0),number_sams)) %>% #fixing mistake in census data, but guessing it was consistent and not just a single age_group, etc.; gives 1 too many total
      uncount(as.numeric(number_sams),.id = "place_born_age_id") %>%
      mutate(del = if_else(number_sams == max(number_sams) & place_born == "Foreign born" & place_born_age_id == number_sams,TRUE,FALSE)) %>%
      filter(!del) #taking risk in future that there's only one
    
    #correct totals
    place_born_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06004")
    place_born_race_data <- place_born_race_from_census %>% #right total - 4525519
      mutate(label = str_remove_all(label,"Estimate!!Total!!"),
             race = substr(name,7,7)) %>%
      filter(label != "Estimate!!Total" & race %in% acs_race_codes) %>%
      rename(place_born = label) %>%
      pivot_longer(4:ncol(place_born_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
      filter(number_sams > 0 & !is.na(race)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_race_id")
    
    #population 15 yrs and over - 3223758 / doesn't match age_categories from other place_born groups
    income_place_born_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06010") #PR at top
    income_place_born_data <- income_place_born_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(income_place_born_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","has_income","income_range"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(income_range = if_else(has_income=="No income","$0",income_range)) %>%
      filter(number_sams > 0 & !is.na(income_range)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_income_id")
    
    #numbers don't match others, but are internally consistent for the dataset - have to think about whether worth fudging in join
    place_born_language_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06007")
    place_born_language_data <- place_born_language_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_language_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","language_at_home","English_proficiency"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(English_proficiency = if_else(language_at_home=="Speak only English","Only English Speaker",English_proficiency)) %>%
      filter(number_sams > 0 & !is.na(English_proficiency)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_language_id")
    
    #population 15 and over - 3560318
    place_born_marital_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06008")
    place_born_marital_data <- place_born_marital_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_marital_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","marital_status"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(marital_status)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_marital_id")
    
    #population of 25 and over: 2923369 folks - count all people over 25 in place_born_age you get 2860024 (63345 too many in educ?)
    place_born_education_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06009")
    place_born_education_data <- place_born_education_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_education_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","educational_status"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(educational_status)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_education_id")
    
    #unique on date_entered: "Entered 2010 or later" "Entered before 1990"   "Entered 2000 to 2009"  "Entered 1990 to 1999"
    #hard to line up with exact, but total comes close to foreign_born in place_born (24892 off)
    place_period_citizen_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05007")
    place_period_citizen_data <- place_period_citizen_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_period_citizen_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("origin_area","origin_region","origin_country","date_entered","citizen"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate( #citizen = if_else(str_detect(date_entered,"citizen"),date_entered,if_else(str_detect(origin_country,"citizen"),origin_country,citizen)),
        citizen = if_else(str_detect(origin_country,"citizen"),origin_country,if_else(str_detect(date_entered,"citizen"),date_entered,citizen)),
             date_entered = if_else(str_detect(origin_country,"Entered"),origin_country,if_else(str_detect(origin_region,"Entered"),origin_region,date_entered)),
             origin_country = if_else(str_detect(origin_country,"Entered"),origin_region,if_else(str_detect(origin_region,"Entered"),origin_area,origin_country))) %>%
      filter(number_sams > 0  & !is.na(citizen)) %>% #
      uncount(as.numeric(number_sams),.id = "place_born_when_id")
    
    sex_place_when_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05008")
    #right number - 119971 
    sex_place_when_data <- sex_place_when_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(sex_place_when_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","origin_area","origin_region","origin_country","date_entered"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(date_entered = if_else(str_detect(origin_region,"Entered") & origin_area!="Latin America",origin_region,
                                    if_else(str_detect(origin_country,"Entered"),origin_country,date_entered)),
             origin_country = if_else(str_detect(origin_region,"Entered") & origin_area!="Latin America",origin_area,
                                      if_else(str_detect(origin_country,"Entered"),origin_region,date_entered)),
             origin_region = if_else(str_detect(origin_region,"Entered") & origin_area!="Latin America",origin_area,origin_country)
             ) %>%
      filter(number_sams > 0 & !is.na(date_entered)) %>% #
      uncount(as.numeric(number_sams),.id = "sex_place_when_id")
    
    kids_place_citizen_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05009")
    #this is a challenging one to include - how many parents are foreign born and how to make a household that matches, for just 10k cases!
    kids_place_citizen_data <- kids_place_citizen_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_place_citizen_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("kid_age_range","both_parents","parent_nativity","kid_nativity"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(kid_nativity = if_else(str_detect(parent_nativity,"Child"),parent_nativity,kid_nativity),
             kid_nativity = if_else(str_detect(both_parents,"Child"),both_parents,kid_nativity),
             parent_nativity = if_else(str_detect(parent_nativity,"Child"),both_parents,parent_nativity),
             parent_nativity = if_else(str_detect(parent_nativity,"Living"),kid_nativity,parent_nativity)) %>%
      filter(number_sams > 0 & !is.na(kid_nativity)) %>% 
      uncount(as.numeric(number_sams),.id = "kids_place_id")
    
    #sex and age for foreign born population - matches with place_period_citizen_data
    sex_nativity_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05013")
    sex_nativity_age_data <- sex_nativity_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(sex_nativity_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(age_range)) %>% 
      uncount(as.numeric(number_sams),.id = "sex_nativity_age_id")
 
    #UGH - probably not worth it!! 
    #https://www.census.gov/topics/population/ancestry/about/faq.html
    mult_ancestry_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B04005")
    mult_ancestry_data <- mult_ancestry_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mult_ancestry_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("multiple_ancestry","sm_area"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(multiple_ancestry) & is.na(sm_area)) 
    
    #people reporting either only or as part of multiple ethnicity - not sure how to code to individuals
    ancestry_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B04006")
    ancestry_data <- ancestry_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(ancestry_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("ancestry","sm_area"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(ancestry) & is.na(sm_area))
    
    #think about how to match with Hispanic number from original sex_age??
    latino_origin_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B03001")
    latino_origin_data <- latino_origin_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(latino_origin_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("latinx","origin_country","origin_country2"), sep = "!!", remove = F, convert = FALSE) %>%
      #mutate(origin_country = if_else(!is.na(origin_country2),))
      mutate(origin_country = if_else(!is.na(origin_country2),
                                      if_else(origin_country=="South American" | origin_country=="Central American",origin_country2, #should remain NA
                                      origin_country),origin_country)) %>%
      filter(!is.na(origin_country) | latinx == "Not Hispanic or Latino") %>%
      rename(census_group_name = name) %>% 
      uncount(as.numeric(number_sams),.id = "latinx_id")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     
    sex_by_age_race_data <- sex_by_age_race_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% #clean up label 
      pivot_longer(4:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams") %>%    
      mutate(race = substr(name,7,7),
             white = if_else(race=="A",1,0),  #add these for PCA dimensions
             black = if_else(race=="B",1,0),
             american_indian = if_else(race=="C",1,0),
             asian = if_else(race=="D",1,0),
             pacific_islander = if_else(race=="E",1,0),
             other_race = if_else(race=="F",1,0),
             bi_racial = if_else(race=="G",1,0)
            ) %>%
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
             hispanic = if_else(hispanic_id == 2, 1, 0),
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
      
      
  
#could try to get race by age_range from above, then use that for calculating if folks are married??
    
    #get marriage data
    marital_status_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B12002")
    
    marital_status_data <-  marital_status_data_from_census %>%
      mutate(census_whole := rowSums(.[4:ncol(marital_status_data_from_census)])) %>%
      pivot_longer(4:ncol(marital_status_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      group_by(tract) %>%
      mutate(race = substr(name,7,7),  #to make race work properly, would have to go by totals - and do the allocation again... there's something weird about the allocation
             pop_A := if_else(race=='A' & label == "Estimate!!Total",number_sams,0),
             pop_B := if_else(race=='B' & label == "Estimate!!Total",number_sams,0),
             pop_C := if_else(race=='C' & label == "Estimate!!Total",number_sams,0),
             pop_D := if_else(race=='D' & label == "Estimate!!Total",number_sams,0),
             pop_E := if_else(race=='E' & label == "Estimate!!Total",number_sams,0),
             pop_F := if_else(race=='F' & label == "Estimate!!Total",number_sams,0),
             pop_G := if_else(race=='G' & label == "Estimate!!Total",number_sams,0),
             pop_tract := if_else(race=='_' & label == "Estimate!!Total",number_sams,0),
             total_tract_pop := sum(pop_tract),
             A_percent_pop := sum(pop_A)/total_tract_pop,
             B_percent_pop := sum(pop_B)/total_tract_pop,
             C_percent_pop := sum(pop_C)/total_tract_pop,
             D_percent_pop := sum(pop_D)/total_tract_pop,
             E_percent_pop := sum(pop_E)/total_tract_pop,
             F_percent_pop := sum(pop_F)/total_tract_pop,
             G_percent_pop := sum(pop_G)/total_tract_pop,
             label := str_remove_all(label,"Estimate!!Total!!")
            ) %>%
      separate(label, into = c("sex", "part2", "part3", "part4","part5"), sep = "!!", remove = T) %>%  
      mutate(age_range = case_when(str_detect(part2, "year") ~ part2,
                                   str_detect(part3, "year") ~ part3,
                                   str_detect(part4, "year") ~ part4,
                                   str_detect(part5, "year") ~ part5),
             spouse_present = case_when(str_detect(part2, "spouse") ~ part2,
                                        str_detect(part3, "spouse") ~ part3,
                                        str_detect(part4, "spouse") ~ part4),
             spouse_separated = case_when(str_detect(part2, "Separated") ~ part2,
                                   str_detect(part3, "Separated") ~ part3,
                                   str_detect(part4, "Separated") ~ part4,
                                   str_detect(part5, "Separated") ~ part5),
             marital_status = case_when(str_detect(part2,"Never married") ~ part2, 
                                        str_detect(part3,"Never married") ~ part3,
                                        str_detect(part4,"Never married") ~ part4,
                                        str_detect(part2,"Now married") ~ part2, 
                                        str_detect(part3,"Now married") ~ part3,
                                        str_detect(part4,"Now married") ~ part4,
                                        str_detect(part2,"Separated") ~ part2, 
                                        str_detect(part3,"Separated") ~ part3,
                                        str_detect(part4,"Separated") ~ part4,
                                        str_detect(part5,"Separated") ~ part5,
                                        str_detect(part2,"Widowed") ~ part2,
                                        str_detect(part3,"Widowed") ~ part3,
                                        str_detect(part4,"Widowed") ~ part4,
                                        str_detect(part2,"Divorced") ~ part2,
                                        str_detect(part3,"Divorced") ~ part3,
                                        str_detect(part4,"Divorced") ~ part4
             )
      ) %>% 
      select(-starts_with("part"),-concept,-starts_with("pop_"))
      
      joined_tract_sam <- sam_sex_race_age %>%
        group_by(tract, sex) %>%
        mutate( 
          sex_by_whole := marital_status_data[which(marital_status_data$sex == sex &
                                                      is.na(marital_status_data$age_range) &
                                                      marital_status_data$race == '_' &
                                                      is.na(marital_status_data$marital_status))[1],"census_whole"][[1]],
          widowed_by_tract := marital_status_data[which(marital_status_data$tract == tract & 
                                                        is.na(marital_status_data$age_range) & 
                                                        marital_status_data$sex == sex &
                                                        marital_status_data$race == '_' &
                                                        marital_status_data$marital_status == "Widowed")[1],"number_sams"][[1]],
          widowed_by_tract := if_else(is.na(widowed_by_tract),0,widowed_by_tract),
          widowed_by_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                          is.na(marital_status_data$age_range) &  
                                                          marital_status_data$sex == sex &
                                                          marital_status_data$race == '_' &
                                                          marital_status_data$marital_status == "Widowed")[1],"census_whole"][[1]],
          widowed_by_whole := if_else(is.na(widowed_by_whole),0,widowed_by_whole),
          divorced_by_tract := marital_status_data[which(marital_status_data$tract == tract & 
                                                          is.na(marital_status_data$age_range) &  
                                                           marital_status_data$sex == sex &
                                                           marital_status_data$race == '_' &
                                                          marital_status_data$marital_status == "Divorced")[1],"number_sams"][[1]],
          divorced_by_tract := if_else(is.na(divorced_by_tract),0,divorced_by_tract),
          divorced_by_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                           is.na(marital_status_data$age_range) &  
                                                           marital_status_data$sex == sex &
                                                           marital_status_data$race == '_' &
                                                           marital_status_data$marital_status == "Divorced")[1],"census_whole"][[1]],
          divorced_by_whole := if_else(is.na(divorced_by_whole),0,divorced_by_whole),
#separated is all weird - not sure how to deal with it, but it's not in same way as others....
          separated_by_tract := marital_status_data[which(marital_status_data$tract == tract & 
                                                           is.na(marital_status_data$age_range) &  
                                                           marital_status_data$sex == sex &
                                                           marital_status_data$race == '_' &
                                                           marital_status_data$marital_status == "Separated")[1],"number_sams"][[1]],
          separated_by_tract := if_else(is.na(separated_by_tract),0,separated_by_tract),
          separated_by_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                           is.na(marital_status_data$age_range) &  
                                                           marital_status_data$sex == sex &
                                                           marital_status_data$race == '_' &
                                                           marital_status_data$marital_status == "Separated")[1],"census_whole"][[1]],
          separated_by_whole := if_else(is.na(separated_by_whole),0,separated_by_whole),
          never_married_by_tract := marital_status_data[which(marital_status_data$tract == tract & 
                                                           is.na(marital_status_data$age_range) &  
                                                             marital_status_data$sex == sex &
                                                             marital_status_data$race == '_' &
                                                           marital_status_data$marital_status == "Never married")[1],"number_sams"][[1]],
          never_married_by_tract := if_else(is.na(never_married_by_tract),0,never_married_by_tract),
          never_married_by_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                                is.na(marital_status_data$age_range) &  
                                                                marital_status_data$sex == sex &
                                                                marital_status_data$race == '_' &
                                                                marital_status_data$marital_status == "Never married")[1],"census_whole"][[1]],
          never_married_by_whole := if_else(is.na(never_married_by_whole),0,never_married_by_whole),
          married_by_tract := marital_status_data[which(marital_status_data$tract == tract & 
                                                          is.na(marital_status_data$age_range) &   
                                                          is.na(marital_status_data$spouse_present) &
                                                          marital_status_data$sex == sex &
                                                          marital_status_data$race == '_' &
                                                          str_detect(marital_status_data$marital_status,"Now married"))[1],"number_sams"][[1]],
          married_by_tract := if_else(is.na(married_by_tract),0,married_by_tract),
          married_by_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                          is.na(marital_status_data$age_range) & 
                                                          is.na(marital_status_data$spouse_present) &
                                                          marital_status_data$sex == sex &
                                                          marital_status_data$race == '_' &
                                                          str_detect(marital_status_data$marital_status,"Now married"))[1],"census_whole"][[1]],
          married_by_whole := if_else(is.na(married_by_whole),0,married_by_whole),
          married_sp_by_tract := marital_status_data[which(marital_status_data$tract == tract & 
                                                          marital_status_data$spouse_present == "Married spouse present" &
                                                          is.na(marital_status_data$age_range) &  
                                                            marital_status_data$sex == sex &
                                                            marital_status_data$race == '_' &
                                                            str_detect(marital_status_data$marital_status,"Now married"))[1],"number_sams"][[1]],
          married_sp_by_tract := if_else(is.na(married_sp_by_tract),0,married_sp_by_tract),
          married_sp_by_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                             marital_status_data$spouse_present == "Married spouse present" &
                                                             is.na(marital_status_data$age_range) &  
                                                             marital_status_data$sex == sex &
                                                             marital_status_data$race == '_' &
                                                             str_detect(marital_status_data$marital_status,"Now married"))[1],"census_whole"][[1]],
          married_sp_by_whole := if_else(is.na(married_sp_by_whole),0,married_sp_by_whole),
          married_sa_by_tract := marital_status_data[which(marital_status_data$tract == tract & 
                                                             marital_status_data$spouse_present == "Married spouse absent" &
                                                             is.na(marital_status_data$age_range) &  
                                                             marital_status_data$sex == sex &
                                                             marital_status_data$race == '_' &
                                                             str_detect(marital_status_data$marital_status,"Now married"))[1],"number_sams"][[1]],
          married_sa_by_tract := if_else(is.na(married_sa_by_tract),0,married_sa_by_tract),
          married_sa_by_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                             marital_status_data$spouse_present == "Married spouse absent" &
                                                             is.na(marital_status_data$age_range) &  
                                                             marital_status_data$sex == sex &
                                                             marital_status_data$race == '_' &
                                                             str_detect(marital_status_data$marital_status,"Now married"))[1],"census_whole"][[1]],
          married_sa_by_whole := if_else(is.na(married_sa_by_whole),0,married_sa_by_whole),
          total_tract_by_census := marital_status_data[which(marital_status_data$tract == tract & 
                                                         marital_status_data$name == "B12002_001E")[1],"number_sams"][[1]],
          total_tract_by_census := if_else(is.na(total_tract_by_census),0,total_tract_by_census)
        )
      #ignore warnings that say: In marital_status_data$tract == tract :
            #longer object length is not a multiple of shorter object length
      
      
      joined_age_married_sam <- joined_tract_sam %>%
        group_by(tract,sex,age_range_marital) %>%
        #to do this properly, would also have all the possible combinations by race - which didn't seem practical.
        mutate( 
          widowed_by_age_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                              marital_status_data$sex == sex &
                                                              marital_status_data$age_range  == age_range_marital & 
                                                          marital_status_data$marital_status == "Widowed")[1],"census_whole"][[1]],
          widowed_by_age_whole := if_else(is.na(widowed_by_age_whole),0,widowed_by_age_whole),
          divorced_by_age_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                               marital_status_data$sex == sex &
                                                               marital_status_data$age_range  == age_range_marital & 
                                                              marital_status_data$marital_status == "Divorced")[1],"census_whole"][[1]],
          divorced_by_age_whole := if_else(is.na(divorced_by_age_whole),0,divorced_by_age_whole),
          separated_by_age_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                               marital_status_data$sex == sex &
                                                               marital_status_data$age_range  == age_range_marital & 
                                                               marital_status_data$marital_status == "Separated")[1],"census_whole"][[1]],
          separated_by_age_whole := if_else(is.na(separated_by_age_whole),0,separated_by_age_whole),
          never_married_by_age_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                                    marital_status_data$sex == sex &
                                                                    marital_status_data$age_range  == age_range_marital & 
                                                               marital_status_data$marital_status == "Never married")[1],"census_whole"][[1]],
          never_married_by_age_whole := if_else(is.na(never_married_by_age_whole),0,never_married_by_age_whole),
          married_by_age_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                              marital_status_data$sex == sex &
                                                              marital_status_data$age_range  == age_range_marital & 
                                                              str_detect(marital_status_data$marital_status,"Now married"))[1],"census_whole"][[1]],
          married_by_age_whole := if_else(is.na(married_by_age_whole),0,married_by_age_whole),
          married_sp_by_age_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                                 marital_status_data$sex == sex &
                                                                 marital_status_data$spouse_present == "Married spouse present" &
                                                              marital_status_data$age_range  == age_range_marital & 
                                                                str_detect(marital_status_data$marital_status,"Now married"))[1],"census_whole"][[1]],
          married_sp_by_age_whole := if_else(is.na(married_sp_by_age_whole),0,married_sp_by_age_whole),
          married_sa_by_age_whole := marital_status_data[which(marital_status_data$tract == tract & 
                                                                 marital_status_data$sex == sex &
                                                                 marital_status_data$spouse_present == "Married spouse absent" &
                                                                 marital_status_data$age_range  == age_range_marital & 
                                                                 str_detect(marital_status_data$marital_status,"Now married"))[1],"census_whole"][[1]],
          married_sa_by_age_whole := if_else(is.na(married_sa_by_age_whole),0,married_sa_by_age_whole),
          married_by_age_whole := married_by_age_whole + married_sa_by_age_whole + married_sp_by_age_whole,  #there's something weird about the whole count so trying to up the numerator
          widowed_by_age := round(widowed_by_tract * (widowed_by_age_whole/widowed_by_whole)),
          divorced_by_age := round(divorced_by_tract * (divorced_by_age_whole/divorced_by_whole)),
          never_married_by_age := round(never_married_by_tract * (never_married_by_age_whole/never_married_by_whole)),
          married_by_age := round((married_by_tract + married_sa_by_tract) * (married_by_age_whole/married_by_whole)),
          married_sp_by_age := round(married_sp_by_tract * (married_sp_by_age_whole/married_sp_by_whole)),
          married_sa_by_age := round(married_sa_by_tract * (married_sa_by_age_whole/married_sa_by_whole)),
          total := widowed_by_age + divorced_by_age + never_married_by_age + married_by_age,
          total_diff := total_tract_by_census - total,
          total_n := n(), #change this to correct bit from census file for total? It doesn't have right total.... problem might be that we're in subgroups that aren't assigned same way, and need to figure out what original numbers per tract are
          total_left := if_else(total_n - total >= 0,total_n - total,0)
          )
      joined_sam_marital <- joined_age_married_sam %>%
        group_by(tract,sex,age_range_marital) %>%
        mutate(
          never_married_by_age := if_else(total_left==0,round(never_married_by_age-((total_n-total)/4)),never_married_by_age), #correct for times that project more than available pop
          divorced_by_age := if_else(total_left==0,round(divorced_by_age-((total_n-total)/4)),divorced_by_age),
          married_by_age := if_else(total_left==0,round(married_by_age-((total_n-total)/4)),married_by_age),
          #separated_by_age := if_else(total_left==0,round(separated_by_age-((total_n-total)/4)),separated_by_age),
          l_marital := length(c(rep("widowed",widowed_by_age[1]),rep("divorced",divorced_by_age[1]),rep("never married",never_married_by_age[1]),
                                rep("married",married_by_age[1]),rep("none",1))), #,rep("none",total_left[1])
          diff := l_marital - total_n,
          marital_status := sample(c(rep("widowed",widowed_by_age[1]),rep("divorced",divorced_by_age[1]),rep("never married",never_married_by_age[1]),
                                     rep("married",married_by_age[1]),rep("none",1)),
                                   total_n[1], replace = TRUE, #size = 1, replace = TRUE,  #not size = total_n[1], replace = FALSE
                                   prob = c(rep((1/l_marital[1]),l_marital[1]))
          ),
          spouse_present := if_else(marital_status=="married",sample(c("married spouse present","married spouse absent or separate")
                                                                     ,size=1,replace = TRUE,prob = c(.65,.35)),
                                    "no spouse")
        ) %>%
        select(-starts_with("total"),-ends_with("_whole"),-ends_with("_tract"),-ends_with("by_age"),-ends_with("by_census"),-l_marital,-census_group_name,-label,-tract_race_year)
    
    saveRDS(joined_sam_marital,"joined_sam_marital.RDS") #temp save
    joined_sam_marital <- readRDS("joined_sam_marital.RDS")
    ##not run
    table(joined_sam_marital$marital_status)
    table(joined_sam_marital$tract, joined_sam_marital$marital_status)
    
     #make group quarters sam residents - sex by age (B26101) is all NA ; marital status (B26104) is all NA; mobility (B26109) is all NA; ed_status (B26109) is all NA
    #very small numbers except in 210100 and 100000 (6099 and 1586) - assuming all in GC not living with spouse, but every other combo possible
    #biggest thing to worry about is inmate population; other GC are not large, as far as I can tell
    #I don't have great confidence that the census is distributing them correctly by census.
    group_quarters_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B26001")
    
    base_group_quarters_data <- group_quarters_data_from_census %>%
      pivot_longer(4:ncol(group_quarters_data_from_census),names_to = "tract", values_to = "number_sams_temp") 
    
    #make into a model based only on the characteristics that are known
    GQ_sam <- uncount(base_group_quarters_data,number_sams_temp,.remove = FALSE,.id="temp_id") %>%
      group_by(tract) %>%
      mutate( #approximated from natl BOP - https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
        GQ_facility_type := if_else(number_sams_temp > 400,"correctional","nursing home"),
        age := if_else(GQ_facility_type == "correctional",
                       sample(c(18:64), #c("18 to 19 years","20 to 24 years","25 to 29 years","30 to 34 years","35 to 44 years","45 to 54 years","55 to 64 years"),
                              replace=TRUE,size = n(),prob=c(rep(.025,2),rep(.018,5),rep(.032,5),rep(.05,5),rep(.024,10),rep(.014,10),rep(.007,10))),
                       sample(c(65:99), #c("65 to 75 years","75 to 85 years","85 years and over"),
                              replace = TRUE,size=n(),prob=c(rep(.01,10),rep(.04,10),rep(.01,15)))), # https://www.cdc.gov/nchs/nsltcp/index.htm
        sex_num := if_else(GQ_facility_type == "correctional",
                       sample(c(0,1), #c('male','female'),
                              replace=TRUE,size = n(),prob=c(.93,.07)),
                       sample(c(0,1), #c('male','female'),
                              replace=TRUE,size = n(),prob=c(.33,.67)),
                       )
      )
    
    sam_marital <- joined_sam_marital %>% #adding sex_num so PCA will have it as numeric
      mutate(
        sex_num := case_when(sex == "Male" ~ 0, sex == "Female" ~1)
      )
    
    sam_marital_GQ <- bind_rows(sam_marital,GQ_sam) #bind them so that the PCA includes GQ by tract 
    sam_marital_DT <- as.data.table(sam_marital_GQ)

    #GQ doesn't include race, so it's imputed by the mean of the variable for the tract - which is reasonable, but not perfect.

    sam_marital_DT[,"white" := if_else(is.na(temp_id),white,mean(white,na.rm = TRUE)),tract]
    sam_marital_DT[,"black" := if_else(is.na(temp_id),black,mean(black,na.rm = TRUE)),tract]
    sam_marital_DT[,"hispanic" := if_else(is.na(temp_id),hispanic,mean(hispanic,na.rm = TRUE)),tract]
    sam_marital_DT[,"asian" := if_else(is.na(temp_id),asian,mean(asian,na.rm = TRUE)),tract]
    sam_marital_DT[,"other_race" := if_else(is.na(temp_id),other_race,mean(other_race,na.rm = TRUE)),tract]
    sam_marital_DT[,"american_indian" := if_else(is.na(temp_id),american_indian,mean(american_indian,na.rm = TRUE)),tract]
    sam_marital_DT[,"pacific_islander" := if_else(is.na(temp_id),pacific_islander,mean(pacific_islander,na.rm = TRUE)),tract]
    sam_marital_DT[,"bi_racial" := if_else(is.na(temp_id),bi_racial,mean(bi_racial,na.rm = TRUE)),tract]
 
#something like this should work - tried lots of variations, but decided to do it by hand, as needed, above       
    add_means <- function(dt,facts){  
      for(fact in facts){
        print(fact)
        print(mean(dt[,..fact][[1]],na.rm = TRUE))
        dt[,(fact) := if_else(is.na(temp_id),fact,mean(dt[,..fact][[1]],na.rm = TRUE)),tract]
      }
      return(dt)
    }

    #for whole, to compare with by tract and to have the GQ_Sam included in the PCA so matching by distance makes sense for whole
    sam_marital_PCA_res <- PCA(sam_marital_DT[,c('age','sex_num','white','black','hispanic','asian','other_race','american_indian','pacific_islander','bi_racial')],scale.unit=TRUE, ncp=3)

    sam_marital_DT[,("harris_coord_1") := sam_marital_PCA_res$ind$coord[,1]] # * sam_marital_PCA_res$eig[1:3,2]/100 #norm coord by percent explained by dim
    sam_marital_DT[,("harris_coord_2") := sam_marital_PCA_res$ind$coord[,2]]
    sam_marital_DT[,("harris_coord_3") := sam_marital_PCA_res$ind$coord[,3]]
   
    saveRDS(sam_marital_DT,"sam_marital_eig.RDS") #temp save
    sam_marital_DT <- readRDS("sam_marital_eig.RDS")

    #general process: 1 - expand census data by tract; 1b - fill (uncount) with averaged so GQ_id, etc is = num_sams; 
              # 2 - create_tract_eigs 
              # 3 - dist - from avg [or median?] temp_person - should fill all with median??
              # 4 - sample - 4b - using is.na(temp_id) etc. ; 4c - normalize by 100 for all distances, and use that as percent 
    
  #initial facts <- c('age','sex_num','white','black','hispanic','asian','other_race','american_indian','pacific_islander','bi_racial')
 
  create_tract_eigs = function(dt,var_name,facts){ 
    for(i in unique(dt$tract)){
      print(i) #not  because it's not handling errors
      pca_res <- PCA(dt[tract==i,..facts],scale.unit=TRUE, ncp=3)  #have to decide if want only three - dim1 by tract is between 18 and 32 var
      dt[tract==i,paste0(var_name,"_eig_1") := pca_res$ind$coord[,1]]
      dt[tract==i,paste0(var_name,"_eig_2") := pca_res$ind$coord[,2]]
      dt[tract==i,paste0(var_name,"_eig_3") := pca_res$ind$coord[,3]]
    #  dt[tract==i,paste0(var_name,"_eig_4") := pca_res$ind$coord[,4]]
    #  dt[tract==i,paste0(var_name,"_eig_5") := pca_res$ind$coord[,5]]
      dt[tract==i,paste0(var_name,"_pve_1") := pca_res$eig[1,2] / 100] #percent variance explained / need to check on factoMineR 
      dt[tract==i,paste0(var_name,"_pve_2") := pca_res$eig[2,2] / 100]
    }
    return(dt)
  }
  sam_race_age_eigs <- create_tract_eigs(sam_marital_DT,"race_age",facts)  #facts <- c('age','sex_num','white','black','hispanic','asian','other_race','american_indian','pacific_islander','bi_racial')
  saveRDS(sam_race_age_eigs,"sam_race_age_eigs.RDS") 
  sam_race_age_eigs <- readRDS("sam_race_age_eigs.RDS")
  
  #calculate euclidean distance to center of each tract (which will be used to match in next step)
  euc_distances = function(dt,vname,facts){ 
    for(i in unique(dt$tract)){
      #uncount to right size with median
      number_sams_added <- sum(dt[tract==i & is.na(temp_id),.N],na.rm = TRUE) - sum(dt[tract==i & !is.na(temp_id),.N],na.rm = TRUE)
      filler_dt <- as.data.table(dt[tract==i & !is.na(temp_id)][1]) 
      if(nrow(filler_dt)>0){
        filler_dt[,"GQ_facility_type" := 'not in Group Quarters']
        center <- dt[tract==i & is.na(temp_id),c(paste0("race_age","_eig_1"),paste0("race_age","_eig_2"),paste0("race_age","_eig_3"))][which.min(abs(race_age_eig_1-median(race_age_eig_1))),]
        filler_dt[,c("race_age_eig_1","race_age_eig_2","race_age_eig_3") := center[,c("race_age_eig_1","race_age_eig_2","race_age_eig_3")]]
        added <- uncount(filler_dt,number_sams_added,.remove = FALSE,.id="temp_id")
        dt <- rbind(dt,added) #does it need to be added for the tract?
        target <- dt[tract==i,c(paste0("race_age","_eig_1"),paste0("race_age","_eig_2"),paste0("race_age","_eig_3"))]
        dt[tract==i,("euc_dist") := sqrt((target[,1]-center[,1][[1]])^2 + (target[,2]-center[,2][[1]])^2 + (target[,3]-center[,3][[1]])^2)]
        dt[tract==i,("prob_euc_dist") := (max(euc_dist) - euc_dist) / max(euc_dist)]
        dt[tract==i & !is.na(temp_id),("norm_prob_euc") := (1-prob_euc_dist / 1) / .N]
      }
    }
    return(dt)
  }
  sam_race_age_eigs_eucs_temp <- euc_distances(sam_race_age_eigs[tract=="410401"],"race_age",facts)
  sam_race_age_eigs_eucs <- euc_distances(sam_race_age_eigs,"race_age",facts)
  
  saveRDS(sam_race_age_eigs_eucs,"sam_race_age_eigs_eucs.RDS") 
  sam_race_age_eigs_eucs <- readRDS("sam_race_age_eigs_eucs.RDS")
  sam_race_age_eigs_eucs[norm_prob_euc==0,("norm_prob_euc") := .000000001]
  
  sample_by_euc = function(dt){ 
    for(i in unique(dt$tract)){
      if(nrow(dt[tract==i & !is.na(temp_id)])){
      #normalize the euc_dist on the !is.na(temp_id) to use as prob paste0(name,"_dist_prob")
      dt[tract==i & is.na(temp_id),("GQ_facility_type") := 
           sample(dt[tract==i & !is.na(temp_id),GQ_facility_type],size = .N,replace = FALSE,
                  prob = c(dt[tract==i & !is.na(temp_id),norm_prob_euc]))] 
      }
    }
    dt_out <- dt[is.na(temp_id)]
    return(dt_out)
  }
  
  sam_GQ <- sample_by_euc(sam_race_age_eigs_eucs)
  sam_GQ[is.na(GQ_facility_type),("GQ_facility_type") := "not in Group Quarters"]
  
  saveRDS(sam_GQ,"sam_GQ.RDS") 
  sam_GQ <- readRDS("sam_GQ.RDS")
  
  
    #give a delivery date? expand by race and by age - then do both as PCAs, then assign age to the ones that have race, and then assign age, race, delivery date?
    
    pregnancy_data_race_marriage_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B13002")
    
    pregnancy_data <- pregnancy_data_race_marriage_from_census %>%
      mutate(sex="Female",label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      mutate(label = str_remove_all(label,"Estimate!!Total"),
        birth = if_else(str_detect(label,"Women who had a birth in the past 12 months!!"),TRUE,FALSE),
        race = substr(name,7,7)) %>%
      pivot_longer(4:ncol(pregnancy_data_race_marriage_from_census),names_to = "tract", values_to = "num") %>%
      separate(label, into = c("birth_label","married","age_range"), sep = "!!", remove = F) %>%
      rename(census_group_name = name) %>%
      #mutate(group_tracts = substr(tract,1,3)) %>% #would let you match by a larger geography, if the census is suppressing
      filter(!is.na(married)) %>%
      filter(!is.na(age_range) | race %in% acs_race_codes) %>%
      uncount(num,.remove = FALSE,.id="temp_id")
      
    preg_data_DT <- as.data.table(pregnancy_data)
    preg_data_DT[,("age_range_race") := sample(rep(.SD[!is.na(age_range),.(age_range)][[1]],2),size = .N,replace = TRUE,
                                                  prob = c(rep(1/.N,.N))),
                 by=.(tract)]
    #if they're equal, then sampling this way should give everyone an age_range according to distribution
    pregnancy_data_DT <- preg_data_DT[race!='_']
    #assign numeric to race, marital_status, etc. then do all the PCA? 
    
    #euc_distance from a norm?  
    
#redoing from logic above, without for loop
    
    sam_GQ[,
           
           ,by=.(tract)] 
    
    
    
  
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



