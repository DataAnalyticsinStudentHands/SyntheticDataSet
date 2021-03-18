library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
#library(rio) #may not need here - playing with it
#library(FactoMineR)
#library(doParallel)
#library(foreach)
#for distance calculation ? may do as own calculation instead
#library(stats)
#census does a process of modeling, based on weighting of responses. Hard to find exactly where they're doing the projections.
#cf. Appendix B, starting at esp. around 103: https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf
#https://usa.ipums.org/ is better in many ways
#https://usa.ipums.org/usa-action/variables/GQTYPE#comparability_section for GQ is helpful, for example

#' exp_census
#'
#' This function creates the expanded census data and runs tests on them
#'
#' @return a list of data.tables with expanded census.
exp_census <- function() {
  #need to break into smaller pieces - not sure best approach for actually programming
  exp_census_data_file <- paste0(censusdir, vintage,"/exp_census.RDS") 
  #Create or read in individual sam residents
  if(file.exists(exp_census)) {
    # import saved sam residents from RDS file
    exp_census <- readRDS(exp_census_data_file)
    print(sprintf("Done reading exp_sam RDS from %s", exp_census_data_file ))
  } else {
    
    #get the census key
    censuskey <- readLines(paste0(censusdir, vintage, "/key"))
    
    #American community survey 1yr variables: https://api.census.gov/data/2018/acs/acs1/variables.html
    #American community survey 5yr variables: https://api.census.gov/data/2018/acs/acs5/variables.html
    #definitions: https://www2.census.gov/programs-surveys/acs/tech_docs/subject_definitions/2018_ACSSubjectDefinitions.pdf
    
    #setup race codes https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
    acs_race_codes <- c("A","B","C","D","E","F","G") #could collect all - add them up, without H and I, and you get the total! H is white alone, not hispanic and I is hispanic and if you add them up they don't equal white alone
    #acs_ethnicity <- c("H","I") #H is White Alone, not Hispanic or Latino; I is Hispanic or Latino #usually just use !acs_race_codes
    
        
    #may have better way of dealing with age numbers...
    #add age_range to work for merge with marital data, uses data.table to make this fast
    #there are tricks that get repeated, which could become subroutines, but you have to make sure the factors match
    #table == table
    #join on id that's been generated so it has same size for total join, even if of different size
    #anti-join to fill in if there are missing rows
    #look for missing id
    #    hh_type_eth_dt <- rbind(hh_type_eth_dt,anti_join(hh_type_race_dt,hh_type_eth_dt,by=c("num_eth_id")),fill = TRUE)
    #and then do id trick 
    #    hh_type_eth_dt[order(match(ethnicity,c("H","I","_"))),  ##can we do it on a sample, there too?
    #                   ("num_eth_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]  #seq.int can also be by sample
    #clean up variables
    #create factor levels
    dec_vintage <- "2010"
    dec_sex_by_age <- decennial_Census_DataFromAPI_byGroupName(censusdir, dec_vintage, state, county, tract, censuskey, groupname = "PCT12")
    dec_race <- decennial_Census_DataFromAPI_byGroupName(censusdir, dec_vintage, state, county, tract, censuskey, groupname = "P3")
    dec_eth <- decennial_Census_DataFromAPI_byGroupName(censusdir, dec_vintage, state, county, tract, censuskey, groupname = "P5")
    #go back to the most detailed individual level without duplication to assign missing pieces from build
    #concept is SEX BY AGE for each race / ethnicity - 4525519 2017 Harris County
    sex_by_age_race_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B01001")
    sex_by_age_race_data <- sex_by_age_race_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!"),
             race = substr(name,7,7)) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE) %>%  #NA warnings in rows with only one; not a problem
      mutate(age_range = str_replace(age_range,"Under 5 years","0  to  4 years"), #have to regularize and make possible to compare
             age_range = str_replace(age_range,"5 to 9 years","05 to  9 years"),
             age_range = str_replace(age_range,"18 and 19 years","18 to 19 years"),
             age_range = str_replace(age_range,"85 years and over","85 to 94 years"),  #have to skew left when assigning.
             first_age = as.numeric(substr(age_range,1,2)),
             last_age = as.numeric(substr(age_range,7,8)),
             age_range_length = last_age-first_age+1
      ) %>%
      filter(number_sams > 0, race %in% acs_race_codes, !is.na(age_range)) %>%
      uncount(number_sams,.id = "sams_id") 
    sex_age_race <- as.data.table(sex_by_age_race_data)
    #assign age here (should decide where to do this):
    sex_age_race[age_range!="85 to 94 years",("age"):= 
                   as.numeric(sample(as.character(first_age[1]:last_age[1]),size=.N,replace = TRUE)),by=.(age_range,sex,race)] 
    sex_age_race[age_range=="85 to 94 years",("age"):=
                   as.numeric(sample(as.character(85:104),size=.N,prob=0.13-(1:20/174:155),replace = TRUE))] #looking for ~1300 centenarians in Houston
    rm(sex_by_age_race_data)
    
    #for Latino population, see: https://www.pewsocialtrends.org/2015/06/11/chapter-7-the-many-dimensions-of-hispanic-racial-identity/
    sex_by_age_ethnicity_data <- sex_by_age_race_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","age_range_29"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(age_range_29 = str_replace(age_range_29,"Under 5 years","0  to  4 years"), #have to regularize and make possible to compare
             age_range_29 = str_replace(age_range_29,"5 to 9 years","05 to  9 years"),
             age_range_29 = str_replace(age_range_29,"18 and 19 years","18 to 19 years"),
             age_range_29 = str_replace(age_range_29,"20 years","20 to 20 years"),
             age_range_29 = str_replace(age_range_29,"21 years","21 to 21 years"),
             age_range_29 = str_replace(age_range_29,"60 and 61 years","60 to 61 years"),
             age_range_29 = str_replace(age_range_29,"65 and 66 years","65 to 66 years"),
             age_range_29 = str_replace(age_range_29,"85 years and over","85 to 94 years"),  #have to skew left when assigning.
             age_range = case_when(
               age_range_29=="20 to 20 years" ~ "20 to 24 years",
               age_range_29=="21 to 21 years" ~ "20 to 24 years",
               age_range_29=="22 to 24 years" ~ "20 to 24 years",
               age_range_29=="35 to 39 years" ~ "35 to 44 years",
               age_range_29=="40 to 44 years" ~ "35 to 44 years",
               age_range_29=="45 to 49 years" ~ "45 to 54 years",
               age_range_29=="50 to 54 years" ~ "45 to 54 years",
               age_range_29=="55 to 59 years" ~ "55 to 64 years",
               age_range_29=="60 to 61 years" ~ "55 to 64 years",
               age_range_29=="62 to 64 years" ~ "55 to 64 years",
               age_range_29=="65 to 66 years" ~ "65 to 74 years",
               age_range_29=="67 to 69 years" ~ "65 to 74 years",
               age_range_29=="70 to 74 years" ~ "65 to 74 years",
               age_range_29=="75 to 79 years" ~ "75 to 84 years",
               age_range_29=="80 to 84 years" ~ "75 to 84 years",
               TRUE ~ age_range_29
             ),
             first_age = as.numeric(substr(age_range_29,1,2)),
             last_age = as.numeric(substr(age_range_29,7,8)),
             age_range_length = last_age-first_age+1,
             ethnicity = substr(name,7,7)) %>% 
      filter(number_sams > 0, !ethnicity %in% acs_race_codes, !is.na(age_range)) %>%
      uncount(as.numeric(number_sams),.id = "sex_age_ethnicity_id")
    #do the id trick so that whitealone, but order ethnicity by H, I and then order race_adj by A,F,G,etc. with percentages of each, then the remaining
    sex_by_age_ethnicity <- as.data.table(sex_by_age_ethnicity_data)
    sex_by_age_ethnicity[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,age_range,sex)]
    sex_by_age_ethnicity[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,age_range,sex)]
    sex_by_age_ethnicity[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE)]
    sex_by_age_eth <- sex_by_age_ethnicity[(tokeep)]
    sex_by_age_eth[age_range_29!="85 to 94 years",("age"):= 
                   as.numeric(sample(as.character(first_age[1]:last_age[1]),size=.N,replace = TRUE)),by=.(age_range_29,sex,ethnicity)] 
    sex_by_age_eth[age_range_29=="85 to 94 years",("age"):=
                   as.numeric(sample(as.character(85:104),size=.N,prob=0.13-(1:20/174:155),replace = TRUE))] #looking for ~1300 centenarians in Houston
    rm(sex_by_age_ethnicity)
    rm(sex_by_age_ethnicity_data)
    rm(sex_by_age_race_data_from_census)
    
    #I think it has one for age and one for race; then join to age_race and join that to hh_type / eth and race, then put eth together?
    #concept:SEX BY MARITAL STATUS BY AGE FOR THE POPULATION 15 YEARS AND OVER == nrow(sex_age_race[age>14]) - 3494885
    marital_status_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B12002")
    marital_status_race_data <- marital_status_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Male") %>%
      filter(label != "Female") %>%
      pivot_longer(4:ncol(marital_status_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("sex","marital_status","spouse_present","separated","age_range_marital"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(race = substr(name,7,7),
             marital_status = case_when(
               marital_status=="Now married (except separated)" ~ "Now married",
               marital_status=="Separated" ~ "Now married",
               TRUE ~ marital_status
             )) %>%
      filter(race %in% acs_race_codes & is.na(age_range_marital)) %>%
      uncount(as.numeric(number_sams),.id = "marital_id",.remove = TRUE)
    marital_status_race_data$spouse_present <- NULL
    marital_status_race_data$separated <- NULL
    marital_status_race_data$age_range_marital <- NULL
    marital_status_race_dt <- as.data.table(marital_status_race_data)
    rm(marital_status_race_data)
    
    marital_status_eth_data <- marital_status_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(marital_status_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("sex","marital_status","spouse_present","separated","age_range_marital"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(ethnicity = substr(name,7,7),
             marital_status = case_when(
               marital_status=="Now married (except separated)" ~ "Now married",
               marital_status=="Separated" ~ "Now married",
               TRUE ~ marital_status
             )) %>%
      filter(!ethnicity %in% acs_race_codes & !is.na(marital_status) & is.na(spouse_present)) %>%
      uncount(as.numeric(number_sams),.id = "marital_id",.remove = TRUE)
    marital_status_eth_dt <- as.data.table(marital_status_eth_data)
    marital_status_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,sex,marital_status)]
    marital_status_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,sex,marital_status)]
    marital_status_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE)]
    marital_status_eth_dt <- marital_status_eth_dt[(tokeep)]
    rm(marital_status_eth_data)
    #marital_status matches race, but spouse_present does not; should use from race!!!!
    
    marital_status_age_data <- marital_status_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(marital_status_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("sex","marital_status","spouse_present","separated","age_range_marital"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(race = substr(name,7,7),
             age_range_marital = case_when(
               str_detect(spouse_present,"years") ~ spouse_present,
               str_detect(separated,"years") ~ separated,
               TRUE ~ age_range_marital
             ),
             spouse_present = case_when(
               str_detect(spouse_present,"present") ~ spouse_present,
               str_detect(spouse_present,"absent") ~ spouse_present,
               TRUE ~ "no spouse"
              ),
             separated = case_when(
                str_detect(separated,"Other") ~ "Other",
                str_detect(separated,"Separated") ~ "Separated",
                TRUE ~ "no absent spouse"
              )) %>%
      filter(race=="_" & !is.na(age_range_marital)) %>%
      uncount(as.numeric(number_sams),.id = "marital_id",.remove = TRUE)
    marital_status_age_dt <- as.data.table(marital_status_age_data)
    rm(marital_status_age_data)
    rm(marital_status_data_from_census)
    
    #concept: WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS x race
    #1169007 - nrow(sex_age_race[age>=15 & age<=50 & sex=="Female"]) = 1165030 (i.e., 3977 women not in sex_age_race) - only doesn't match in oldest group; maybe make them over 50?
    pregnancy_data_race_marriage_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B13002")
    pregnancy_race <- pregnancy_data_race_marriage_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!"),
             sex="Female",
             race = substr(name,7,7)) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(pregnancy_data_race_marriage_from_census),names_to = "tract", values_to = "num") %>%
      separate(label, into = c("birth_label","married","preg_age_range"), sep = "!!", remove = F) %>%
      rename(census_group_name = name) %>%
      filter(!is.na(married)) %>%
      filter(race %in% acs_race_codes) %>%  
      uncount(num,.remove = FALSE,.id="preg_race_id")
    preg_race_dt <- as.data.table(pregnancy_race)
    rm(pregnancy_race)
    
    pregnancy_eth <- pregnancy_data_race_marriage_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!"),
             sex="Female",
             ethnicity = substr(name,7,7)) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(pregnancy_data_race_marriage_from_census),names_to = "tract", values_to = "num") %>%
      separate(label, into = c("birth_label","married","preg_age_range"), sep = "!!", remove = F) %>%
      rename(census_group_name = name) %>%
      filter(!is.na(married) & is.na(preg_age_range)) %>%
      filter(!ethnicity %in% acs_race_codes) %>%  
      uncount(num,.remove = FALSE,.id="preg_race_id")
    preg_eth_dt <- as.data.table(pregnancy_eth)
    preg_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,birth_label,married)]
    preg_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,birth_label,married)]
    preg_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,birth_label,married)]
    preg_eth_dt <- preg_eth_dt[(tokeep)]
    rm(pregnancy_eth)
    
    pregnancy_age <- pregnancy_data_race_marriage_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!"),
             sex="Female",
             race = substr(name,7,7)) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(pregnancy_data_race_marriage_from_census),names_to = "tract", values_to = "num") %>%
      separate(label, into = c("birth_label","married","preg_age_range"), sep = "!!", remove = F) %>%
      rename(census_group_name = name) %>%
      filter(!is.na(married)) %>%
      filter(!is.na(preg_age_range)) %>%  
      uncount(num,.remove = FALSE,.id="preg_age_id")
    preg_age_dt <- as.data.table(pregnancy_age)
    rm(pregnancy_age)
    rm(pregnancy_data_race_marriage_from_census)
    
    #concept:SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER - 
    sex_age_educ_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B15001") 
    sex_age_educ_data <- sex_age_educ_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(sex_age_educ_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("sex","age","education_level"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(education_level) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "sex_age_hheduc_id",.remove = TRUE)
    sex_age_educ_dt <- as.data.table(sex_age_educ_data)
    
    #this gets you adults and seniors, too - slightly different totals from one with just seniors, for some reason, by 3 part age_range
    #concept:LIVING ARRANGEMENTS OF ADULTS 18 YEARS AND OVER BY AGE - have to add in group_quarters to get right total
    household_adults_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09021")
    household_adults_relation_data <- household_adults_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_adults_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("age_range_3","relation_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(relation_hh)) %>%
      uncount(as.numeric(number_sams),.id = "family_id",.remove = TRUE) 
    adults_relations <- as.data.table(household_adults_relation_data)
    rm(household_adults_relation_from_census)
    rm(household_adults_relation_data)
    
    #1223249 matches age_sex_race under 18
    #concept#HOUSEHOLD TYPE FOR CHILDREN UNDER 18 YEARS IN HOUSEHOLDS (EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS)
    household_type_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09005")
    household_type_kids_data <- household_type_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "In family households") %>%
      pivot_longer(4:ncol(household_type_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_or_non","in_family_type"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(family_role_3=(if_else(in_family_type=="In male householder no wife present family","Male householder no wife present",
                                    if_else(in_family_type=="In female householder no husband present family",
                                            "Female householder no husband present",in_family_type)))) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "type_kids_id")
    hh_type_kids <- as.data.table(household_type_kids_data)
    rm(household_type_kids_from_census)
    rm(household_type_kids_data)
    
    #get seniors by role
    #concept: RELATIONSHIP BY HOUSEHOLD TYPE (INCLUDING LIVING ALONE) FOR THE POPULATION 65 YEARS AND OVER
    household_seniors_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09020")
    household_seniors_relation_data <- household_seniors_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "In households") %>%
      filter(label != "In households!!In family households") %>%
      filter(label != "In households!!In nonfamily households") %>%
      filter(label != "In households!!In family households!!Householder") %>%
      filter(label != "In households!!In nonfamily households!!Householder") %>%
      filter(label != "In households!!In nonfamily households!!Householder!!Male") %>%
      filter(label != "In households!!In nonfamily households!!Householder!!Female") %>%
      pivot_longer(4:ncol(household_seniors_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("group_or_hh","family_or_non","relative","sex","living_alone"), sep = "!!", remove = F, convert = FALSE) %>%
      uncount(as.numeric(number_sams),.id = "sr_role_id",.remove = TRUE)
    sr_relations <- as.data.table(household_seniors_relation_data)
    rm(household_seniors_relation_from_census)
    rm(household_seniors_relation_data)
    
    #concept: HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP gives exact number (4525519) - it's a mess in the mutate; should be able to fix
    household_type_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09019") 
    household_type_relation_data <- household_type_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("group_or_hh","family_or_non","relative","role_family","living_alone"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        living_alone = if_else(family_or_non=="In family households" & relative=="Householder","Not living alone",living_alone),
        sex = if_else(str_detect(role_family,"ale")&!is.na(living_alone),role_family,"none"),
        role_in_family = case_when(
          group_or_hh=="In group quarters" ~ "In group quarters", 
          relative=="Child" | relative=="Nonrelatives" ~ role_family,
          relative=="Parent" | relative=="Spouse" | relative=="Parent-in-law" |
            relative=="Son-in-law or daughter-in-law" |
            relative=="Brother or sister" | relative=="Other relatives" |
            relative=="Grandchild" |
            sex!="none" ~ relative,
        )
      ) %>%
      filter(number_sams > 0 & !is.na(role_in_family)) %>%
      uncount(as.numeric(number_sams),.id = "role_id") #not sure why it needed as.numeric this time, but still works on filter above...
    hh_relations_dt <- as.data.table(household_type_relation_data)
    rm(household_type_relation_from_census)
    rm(household_type_relation_data)
    
    #concept:HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS by acs_race_codes
    #population in total gives 4484299 (total population in HH, plus 41220 in group quarters gives 4525519)
    #the totals by race are wonky, though - worse for ethnicity - not using now except for in_family_type
    household_relatives_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11002")
    household_relatives_data <- household_relatives_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_relatives_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("family_or_non","in_family_type","relative_or_non"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(race = substr(name,7,7),
             family_role_3=(if_else(in_family_type=="In male householder no wife present family","Male householder no wife present",
                                    if_else(in_family_type=="In female householder no husband present family",
                                            "Female householder no husband present",in_family_type)))) %>%
      filter(!is.na(relative_or_non) | family_or_non=="In nonfamily households") %>%
      filter(race %in% acs_race_codes) %>% 
      uncount(as.numeric(number_sams),.id = "household_relatives_id",.remove = TRUE)
    household_relatives_race_dt <- as.data.table(household_relatives_data)
    
    household_relatives_eth_data <- household_relatives_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_relatives_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("family_or_non","in_family_type","relative_or_non"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(ethnicity = substr(name,7,7),
             family_role_3=(if_else(in_family_type=="In male householder no wife present family","Male householder no wife present",
                                    if_else(in_family_type=="In female householder no husband present family",
                                            "Female householder no husband present",in_family_type)))) %>%
      filter(!is.na(relative_or_non) | family_or_non=="In nonfamily households") %>%
      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "household_relatives_id",.remove = TRUE)
    household_relatives_eth_dt <- as.data.table(household_relatives_eth_data) 
    #find right one
    household_relatives_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),
                               by=.(tract,family_or_non,relative_or_non,in_family_type)]
    household_relatives_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),
                               by=.(tract,family_or_non,relative_or_non,in_family_type)]
    household_relatives_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),
                               by=.(tract,family_or_non,relative_or_non,in_family_type,ethnicity)]
    household_relatives_eth_dt <- household_relatives_eth_dt[(tokeep)]
    rm(household_relatives_data_from_census)
    rm(household_relatives_data)
    rm(household_relatives_eth_data)
    
    #4525519    
    #unique(place_born) - "Born in state of residence" "Born in other state in the United States" "Native; born outside the United States"   "Foreign born" 
    place_born_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06004")
    place_born_race_data <- place_born_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(place_born = label) %>%
      filter(race %in% acs_race_codes & number_sams>0) %>%
      uncount(as.numeric(number_sams),.id = "transport_race_id",.remove = TRUE)
    place_born_eth_data <- place_born_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(ethnicity = substr(name,7,7)) %>%
      rename(place_born = label) %>%
      filter(number_sams>0) %>% #!ethnicity %in% acs_race_codes & 
      uncount(as.numeric(number_sams),.id = "transport_race_id",.remove = TRUE)
    place_born_race_dt <- as.data.table(place_born_race_data)
    place_born_eth_dt <- as.data.table(place_born_eth_data)
    place_born_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity %in% acs_race_codes]),by=.(tract,place_born)]
    place_born_eth_dt[order(match(ethnicity,c("H","I",ethnicity %in% acs_race_codes))),c("cnt_ethn"):=list(1:.N),by=.(tract,place_born)]
    place_born_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,place_born)]
    place_born_eth_dt <- place_born_eth_dt[(tokeep)]
    place_born_eth_dt[ethnicity!="H" & ethnicity !="I",("ethnicity"):=list("_")]
    rm(place_born_eth_data)
    rm(place_born_race_data)
    rm(place_born_race_from_census)
    #test <- table(place_born_eth_dt$tract,place_born_eth_dt$place_born)==table(place_born_race_dt$tract,place_born_race_dt$place_born)
    #should do it by households, too??? would match on hh_size, then on race, etc., 
    #    place_born_race_data <- place_born_race_from_census %>% #right total - 4525519
    
    #should check to see if has same problem in other years and other census areas!!! some sort of catch??
    #correct if do by either age or by category, but 465645 short if do by agesxcategories!!all missing from foreign born, under age - foreign born by total is right
    #could redistribute foreign_born by age category and total foreign_born?? excel B06001 work shows calculations.
    #means that this cannot be used without checking for other years / counties
    place_born_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06001")
    place_born_age_data <- place_born_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Born in state of residence") %>%
      filter(label != "Foreign born") %>%
      filter(label != "Born in state of residence") %>%
      filter(label != "Born in other state in the United States") %>%
      filter(label != "Native; born outside the United States") %>%
      pivot_longer(4:ncol(place_born_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("age_range","empty"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        age_range = str_replace(age_range,"Under 5 years","0  to  4 years"), #have to make sort in order
        age_range = str_replace(age_range,"5 to 17 years","05 to 17 years"),
        age_range = str_replace(age_range,"60 and 61 years","60 to 62 years"),
        age_range = str_replace(age_range,"75 years and over","75 to 89 years")
      )%>%
      filter(as.numeric(number_sams) > 0) %>%  
      filter(is.na(empty)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_age_id") 
    place_born_age_full_dt <- as.data.table(place_born_age_data)
    #age_range totals match sex_age_race
    
    place_born_age2_data <- place_born_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","age_range"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        age_range = str_replace(age_range,"Under 5 years","0  to  4 years"), #have to make sort in order
        age_range = str_replace(age_range,"5 to 17 years","05 to 17 years"),
        age_range = str_replace(age_range,"60 and 61 years","60 to 62 years"),
        age_range = str_replace(age_range,"75 years and over","75 to 89 years")
      )%>%
      filter(as.numeric(number_sams) > 0) %>%  # & !is.na(age_range)
      filter(!is.na(age_range)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_age_id") 
    place_born_age_partial_dt <- as.data.table(place_born_age2_data)
    place_born_age_partial_dt <- place_born_age_partial_dt[place_born!="Foreign born"] #tract totals are too far off
    rm(place_born_age_from_census)
    rm(place_born_age_data)
    rm(place_born_age2_data)
    #fix missing foreign born ages - they don't match once you get to tract level, so just ordering
    place_born_age_full_dt[,("remake_pb_id"):=paste0(tract,age_range,
                                                     as.character(1000000+seq.int(1:.N))),
                           by=.(tract,age_range)]
    place_born_age_partial_dt[,("remake_pb_id"):=paste0(tract,age_range,
                                                        as.character(1000000+seq.int(1:.N))),
                              by=.(tract,age_range)]
    place_born_age_full_dt[,c("place_born"):=
                             place_born_age_partial_dt[.SD,list(place_born),on = .(remake_pb_id)]]
    place_born_age_full_dt[is.na(place_born),("place_born"):="Foreign born"]
    #I worry that these numbers are still wrong - it's getting what the acs provides, though
    rm(place_born_age_partial_dt)

    #PLACE OF BIRTH FOR THE FOREIGN-BORN POPULATION IN THE UNITED STATES - 1174879
    origin_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05006") #has PR at bottom
    #very odd missing data, redistributed through a partial vs. full
    origin_data_partial <- origin_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%  
      filter(label != "Americas!!Latin America!!South America") %>% 
      filter(label != "Americas!!Latin America!!Central America") %>% 
      filter(label != "Asia!!Eastern Asia!!China") %>%
      pivot_longer(4:ncol(origin_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("origin_continent","origin_area","origin_region","origin_country"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        origin_country_2 = if_else(is.na(origin_country),if_else(str_detect(origin_area,"n.e.c") | 
                                                                 origin_area=="Fiji",origin_area,origin_region),origin_country)
      ) %>%
      filter(number_sams > 0 & !is.na(origin_country)) %>%
      uncount(as.numeric(number_sams),.id = "origin_id")
    origin_data_partial_dt <- as.data.table(origin_data_partial)
    
    origin_data_full <- origin_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%  
      filter(label != "Americas!!Latin America!!South America") %>% 
      filter(label != "Americas!!Latin America!!Central America") %>% 
      filter(label != "Asia!!Eastern Asia!!China") %>%
      filter(str_detect(label,"!!")) %>%
      pivot_longer(4:ncol(origin_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("origin_continent","origin_area","origin_region","origin_country"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        origin_country2 = if_else(is.na(origin_country),if_else(str_detect(origin_area,"n.e.c") | 
                                                                 origin_area=="Fiji",origin_area,origin_region),origin_country)
      ) %>%
      filter(number_sams > 0 & is.na(origin_region)) %>%
    uncount(as.numeric(number_sams),.id = "origin_id")
    origin_data_full_dt <- as.data.table(origin_data_full)
    #only areas that don't match are Northern Europe and Latin America
    origin_data_partial_dt[,("oc_id"):=
                             paste0(tract,origin_area,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,origin_area)]
    origin_data_full_dt[,("oc_id"):=
                             paste0(tract,origin_area,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,origin_area)]
    origin_data_full_dt[,c("origin_region","origin_country"):=
                          origin_data_partial_dt[.SD, c(list(origin_region),list(origin_country)),
                                                 on = .(oc_id)]]
#for the ones that are missing, just match back to origin_area a second time as sample since the data seems to just be missing from original
    origin_data_partial_dt[,("oc2_id"):=
                             paste0(tract,origin_area,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,origin_area)]
    origin_data_full_dt[is.na(origin_region),("oc2_id"):=
                          paste0(tract,origin_area,
                                 as.character(1000000+sample(.N))),
                        by=.(tract,origin_area)]
    origin_data_full_dt[is.na(origin_region),c("origin_region","origin_country"):=
                          origin_data_partial_dt[.SD, c(list(origin_region),list(origin_country)),
                                                 on = .(oc2_id)]]
#third time for last little bit, with no tracts
    origin_data_partial_dt[,("oc3_id"):=
                             paste0(origin_area,
                                    as.character(1000000+sample(.N))),
                           by=.(origin_area)]
    origin_data_full_dt[is.na(origin_region),("oc3_id"):=
                          paste0(origin_area,
                                 as.character(1000000+sample(.N))),
                        by=.(origin_area)]
    origin_data_full_dt[is.na(origin_region),c("origin_region","origin_country"):=
                          origin_data_partial_dt[.SD, c(list(origin_region),list(origin_country)),
                                                 on = .(oc3_id)]]
    origin_data_full_dt[is.na(origin_country),("origin_country"):=origin_region]
    origin_data_dt <- origin_data_full_dt
    rm(origin_from_census)
    rm(origin_data_full)
    rm(origin_data_full_dt)
    rm(origin_data_partial)
    rm(origin_data_partial_dt)
    
  
    #think about how to match with Hispanic number from original sex_age??
    #total right - 4525519, but would have to match before making latinx to get numbers right, and not sure about matching spouses, etc.
    latinx_origin_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B03001")
    latinx_origin_data <- latinx_origin_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Hispanic or Latino") %>%
      filter(label != "Hispanic or Latino!!Other Hispanic or Latino") %>%
      filter(label != "Hispanic or Latino!!South American") %>%
      filter(label != "Hispanic or Latino!!Central American") %>%
      pivot_longer(4:ncol(latinx_origin_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("latinx","origin_country","origin_country2"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(origin_country = if_else(!is.na(origin_country2),
                                      origin_country2,origin_country)) %>%
      filter(!is.na(origin_country) | latinx == "Not Hispanic or Latino") %>%
      rename(census_group_name = name) %>% 
      uncount(as.numeric(number_sams),.id = "latinx_id")
    latinx_dt <- as.data.table(latinx_origin_data)
    rm(latinx_origin_from_census)
    rm(latinx_origin_data)
    
    #population 15 yrs and over - 3223758 / doesn't match age_categories from other place_born groups
    #nrow(sam_sex_race_age_dt[age_range!="Under 5 years" & age_range!="5 to 9 years" & age_range!="10 to 14 years"]) = 3494885 (not a match!)
    income_place_born_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06010") #PR at top
    income_place_born_data <- income_place_born_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(income_place_born_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","has_income","income_range"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(income_range = if_else(has_income=="No income","$0",income_range)) %>%
      filter(number_sams > 0 & !is.na(income_range)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_income_id")
    income_place_born_dt <- as.data.table(income_place_born_data)
    rm(income_place_born_from_census)
    rm(income_place_born_data)
    
    #numbers don't match others, but are internally consistent for the dataset - have to think about whether worth fudging in join - not sure how to capture probable differences around age
    place_born_language_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06007")
    place_born_language_data <- place_born_language_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_language_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","language_at_home","English_proficiency"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(English_proficiency = if_else(language_at_home=="Speak only English","Only English Speaker",English_proficiency)) %>%
      filter(number_sams > 0 & !is.na(English_proficiency)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_language_id")
    place_born_language_dt <- as.data.table(place_born_language_data)
    rm(place_born_language_from_census)
    rm(place_born_language_data)
    
    #population 15 and over - 3560318 
    place_born_marital_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06008")
    place_born_marital_data <- place_born_marital_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_marital_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","marital_status"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(marital_status)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_marital_id")
    place_born_marital_dt <- as.data.table(place_born_marital_data)
    rm(place_born_marital_data)
    rm(place_born_marital_from_census)
    
    #population of 25 and over: 2923369 folks - count all people over 25 in place_born_age you get 2860024 (63345 too many in educ?)
    place_born_education_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06009")
    place_born_education_data <- place_born_education_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_education_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","educational_status"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(educational_status)) %>%
      uncount(as.numeric(number_sams),.id = "place_born_education_id")
    place_born_education_dt <- as.data.table(place_born_education_data)
    rm(place_born_education_from_census)
    rm(place_born_education_data)
    
    #unique on date_entered: "Entered 2010 or later" "Entered before 1990"   "Entered 2000 to 2009"  "Entered 1990 to 1999"
    #hard to line up with exact, but total comes close to foreign_born in place_born (24892 off)
    #origin_region is messed up
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
    place_period_citizen_dt <- as.data.table(place_period_citizen_data)
    rm(place_period_citizen_from_census)
    rm(place_period_citizen_data)
    
    #right number - 1174879 
    sex_place_when_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05008")
    sex_place_when_data <- sex_place_when_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(sex_place_when_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","origin_area","origin_region","origin_country","date_entered"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(date_entered = if_else(str_detect(origin_region,"Entered") & origin_area!="Latin America",origin_region,
                                    if_else(str_detect(origin_country,"Entered"),origin_country,
                                            if_else(str_detect(origin_area,"Entered"),origin_area,date_entered))),
             origin_country = if_else(str_detect(origin_country,"Entered"),origin_region,
                                      if_else(str_detect(origin_region,"Central"),origin_region,date_entered)),
             origin_country = if_else(is.na(origin_country),origin_area,origin_country),
             origin_region = if_else(str_detect(origin_region,"Entered"),origin_area,origin_region)
      ) %>%
      filter(number_sams > 0 & !is.na(date_entered)) %>% #
      uncount(as.numeric(number_sams),.id = "sex_place_when_id")
    sex_place_when_dt <- as.data.table(sex_place_when_data)
    rm(sex_place_when_from_census)
    rm(sex_place_when_data)
    
    
#    kids_place_citizen_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05009")
#    #this is a challenging one to include - how many parents are foreign born and how to make a household that matches, for just 10k cases!
#    kids_place_citizen_data <- kids_place_citizen_from_census %>%
#      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
#      filter(label != "Estimate!!Total") %>%
#      pivot_longer(4:ncol(kids_place_citizen_from_census),names_to = "tract", values_to = "number_sams") %>% 
#      separate(label, c("kid_age_range","both_parents","parent_nativity","kid_nativity"), sep = "!!", remove = F, convert = FALSE) %>%
#      mutate(kid_nativity = if_else(str_detect(parent_nativity,"Child"),parent_nativity,kid_nativity),
#             kid_nativity = if_else(str_detect(both_parents,"Child"),both_parents,kid_nativity),
#             parent_nativity = if_else(str_detect(parent_nativity,"Child"),both_parents,parent_nativity),
#             parent_nativity = if_else(str_detect(parent_nativity,"Living"),kid_nativity,parent_nativity)) %>%
#      filter(number_sams > 0 & !is.na(kid_nativity)) %>% 
#      uncount(as.numeric(number_sams),.id = "kids_place_id")
#    kids_place_citizen_dt <- as.data.table(kids_place_citizen_data)
#    rm(kids_place_citizen_data)
#    rm(kids_place_citizen_from_census)
    
    #sex and age for foreign born population - matches with place_period_citizen_data
    sex_nativity_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05013")
    sex_nativity_age_data <- sex_nativity_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(sex_nativity_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(age_range)) %>% 
      uncount(as.numeric(number_sams),.id = "sex_nativity_age_id")
    sex_nativity_age_dt <- as.data.table(sex_nativity_age_data)
    rm(sex_nativity_age_data)
    rm(sex_nativity_age_from_census)
    
  }
}
    
    
    
    
    
    #NOT USED YET
    #concept is: "PRESENCE OF UNMARRIED PARTNER OF HOUSEHOLDER BY HOUSEHOLD TYPE FOR CHILDREN UNDER 18 YEARS IN HOUSEHOLDS"
    #children come in at 1223249 - nrow(sex_age_race_latinx_dt[age<18]) = 1225059 - 1810 kids could be cps / foster waiting?
    household_unmarried_children_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09008")
    hh_unmarried_children_data <- household_unmarried_children_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_unmarried_children_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("partner_present_kids","family","family_type"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(family_type = if_else(family=="In nonfamily households","In nonfamily households",family_type)) %>%
      filter(!is.na(family_type) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "hh_unmarried_kids_id",.remove = TRUE)
    
    #not used yet
    
    

    
    #concept is:"RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS BY NATIVITY OF CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY LIVING ARRANGEMENTS AND NATIVITY OF PARENTS"
    #tells whether living with one parent or two
    pov_ratio_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05010") #count of kids with family income less than pov.
    pov_ratio_kids_data <- pov_ratio_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(pov_ratio_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("poverty_ratio","parent_type","parent_nativity"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(parent_nativity) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "poverty_kids_id",.remove = TRUE)
    
    
    
                        
        
    #adults and kids gets you right(ish) total (20,000, depending on using seniors from inside adults or separately)
    #could be worth doing, to get the right relationship with seniors, but not now

    
    

    
    kids_SSI_household_type_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09010")
    kids_SSI_household_type_data <- kids_SSI_household_type_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_SSI_household_type_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("SSI","family","family_type"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(family_type=if_else(family=="In nonfamily households",family,family_type)) %>%
      filter(!is.na(family_type) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "kids_SSI_id",.remove = TRUE)
    
    #vacant_occupied=="Occupied" gives 1562813, which is equal to number of households
    housing_occupancy_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25002") #gives vacant vs. occup
    occupied_vacant_data <- housing_occupancy_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occupancy_from_census),names_to = "tract", values_to = "number_sams") %>% 
      rename(vacant_occupied = label) %>%
      uncount(as.numeric(number_sams),.id = "occup_vacant_id",.remove = TRUE)
    
    #MORTGAGE STATUS BY AGE OF HOUSEHOLDER
    #shows for 855629 - with different age groups from housing_per_room_age_data (which also has 1562813 / hh) / = to "Owner occupied" in own_rent
    mortgage_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25027") 
    mortgage_age_data <- mortgage_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_age_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("mortgage","householder_age"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(householder_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_age_id",.remove = TRUE)
    
    #concept:TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT
    #4484299 - plus group quarters gives right total
    housing_occup_date_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25026") #of occup, own or rent by move in date
    housing_occup_date_data <- housing_occup_date_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total population in occupied housing units!!")) %>%
      filter(label != "Estimate!!Total population in occupied housing units") %>%
      pivot_longer(4:ncol(housing_occup_date_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","move_in_date"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(move_in_date) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_date_id",.remove = TRUE)
    
    
        
    
        #has population of 4458402 -- should do whole group of them!!!
    moved_1yr_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07004")
    moved_1yr_race_data <- moved_1yr_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(moved_1yr_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(moved_1yr = label) %>%
      filter(race %in% acs_race_codes & number_sams>0) %>%
      uncount(as.numeric(number_sams),.id = "moved_1yr_race_id",.remove = TRUE)
    
    

    
    
    #UGH - probably not worth it!! 
    #https://www.census.gov/topics/population/ancestry/about/faq.html
    #concept:PEOPLE REPORTING MULTIPLE ANCESTRY 12225
    mult_ancestry_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B04005")
    mult_ancestry_data <- mult_ancestry_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mult_ancestry_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("multiple_ancestry","sm_area"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(multiple_ancestry) & is.na(sm_area)) 
    
    #people reporting either only or as part of multiple ethnicity - not sure how to code to individuals
    #concept: PEOPLE REPORTING ANCESTRY 16626
    ancestry_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B04006")
    ancestry_data <- ancestry_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(ancestry_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("ancestry","sm_area"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0 & !is.na(ancestry) & is.na(sm_area))
    
    
    
    #income median by race B19013 - one value per tract per race
    #race_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19013")
    
    #gets per_capita by race per tract - one value per tract per race
    #per_capita_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19301")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

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
  
  
    

    #gather information from census data,
    #Household - multigenerational B11017 all NA
    #above just says how many people below or above poverty, but number of people and type of HH is given - something... match on family_type of Head of Household, but have to already have households
    
        
    #AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS FOR FAMILIES BY FAMILY TYPE -only below poverty... too complicated to unwind and explain
    agg_deficit_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17011")
    
    #by number of children and poverty B17023
    number_children_poverty_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17023")
    
    #by tenure B17019 (whether renter or not)
    household_tenure_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17019")




moved_1yr_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07001")
moved_1yr_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07003")
moved_1yr_citizen_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07007")
moved_1yr_marital_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07008")
moved_1yr_education_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07009")
moved_1yr_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07010")
when_go_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08011")
time_to_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08012")
vehicles_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08014")




# poverty - type - number of persons in HH and family_type, but only below or above poverty in last 12 months
#    household_poverty_people_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17013")

#type by age of HH only above and below
#    household_poverty_age_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17017")
#type by education of HH B17018 - only above / below poverty
#    household_poverty_educ_level_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17018")

#kids_to_householder_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09018")



#concept: DISABILITY STATUS OF GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT
kids_respons_grands_disability_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10052")

#concept:NATIVITY BY GRANDPARENTS RESPONSIBLE FOR OWN GRANDCHILDREN UNDER 18 YEARS BY AGE OF GRANDPARENT
kids_respons_grands_nativity_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10053")


housing_units_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25001") #total number (same as adding occupied and vacant)



health_insurance_sex_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27001")
private_health_insurance_sex_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27002")
public_health_insurance_sex_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27003")
type_health_insurance_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27010")
health_insurance_employment_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27011")
health_insurance_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27015")
health_insurance_age_educ_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27019")
health_insurance_citizenship_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B27020")
citizen - B29001
computers - B28001


#have to explore later
contract_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25056")
bedrooms_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25068")
income_value_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25121")





