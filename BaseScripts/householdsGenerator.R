library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
#library(rio) #may not need here - playing with it
library(FactoMineR)
library(doParallel)
library(foreach)
#for distance calculation
library(stats)

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
      
      #expand into sam
      sam_sex_race_age <- uncount(sex_by_age_race_data,number_sams,.id = "sams_id") %>% # should equal 4525519 per B10001 row 166 total
        rowwise() %>%
        mutate(
          age=as.numeric(sample(as.character(first_age:last_age),1,prob = rep(1/age_range_length,age_range_length),replace = FALSE))
        )
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
    
    #for other individual characteristics: get age_range to year; get percentages by race and age, sample with tract_race_year multiplied into probability?
     
     
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
    #I don't have great confidence that the census is distributing them correctly - it may be an algorithm meant to hide concentrations.
    group_quarters_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B26001")
    
    base_group_quarters_data <- group_quarters_data_from_census %>%
      pivot_longer(4:ncol(group_quarters_data_from_census),names_to = "tract", values_to = "number_sams_temp") 
    
    #make into a model based only on the characteristics that are known
    GQ_sam <- uncount(base_group_quarters_data,number_sams_temp,.remove = FALSE,.id="temp_id") %>%
      group_by(tract) %>%
      mutate( #approximated from natl BOP - https://www.bop.gov/about/statistics/statistics_inmate_age.jsp
        GQ_facility_type := if_else(number_sams_temp > 400,"correctional","nursing home"),
        age_range_num := if_else(GQ_facility_type == "correctional",
                             sample(c(1,2,3,4,5,6,7), #c("18 to 19 years","20 to 24 years","25 to 29 years","30 to 34 years","35 to 44 years","45 to 54 years","55 to 64 years"),
                                    replace=TRUE,size = n(),prob=c(.05,.09,.16,.25,.24,.14,.07)),
                             sample(c(8,9,10), #c("65 to 75 years","75 to 85 years","85 years and over"),
                                    replace = TRUE,size=n(),prob=c(.1,.4,.5))), # https://www.cdc.gov/nchs/nsltcp/index.htm
        sex_num := if_else(GQ_facility_type == "correctional",
                       sample(c(0,1), #c('male','female'),
                              replace=TRUE,size = n(),prob=c(.93,.07)),
                       sample(c(0,1), #c('male','female'),
                              replace=TRUE,size = n(),prob=c(.33,.67)),
                       )
      )
    
    GQ_sam$age_range <- as.character(GQ_sam$age_range_num) #so bind_rows works
    
    sam_marital <- joined_sam_marital %>% #adding sex_num so PCA will have it as numeric
      mutate(
        sex_num := case_when(sex == "Male" ~ 0, sex == "Female" ~1)
      )
    
    sam_marital_GQ <- bind_rows(joined_sam_marital,GQ_sam) #bind them so that the PCA includes GQ by tract 
    sam_marital_DT <- as.data.table(sam_marital_GQ)

    #GQ doesn't include race, so it's imputed by the mean of the variable for the tract - which is reasonable, but not perfect.
    
# for NA
    sam_marital_DT[is.na(age),"age" := mean(age,na.rm = TRUE),tract]
    sam_marital_DT[is.nan(age),"age" := mean(age,na.rm = TRUE),tract]
    sam_marital_DT[is.nan(age),"age" := as.double(34),tract]
    sam_marital_DT[is.nan(sex_num),"sex_num" := as.double(.502),tract]
    sam_marital_DT[is.na(sex_num),"sex_num" := mean(sex_num,na.rm = TRUE),tract]
    sam_marital_DT[is.nan(sex_num),"sex_num" := mean(sex_num,na.rm = TRUE),tract]
#better way to do mean, etc - should change some of logic from above to DT
#first two are only for those that are created in second stage - uncount within 
    sam_marital_DT[,"age" := if_else(is.na(age),age,mean(age,na.rm = TRUE)),tract]
    sam_marital_DT[,"sex_num" := if_else(is.na(temp_id),sex_num,mean(sex_num,na.rm = TRUE)),tract]

#these are for both times through    
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

    #should change names - have to think through adding multiples, too.
    #for whole, to compare with by tract and to have the GQ_Sam included in the PCA so matching by distance makes sense for whole
    sam_marital_PCA_res2 <- PCA(sam_marital_DT[,c('age','sex_num','white','black','hispanic','asian','other_race','american_indian','pacific_islander','bi_racial')],scale.unit=TRUE, ncp=3)

    sam_marital_DT[,("harris_coord_1") := sam_marital_PCA_res$ind$coord[,1]] # * sam_marital_PCA_res$eig[1:3,2]/100 #norm coord by percent explained by dim
    sam_marital_DT[,("harris_coord_2") := sam_marital_PCA_res$ind$coord[,2]]
    sam_marital_DT[,("harris_coord_3") := sam_marital_PCA_res$ind$coord[,3]]
   
    saveRDS(sam_marital_DT,"sam_marital_eig.RDS") #temp save
    sam_marital_DT <- readRDS("sam_marital_eig.RDS")

    #general process: 1 - expand census data by tract; 1b - fill (uncount) with averaged so GQ_id, etc is = num_sams; 
              # 2 - create_tract_eigs 
              # 3 - dist - from avg [or median?] temp_person - should fill all with median??
              # 4 - sample - 4b - using is.na(temp_id) etc. ; 4c - normalize by 100 for all distances, and use that as percent for sample tract 980000 is weird (exclude?)
    
  #initial facts <- c('age','sex_num','white','black','hispanic','asian','other_race','american_indian','pacific_islander','bi_racial')
                #initial fills <- c("white","black","hispanic","asian","other_race","american_indian","pacific_islander","bi_racial") 
                #initial new_vars <- c("GQ_facility_type")
 
  cl <- makeCluster(8) #have to decide how many cores 
  registerDoParallel(cl)
  #where you have a data.table, a vector of factors for PCA, a name to start with, ft as a vector of columns that need to be set to median, and new_vars are columns to move over to sam
  create_tract_eigs = function(dt,var_name,facts){ 
  #  foreach(i = unique(dt$tract),.combine=rbind,facts=facts, .packages = c("doParallel","FactoMineR","data.table","tidyr")) %dopar% {  #does it need doParallel and data.table?? test??
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
  
  euc_distances = function(dt,vname,facts){ 
    for(i in unique(dt$tract)){
    #foreach(i = unique(dt$tract),.combine=rbind,facts=facts, .packages = c("doParallel","FactoMineR","data.table","tidyr")) %dopar% {  #does it need doParallel and data.table?? test??
      #uncount to right size with median
      number_sams_added <- sum(dt[tract==i & is.na(temp_id),.N],na.rm = TRUE) - sum(dt[tract==i & !is.na(temp_id),.N],na.rm = TRUE)
      filler_dt <- as.data.table(dt[tract==i & !is.na(temp_id)][1]) 
      filler_dt[,"GQ_facility_type" := 'not in Group Quarters']
      added <- uncount(filler_dt,number_sams_added,.remove = FALSE,.id="temp_id")
      dt <- rbind(dt,added) #does it need to be added for the tract?
      #return euclidean distance
      center <- dt[tract==i & !is.na(temp_id),c(paste0(vname,"_eig_1"),paste0(vname,"_eig_2"),paste0(vname,"_eig_3"))]
      #center <- dt[tract==i & !is.na(temp_id),c(paste0("race_age","_eig_1"),paste0("race_age","_eig_2"),paste0("race_age","_eig_3"))][1]
      print(center)
      target <- dt[tract==i & is.na(temp_id),c(paste0(var_name,"_eig_1"),paste0(var_name,"_eig_2"),paste0(var_name,"_eig_3"))]
      print(center[,1][[1]])
      dt[tract==i & is.na(temp_id),("euc_dist") := sqrt((target[,1]-center[,1][[1]])^2 + (target[,2]-center[,2][[1]])^2 + (target[,3]-center[,3][[1]])^2)]
    }
    return(dt)
  }
  sam_race_age_eigs_eucs <- euc_distances(sam_race_age_eigs,"race_age",facts)
  
  sample_by_euc = function(dt,name,new_vars){ 
    foreach(i = unique(dt$tract),.combine=rbind, .packages = c("doParallel","FactoMineR","data.table")) %dopar% {  #does it need doParallel and data.table?? test??
      #normalize the euc_dist on the !is.na(temp_id) to use as prob paste0(name,"_dist_prob")
      #sample, with ..new_vars returned from sample - every is.na(temp_id)  - 
      dt[tract==i & is.na(temp_id),..new_vars :=  
           sample(dt[tract==i & !is.na(temp_id),..new_vars],size = .N,replace = FALSE,prob = dt[tract==i & !is.na(temp_id),paste0(name,"_dist_prob")])]
    }
    dt_out <- dt[is.na(temp_id)]
    return(dt_out)
  }
  
  dt[!is.na(temp_id),..new_vars]
  
    
    
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



