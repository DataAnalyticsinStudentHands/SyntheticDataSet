library(tidyr)
library(dplyr)
library(stringr)

#' createIndividuals
#'
#' This function simulates individual people based on census data (age, sex, race, maritial status).
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
      #keeping all race - for total,  #   filter(substr(name,7,7) %in% acs_race_codes) gets you the ones that added together would = total pop; ages added together get you total by that race/ethnicity     
      mutate(race = substr(name,7,7)) %>%
      separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE) %>%  #NA in rows with only one
      rename(census_group_name = name) %>%
      filter(number_sams != 0, !is.na(age_range),race!="_",age_range!="Total") %>% 
      select(-concept) %>%
      group_by(tract, sex, age_range) %>% 
      #as with married, below, do pivot wider then assign??
      mutate(
        hispanic_tract_age = if_else(race=="I", as.integer(number_sams),as.integer(0)), #number in tract in that age_range who are Hispanic 
        white_tract_age = if_else(race=="A", as.integer(number_sams),as.integer(0)),
        black_tract_age = if_else(race=="B", as.integer(number_sams),as.integer(0)),
        r2_tract_age = if_else(race=="G", as.integer(number_sams),as.integer(0)),
        other_tract_age = if_else(race=="F", as.integer(number_sams),as.integer(0)),
        white_nh_tract_age = if_else(race=="H", as.integer(number_sams),as.integer(0)),
        white_hispanic = sum(white_tract_age)-((sum(white_nh_tract_age)+(sum(other_tract_age)*.3)+(sum(r2_tract_age)*.2)+(sum(black_tract_age)*.03))),
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
      select(-ends_with("_tract_age")) %>% 
      mutate(number_sams = case_when(hispanic_id==1 & hispanic_number > 0 ~ as.integer(number_sams) - as.integer(hispanic_number),
                                     hispanic_id==2 & hispanic_number > 0 ~ as.integer(hispanic_number),
                                     hispanic_id==2 & hispanic_number == 0 ~ as.integer(0),
                                     TRUE ~ as.integer(number_sams)),
             hispanic = if_else(hispanic_id == 2, TRUE, FALSE)
      ) %>%
      filter(number_sams!=0) %>%
      uncount(number_sams,.id = "individual_id") # for testing purposes - should equal 4525519 per B10001 row 166 total
    #sum(sex_by_age_race_data$hispanic) = 1910672 - need to compare with excel row 222, which has 1910535; did some random tweaks...
    
    #get marriage data
    marital_status_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B12002")
    
    marital_status_data <-  marital_status_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      #filter(substr(name,7,7) %in% acs_race_codes) %>%
      mutate(race = substr(name,7,7)) %>%
      pivot_longer(4:ncol(marital_status_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      filter(number_sams != 0) %>%
      separate(label, into = c("sex", "part2", "part3", "part4","part5"), sep = "!!", remove = T) %>%  #remove = F started throwing errors, but after using it for a long time!!!
      #breaking out the variable we need for calculation, and a few to carry with per line:
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
      ) %>% #instead of trying to find places where they give totals, find only ones we want and count ourselves
      #can we sum by row, and then pivot wide? 
      pivot_wider(names_from = "age_range",values_from = number_sams) %>%
      group_by(sex,marital_status,spouse_present) %>%
      replace(is.na(.), 0) %>%
      rowwise() %>%
      mutate(tract_total=sum(.[,18:24])
      )
    #distribute by race / number in age_group / total in tract times existing numbers_sam
    filter(!is.na(age_range)) %>% #get correct number, at 3494885 uncount(marital_status_data,marital_status_data$number_sams,.id="id")
      
      #filter(race %in% acs_race_codes,!is.na(marital_status)) # you either get age_range or race - is.na on age_range gets right number, this is 3396192 (100k off) 
      
      
      
      #group_by to get smallest units - 
      
      #sp_tract = if_else(!is.na(spouse_present) & spouse_present=="Married spouse present" & race != "_",as.integer(number_sams), as.integer(0)), #should be total by race
      sptotal_tract = if_else(!is.na(spouse_present) & spouse_present=="Married spouse present" & race == "_",as.integer(number_sams), as.integer(0)), #should be total, all races
    #sa_tract = if_else(!is.na(spouse_present) & spouse_present=="Married spouse absent" & race != "_",as.integer(number_sams), as.integer(0)), #shoul be total by race
    satotal_tract = if_else(!is.na(spouse_present) & spouse_present=="Married spouse absent" & race == "_",as.integer(number_sams), as.integer(0)),
    widow_tract = if_else(!is.na(marital_status) & marital_status=="Widowed",as.integer(number_sams), as.integer(0)),
    divorced_tract = if_else(!is.na(marital_status) & marital_status=="Divorced",as.integer(number_sams), as.integer(0)),
    married_tract = if_else(!is.na(marital_status) & marital_status=="Now married",as.integer(number_sams), as.integer(0)),
    single_tract = if_else(!is.na(marital_status) & marital_status=="Never married",as.integer(number_sams), as.integer(0)),
    tract_total = if_else(sex=="Estimate" & race == "_" & is.na(marital_status),as.integer(number_sams), as.integer(0)),
    tract_total_race = if_else(sex == "Estimate" & race == "_", as.integer(number_sams), as.integer(0)), #should be whole adult pop in tract
    tract_total_male = if_else(sex == "Male" & race == "_", as.integer(number_sams), as.integer(0)),
    tract_total_female = if_else(sex == "Female" & race == "_", as.integer(number_sams), as.integer(0)),
    A_tract = if_else(sex == "Estimate" & race == "A", as.integer(number_sams), as.integer(0)),
    B_tract = if_else(sex == "Estimate" & race == "B", as.integer(number_sams), as.integer(0)),  #as.integer(number_sams * (1 - 3*(G_total/(trac_total)))))
    C_tract = if_else(sex == "Estimate" & race == "C", as.integer(number_sams), as.integer(0)),
    D_tract = if_else(sex == "Estimate" & race == "D", as.integer(number_sams), as.integer(0)),
    E_tract = if_else(sex == "Estimate" & race == "E", as.integer(number_sams), as.integer(0)),
    F_tract = if_else(sex == "Estimate" & race == "F", as.integer(number_sams), as.integer(0)),
    G_tract = if_else(sex == "Estimate" & race == "G", as.integer(number_sams), as.integer(0)),
    H_tract = if_else(sex == "Estimate" & race == "H", as.integer(number_sams), as.integer(0)),
    I_tract = if_else(sex == "Estimate" & race == "I", as.integer(number_sams), as.integer(0))
    ) # %>%
group_by(tract,sex,race,marital_status,spouse_present) %>%
  #put the variable for tract on each row
  mutate(sp_total = sum(sptotal_tract,na.rm = T), #shouldn't need na.rm anymore, but haven't tested completely.
         sa_total = sum(satotal_tract,na.rm = T),
         widow_total = sum(widow_tract,na.rm = T),
         divorced_total = sum(divorced_tract,na.rm = T),
         married_total = sum(married_tract,na.rm = T),
         single_total = sum(single_tract,na.rm = T),
         A_total = sum(A_tract),
         B_total = sum(B_tract),
         C_total = sum(C_tract),
         D_total = sum(D_tract),
         E_total = sum(E_tract),
         F_total = sum(F_tract),
         G_total = sum(G_tract),
         H_total = sum(H_tract),
         I_total = sum(I_tract),
         t_tract_total = sum(tract_total,na.rm = T),
         r_trac_total = sum(tract_total_race * (1 - 3*(G_total/t_tract_total)),na.rm = T), #sorting out two or more races, in same way as for base
         Single = as.numeric(single_total/t_tract_total,na.rm = T),
         Married_sp = as.numeric(sp_total/t_tract_total,na.rm = T),
         Married_sa = as.numeric(sa_total/t_tract_total,na.rm = T),
         Divorced = as.numeric(divorced_total/t_tract_total,na.rm = T),
         Widowed = as.numeric(widow_total/t_tract_total,na.rm = T),
         A = as.numeric(A_total/r_trac_total),
         B = as.numeric(B_total/r_trac_total), # add sample possibility if less than 1? Or above? 
         C = as.numeric(C_total/r_trac_total),
         D = as.numeric(D_total/r_trac_total),
         E = as.numeric(E_total/r_trac_total),
         F_Race = as.numeric(F_total/r_trac_total),
         G = as.numeric(G_total/r_trac_total), #change to .01 for testing?? then do the r2_total bit?
         H = as.numeric(H_total/r_trac_total),
         I = as.numeric(I_total/r_trac_total)
  ) %>%
  #    filter(number_sams!=0) %>% # & sex!="Estimate" !is.na(age_range) & 
  #    select(-ends_with("_total"),-ends_with("_tract"),-concept, -part2, -part3, -part4) %>%  #-number_sams, -race, 
  pivot_longer(c("A","B","C","D","E","F_Race","G","H","I"),names_to = "race_percent",values_to = "percent_race_numbers_sam") %>%  #all zeros????
  pivot_longer(c("Widowed","Married_sp","Married_sa","Divorced","Single"),names_to = "marital_status_percent",values_to = "m_percent_sam") %>%
  mutate(new_numbers_sam = as.integer(number_sams*percent_race_numbers_sam),
         marital_numbers_sam = as.integer(new_numbers_sam*m_percent_sam)) %>%
  #      rename(number_sams = new_numbers_sam,marital_group_name = name) %>%
  ungroup()
#G is too small and divorce might have two ?? ugh.....

#for-loop to assign people, give percentages on each row...
columns <- colnames(marital_status_data)
column_tracts <- which(str_detect(columns,'_total'))

test <- data.frame()

for(tract in unique(marital_status_data$tract)){
  for(col in columns[column_tracts]){
    #test <- assign(str_split(col,"_")[[1]][1],marital_status_data[tract,col])
    test[tract,col] <- marital_status_data[tract,col]
  }
  #as you sample from each, have to get it to subtract from each part that makes up a total
}


#not run but for testing - sum(marital_status_data$number_sams, na.rm=T) = 3655799 - this looks like the right number of adults who should have some status

base_married_join <- left_join(sex_by_age_race_data, marital_status_data,by=c("tract","sex","race","age_range"),suffix = c("_base", "_married"))  
#, -- pivot

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


pregnancy_data_age_marriage_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B13002")

pregnancy_data <- pregnancy_data_age_marriage_from_census %>%
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
    #Household - type by size - tells you if it's a family or non-family household, but 
    household_type_size_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11016")
    #Household - multigenerational B11017 all NA
    # poverty - type - number of persons in HH
    household_poverty_people_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17013")
    #above just says how many people below or above poverty, but number of people and type of HH is given - something...
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



