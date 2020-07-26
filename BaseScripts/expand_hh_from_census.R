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

#' exp_census
#'
#' This function creates the expanded census data and runs tests on them
#'
#' @return a list of data.tables with expanded census.
exp_census_hh <- function() {
  #need to break into smaller pieces - not sure best approach for actually programming - need folder structure right
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
    
#do the households first, then expand and match with individual level data 
#start with household type, since it has lots of data associated with it, and with race for most variation that will help matching
    #in general, the ethnicity stuff allows for several answers on the sub-categories - if you could distribute them easily across sufficient
    #categories, you could resolve, but we don't have enough - and ethnicity is an ill-defined category, poorly applied in the moment
    #gives 1562813; householders by tract is one of the base constants, with hh X race and X ethnicity stable
    
    
    #concept: TENURE (householder rents or owns)
    housing_occup_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25003") #of occup, own or rent by race
    occupied_race_data <- housing_occup_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(own_rent = label) %>%
      filter(race %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_race_id",.remove = TRUE)
    occupied_race_dt <- as.data.table(occupied_race_data)
    rm(occupied_race_data)
    
    occupied_eth_data <- housing_occup_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(ethnicity = substr(name,7,7)) %>%
      rename(own_rent = label) %>%
      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "own_ethnicity_race_id",.remove = TRUE)
    occupied_eth_dt <- as.data.table(occupied_eth_data)
    occupied_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,own_rent)]
    occupied_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,own_rent)]
    occupied_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE)]
    occupied_eth_dt <- occupied_eth_dt[(tokeep)]
    rm(housing_occup_race_from_census)
    rm(occupied_eth_data)
    #test <- table(occupied_eth_dt$tract,occupied_eth_dt$own_rent)==table(occupied_race_dt$tract,occupied_race_dt$own_rent)
    #length(test[test==TRUE])/length(test)

    #concept: "HOUSEHOLD TYPE (INCLUDING LIVING ALONE) for acs_race_codes"
    household_type_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11001") #vgl. B25006??
    household_type_race_data <- household_type_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_type","single_hh_sex"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        race = substr(name,7,7),
        family_role = if_else(family_type=="Other family",single_hh_sex,family_type)
      ) %>% 
      filter(!is.na(family_role)) %>%
      filter(number_sams > 0 & race %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "hh_type_race_id") 
    hh_type_race_dt <- as.data.table(household_type_race_data)
    rm(household_type_race_data)
    #table(household_type_race_data$family) 
    #Family households Nonfamily households 
    # 1066649               496164
    #table(household_type_race_data$family_type)
    #Householder living alone Householder not living alone        Married-couple family                 Other family 
    # 408614                        87550                       734108                       332541 
    #single_hh_sex is only for Other family (Male or Female with no spouse)
    #doesn't seem to add anything more than above
    household_type_ethnicity_data <- household_type_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_type","single_hh_sex"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        ethnicity = substr(name,7,7),
        family_role = if_else(family_type=="Other family",single_hh_sex,family_type)
      ) %>% 
      filter(!is.na(family_role)) %>%
      filter(number_sams > 0 & !ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "hh_type_ethnicity_id")
    hh_type_ethnicity_dt <- as.data.table(household_type_ethnicity_data)
    hh_type_ethnicity_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,family,family_type,single_hh_sex)]
    hh_type_ethnicity_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,family,family_type,single_hh_sex)]
    hh_type_ethnicity_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE)]
    hh_type_eth_dt <- hh_type_ethnicity_dt[(tokeep)]
    rm(household_type_race_from_census)
    rm(hh_type_ethnicity_dt)
    rm(household_type_ethnicity_data)
    #test <- table(hh_type_eth_dt$tract,hh_type_eth_dt$family_role,hh_type_eth_dt$single_hh_sex)==table(hh_type_race_dt$tract,hh_type_race_dt$family_role,hh_type_race_dt$single_hh_sex)
    #length(test[test==TRUE])/length(test)

    housing_occup_hhtype_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25011") #of occup, own or rent by household type
    housing_occup_hhtype_data <- housing_occup_hhtype_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_hhtype_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","family","family_type","single_hh_sex","householder_age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        householder_age = if_else(str_detect(single_hh_sex,"years"),single_hh_sex,householder_age),
        householder_age = if_else(str_detect(family_type,"years"),family_type,householder_age),
        single_hh_sex = if_else(str_detect(single_hh_sex,"present"),single_hh_sex,NULL)  #need a policy on NULL or NA
      ) %>% 
      filter(!is.na(householder_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hhtype_id",.remove = TRUE)
    occup_type_dt <- as.data.table(housing_occup_hhtype_data)
    rm(housing_occup_hhtype_from_census)
    rm(housing_occup_hhtype_data)
    
    #concept: TENURE BY AGE OF HOUSEHOLDER - has 9 factors for age
    housing_occup_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25007") #of occup, own or rent by age
    housing_occup_age_data <- housing_occup_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_age_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","householder_age_9"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(householder_age_9) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_age_id",.remove = TRUE)
    hh_age_dt <- as.data.table(housing_occup_age_data)
    rm(housing_occup_age_from_census)
    rm(housing_occup_age_data)
    
    #concept: UNITS IN STRUCTURE x race in acs_race_codes
    #can we get percentage own_rent by number of units from HCAD somehow?
    housing_units_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25032") #units in structure by race
    housing_units_race_data <- housing_units_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_units_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(housing_units = label) %>%
      filter(race %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "housing_units_race_id",.remove = TRUE)
    housing_units_race_dt <- as.data.table(housing_units_race_data)
    rm(housing_units_race_data)
    
    housing_units_eth_data <- housing_units_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Owner-occupied housing units") %>%
      filter(label != "Renter-occupied housing units") %>%
      pivot_longer(4:ncol(housing_units_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("housing_units","empty"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(ethnicity = substr(name,7,7),
             housing_units = if_else(!is.na(empty),empty,housing_units)
      ) %>%
      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "housing_units_race_id",.remove = TRUE)
    housing_units_eth_dt <- as.data.table(housing_units_eth_data)
    housing_units_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,housing_units)]
    housing_units_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,housing_units)]
    housing_units_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,housing_units)]
    housing_units_eth_dt <- housing_units_eth_dt[(tokeep)]
    rm(housing_units_eth_data)
    #test <- table(housing_units_eth_dt$tract,housing_units_eth_dt$housing_units)==table(housing_units_race_dt$tract,housing_units_race_dt$housing_units)
    
    #either get race or owner_renter - housing units comes for both
    housing_units_rent_data <- housing_units_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_units_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("owner_renter","housing_units"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(own_rent = case_when(str_detect(owner_renter,"Owner") ~ "Owner occupied",
                                  str_detect(owner_renter,"Renter") ~ "Renter occupied")) %>%
      filter(!is.na(housing_units)) %>%
      uncount(as.numeric(number_sams),.id = "units_rent_id",.remove = TRUE)
    housing_units_rent_dt <- as.data.table(housing_units_rent_data)
    rm(housing_units_race_from_census)
    rm(housing_units_rent_data)
    #test: table(housing_units_race_data$tract,housing_units_race_data$housing_units)==table(housing_units_rent_data$tract,housing_units_rent_data$housing_units)

    #concept: HOUSEHOLD TYPE BY UNITS IN STRUCTURE
    #could use this to match with the housing_units_race?? - seems like you lose a lot of information with this one...
    #tells only if household is in single structure or complex - also worth adding, and gets correct number of 1562813
    household_type_units_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11011")
    household_type_units_data <- household_type_units_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>% 
      filter(label != "Nonfamily households") %>%
      pivot_longer(4:ncol(household_type_units_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_role_4","structs","num_structures"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        num_structures = if_else(family_role_4=="Other family",num_structures,if_else(family=="Nonfamily households",family_role_4,structs)),
        family_role_4 = if_else(family_role_4=="Other family",structs,if_else(family=="Nonfamily households",family,family_role_4))
      ) %>% 
      filter(!is.na(num_structures)) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "hh_units_id")
    hh_type_units_dt <- as.data.table(household_type_units_data)
    rm(household_type_units_data)

    #concept: TENURE BY OCCUPANTS PER ROOM, 1562813 (all hh) --only gives either >1 or <1 for each race
    housing_per_room_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25014") #ratio per tract occup per/room/race
    housing_per_room_race_data <- housing_per_room_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_per_room_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      rename(num_per_room=label) %>%
      mutate(race = substr(name,7,7),
             num_per = case_when(
               num_per_room=="1.00 or less occupants per room" ~ "<1",
               num_per_room=="1.01 or more occupants per room" ~ ">1"
             )) %>%
      filter(race %in% acs_race_codes & number_sams>0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_num_per_rooms_id",.remove = TRUE)
    housing_per_room_race_dt <- as.data.table(housing_per_room_race_data)
    rm(housing_per_room_race_data)
    #ethnicity
    housing_per_room_eth_data <- housing_per_room_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Owner occupied") %>%
      filter(label != "Renter occupied") %>%
      pivot_longer(4:ncol(housing_per_room_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("num_per_room","empty"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(ethnicity = substr(name,7,7),
             num_per = case_when(
               empty=="0.50 or less occupants per room" ~ "<1",
               empty=="0.51 to 1.00 occupants per room" ~ "<1",
               empty=="1.01 to 1.50 occupants per room" ~ ">1",
               empty=="1.51 to 2.00 occupants per room" ~ ">1",
               empty=="2.01 or more occupants per room" ~ ">1",
               num_per_room=="1.00 or less occupants per room" ~ "<1",
               num_per_room=="1.01 or more occupants per room" ~ ">1"
             ),
             num_per_room = case_when(
               !is.na(empty) & str_detect(empty,"0.5") ~ "1.00 or less occupants per room",
               !is.na(empty) & !str_detect(empty,"0.5") ~ "1.01 or more occupants per room",
               TRUE ~ num_per_room
             )
             ) %>%
      filter(!ethnicity %in% acs_race_codes) %>% 
      uncount(as.numeric(number_sams),.id = "per_room_ethnicity_race_id",.remove = TRUE)
    housing_per_room_eth_dt <- as.data.table(housing_per_room_eth_data)
    housing_per_room_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,num_per_room)]
    housing_per_room_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,num_per_room)]
    housing_per_room_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,num_per)]
    housing_per_room_eth_dt <- housing_per_room_eth_dt[(tokeep)]
    rm(housing_per_room_eth_data)
    
    #same one, for diff. part - five categories for per_room, but no diff. by race - could just move over by sampling...; it also has own_rent
    housing_per_room_rent_data <- housing_per_room_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_per_room_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","num_per_room_5"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(num_per = case_when(
        num_per_room_5=="0.50 or less occupants per room" ~ "<1",
        num_per_room_5=="0.51 to 1.00 occupants per room" ~ "<1",
        num_per_room_5=="1.01 to 1.50 occupants per room" ~ ">1",
        num_per_room_5=="1.51 to 2.00 occupants per room" ~ ">1",
        num_per_room_5=="2.01 or more occupants per room" ~ ">1"
             )) %>%
      filter(!is.na(num_per_room_5) & number_sams > 0) %>% 
      uncount(as.numeric(number_sams),.id = "own_rent_num_per_rooms_id",.remove = TRUE)
    housing_per_room_rent_dt <- as.data.table(housing_per_room_rent_data)
    rm(housing_per_room_race_from_census)
    rm(housing_per_room_rent_data)
    
    #concept is: TENURE BY AGE OF HOUSEHOLDER BY OCCUPANTS PER ROOM
    #num_per_room is different, but can be aggregated to match above 
    housing_per_room_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25015") #ratio per tract occup per/room/race
    housing_per_room_age_data <- housing_per_room_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_per_room_age_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","householder_age", "num_per_room"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(num_per = case_when(
               num_per_room=="1.00 or less occupants per room" ~ "<1",
               num_per_room=="1.01 to 1.50 occupants per room" ~ ">1",
               num_per_room=="1.51 or more occupants per room" ~ ">1"
             )) %>%
      filter(!is.na(num_per_room) & number_sams > 0) %>% #seems to keep num_per_room only for all age totals, not individual age
      uncount(as.numeric(number_sams),.id = "own_rent_age_per_rooms_id",.remove = TRUE)
    housing_per_room_age_dt <- as.data.table(housing_per_room_age_data)
    rm(housing_per_room_age_from_census)
    rm(housing_per_room_age_data)
                      
    #TENURE BY ROOMS- 1562813
    housing_occup_rooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25020") #of occup, number of rooms
    housing_occup_rooms_data <- housing_occup_rooms_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_rooms_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","num_rooms"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(num_rooms) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_num_rooms_id",.remove = TRUE)
    hh_occup_rooms_dt <- as.data.table(housing_occup_rooms_data)
    rm(housing_occup_rooms_from_census)
    rm(housing_occup_rooms_data)
    
    #TENURE BY BEDROOMS- 1562813                          
    housing_occup_bedrooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25042") #of occup, number of bedrooms
    housing_occup_bedrooms_data <- housing_occup_bedrooms_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_bedrooms_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","num_bedrooms"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(num_bedrooms=if_else(num_bedrooms=="No bedroom","0 bedrooms",num_bedrooms)) %>%
      filter(!is.na(num_bedrooms) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_num_bedrooms_id",.remove = TRUE)
    hh_occup_bedrooms_dt <- as.data.table(housing_occup_bedrooms_data)
    rm(housing_occup_bedrooms_from_census)
    rm(housing_occup_bedrooms_data)
    
#NOT USED YET    #may help as expanding, if group by date gives you family units...
    #concept:TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT
    #4484299 - just people in households - need to put in terms of full sam
    housing_occup_date_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25026") #of occup, own or rent by move in date
    housing_occup_date_data <- housing_occup_date_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total population in occupied housing units!!")) %>%
      filter(label != "Estimate!!Total population in occupied housing units") %>%
      pivot_longer(4:ncol(housing_occup_date_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","move_in_date"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(move_in_date) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_date_id",.remove = TRUE)
    housing_occup_date_dt <- as.data.table(housing_occup_date_data)
    rm(housing_occup_date_from_census)
    rm(housing_occup_date_data)
    
    #If you take the size and multiply it out, you get 4,194,975 in 2017 - so missing 330,554 - some in 7 or more 41220 in GQ? has right size for number of households total!!! Also matches total for housing_occup_hh_size
    #has right number of householders / households - it only mentions group quarters as not in a household, but that number is too low - https://www.census.gov/programs-surveys/cps/technical-documentation/subject-definitions.html#household
    #concept:HOUSEHOLD TYPE BY HOUSEHOLD SIZE
    household_type_size_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11016") 
    household_type_size_data <- household_type_size_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_size_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_or_non","hh_size"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_size)) %>%
      uncount(number_sams,.id = "family_id",.remove = TRUE) 
    hh_size_dt <- as.data.table(household_type_size_data)
    rm(household_type_size_from_census)
    rm(household_type_size_data)
    
    #add occupancy on same size
    #concept: TENURE BY HOUSEHOLD SIZE (own or rent of HH - not second mortgage, etc)
    housing_occup_hhsize_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25009") #of occup, own or rent by household size
    housing_occup_hhsize_data <- housing_occup_hhsize_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_hhsize_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_size"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_size) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hhsize_id",.remove = TRUE)
    hh_occup_size_dt <- as.data.table(housing_occup_hhsize_data)
    rm(housing_occup_hhsize_from_census)
    rm(housing_occup_hhsize_data)
    #make sure testtables <- table(hh_size_dt$tract,hh_size_dt$hh_size)==table(occup_size_dt$tract,occup_size_dt$hh_size) and FALSE for FALSE %in% testtables

#add this and transportation after expand household with number of workers, etc.    
    #concept is: NUMBER OF WORKERS IN HOUSEHOLD BY VEHICLES AVAILABLE - total is 1562813
    vehicles_workers_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08203")
    vehicles_workers_data <- vehicles_workers_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(vehicles_workers_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("number_workers_in_hh","number_vehicles_in_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(number_vehicles_in_hh)) %>%
      uncount(as.numeric(number_sams),.id = "workers_vehicles_id",.remove = TRUE)
    vehicles_workers_dt <- as.data.table(vehicles_workers_data)
    rm(vehicles_workers_from_census)
    rm(vehicles_workers_data)
    
    
    #means of transportation to work - perhaps use for all of the ones at bottom??
    #population of 2140881 - not sure what it matches to - if you expand household_workers you get 100k too few; it has two more tracts than the ones with 2135069
    transport_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08105")
    transport_race_data <- transport_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(means_transport = label) %>%
      mutate(means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport)) %>%
      filter(race %in% acs_race_codes & number_sams>0) %>%
      uncount(as.numeric(number_sams),.id = "transport_race_id",.remove = TRUE)
    transport_eth_data <- transport_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(ethnicity = substr(name,7,7)) %>%
      rename(means_transport = label) %>%
      mutate(means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport)) %>%
      uncount(as.numeric(number_sams),.id = "transport_race_id",.remove = TRUE)
    transport_race_dt <- as.data.table(transport_race_data)
    transport_eth_dt <- as.data.table(transport_eth_data)
    transport_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity %in% acs_race_codes]),by=.(tract,means_transport)]
    transport_eth_dt[ethnicity %in% acs_race_codes,("ethnicity"):="_"]
    transport_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,means_transport)]
    transport_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,means_transport)]
    transport_eth_dt <- transport_eth_dt[(tokeep)]
    #test <- table(transport_eth_dt$means_transport)==table(transport_race_dt$means_transport)
    rm(transport_eth_data)
    rm(transport_race_data)
    rm(transport_race_from_census)
    
    #concept: MEANS OF TRANSPORTATION TO WORK BY TENURE - for workers - 2135069
    transport_tenure_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08137")
    transport_tenure_data <- transport_tenure_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_tenure_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("means_transport","owner_renter"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(owner_renter) & number_sams > 0) %>%
      mutate(own_rent = if_else(str_detect(owner_renter,"owner"),"Owner occupied","Renter occupied"),
             means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport)
      ) %>%
      uncount(as.numeric(number_sams),.id = "workers_vehicles_id",.remove = TRUE)
    transport_tenure_dt <- as.data.table(transport_tenure_data)
    rm(transport_tenure_data)
    rm(transport_tenure_from_census)
    
    #vehicles_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08014")
    #vehicles_sex_data <- vehicles_sex_from_census %>%
    #  mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
    #  filter(label != "Estimate!!Total") %>%
    #  pivot_longer(4:ncol(vehicles_sex_from_census),names_to = "tract", values_to = "number_sams") %>% 
    #  separate(label, c("sex","number_vehicles_in_hh"), sep = "!!", remove = F, convert = FALSE) %>%
    #  filter(!is.na(number_vehicles_in_hh)) %>%
    #  uncount(as.numeric(number_sams),.id = "sex_vehicles_id",.remove = TRUE)
    #vehicles_sex_dt <- as.data.table(vehicles_sex_data)
    #rm(vehicles_sex_from_census)
    #rm(vehicles_sex_data)
    
    transport_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08006")
    transport_sex_data <- transport_sex_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_sex_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","means_transport","how_transport","carpool"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        how_transport=case_when(
        means_transport=="Bicycle" | means_transport=="Taxicab motorcycle or other means" | 
          means_transport=="Walked" | means_transport=="Worked at home" ~ means_transport,
        TRUE ~ how_transport
      ),
      means_transport_5=if_else(means_transport=="Bicycle" | means_transport=="Taxicab motorcycle or other means",
                                "Taxicab motorcycle bicycle or other means",means_transport)
      ) %>%
      filter(!is.na(how_transport) & str_detect(sex,"ale") & is.na(carpool)) %>%
      uncount(as.numeric(number_sams),.id = "sex_transport_id",.remove = TRUE)
    transport_sex_dt <- as.data.table(transport_sex_data)
    rm(transport_sex_data)
    rm(transport_sex_from_census)
    
    #concept: SEX OF WORKERS BY TRAVEL TIME TO WORK
    time_to_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08012")
    time_to_work_sex_data <- time_to_work_sex_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(time_to_work_sex_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","time_to_work"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(time_to_work)) %>%
      uncount(as.numeric(number_sams),.id = "time_to_work_id",.remove = TRUE)
    time_to_work_sex_dt <- as.data.table(time_to_work_sex_data)
    rm(time_to_work_sex_from_census)
    rm(time_to_work_sex_data)
  
    #concept: SEX OF WORKERS BY TIME LEAVING HOME TO GO TO WORK - 2061700
    when_go_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08011")
    when_go_work_sex_data <- when_go_work_sex_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(when_go_work_sex_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("sex","when_go_to_work"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(when_go_to_work)) %>%
      uncount(as.numeric(number_sams),.id = "when_work_id",.remove = TRUE)
    when_go_work_sex_dt <- as.data.table(when_go_work_sex_data)
    rm(when_go_work_sex_from_census)
    rm(when_go_work_sex_data)
    
    transport_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08101")
    transport_age_data <- transport_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("means_transport","age_range_7"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport)) %>%
      filter(!is.na(age_range_7)) %>%
      uncount(as.numeric(number_sams),.id = "trans_age_id",.remove = TRUE)
    transport_age_dt <- as.data.table(transport_age_data)
    rm(transport_age_from_census)
    rm(transport_age_data)
    
    #concept: MEANS OF TRANSPORTATION TO WORK BY LANGUAGE SPOKEN AT HOME AND ABILITY TO SPEAK ENGLISH
    transport_language_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08113")
    transport_language_data <- transport_language_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_language_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("means_transport","language","English_level"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(English_level=if_else(language=="Speak only English",language,English_level),
             means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport)) %>%
      filter(!is.na(English_level)) %>%
      uncount(as.numeric(number_sams),.id = "trans_language_id",.remove = TRUE)
    transport_language_dt <- as.data.table(transport_language_data)
    rm(transport_language_from_census)
    rm(transport_language_data)
    
    #numbers are 159 off - can't tell why...
    #concept: MEANS OF TRANSPORTATION TO WORK BY WORKERS' EARNINGS IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) 
    transport_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08119")
    transport_income_data <- transport_income_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_income_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("means_transport","income_range"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport)) %>%
      filter(!is.na(income_range)) %>%
      uncount(as.numeric(number_sams),.id = "trans_income_id",.remove = TRUE)
    transport_income_dt <- as.data.table(transport_income_data)
    rm(transport_income_from_census)
    rm(transport_income_data)
    
    #concept: MEANS OF TRANSPORTATION TO WORK BY TRAVEL TIME TO WORK - 2061700 - missing "work at home", but others also just a bit off
    transport_time_work_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08134")
    transport_time_work_data <- transport_time_work_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_time_work_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("means_transport_4","how_transport","time_to_work","time2"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(carpool_size=if_else(how_transport=="Carpooled",time_to_work,NULL),
             time_to_work=case_when(str_detect(means_transport_4,"Public") & is.na(time_to_work) ~ time2, #for NA
                                    str_detect(how_transport,"minutes") & means_transport_4!="Car truck or van" ~ how_transport,
                                    how_transport=="Carpooled" ~ time2,
                                    TRUE ~ time_to_work),
             how_transport=if_else(str_detect(how_transport,"minutes"),means_transport_4,how_transport)) %>%
      filter(!is.na(time_to_work)) %>%
      uncount(as.numeric(number_sams),.id = "trans_income_id",.remove = TRUE)
    transport_time_work_dt <- as.data.table(transport_time_work_data)
    rm(transport_time_work_from_census)
    rm(transport_time_work_data)
    
    transport_occupation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08124")
    transport_occupation_data <- transport_occupation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_occupation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("means_transport","occupation"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport)) %>%
      filter(!is.na(occupation)) %>%
      uncount(as.numeric(number_sams),.id = "trans_occupation_id",.remove = TRUE)
    transport_occupation_dt <- as.data.table(transport_occupation_data)
    rm(transport_occupation_from_census)
    rm(transport_occupation_data)
    
    transport_industry_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08126")
    transport_industry_data <- transport_industry_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_industry_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("means_transport","industry"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(means_transport_5=if_else(str_detect(means_transport,"truck"),"Car truck or van",means_transport),
             occupation_match=case_when( #a little random - no real way of matching on people's roles/occupation inside industry
               industry=="Agriculture forestry fishing and hunting and mining" |
                industry=="Transportation and warehousing and utilities"
                   ~ "Management business science and arts occupations",
               industry=="Construction" | industry=="Wholesale trade"
                   ~ "Natural resources construction and maintenance occupations",
               industry=="Manufacturing"
                   ~ "Production transportation and material moving occupations",
               industry=="Finance and insurance and real estate and rental and leasing" |
                 industry=="Information" | industry=="Public administration" | industry=="Retail trade"
                   ~ "Sales and office occupations",
               industry=="Arts entertainment and recreation and accommodation and food services" |
                 industry=="Educational services and health care and social assistance" |
                 industry=="Other services (except public administration)" | 
                 industry=="Professional scientific and management and administrative and waste management services"
                   ~ "Service occupations",
               industry=="Armed forces"
                   ~ "Military specific occupations",
               TRUE ~ "none given"
             )) %>%
      filter(!is.na(industry)) %>%
      uncount(as.numeric(number_sams),.id = "trans_occupation_id",.remove = TRUE)
    transport_industry_dt <- as.data.table(transport_industry_data)
    rm(transport_industry_from_census)
    rm(transport_industry_data)
 
    
    #concept is: HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD 
    household_workers_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08202")
    household_workers_data <- household_workers_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_workers_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("hh_size_4","number_workers_in_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(number_workers_in_hh) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "workers_id",.remove = TRUE)
    #when adding age, workers need to be 18
    hh_workers <- as.data.table(household_workers_data)
    hh_workers[,("no_workers"):=nrow(.SD[number_workers_in_hh=="No workers"]),by=.(tract)]
    rm(household_workers_from_census)
    rm(household_workers_data)
    
    #concept is:  HOUSEHOLD SIZE BY VEHICLES AVAILABLE - get hh / 1562813
    vehicles_household_size_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08201")
    vehicles_household_data <- vehicles_household_size_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(vehicles_household_size_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("hh_size_4","number_vehicles_in_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(number_vehicles_in_hh) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "vehicles_id",.remove = TRUE)
    vehicles_hh <- as.data.table(vehicles_household_data)
    rm(vehicles_household_size_from_census)
    rm(vehicles_household_data)
    #test: table(sam_hh$tract,sam_hh$number_vehicles_in_hh)==table(vehicles_hh$tract,vehicles_hh$number_vehicles_in_hh)
    
    #concept:PRESENCE OF OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE BY EMPLOYMENT STATUS
    #wife of a husband not in labor force is not listed as existing
    #attaches to family households, under sam_hh$family - Estimate!!Total gives 1062265 (which is 4384 short of Family households; all missing are family_type: Married couples)
    family_employment_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B23007")
    family_employment_data <- family_employment_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>% #doing just from this still give 1062265 and not 1066649
      filter(label != "With own children under 18 years!!Other family") %>%
      filter(label != "With own children under 18 years!!Other family!!Male householder no wife present") %>%
      filter(label != "With own children under 18 years!!Other family!!Male householder no wife present!!In labor force") %>%
      filter(label != "With own children under 18 years!!Other family!!Female householder no husband present!!In labor force") %>%
      filter(label != "With own children under 18 years!!Other family!!Female householder no husband present") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces!!Wife in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed!!Wife in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband not in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband not in labor force!!Wife in labor force") %>%
      filter(label != "With own children under 18 years") %>%
      filter(label != "No children under 18 years!!Other family") %>%
      filter(label != "No children under 18 years!!Other family!!Male householder no wife present!!In labor force") %>%
      filter(label != "No children under 18 years!!Other family!!Male householder no wife present") %>%
      filter(label != "No children under 18 years!!Other family!!Female householder no husband present!!In labor force") %>%
      filter(label != "No children under 18 years!!Other family!!Female householder no husband present") %>%
      filter(label != "No children under 18 years!!Married-couple family") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces!!Wife in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed!!Wife in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband not in labor force!!Wife in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband not in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force") %>%
      filter(label != "No children under 18 years") %>%
      pivot_longer(4:ncol(family_employment_data_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("own_kids","family_type","family_role","employed","husband_employ","wife_employed"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(#hh_employ=if_else(family_role=="Husband in labor force",employed,husband_employ),
        single_hh_employ=if_else(family_type=="Other family",employed,NULL),
        single_hh_employ=if_else(single_hh_employ=="In labor force",husband_employ,single_hh_employ),
        husband_employed=if_else(family_role=="Husband in labor force",employed,family_role),
        husband_employed=if_else(family_type=="Married-couple family",husband_employed,NULL),
        wife_employed=if_else(husband_employ=="Wife in labor force",wife_employed,husband_employ),
        wife_employed=if_else(employed=="Wife in labor force",husband_employ,wife_employed),
        wife_employed=if_else(employed=="Wife not in labor force",employed,wife_employed),
        wife_employed=if_else(family_type=="Married-couple family",wife_employed,NULL),
        family_role=if_else(str_detect(family_role,"householder"),family_role,family_type)
             ) %>%
      #     filter(family_type=="Other family" | !is.na(husband_employ) & number_sams > 0) %>%
      #      filter(!is.na(husband_employ) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "family_employ_id",.remove = TRUE)
    family_employment_data$husband_employ<-NULL
    family_employment_data$employed<-NULL
    family_employment_dt <- as.data.table(family_employment_data)
    rm(family_employment_data_from_census)
    rm(family_employment_data)
    
    #concept: TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT BY UNITS IN STRUCTURE
    tenure_yr_moved_units_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25129")
    tenure_yr_moved_units_data <- tenure_yr_moved_units_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(tenure_yr_moved_units_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","when_moved","housing_units"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(housing_units) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "tenure_when_id",.remove = TRUE)
    tenure_yr_moved_units_hh <- as.data.table(tenure_yr_moved_units_data)
    
    
    #just in case
#    saveRDS(sam_hh,file = paste0(housingdir, vintage, "/sam_hh_l.768",Sys.Date(),".RDS")) #"2020-04-15"
#    saveRDS(sam_rent,file = paste0(housingdir, vintage, "/sam_rent_",Sys.Date(),".RDS")) #"2020-04-06"

    #gives unmarried partners, straight and same-sex concept: UNMARRIED-PARTNER HOUSEHOLDS BY SEX OF PARTNER 
    #https://www.census.gov/library/stories/2019/09/unmarried-partners-more-diverse-than-20-years-ago.html - by 2017, close to even across ages / ethnicities, etc.
    household_type_partners_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11009") #only unmarried but same sex included
    household_type_partners_data <- household_type_partners_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Unmarried-partner households") %>%
      pivot_longer(4:ncol(household_type_partners_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("unmarried","partner_type"), sep = "!!", remove = F, convert = FALSE) %>%
      uncount(as.numeric(number_sams),.id = "partner_id",.remove = TRUE)
    #unmarried partners who are not householders are not counted here!!
    hh_partner_dt <- as.data.table(household_type_partners_data)
    
    #concept: FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN UNDER 18 YEARS 1066649 = sam_hh[family=="Family households"]
    #gives amount equal to hh_relation_dt[family_role=="Householder",family_or_non], In family households (1066649); In nonfamily households, at 496164 gets up to 1562813 total for households
    household_related_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11004")
    household_related_kids_data <- household_related_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_related_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_or_non","family_role","related_kids","age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        age = if_else(family_or_non=="Married-couple family",related_kids,age),
        kid_age = if_else(str_detect(related_kids,"No related") | str_detect(family_role,"No related"),"No children",age),
        family_role = if_else(family_or_non=="Married-couple family","Married-couple family",family_role)
      ) %>%
      filter(!is.na(kid_age)) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "related_kids_id")
    hh_kids <- as.data.table(household_related_kids_data)
    
    #get hh_type_kids is used on individuals side, too
    #hh_type_kids includes grandkids, but excludes hh, spouses, partners, and is everyone under 18 otherwise - marital should give that extra - could be less than 1k
    #hh_type_kids[family_or_non=="In nonfamily households",("in_family_type"):="In nonfamily households"]
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
    
    #nrow(sex_age_race[age_range=="0  to  5 years" | age_range=="5  to  9 years" | age_range=="10 to 14 years" | age_range=="15 to 17 years"]) - 1225059
    #concept is:"OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE" - each kid, not each hh - 1065925
    kids_family_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09002")
    kids_family_age_data <- kids_family_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_family_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_role_3","kid_age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(kid_age = if_else(family=="In married-couple families",family_role_3,kid_age),
             kid_age = if_else(kid_age=="Under 3 years","0 to 3 years",kid_age),
             family_role_3 = if_else(family=="In other families",family_role_3,"In married-couple family")) %>%
      filter(!is.na(kid_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "kids_age_id",.remove = TRUE)
    kids_ages_dt <- as.data.table(kids_family_age_data)
    rm(kids_family_age_from_census)
    rm(kids_family_age_data)
    
    #GRANDCHILDREN UNDER 18 YEARS LIVING WITH A GRANDPARENT HOUSEHOLDER BY AGE OF GRANDCHILD - 103908 
    #nrow(hh_relations_dt[family_role=="Grandchild"]) - 126406; other 22498 are grandchildren, but not of hh
    kids_age_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10001")
    kids_grandparents_age_data <- kids_age_grandparents_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_age_grandparents_from_census),names_to = "tract", values_to = "number_sams") %>% 
      #some may live 2 or more to a house
      rename(grandkid_age = label) %>%
      uncount(as.numeric(number_sams),.id = "kids_grandparents_id",.remove = TRUE)
    kids_grand_age <- as.data.table(kids_grandparents_age_data)
    rm(kids_age_grandparents_from_census)
    rm(kids_grandparents_age_data)
    
    #concept:GRANDCHILDREN UNDER 18 YEARS LIVING WITH A GRANDPARENT HOUSEHOLDER BY GRANDPARENT RESPONSIBILITY AND PRESENCE OF PARENT
    #103908 is number gp_hh - this has more gp_hh responsible than the others have as responsible!!
    kids_respons_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10002")
    kids_gp_resp_data <- kids_respons_grandparents_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_respons_grandparents_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("gp_respon","parent_present"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(parent_present = if_else(str_detect(gp_respon,"not"),"gp_not_resp",parent_present)) %>%
      filter(!is.na(parent_present)) %>%
      uncount(as.numeric(number_sams),.id = "kids_gp_id",.remove = TRUE)
    kids_gp_resp <- as.data.table(kids_gp_resp_data)
    rm(kids_respons_grandparents_from_census)
    rm(kids_gp_resp_data)
    
    #130470 is all grandparents with their kids, including married couples - this allows separated as under married
    #concept:MARITAL STATUS BY GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT
    #number of grandparents, not number of kids; includes householders; counts both sex, and if their married, above, at 130470
    kids_respons_grands_marital_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10057")
    kids_grandparents_marital_data <- kids_respons_grands_marital_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_respons_grands_marital_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("marital_status_gp","gp_respon","age_range_gp"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(age_range_gp=if_else(str_detect(gp_respon,"not responsible"),"not responsible",age_range_gp)) %>%
      filter(!is.na(age_range_gp)) %>%
      uncount(as.numeric(number_sams),.id = "kids_grandparents_id",.remove = TRUE)
    kids_grand_marital <- as.data.table(kids_grandparents_marital_data)
    rm(kids_respons_grands_marital_from_census)
    rm(kids_grandparents_marital_data)
    
    #concept:GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN BY LENGTH OF TIME RESPONSIBLE FOR OWN GRANDCHILDREN FOR THE POPULATION 30 YEARS AND OVER
    #number of grandparents, not number of kids; includes householders; counts both sex, and if their married, above, at 130470
    kids_respons_grands_time_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10050")
    kids_gp_time_data <- kids_respons_grands_time_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_respons_grands_time_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("live_grandkids","gp_respon","time_gp_respon"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(time_gp_respon = if_else(str_detect(gp_respon,"not"),"gp_not_resp",time_gp_respon)) %>%
      filter(!is.na(time_gp_respon)) %>%
      uncount(as.numeric(number_sams),.id = "kids_gp_id",.remove = TRUE)
    kids_gp_time <- as.data.table(kids_gp_time_data)
    rm(kids_respons_grands_time_from_census)
    rm(kids_gp_time_data)
    
    #concept: by acs_race: GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT
    #number of grandparents, not number of kids; includes householders; counts both sex, and if their married, above, at 130470
    kids_respons_grands_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10051")
    kids_grandparents_race_data <- kids_respons_grands_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      mutate(race = substr(name,7,7)) %>%
      pivot_longer(4:ncol(kids_respons_grands_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("gp_respon","age_range_gp"), sep = "!!", remove = F, convert = FALSE) %>%
      #mutate(age_range_gp=if_else(str_detect(resp_grandkids,"not responsible"),"not responsible",age_range_gp)) %>%
      filter(!is.na(age_range_gp) & race%in%LETTERS[1:9]) %>% #acs_race_codes gives total of 130470; this just needs to match separately
      uncount(as.numeric(number_sams),.id = "kids_grandparents_id",.remove = TRUE)
    kids_grand_race <- as.data.table(kids_grandparents_race_data)
    rm(kids_respons_grands_race_from_census)
    rm(kids_grandparents_race_data)
    
}}
    



