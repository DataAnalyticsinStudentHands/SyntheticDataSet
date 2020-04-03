library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(Hmisc)
#library(rio) #may not need here - playing with it
library(FactoMineR)
library(doParallel)
library(foreach)
#for distance calculation ? may do as own calculation instead
#library(stats)
#census does a process of modeling, based on weighting of responses. Hard to find exactly where they're doing the projections.
#cf. Appendix B, starting at esp. around 103: https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf

#' createHouseholds
#'
#' This function simulates individual people based on census data (age, sex, race, as of April 1, vintage year).
#' If simulated data already exists, it will read it from an RDS file.
#'
#' @return sam_residents A dataframe of simulated people.
createHouseholds <- function() {
  
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
    #American community survey 5yr variables: https://api.census.gov/data/2018/acs/acs5/variables.html
    #definitions: https://www2.census.gov/programs-surveys/acs/tech_docs/subject_definitions/2018_ACSSubjectDefinitions.pdf
    
    #setup race codes https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
    acs_race_codes <- c("A","B","C","D","E","F","G") #could collect all - add them up, without H and I, and you get the total! H is white alone, not hispanic and I is hispanic and if you add them up they don't equal white alone
    acs_ethnicity <- c("H","I") #H is White Alone, not Hispanic or Latino; I is Hispanic or Latino #usually just use !acs_race_codes
    
        
    #may have better way of dealing with age numbers...
    #add age_range to work for merge with marital data, uses data.table to make this fast
    #there are tricks that get repeated, which could become subroutines, but you have to make sure the factors match
    #table == table
    #join on id that's been generated so it has same size for total join, even if of different size
    #anti-join to fill in if there are missing rows
    #look for missing id
    #    hh_type_eth_dt <- rbind(hh_type_eth_dt,anti_join(hh_type_race_dt,hh_type_eth_dt,by=c("num_eth_id")),fill = TRUE)
    #and then do id trick 
    #    hh_type_eth_dt[order(match(ethnicity,c("H","I","_"))),
    #                   ("num_eth_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]  
    #clean up variables
    #create factor levels
    
    #gives 1562813; householders by tract is one of the base constants
    #concept: "HOUSEHOLD TYPE (INCLUDING LIVING ALONE) for acs_race_codes"
    household_type_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11001") #vgl. B25006??
    household_type_race_data <- household_type_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_type","hh_role"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        race = substr(name,7,7),
        family_role = if_else(family_type=="Other family",hh_role,family_type)
      ) %>% 
      filter(!is.na(family_role)) %>%
      filter(number_sams > 0 & race %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "hh_type_race_id") 
    hh_type_race_dt <- as.data.table(household_type_race_data)
    
    #table(household_type_race_data$family) 
    #Family households Nonfamily households 
    # 1066649               496164
    #table(household_type_race_data$family_type)
    #Householder living alone Householder not living alone        Married-couple family                 Other family 
    # 408614                        87550                       734108                       332541 
    #better to use family role, if we can match
    #doesn't seem to add anything more than above
    #household_related_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11002")
    household_type_ethnicity_data <- household_type_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_type","hh_role"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        ethnicity = substr(name,7,7),
        family_role = if_else(family_type=="Other family",hh_role,family_type)
      ) %>% 
      filter(!is.na(family_role)) %>%
      filter(number_sams > 0 & !ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "hh_type_ethnicity_id")
    #do the id trick so that whitealone, but order ethnicity by H, I and then order race_adj by A,F,G,etc. with percentages of each, then the remaining
    hh_type_ethnicity_dt <- as.data.table(household_type_ethnicity_data)
    hh_type_ethnicity_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,family_role)]
    hh_type_ethnicity_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,family_role)]
    hh_type_ethnicity_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,family_role)]
    hh_type_eth_dt <- hh_type_ethnicity_dt[(tokeep)]
    #assign ids #what you want is for each id to have counted out for the family_role, so that the family role total will match
    hh_type_race_dt[(order(match(race,c("A","F","G","C","B","E","D")))),
                    ("num_eth_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    hh_type_eth_dt[order(match(ethnicity,c("H","I","_"))),
                   ("num_eth_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    
    #join back to hh_type_race - ethnicity is really just hispanic and white, not hispanic
    hh_type_race_dt[,c("ethnicity") := hh_type_eth_dt[.SD, list(ethnicity), on = .(tract,family_role,num_eth_id)]]
    
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
    occupied_eth_data <- housing_occup_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(ethnicity = substr(name,7,7)) %>%
      rename(own_rent = label) %>%
      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "own_ethnicity_race_id",.remove = TRUE)
    
    occupied_race_dt <- as.data.table(occupied_race_data)
    occupied_eth_dt <- as.data.table(occupied_eth_data)
    occupied_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,own_rent)]
    occupied_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,own_rent)]
    occupied_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,own_rent)]
    occupied_eth_dt <- occupied_eth_dt[(tokeep)]
    #assign ids #what you want is for each id to have counted out for the family_role, so that the family role total will match
    occupied_race_dt[(order(match(race,c("A","F","G","C","B","E","D")))),
                    ("num_eth_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    occupied_eth_dt[order(match(ethnicity,c("H","I","_"))),
                   ("num_eth_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    occupied_race_dt[,c("ethnicity") := occupied_eth_dt[.SD, list(ethnicity), on = .(tract,own_rent,num_eth_id)]]
    #test <- table(occupied_race_dt$tract,occupied_race_dt$race,occupied_race_dt$ethnicity)==table(hh_type_race_dt$tract,hh_type_race_dt$race,hh_type_race_dt$ethnicity)
#then match on race, ethnicity both each time - if tables don't match, need an overarching count that can be matched...like just ethnicity
    
    housing_occup_hhtype_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25011") #of occup, own or rent by household type
    housing_occup_hhtype_data <- housing_occup_hhtype_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_hhtype_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","family","family_type","partner_present","householder_age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        householder_age = if_else(str_detect(partner_present,"years"),partner_present,householder_age),
        householder_age = if_else(str_detect(family_type,"years"),family_type,householder_age),
        partner_present = if_else(str_detect(partner_present,"present"),partner_present,"")
      ) %>% 
      filter(!is.na(householder_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hhtype_id",.remove = TRUE)
    occup_type_dt <- as.data.table(housing_occup_hhtype_data)
    #sample inside each group
    occup_type_dt[order(match(own_rent,c("Owner occupied","Renter occupied"))),
                  ("num_type_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),by=.(tract,own_rent)]
    occupied_race_dt[order(match(own_rent,c("Owner occupied","Renter occupied"))),
                  ("num_type_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),by=.(tract,own_rent)]
    occupied_race_dt[,c("family_type","householder_age") := 
                       occup_type_dt[.SD, c(list(family_type),list(householder_age)), on = .(tract,own_rent,num_type_id)]]
    #test <- table(occupied_race_dt$tract,occupied_race_dt$family_type)==table(hh_type_race_dt$tract,hh_type_race_dt$family_type)
    #join back to hh_type_race_dt
    #need to add an id with it sorted by race, ethnicity, own_rent, family_type - for both
    occupied_race_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                            match(ethnicity,c("H","I")), #ethnicity has just a couple of tracts that don't match - sort to keep them in order
                            match(family_type,c("Householder living alone","Householder not living alone",
                                                "Married-couple family","Other family"))
                            )),
                     ("hh_age_id"):=paste0(tract,family_type,as.character(1000000+sample(.N))),
                     by=.(tract,family_type)]
    hh_type_race_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                            match(ethnicity,c("H","I")),
                            match(family_type,c("Householder living alone","Householder not living alone",
                                                "Married-couple family","Other family"))
                      )),
                      ("hh_age_id"):=paste0(tract,family_type,as.character(1000000+sample(.N))),
                      by=.(tract,family_type)]
    hh_type_race_dt[,c("own_rent","householder_age") := 
                       occupied_race_dt[.SD, c(list(own_rent),list(householder_age)), on = .(hh_age_id)]]
    #test: table(occupied_race_dt$tract,occupied_race_dt$householder_age)==table(hh_type_race_dt$tract,hh_type_race_dt$householder_age)
    
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
    housing_units_eth_data <- housing_units_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Owner-occupied housing units") %>%
      filter(label != "Renter-occupied housing units") %>%
      pivot_longer(4:ncol(housing_units_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("owner_renter","housing_units"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(ethnicity = substr(name,7,7),
             housing_units = if_else(is.na(housing_units),owner_renter,housing_units),
             own_rent = case_when(str_detect(owner_renter,"Owner") ~ "Owner occupied",
                                  str_detect(owner_renter,"Renter") ~ "Renter occupied")) %>%
      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "units_ethnicity_race_id",.remove = TRUE)
    housing_units_rent_data <- housing_units_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_units_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("owner_renter","housing_units"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(own_rent = case_when(str_detect(owner_renter,"Owner") ~ "Owner occupied",
                                  str_detect(owner_renter,"Renter") ~ "Renter occupied")) %>%
      filter(!is.na(housing_units)) %>%
      uncount(as.numeric(number_sams),.id = "units_ethnicity_race_id",.remove = TRUE)
    
    #test: table(housing_units_race_data$tract,housing_units_race_data$housing_units)==table(housing_units_rent_data$tract,housing_units_rent_data$housing_units)
    housing_units_race_dt <- as.data.table(housing_units_race_data)
    housing_units_eth_dt <- as.data.table(housing_units_eth_data)
    housing_units_rent_dt <- as.data.table(housing_units_rent_data)
    housing_units_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,housing_units)]
    housing_units_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,housing_units)]
    housing_units_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,housing_units)]
    housing_units_eth_dt <- housing_units_eth_dt[(tokeep)]
    housing_units_race_dt[(order(match(race,c("A","F","G","C","B","E","D")))),
                     ("num_eth_id"):=paste0(tract,housing_units,as.character(1000000+seq.int(1:.N))),by=.(tract,housing_units)]
    housing_units_eth_dt[order(match(ethnicity,c("H","I","_"))),
                    ("num_eth_id"):=paste0(tract,housing_units,as.character(1000000+seq.int(1:.N))),by=.(tract,housing_units)]
    housing_units_race_dt[,c("ethnicity") := housing_units_eth_dt[.SD, list(ethnicity), on = .(num_eth_id)]]
    #test <- table(housing_units_race_dt$tract,housing_units_race_dt$race,housing_units_race_dt$ethnicity)==table(hh_type_race_dt$tract,hh_type_race_dt$race,hh_type_race_dt$ethnicity)
    #length(test[test==FALSE])/length(test) = 0.047
    housing_units_race_dt[(order(match(housing_units,c("1 attached","1 detached","10 to 19","2","20 to 49",
                                                       "3 or 4","5 to 9","50 or more","Boat RV van etc.","Mobile home")))),
                          ("num_units_id"):=paste0(tract,housing_units,as.character(1000000+seq.int(1:.N))),by=.(tract,housing_units)]
    housing_units_rent_dt[(order(match(housing_units,c("1 attached","1 detached","10 to 19","2","20 to 49",
                                                       "3 or 4","5 to 9","50 or more","Boat RV van etc.","Mobile home")))),
                          ("num_units_id"):=paste0(tract,housing_units,as.character(1000000+seq.int(1:.N))),by=.(tract,housing_units)]
    housing_units_race_dt[,c("own_rent") := housing_units_rent_dt[.SD, list(own_rent), on = .(tract,housing_units,num_units_id)]]
    #test: table(housing_units_race_dt$tract,housing_units_race_dt$own_rent)==table(hh_type_race_dt$tract,hh_type_race_dt$own_rent)
    #need to add an id with it sorted by race, ethnicity, own_rent, family_type - for both
    housing_units_race_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                            match(ethnicity,c("H","I")), #ethnicity has just a couple of tracts that don't match - sort to keep them in order
                            match(own_rent,c("Owner occupied","Renter occupied"))
    )),
    ("hh_units_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),
    by=.(tract,own_rent)]
    hh_type_race_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                           match(ethnicity,c("H","I")),
                           match(own_rent,c("Owner occupied","Renter occupied"))
    )),
    ("hh_units_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),
    by=.(tract,own_rent)]
    hh_type_race_dt[,c("housing_units") := 
                      housing_units_race_dt[.SD, c(list(housing_units)), on = .(hh_units_id)]]
    #test: table(occupied_race_dt$tract,occupied_race_dt$householder_age)==table(hh_type_race_dt$tract,hh_type_race_dt$householder_age)
  
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
    #ethnicity
    housing_per_room_eth_data <- housing_per_room_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Owner occupied") %>%
      filter(label != "Renter occupied") %>%
      pivot_longer(4:ncol(housing_per_room_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","num_per_room"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(ethnicity = substr(name,7,7),
             num_per_room = if_else(is.na(num_per_room),own_rent,num_per_room),
             num_per = case_when(
               num_per_room=="1.00 or less occupants per room" ~ "<1",
               num_per_room=="1.01 or more occupants per room" ~ ">1",
               num_per_room=="0.50 or less occupants per room" ~ "<1",
               num_per_room=="0.51 to 1.00 occupants per room" ~ "<1",
               num_per_room=="1.01 to 1.50 occupants per room" ~ ">1",
               num_per_room=="1.51 to 2.00 occupants per room" ~ ">1",
               num_per_room=="2.01 or more occupants per room" ~ ">1"
             )) %>%
      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "per_room_ethnicity_race_id",.remove = TRUE)
    #same one, for diff. part - five categories for per_room, but no diff. by race - could just move over by sampling...; it also has own_rent
    housing_per_room_rent_data <- housing_per_room_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_per_room_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","num_per_room"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(race = substr(name,7,7),
             num_per = case_when(
               num_per_room=="0.50 or less occupants per room" ~ "<1",
               num_per_room=="0.51 to 1.00 occupants per room" ~ "<1",
               num_per_room=="1.01 to 1.50 occupants per room" ~ ">1",
               num_per_room=="1.51 to 2.00 occupants per room" ~ ">1",
               num_per_room=="2.01 or more occupants per room" ~ ">1"
             )) %>%
      filter(!is.na(num_per_room) & number_sams > 0) %>% 
      uncount(as.numeric(number_sams),.id = "own_rent_num_per_rooms_id",.remove = TRUE)
    housing_per_room_race_dt <- as.data.table(housing_per_room_race_data)
    housing_per_room_eth_dt <- as.data.table(housing_per_room_eth_data)
    housing_per_room_rent_dt <- as.data.table(housing_per_room_rent_data)
    housing_per_room_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,num_per)]
    housing_per_room_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,num_per)]
    housing_per_room_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,num_per)]
    housing_per_room_eth_dt <- housing_per_room_eth_dt[(tokeep)]
    housing_per_room_race_dt[(order(match(race,c("A","F","G","C","B","E","D")))),
                          ("num_eth_id"):=paste0(tract,num_per,as.character(1000000+seq.int(1:.N))),by=.(tract,num_per)]
    housing_per_room_eth_dt[order(match(ethnicity,c("H","I","_"))),
                         ("num_eth_id"):=paste0(tract,num_per,as.character(1000000+seq.int(1:.N))),by=.(tract,num_per)]
    housing_per_room_race_dt[,c("ethnicity") := housing_per_room_eth_dt[.SD, list(ethnicity), on = .(num_eth_id)]]
    #test <- table(housing_per_room_race_dt$tract,housing_per_room_race_dt$race,housing_per_room_race_dt$ethnicity)==table(hh_type_race_dt$tract,hh_type_race_dt$race,hh_type_race_dt$ethnicity)
    #length(test[test==FALSE])/length(test) = 0.051
    housing_per_room_race_dt[(order(match(num_per,c("<1",">1")))),
                          ("num_units_id"):=paste0(tract,num_per,as.character(1000000+seq.int(1:.N))),by=.(tract,num_per)]
    housing_per_room_rent_dt[(order(match(num_per,c("<1",">1")))),
                          ("num_units_id"):=paste0(tract,num_per,as.character(1000000+seq.int(1:.N))),by=.(tract,num_per)]
    housing_per_room_race_dt[,c("own_rent","person_per_room") := housing_per_room_rent_dt[.SD, c(list(own_rent),list(num_per_room)), 
                                                                  on = .(tract,num_per,num_units_id)]]
    #test: table(housing_per_room_race_dt$tract,housing_per_room_race_dt$own_rent)==table(hh_type_race_dt$tract,hh_type_race_dt$own_rent)
    housing_per_room_race_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                                 match(ethnicity,c("H","I")), #ethnicity has just a couple of tracts that don't match - sort to keep them in order
                                 match(own_rent,c("Owner occupied","Renter occupied"))
    )),
    ("hh_per_room_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),
    by=.(tract,own_rent)]
    hh_type_race_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                           match(ethnicity,c("H","I")),
                           match(own_rent,c("Owner occupied","Renter occupied"))
    )),
    ("hh_per_room_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),
    by=.(tract,own_rent)]
    hh_type_race_dt[,c("person_per_room") := 
                      housing_per_room_race_dt[.SD, c(list(person_per_room)), on = .(hh_per_room_id)]]

    #foodstamps B22005 race of HH
    food_stamps_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B22005")
    food_stamps_data <- food_stamps_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(food_stamps_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(food_stamps = label) %>%
      filter(race %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "food_stamps_id",.remove = TRUE)
    food_stamps_eth_data <- food_stamps_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(food_stamps_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(ethnicity = substr(name,7,7)) %>%
      rename(food_stamps = label) %>%
#      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "food_stamps_id",.remove = TRUE)
    food_stamps_dt <- as.data.table(food_stamps_data)
    food_stamps_eth_dt <- as.data.table(food_stamps_eth_data) 
    food_stamps_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity %in% acs_race_codes]),by=.(tract,food_stamps)]
    food_stamps_eth_dt[order(match(ethnicity,c("H","I",ethnicity %in% acs_race_codes))),c("cnt_ethn"):=list(1:.N),by=.(tract,food_stamps)]
    food_stamps_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,food_stamps)]
    food_stamps_eth_dt <- food_stamps_eth_dt[(tokeep)]
    #assign ids #what you want is for each id to have counted out for the family_role, so that the family role total will match
    food_stamps_dt[(order(match(race,c("A","F","G","C","B","E","D")))),
                     ("num_eth_id"):=paste0(tract,food_stamps,as.character(1000000+seq.int(1:.N))),by=.(tract,food_stamps)]
    food_stamps_eth_dt[order(match(ethnicity,c("H","I",ethnicity %in% acs_race_codes))),
                    ("num_eth_id"):=paste0(tract,food_stamps,as.character(1000000+seq.int(1:.N))),by=.(tract,food_stamps)]
    food_stamps_eth_dt[ethnicity %in% acs_race_codes,("ethnicity"):="_"]
    food_stamps_dt[,c("ethnicity") := food_stamps_eth_dt[.SD, list(ethnicity), on = .(tract,food_stamps,num_eth_id)]]
    #test <- table(food_stamps_dt$tract,food_stamps_dt$race,food_stamps_dt$ethnicity)==table(hh_type_race_dt$tract,hh_type_race_dt$race,hh_type_race_dt$ethnicity)
    #length(test[test==FALSE])/length(test) = 0.54 
    food_stamps_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                                    match(ethnicity,c("H","I"))) 
              ),
              ("hh_food_stamps_id"):=paste0(tract,race,as.character(1000000+sample(.N))),
              by=.(tract,race)]
    hh_type_race_dt[(order(match(race,c("A","F","G","C","B","E","D")),
                           match(ethnicity,c("H","I")))
              ),
              ("hh_food_stamps_id"):=paste0(tract,race,as.character(1000000+sample(.N))),
              by=.(tract,race)]
    hh_type_race_dt[,c("SNAP") := 
                      food_stamps_dt[.SD, c(list(food_stamps)), on = .(hh_food_stamps_id)]]
    #test: table(hh_type_race_dt$tract,hh_type_race_dt$SNAP)==table(food_stamps_dt$tract,food_stamps_dt$food_stamps)
    
    
    #concept: SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER x acs_race_codes
    marital_status_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B12002")
    
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
    preg_data_dt <- as.data.table(pregnancy_data)
    
    
    
    #ugh get 4,194,975 in 2017 - so missing 330,554 - some in 7 or more 41220 in GQ? has right size for number of households total!!! Also matches total for housing_occup_hh_size
    #could be that householders not living alone (87550) should somehow be counted as having roommates; up to 330554-41220 / 87550 = 3.3 roommates on avg?
    #has right number of householders / households - it only mentions group quarters as not in a household, but that number is too low - https://www.census.gov/programs-surveys/cps/technical-documentation/subject-definitions.html#household
    household_type_size_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11016") 
    household_type_size_data <- household_type_size_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_type_size_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","hh_size"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_size)) %>%
      #mutate(numeric_in_family = as.numeric(substr(hh_size,1,1))) %>%
      uncount(number_sams,.id = "family_id",.remove = TRUE) #%>% #has right size for number of households total!!! maybe family / non-family mixed don't add in??
#      uncount(numeric_in_family,.id="num_family_id",.remove = FALSE)  
    
    #concept is: HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD
    household_workers_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08202")
    household_workers_data <- household_workers_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_workers_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("number_in_hh","number_workers_in_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(number_workers_in_hh) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "workers_id",.remove = TRUE)

    #concept is:  HOUSEHOLD SIZE BY VEHICLES AVAILABLE
    vehicles_household_size_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08201")
    vehicles_household_data <- vehicles_household_size_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(vehicles_household_size_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("number_in_hh","number_vehicles_in_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(number_vehicles_in_hh) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "vehicles_id",.remove = TRUE)
    
    #concept is: NUMBER OF WORKERS IN HOUSEHOLD BY VEHICLES AVAILABLE - total is 3125626 - which is all adults??
    vehicles_workers_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08203")
    vehicles_workers_data <- vehicles_workers_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(vehicles_workers_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("number_workers_in_hh","number_vehicles_in_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(str_detect(number_workers_in_hh,"worker") & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "workers_vehicles_id",.remove = TRUE)
    
    #means of transportation to work
    #population of 2140881 - not sure what it matches to - if you expand household_workers you get 100k too few
    transport_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08105")
    transport_race_data <- transport_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(transport_to_work = label) %>%
      filter(race %in% acs_race_codes & number_sams>0) %>%
      uncount(as.numeric(number_sams),.id = "transport_race_id",.remove = TRUE)
    transport_eth_data <- transport_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(ethnicity = substr(name,7,7)) %>%
      rename(transport_to_work = label) %>%
      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "transport_race_id",.remove = TRUE)
#DIDN'T GIVE "_" numbers for some reason, so have to work on how to make this trick work    
    transport_race_dt <- as.data.table(transport_race_data)
    transport_eth_dt <- as.data.table(transport_eth_data)
    transport_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity %in% acs_ethnicity]),by=.(tract,transport_to_work)]
    transport_eth_dt[order(match(ethnicity,c("H","I",!ethnicity %in% acs_ethnicity))),c("cnt_ethn"):=list(1:.N),by=.(tract,transport_to_work)]
    transport_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,transport_to_work)]
    transport_eth_dt <- transport_eth_dt[(tokeep)]
    #assign ids #what you want is for each id to have counted out for the family_role, so that the family role total will match
    transport_race_dt[order(match(race,c("A","F","G","C","B","E","D"))),
                      c("num_eth_id"):=paste0(tract,transport_to_work,as.character(1000000+seq.int(1:.N))),by=.(tract,transport_to_work)]
    transport_eth_dt[order(match(ethnicity,c("H","I","_"))),
                     c("num_eth_id"):=paste0(tract,transport_to_work,as.character(1000000+seq.int(1:.N))),by=.(tract,transport_to_work)]
    transport_race_dt[,c("ethnicity") := transport_eth_dt[.SD, list(ethnicity), on = .(tract,transport_to_work,num_eth_id)]]
    #test <- table(occupied_race_dt$tract,occupied_race_dt$race,occupied_race_dt$ethnicity)==table(hh_type_race_dt$tract,hh_type_race_dt$race,hh_type_race_dt$ethnicity)
    #then move to sam after it's more than just households
    
    #concept: MEANS OF TRANSPORTATION TO WORK BY TENURE
    transport_tenure_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08137")
    transport_tenure_data <- transport_tenure_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_tenure_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("hh_means_transport","owner_renter"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(owner_renter) & number_sams > 0) %>%
      mutate(own_rent = if_else(str_detect(owner_renter,"owner"),"Owner occupied","Renter occupied")) %>%
      uncount(as.numeric(number_sams),.id = "workers_vehicles_id",.remove = TRUE)
    
    #concept is:"OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE" - 
    #get kids' race, etc. and add to the kids side; already have it for the householders; then distribute adults, etc.
    kids_family_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09002")
    kids_family_age_data <- kids_family_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_family_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_type","kid_age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(kid_age = if_else(family=="In married-couple families",family_type,kid_age)) %>%
      filter(!is.na(kid_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "kids_age_id",.remove = TRUE)
    
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
    
    #concept is:"RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS BY NATIVITY OF CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY LIVING ARRANGEMENTS AND NATIVITY OF PARENTS"
    pov_ratio_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05010") #count of kids with family income less than pov.
    pov_ratio_kids_data <- pov_ratio_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(pov_ratio_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("poverty_ratio","parent_type","parent_nativity"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(parent_nativity) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "poverty_kids_id",.remove = TRUE)
    
    #gives unmarried partners, straight and same-sex 
    #https://www.census.gov/library/stories/2019/09/unmarried-partners-more-diverse-than-20-years-ago.html - by 2017, close to even across ages / ethnicities, etc.
    household_type_partners_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11009") #only unmarried but same sex included
    household_type_partners_data <- household_type_partners_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "Unmarried-partner households") %>%
      pivot_longer(4:ncol(household_type_partners_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("unmarried","partner_type"), sep = "!!", remove = F, convert = FALSE) %>%
      uncount(as.numeric(number_sams),.id = "partner_id",.remove = TRUE)
    
    #concept: HOUSEHOLD TYPE BY UNITS IN STRUCTURE
    #could use this to match with the housing_units_race??
    #tells only if household is in single structure or complex - also worth adding, and gets correct number of 1562813
    household_type_units_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11011")
    household_type_units_data <- household_type_units_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>% 
      filter(label != "Nonfamily households") %>%
      pivot_longer(4:ncol(household_type_units_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","fam_role_units","structs","num_structures"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        num_structures = if_else(fam_role_units=="Other family",num_structures,if_else(family=="Nonfamily households",fam_role_units,structs)),
        fam_role_units = if_else(fam_role_units=="Other family",structs,if_else(family=="Nonfamily households",hh_type,fam_role_units))
      ) %>% 
      filter(!is.na(num_structures)) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "hh_units_id")
    
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
    
    #this gets you seniors, too - slightly different totals from one with just seniors, for some reason
    household_adults_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09021")
    household_adults_relation_data <- household_adults_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_adults_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("age_range","relation_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(relation_hh)) %>%
      uncount(as.numeric(number_sams),.id = "family_id",.remove = TRUE) 
    
    #adults and kids gets you right(ish) total (20,000, depending on using seniors from inside adults or separately)
    #could be worth doing, to get the right relationship with seniors, but not now
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
    
    #gives amount equal to hh_relation_dt[family_role=="Householder",family_or_non], In family households (1066649); In nonfamily households, at 496164 gets up to 1562813 total for households
    household_related_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11004")
    household_related_kids_data <- household_related_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_related_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_or_non","family_type","related_kids","age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        age = if_else(family_or_non=="Married-couple family",related_kids,age),
        kid_age = if_else(str_detect(related_kids,"No related") | str_detect(family_type,"No related"),"No children",age),
        family_type = if_else(family_or_non=="Married-couple family","Married-couple family",family_type)
      ) %>%
      filter(!is.na(kid_age)) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "related_kids_id")
    
    #seems to repeat other info
    household_own_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11003") 
    household_own_kids_data <- household_own_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_own_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_type","family_role","own_kids","age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        own_kids = if_else(family_type=="Other family",own_kids,family_role),
        family_role = if_else(family_type=="Other family",family_type,family_role), #family_role gives with and without own children and other families
        age = if_else(family_type=="Other family",
                      if_else(str_detect(family_role,"No own"),family_role,
                              if_else(str_detect(own_kids,"No own"),own_kids,age)),
                      if_else(str_detect(family_role,"No own"),family_role,own_kids))
      ) %>% 
      filter(!is.na(age)) %>%
      rename(kid_age = age) %>%
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
    
    housing_occup_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25007") #of occup, own or rent by age
    housing_occup_age_data <- housing_occup_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_age_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","householder_age"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(householder_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_age_id",.remove = TRUE)
    
    #MORTGAGE STATUS BY AGE OF HOUSEHOLDER
    #shows for 855629 - with different age groups from housing_per_room_age_data (which also has 1562813 / hh)
    mortgage_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25027") 
    mortgage_age_data <- mortgage_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_age_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("mortgage","householder_age"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(householder_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_age_id",.remove = TRUE)
    
    #concept:TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT
    #4484299 - not sure why not complete? group quarters?
    housing_occup_date_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25026") #of occup, own or rent by move in date
    housing_occup_date_data <- housing_occup_date_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total population in occupied housing units!!")) %>%
      filter(label != "Estimate!!Total population in occupied housing units") %>%
      pivot_longer(4:ncol(housing_occup_date_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","move_in_date"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(move_in_date) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_date_id",.remove = TRUE)
    
    #concept: TENURE BY HOUSEHOLD SIZE (own or rent of HH - not second mortgage, etc)
    housing_occup_hhsize_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25009") #of occup, own or rent by household size
    housing_occup_hhsize_data <- housing_occup_hhsize_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_hhsize_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_size"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_size) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hhsize_id",.remove = TRUE)
    
    housing_occup_educ_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25013") #of occup, own or rent by educ attainment
    housing_occup_educ_data <- housing_occup_educ_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_educ_data),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_education_level"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_education_level) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hheduc_id",.remove = TRUE)
    
    housing_occup_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25118") #of occup, own or rent by income
    housing_occup_income_data <- housing_occup_income_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_income_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_income_level"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_income_level) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hheduc_id",.remove = TRUE)
    
    housing_occup_rooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25020") #of occup, number of rooms
    housing_occup_rooms_data <- housing_occup_rooms_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_rooms_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","num_rooms"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(num_rooms) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_num_rooms_id",.remove = TRUE)
    
    housing_occup_bedrooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25042") #of occup, number of bedrooms
    housing_occup_bedrooms_data <- housing_occup_bedrooms_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_bedrooms_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","num_bedrooms"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(num_bedrooms) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_num_bedrooms_id",.remove = TRUE)
    

    
    
    #concept is: TENURE BY AGE OF HOUSEHOLDER BY OCCUPANTS PER ROOM
    #num_per_room is different, but can be aggregated to match above 
    #                                       1.00 or less occupants per room 1.01 to 1.50 occupants per room 1.51 or more occupants per room
    #Householder 15 to 34 years                             350128                           22772                            8214
    #Householder 35 to 64 years                             871976                           39863                           14692
    #Householder 65 years and over                          251823                            2277                            1068
    housing_per_room_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25015") #ratio per tract occup per/room/race
    housing_per_room_age_data <- housing_per_room_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_per_room_age_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","householder_age", "num_per_room"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(num_per_room) & number_sams > 0) %>% #seems to keep num_per_room only for all age totals, not individual age
      uncount(as.numeric(number_sams),.id = "own_rent_age_per_rooms_id",.remove = TRUE)
    
        #has population of 4458402
    moved_1yr_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07004")
    moved_1yr_race_data <- moved_1yr_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(moved_1yr_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(moved_1yr = label) %>%
      filter(race %in% acs_race_codes & number_sams>0) %>%
      uncount(as.numeric(number_sams),.id = "moved_1yr_race_id",.remove = TRUE)
    
    #used for percentages used other source for age because the place_born didn't match by tract, so couldn't combine!!
    place_born_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06001")
    #correct if do by either age or by category, but 465645 short if do by agesxcategories!! so I fudged it, with 1.6564 for that one category; all missing from foreign born, under age - foreign born by total is right
    #could redistribute foreign_born by age category and total foreign_born?? excel B06001 work shows calculations.
    #means that this cannot be used without checking for other years / counties
    place_born_age_data <- place_born_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(place_born_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("place_born","age_range"), sep = "!!", remove = F, convert = FALSE) %>%
      #      mutate(age_range = if_else(place_born=="Foreign born" & is.na(age_range),"fb_age_total",age_range)) %>%
      filter(as.numeric(number_sams) > 0 & !is.na(age_range)) %>%
      mutate(number_sams = if_else(place_born=="Foreign born",round(number_sams*1.6564,digits = 0),number_sams)) %>% #fixing mistake in census data, but guessing it was consistent and not just a single age_group, etc.; gives 1 too many total
      uncount(as.numeric(number_sams),.id = "place_born_age_id") # %>%
    #      mutate(del = if_else(number_sams == max(number_sams) & place_born == "Foreign born" & place_born_age_id == number_sams,TRUE,FALSE)) %>%
    #      filter(!del) #taking risk in future that there's only one extra...
    place_born_age_dt <- as.data.table(place_born_age_data)
    
    
    
    
    #can we join original place to the hispanic data? 1174879 from Estimate!!Total, which is same as "foreign_born" for the place_born sets
    #I get 1083547 (91332) - it doesn't add up on the excel (with work in title), and I'm not sure how to fudge it, even, because I see no pattern to the errors
    #chose to add 1.1481 to the Americas, and .78286  to Europe, because I couldn't find a simple reason those two continents didn't match (others did).
    #combine all place_born_data into one, then match to sex_by_age
    origin_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05006") #has PR at bottom
    origin_data <- origin_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%  
      filter(label != "Americas!!Latin America!!South America") %>% 
      filter(label != "Americas!!Latin America!!Central America") %>% 
      filter(label != "Asia!!Eastern Asia!!China") %>%
      pivot_longer(4:ncol(origin_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("origin_continent","origin_area","origin_region","origin_country"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(
        origin_country = if_else(is.na(origin_country),if_else(str_detect(origin_area,"n.e.c") | 
                                       origin_area=="Fiji",origin_area,origin_region),origin_country),
        number_sams = case_when(origin_region=="Central America" ~ round(number_sams*1.198435,digits=0),
                                origin_region=="South America" ~ round(number_sams*.8237,digits=0),
                                origin_continent=="Europe" ~ round(number_sams*.78286,digits=0),
                                TRUE ~ number_sams
                                )
        ) %>%
      filter(number_sams > 0 & !is.na(origin_country)) %>%
      uncount(as.numeric(number_sams),.id = "origin_id")
    
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
    
    #population 15 and over - 3560318 NOT A MATCH with sex_age_race...
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
    
    
    
    #income median by race B19013 - one value per tract per race
    #race_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19013")
    
    #gets per_capita by race per tract - one value per tract per race
    #per_capita_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19301")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     
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
    
    
    #AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS FOR FAMILIES BY FAMILY TYPE -only below poverty... too complicated to unwind and explain
    agg_deficit_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17011")
    
    #by number of children and poverty B17023
    number_children_poverty_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17023")
    
    #by tenure B17019 (whether renter or not)
    household_tenure_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17019")
    
    #family data - family type by employment status B23007
    family_employment_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B23007")
    
    #unmarried partner household by sex of partner B11009 (2015 - not sure who might be married now)
    unmarried_partner_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11009")
    
    #occupation by earnings B24121 /122 is male /123 is female /124 is total? ALL NAs
    occupation_earnings_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B24124")
  }
}

moved_1yr_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07001")
moved_1yr_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07003")
moved_1yr_citizen_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07007")
moved_1yr_marital_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07008")
moved_1yr_education_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07009")
moved_1yr_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B07010")
transport_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08006")
transport_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08101")

transport_language_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08113")
transport_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08119")
transport_occupation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08124")
transport_industry_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08126")
transport_time_work_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08134")
when_go_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08011")
time_to_work_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08012")
vehicles_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08014")




#kids_to_householder_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09018")

kids_age_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10001")
kids_respons_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10002")
kids_respons_grands_time_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10050")
kids_respons_grands_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10051")
kids_respons_grands_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10057")
kids_respons_grands_disability_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10052")
kids_respons_grands_nativity_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10053")


housing_units_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25001") #total number (same as adding occupied and vacant)

#gross rent is contract plus estimate for utilities, etc.
gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25063")
contract_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25056")
bedrooms_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25068")
income_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25122")
income_value_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25121")
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



