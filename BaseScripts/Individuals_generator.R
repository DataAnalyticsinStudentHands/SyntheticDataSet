#https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf

#' createIndividuals
#'
#' This function simulates individual people based on census data (age, sex, race, as of April 1, vintage year).
#' If simulated data already exists, it will read it from an RDS file.
#'Input should include exp_census from expand_from_census.R, which is a list of data.tables
#' @return sam_residents A dataframe of simulated people.
createIndividuals <- function() {
  #have to fix
  sam_residents_data_file <- paste0(censusdir, vintage,"/sam_hh.RDS") 
  #Create or read in individual sam residents
  if(file.exists(sam_residents_data_file)) {
    # import saved sam residents from RDS file
    sam_residents <- readRDS(sam_residents_data_file)
    print(sprintf("Done reading sam residents RDS from %s", sam_residents_data_file ))
  } else {
    sex_age_race[,"individual_id":=paste0(tract,as.character(2000000+sample(.N))),by=.(tract)]
    sex_by_age_eth[,"ind_id_eth":=paste0(tract,as.character(2000000+sample(.N))),by=.(tract)]
    
    #start by putting place_born on marital_status to match later - underdetermined here, and to be replaced if better info later
    #start with marital because it has only a subset
    place_born_marital_dt[,("marital_status_1"):=if_else(str_detect(marital_status,"eparated"),"Now married",marital_status)]
    marital_status_age_dt[,("marriedpb_id1"):=paste0(tract,marital_status,
                                                   as.character(1000000+sample(1:.N))),
                          by=.(tract,marital_status)]
    place_born_marital_dt[,("marriedpb_id1"):=paste0(tract,marital_status_1,
                                          as.character(1000000+sample(1:.N))),
                 by=.(tract,marital_status_1)]
    marital_status_age_dt[,c("place_born","marital_status"):=  #because place_born has 5 categories for marital_status
                            place_born_marital_dt[.SD, c(list(place_born),list(marital_status)), on = .(marriedpb_id1)]]
    #test1
    #test <- table(place_born_marital_dt$tract,place_born_marital_dt$place_born)==
    #  table(marital_status_age_dt$tract,marital_status_age_dt$place_born)
    #length(test[test==F]) ==0
    
    #merge the ones with race first then the ones with ethnicity, then do a merge on all those factors for ethnicity back to race...
    marital_status_age_dt[,("age_range"):=case_when(
      age_range_marital=="18 and 19 years" ~ "18 to 19 years",
      age_range_marital=="35 to 39 years" ~ "35 to 44 years",
      age_range_marital=="40 to 44 years" ~ "35 to 44 years",
      age_range_marital=="45 to 49 years" ~ "45 to 54 years",
      age_range_marital=="50 to 54 years" ~ "45 to 54 years",
      age_range_marital=="55 to 59 years" ~ "55 to 64 years",
      age_range_marital=="60 to 64 years" ~ "55 to 64 years",
      age_range_marital=="85 years and over" ~ "85 to 94 years",
      TRUE ~ age_range_marital
    )]
    #test2
    #test <- table(sex_age_race[as.numeric(substr(age_range,1,2))>14]$tract,sex_age_race[as.numeric(substr(age_range,1,2))>14]$sex,
    #              sex_age_race[as.numeric(substr(age_range,1,2))>14]$age_range)==
    #  table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range)
    #length(test[test==F])==0
    marital_status_age_dt[,("marital_status_4"):=if_else(str_detect(marital_status,"eparated"),"Now married",marital_status)]
    #determine a possible match for race/eth in terms of age and marital_status and record on marital_status_age
    marital_status_age_dt[,("married_id1"):=paste0(tract,sex,age_range,
                                                  as.character(1000000+sample(1:.N))),
                          by=.(tract,sex,age_range)]
    sex_age_race[,("married_id1"):=paste0(tract,sex,age_range,
                                         as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range)]
    marital_status_age_dt[,c("race","individual_id"):=
                            sex_age_race[.SD, c(list(race),list(individual_id)), on = .(married_id1)]]
    sex_by_age_eth[,("married_id1"):=paste0(tract,sex,age_range,
                                           as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,age_range)]
    marital_status_age_dt[,c("ethnicity","ind_id_eth","age"):=
                            sex_by_age_eth[.SD, c(list(ethnicity),list(ind_id_eth),list(age)), on = .(married_id1)]]
    sex_age_race$married_id1 <- NULL
    sex_by_age_eth$married_id1 <- NULL
    #test2b
    #test <- table(sex_age_race[age>14]$tract,sex_age_race[age>14]$sex,sex_age_race[age>14]$age_range,sex_age_race[age>14]$race)==
    #  table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range,marital_status_age_dt$race)
    #length(test[test==F])==0
    #test <- table(sex_by_age_eth[age>14]$tract,sex_by_age_eth[age>14]$sex,sex_by_age_eth[age>14]$age_range,sex_by_age_eth[age>14]$ethnicity)==
    #   table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range,marital_status_age_dt$ethnicity)
    #length(test[test==F])==0
    #test <- table(sex_age_race[age>14]$tract,sex_age_race[age>14]$sex,sex_age_race[age>14]$age_range)==
    #  table(sex_by_age_eth[age>14]$tract,sex_by_age_eth[age>14]$sex,sex_by_age_eth[age>14]$age_range)
    #length(test[test==F])==0
    
    #make matching age for preg_age_range
    marital_status_age_dt[,("preg_age_range"):=case_when(
      age_range_marital=="15 to 17 years" | age_range_marital=="18 and 19 years" ~ "15 to 19 years old",
      age_range_marital=="20 to 24 years" | age_range_marital=="25 to 29 years"  | 
        age_range_marital== "30 to 34 years" ~ "20 to 34 years old",
      age_range=="35 to 44 years" | age_range_marital=="45 to 49 years" ~ "35 to 50 years old",
      TRUE ~ age_range_marital
    )]
    #put an age on marital_status_race/eth
    marital_status_race_dt[,("married_id2"):=
                             paste0(tract,marital_status,sex,race,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,marital_status,sex,race)]
    marital_status_age_dt[,("married_id2"):=
                            paste0(tract,marital_status_4,sex,race,
                                   as.character(1000000+sample(.N))),
                          by=.(tract,marital_status_4,sex,race)]
    marital_status_race_dt[,c("age_range","age","preg_age_range","spouse_present","separated","place_born",
                              "marital_status_5","individual_id"):= #spouse_present and separated exist but aren't on it correctly
                             marital_status_age_dt[.SD, c(list(age_range),list(age),list(preg_age_range),list(spouse_present),list(separated),
                                                          list(place_born),list(marital_status),list(individual_id)), 
                                                   on = .(married_id2)]]
    #
    anti_msr <- as.data.table(anti_join(marital_status_race_dt,marital_status_age_dt,by="married_id2"))
    #nrow(anti_msr)/nrow(marital_status_race_dt) = 0.07409486 - this anti_msa gives you a way to shuffle age_range race from sex_race/eth_age 
    anti_msa <- as.data.table(anti_join(marital_status_age_dt,marital_status_race_dt,by="married_id2"))
    anti_msa[,("married2_id"):=  #letting it vary on race, so we can do a match that respects age and marital_status - all possible because totals for tables are same, just not cross-tabs
               paste0(tract,sex,marital_status_4,
                      as.character(1000000+sample(.N))),
             by=.(tract,sex,marital_status_4)]
    anti_msr[,("married2_id"):=  
                paste0(tract,sex,marital_status,
                       as.character(1000000+sample(.N))),
              by=.(tract,sex,marital_status)]
    anti_msr[,("race"):=
               anti_msa[.SD,list(race),on=.(married2_id)]]
    anti_msr[,("married_id2b"):=
               paste0(tract,marital_status,sex,race,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status,sex,race)]
    anti_msa[,("married_id2b"):=
               paste0(tract,marital_status_4,sex,race,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status_4,sex,race)]
    anti_msr[,c("age_range","age","preg_age_range","spouse_present","separated","place_born","marital_status","marital_status_5","individual_id"):= 
               anti_msa[.SD, c(list(age_range),list(age),list(preg_age_range),list(spouse_present),list(separated),
                               list(place_born),list(marital_status_4),list(marital_status),list(individual_id)), 
                         on = .(married_id2b)]]
    marital_status_race_dt[is.na(age_range),c("age_range","age","preg_age_range","spouse_present","separated","place_born","marital_status",
                                              "marital_status_5","individual_id","race"):= 
                             anti_msr[.SD, c(list(age_range),list(age),list(preg_age_range),list(spouse_present),list(separated),
                                             list(place_born),list(marital_status),list(marital_status_5),list(individual_id),list(race)), 
                                      on = .(married_id2)]]
    #test 2b1
    #test <- table(marital_status_race_dt$tract,
    #              marital_status_race_dt$marital_status,
    #              marital_status_race_dt$sex,
    #              marital_status_race_dt$race,
    #              marital_status_race_dt$age_range
    #              )==table(marital_status_age_dt$tract,
    #                       marital_status_age_dt$marital_status_4,
    #                       marital_status_age_dt$sex,
    #                       marital_status_age_dt$race,
    #                       marital_status_age_dt$age_range
    #                       )
    #length(test[test==F])==0
    #test <- table(sex_age_race[age>14]$tract,
    #              sex_age_race[age>14]$sex,
    #              sex_age_race[age>14]$race,
    #              sex_age_race[age>14]$age_range
    #)==table(marital_status_age_dt$tract,
    #         marital_status_age_dt$sex,
    #         marital_status_age_dt$race,
    #         marital_status_age_dt$age_range
    #)
    #length(test[test==F])==0
    #separate line developed for ethnicity
    marital_status_eth_dt[,("married_id3"):=
                             paste0(tract,marital_status,sex,ethnicity,
                                                   as.character(1000000+sample(.N))),
                           by=.(tract,marital_status,sex,ethnicity)]
    marital_status_age_dt[,("married_id3"):=
                            paste0(tract,marital_status_4,sex,ethnicity,
                                   as.character(1000000+sample(.N))),
                          by=.(tract,marital_status_4,sex,ethnicity)]
    #do just age_range, then get the match on to marital status_race
    marital_status_eth_dt[,c("age_range","ind_id_eth"):= #spouse_present and separated exist but aren't on it correctly
                            marital_status_age_dt[.SD, c(list(age_range),list(ind_id_eth)), 
                                                  on = .(married_id3)]]
    #marital_status_eth_dt[,c("age_range","spouse_present","separated","place_born","marital_status_5","ind_id_eth"):= #spouse_present and separated exist but aren't on it correctly
    #                        marital_status_age_dt[.SD, c(list(age_range),list(spouse_present),list(separated),
    #                                                     list(place_born),list(marital_status),list(ind_id_eth)), 
    #                                              on = .(married_id3)]]
    anti_mse <- as.data.table(anti_join(marital_status_eth_dt,marital_status_age_dt,by="married_id3"))
    anti_msa2 <- as.data.table(anti_join(marital_status_age_dt,marital_status_eth_dt,by="married_id3"))
    anti_msa2[,("married3_id"):=  #letting it shuffle on marital status, so we can do that match - 
                #could try an algorithm that shifts matches instead of sample, so that only first pass is really a sample, but this seems to get all
               paste0(tract,sex,marital_status_4,
                      as.character(1000000+sample(1:.N))),
             by=.(tract,sex,marital_status_4)]
    anti_mse[,("married3_id"):=  
               paste0(tract,sex,marital_status,
                      as.character(1000000+sample(1:.N))),
             by=.(tract,sex,marital_status)]
    anti_mse[,("ethnicity"):=
               anti_msa2[.SD,list(ethnicity),on=.(married3_id)]]
    anti_mse[,("married_id3b"):=
               paste0(tract,marital_status,sex,ethnicity,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status,sex,ethnicity)]
    anti_msa2[,("married_id3b"):=
               paste0(tract,marital_status_4,sex,ethnicity,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status_4,sex,ethnicity)]
    anti_mse[,c("age_range","ind_id_eth"):= 
               anti_msa2[.SD, c(list(age_range),list(ind_id_eth)), 
                         on = .(married_id3b)]]
    marital_status_eth_dt[is.na(age_range),c("age_range","ind_id_eth","ethnicity"):= 
                            anti_mse[.SD, c(list(age_range),list(ind_id_eth),list(ethnicity)), 
                                     on = .(married_id3)]]
    #test 2b2
    #test <- table(marital_status_eth_dt$tract,
    #              marital_status_eth_dt$marital_status,
    #              marital_status_eth_dt$sex,
    #              marital_status_eth_dt$ethnicity,
    #              marital_status_eth_dt$age_range
    #              )==table(marital_status_age_dt$tract,
    #                       marital_status_age_dt$marital_status_4,
    #                       marital_status_age_dt$sex,
    #                       marital_status_age_dt$ethnicity,
    #                       marital_status_age_dt$age_range
    #                       )
    #length(test[test==F])==0
    
    #put both together on marital_status_race, with eth and ind_id - eth and race are NOT correlated, just stored for now
    marital_status_eth_dt[,("married_join_eth_id"):=
                            paste0(tract,marital_status,sex,age_range,
                                   as.character(1000000+sample(.N))),
                          by=.(tract,marital_status,sex,age_range)]
    marital_status_race_dt[,("married_join_eth_id"):=
                             paste0(tract,marital_status,sex,age_range,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,marital_status,sex,age_range)]
    marital_status_race_dt[,c("ethnicity","ind_id_eth"):= 
                            marital_status_eth_dt[.SD, c(list(ethnicity),list(ind_id_eth)), 
                                                   on = .(married_join_eth_id)]]

    #test 2b3
    #test <- table(marital_status_eth_dt$tract,
    #              marital_status_eth_dt$marital_status,
    #              marital_status_eth_dt$sex,
    #              marital_status_eth_dt$ethnicity,
    #              marital_status_eth_dt$age_range
    #              )==table(marital_status_race_dt$tract,
    #                       marital_status_race_dt$marital_status,
    #                       marital_status_race_dt$sex,
    #                       marital_status_race_dt$ethnicity,
    #                       marital_status_race_dt$age_range
    #                       )
    #length(test[test==F])==0
    
    #clean up - there are better patterns for cleaning up...
    rm(list=ls(pattern="^anti_"))
    rm(list=ls(pattern="^ms"))
    
    #put together preg and marital_status by race and age - slightly more women by age in oldest group for pregnant_age women, b/c of how sampling is done on sex_age_race and non-overlapping categories
    
    #adding preg_age_range from race/eth - no other information given so losing some granularity
    preg_race_dt[,("preg_id"):=paste0(tract,birth_label,married,
                                      as.character(1000000+sample(.N))),
                 by=.(tract,birth_label,married)]
    preg_eth_dt[,("preg_id"):=paste0(tract,birth_label,married,
                                     as.character(1000000+sample(.N))),
                by=.(tract,birth_label,married)]
    preg_age_dt[,("preg_id"):=paste0(tract,birth_label,married,
                                     as.character(1000000+sample(.N))),
                by=.(tract,birth_label,married)]
    preg_age_dt[,c("race"):=
        preg_race_dt[.SD, list(race), on = .(preg_id)]]
    preg_age_dt[,c("ethnicity"):=
        preg_eth_dt[.SD, list(ethnicity), on = .(preg_id)]]
    preg_age_dt[,("preg_id"):=NULL]
    #nrow(preg_age_dt[is.na(race)])==0
    #nrow(preg_age_dt[is.na(ethnicity)])==0
    
    marital_status_race_dt[,("married_preg_id"):=paste0(tract,preg_age_range,ethnicity,race,sex,if_else(str_detect(marital_status,"Now"),"Now","Not_now"),
                                                       as.character(1000000+sample(1:.N))),
                           by=.(tract,preg_age_range,ethnicity,race,sex,str_detect(marital_status,"Now"))]
    preg_age_dt[,("married_preg_id"):=paste0(tract,preg_age_range,ethnicity,race,sex,if_else(str_detect(married,"Now"),"Now","Not_now"),
                                                       as.character(1000000+sample(1:.N))),
                          by=.(tract,preg_age_range,ethnicity,race,sex,str_detect(married,"Now"))]
    marital_status_race_dt[,c("pregnant"):=
                            preg_age_dt[.SD, list(birth_label), on = .(married_preg_id)]]
    preg_age_dt[,c("miss_pregnant"):=
                  marital_status_race_dt[.SD, list(pregnant), on = .(married_preg_id)]]
    #nrow(marital_status_race_dt[!is.na(pregnant)])/nrow(preg_age_dt) #.822
    #because preg_age_range wasn't connected to race/eth before, reshuffle
    #on marital_status,sex,age_range
    preg_age_re <- preg_age_dt[is.na(miss_pregnant)]
    preg_age_re[,("married_preg1_id"):=paste0(tract,sex,
                                              as.character(1000000+sample(1:.N))),
                by=.(tract,sex)]
    preg_age_dt[is.na(miss_pregnant),
                ("married_preg1_id"):=paste0(tract,sex,
                                              as.character(1000000+sample(1:.N))),
                by=.(tract,sex)]
    preg_age_dt[is.na(miss_pregnant),c("preg_age_range","ethnicity"):=
                  preg_age_re[.SD,c(list(preg_age_range),list(ethnicity)),on=.(married_preg1_id)]]
    #then try again
    marital_status_race_dt[is.na(pregnant),
                           ("married_preg2_id"):=paste0(tract,preg_age_range,race,sex,if_else(str_detect(marital_status,"Now"),"Now","Not_now"),
                                                        as.character(1000000+seq.int(1:.N))),
                           by=.(tract,preg_age_range,race,sex,str_detect(marital_status,"Now"))]
    preg_age_dt[is.na(miss_pregnant),
                ("married_preg2_id"):=paste0(tract,preg_age_range,race,sex,if_else(str_detect(married,"Now"),"Now","Not_now"),
                                             as.character(1000000+seq.int(1:.N))),
                by=.(tract,preg_age_range,race,sex,str_detect(married,"Now"))]
    marital_status_race_dt[is.na(pregnant),
                           c("pregnant"):=
                             preg_age_dt[.SD, list(birth_label), on = .(married_preg2_id)]]
    preg_age_dt[is.na(miss_pregnant),
                c("miss_pregnant"):=
                  marital_status_race_dt[.SD, list(pregnant), on = .(married_preg2_id)]]
    #nrow(marital_status_race_dt[!is.na(pregnant)])/nrow(preg_age_dt) #.8887
    marital_status_race_dt[is.na(pregnant),
                           ("married_preg3_id"):=paste0(tract,preg_age_range,sex,if_else(str_detect(marital_status,"Now"),"Now","Not_now"),
                                                        as.character(1000000+seq.int(1:.N))),
                           by=.(tract,preg_age_range,sex,str_detect(marital_status,"Now"))]
    preg_age_dt[is.na(miss_pregnant),
                ("married_preg3_id"):=paste0(tract,preg_age_range,sex,if_else(str_detect(married,"Now"),"Now","Not_now"),
                                             as.character(1000000+seq.int(1:.N))),
                by=.(tract,preg_age_range,sex,str_detect(married,"Now"))]
    marital_status_race_dt[is.na(pregnant),
                           c("pregnant"):=
                             preg_age_dt[.SD, list(birth_label), on = .(married_preg3_id)]]
    preg_age_dt[is.na(miss_pregnant),
                c("miss_pregnant"):=
                  marital_status_race_dt[.SD, list(pregnant), on = .(married_preg3_id)]]
    #nrow(marital_status_race_dt[!is.na(pregnant)])/nrow(preg_age_dt) #.948
    marital_status_race_dt[is.na(pregnant),
                           ("married_preg4_id"):=paste0(tract,preg_age_range,sex,
                                                        as.character(1000000+seq.int(1:.N))),
                           by=.(tract,preg_age_range,sex)]
    preg_age_dt[is.na(miss_pregnant),
                ("married_preg4_id"):=paste0(tract,preg_age_range,sex,
                                             as.character(1000000+seq.int(1:.N))),
                by=.(tract,preg_age_range,sex)]
    marital_status_race_dt[is.na(pregnant),
                           c("pregnant"):=
                             preg_age_dt[.SD, list(birth_label), on = .(married_preg4_id)]]
    preg_age_dt[is.na(miss_pregnant),
                c("miss_pregnant"):=
                  marital_status_race_dt[.SD, list(pregnant), on = .(married_preg4_id)]]
    #nrow(marital_status_race_dt[!is.na(pregnant)])/nrow(preg_age_dt) #.973
    marital_status_race_dt[is.na(pregnant),
                           ("married_preg5_id"):=paste0(tract,sex,
                                                        as.character(1000000+seq.int(1:.N))),
                           by=.(tract,sex)]
    preg_age_dt[is.na(miss_pregnant),
                ("married_preg5_id"):=paste0(tract,sex,
                                             as.character(1000000+seq.int(1:.N))),
                by=.(tract,sex)]
    marital_status_race_dt[is.na(pregnant),
                           c("pregnant"):=
                             preg_age_dt[.SD, list(birth_label), on = .(married_preg5_id)]]
    preg_age_dt[is.na(miss_pregnant),
                c("miss_pregnant"):=
                  marital_status_race_dt[.SD, list(pregnant), on = .(married_preg5_id)]]
    #nrow(marital_status_race_dt[!is.na(pregnant)])/nrow(preg_age_dt) #1
    
    rm(list=ls(pattern="^preg"))
    
    sam_pw_hh_race <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/sam_pw_hh_race_2020-07-26.RDS")
    sam_pw_hh_eth <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/sam_pw_hh_eth_2020-07-26.RDS")
    
    marital_status_race_dt[,("householder_age_9"):=case_when(
      age < 25 ~ "Householder 15 to 24 years",
      age > 24 & age < 35 ~ "Householder 25 to 34 years",
      age > 34 & age < 45 ~ "Householder 25 to 34 years",
      age > 44 & age < 55 ~ "Householder 45 to 54 years",
      age > 54 & age < 60 ~ "Householder 54 to 59 years",
      age > 59 & age < 65 ~ "Householder 60 to 64 years",
      age > 64 & age < 75 ~ "Householder 65 to 74 years",
      age > 74 & age < 85 ~ "Householder 75 to 84 years",
      TRUE ~ "Householder 85 years and over"
    )]
    sam_pw_hh_race[,("marital_match"):=case_when( #gets a shallow copy warning
      family_role == "Married-couple family" ~ "Married",
      family_role == "Wife" ~ "Married",
      TRUE ~ "Not married"
    )]
    marital_status_race_dt[,("marital_match"):=if_else(
      marital_status_5!="Now married except separated","Not married","Married"
    )]
    sam_pw_hh_eth[,("marital_match"):=case_when( #gets a shallow copy warning
      family_role == "Married-couple family" ~ "Married",
      family_role == "Wife" ~ "Married",
      TRUE ~ "Not married"
    )]
    sam_pw_hh_race[,("hh_marital_id"):=
                   paste0(tract,sex,householder_age_9,race,marital_match,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex,householder_age_9,race,marital_match)]
    marital_status_race_dt[,("hh_marital_id"):=
                             paste0(tract,sex,householder_age_9,race,marital_match,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,sex,householder_age_9,race,marital_match)]
    marital_status_race_dt[,c(c("household_id","inhousehold_id","own_rent","family","family_type",
                                "family_role","family_role_4","single_hh_sex",
                                "housing_units","housing_units_6","num_structures","people_per_room",
                                "hh_size","hh_size_4","hh_size_10","num_rooms","num_bedrooms","when_moved_in",
                                "means_transport","number_workers_in_hh","industry","occupation",
                                "commute_time","when_go_to_work","income_range_workers",
                                "number_vehicles_hh","kids_by_age","own_kids",
                                "husband_employed","wife_employed","single_hh_employed",
                                "partner_type","sex_partner","role_in_family",
                                "marital_status_non_hh_gp","non_hh_gp_respon","time_non_hh_gp_respon","non_hh_gkids_by_age", 
                                "marital_status_gp","gp_respon","gp_hh_parent_present","time_gp_respon")):=
                     sam_pw_hh_race[.SD, c(list(household_id),list(inhousehold_id),list(own_rent),list(family),
                                           list(family_type),list(family_role),list(family_role_4),list(single_hh_sex),
                                           list(housing_units),list(housing_units_6),list(num_structures),list(people_per_room),
                                           list(hh_size),list(hh_size_4),list(hh_size_10),list(num_rooms),list(num_bedrooms),
                                           list(when_moved_in),list(means_transport),list(number_workers_in_hh),list(industry),
                                           list(occupation),list(commute_time),list(when_go_to_work),list(income_range_workers),
                                           list(number_vehicles_hh),list(kids_by_age),list(own_kids),list(husband_employed),
                                           list(wife_employed),list(single_hh_employed),list(partner_type),list(sex_partner),
                                           list(role_in_family),list(marital_status_non_hh_gp),list(non_hh_gp_respon),
                                           list(time_non_hh_gp_respon),list(non_hh_gkids_by_age),list(marital_status_gp),
                                           list(gp_respon),list(gp_hh_parent_present),list(time_gp_respon)), 
                                    on = .(hh_marital_id)]]
    sam_pw_hh_race[,("missing_hh_race"):=
                     marital_status_race_dt[.SD, c(list(race)), on = .(hh_marital_id)]]
    
    
    marital_status_eth2_dt <- marital_status_race_dt[,c("sex","marital_status","age_range","age","preg_age_range",
                                                        "spouse_present","separated","place_born","marital_status_5",
                                                        "ethnicity","ind_id_eth","pregnant","householder_age_9")]
    
    #put an age/race on seniors, to recalibrate at end and to store references to group_quarters
    #test <- table(sr_relations$tract)==table(sex_age_race[age>64]$tract) 
    #clean up a bit - these are roughly equal in number, just moving a bit to get lined up
    #just dropping separated as a duplicate of marital_status_5, in any case
    ##marital_status_race_dt[marital_status_5=="Separated" & separated!="Separated",
    ##                       ("marital_status_5"):="Now married except separated"]
    ##marital_status_race_dt[marital_status_5=="Now married except separated" & separated=="Separated",
    ##                       ("marital_status_5"):="Separated"]
    #then sr_relations
    #sr_relations[,("marital_match"):=case_when(
    #  relative=="Spouse" ~ "Now married",
    #  TRUE ~ sample(c("Not married","Now married"),size = .N,prob = c(.6,.4),replace = TRUE)
    #)]
    sr_relations[,("sex"):=case_when(
      relative=="Spouse" ~ "Female",
      relative=="Householder" & family_or_non=="In family households" & is.na(sex) ~ 
        sample(c("Female","Male"),size = .N,prob = c(.3,.7),replace = TRUE),
      TRUE ~ sex
    )]
    sr_relations[,("marital_match"):=case_when(
      relative=="Spouse" ~ "Now married",
      living_alone=="Living alone" ~ "Not married", 
      relative=="Householder" & family_or_non=="In family households" & sex=="Male" ~ "Now married", #this puts them all in first match, then spreads them out on last
      TRUE ~ "Not married" # marital_match
    )]
#    marital_status_race_dt[,("marital_match"):=if_else(
##      marital_status_5!="Now married except separated","Not married","Now married"
      #marital_status!="Now married","Not married","Now married"
#    )]
    #sr_relations only has sex for householders living alone or not
    sr_relations[group_or_hh=="In households",("race_sr_id"):=
                   paste0(tract,sex,marital_match,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex,marital_match)]
    marital_status_race_dt[age>64,("race_sr_id"):=
                   paste0(tract,sex,marital_match,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex,marital_match)]
    sr_relations[group_or_hh=="In households",c("race","ethnicity","age_range","age",
                                                "spouse_present","separated","place_born","marital_status",
                                                "marital_status_5","individual_id","ind_id_eth"):=
                   marital_status_race_dt[.SD, c(list(race),list(ethnicity),list(age_range),list(age),
                                                 list(spouse_present),list(separated),list(place_born),
                                                 list(marital_status),list(marital_status_5),
                                                 list(individual_id),list(ind_id_eth)), on = .(race_sr_id)]]
    marital_status_race_dt[age>64,("missing_race"):=
                   sr_relations[.SD, c(list(race)), on = .(race_sr_id)]] 

    #nrow(sr_relations[is.na(race)&!is.na(sex)])#16464
    #then rest (that didn't have sex listed) 
    sr_relations[group_or_hh=="In households" & is.na(race),
                 ("race2_sr_id"):=
                   paste0(tract,marital_match,
                          as.character(1000000+sample(.N))),
                 by=.(tract,marital_match)]
    marital_status_race_dt[age>64&is.na(missing_race),("race2_sr_id"):=
                             paste0(tract,marital_match,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,marital_match)]
    sr_relations[group_or_hh=="In households" & is.na(race),
                 c("race","ethnicity","age_range","age","sex",
                   "spouse_present","separated","place_born","marital_status",
                   "marital_status_5","individual_id","ind_id_eth"):=
                   marital_status_race_dt[.SD, c(list(race),list(ethnicity),list(age_range),list(age),list(sex),
                                                 list(spouse_present),list(separated),list(place_born),
                                                 list(marital_status),list(marital_status_5),
                                                 list(individual_id),list(ind_id_eth)), on = .(race2_sr_id)]]
    marital_status_race_dt[age>64&is.na(missing_race),("missing_race"):=
                             sr_relations[.SD, c(list(race)), on = .(race2_sr_id)]] 
    #clean up a few that weren't matched so they can make sense as gq or otherwise alone [also comes close to fixing the imbalance from first shuffle on separated]
    marital_status_race_dt[age>64&is.na(missing_race),("marital_status_5"):=if_else(
      marital_status_5=="Now married except separated","Separated",marital_status_5
    )]
    #for gq and rest - it's really weird where the matches don't happen and seems to be at the tract level - about 10% of total
    sr_relations[is.na(race),
                 ("race3_sr_id"):=
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    marital_status_race_dt[age>64&is.na(missing_race),("race3_sr_id"):=
                             paste0(tract,
                                    as.character(1000000+sample(.N))),
                           by=.(tract)]
    sr_relations[is.na(race),
                 c("race","ethnicity","age_range","age","sex",
                   "spouse_present","separated","place_born","marital_status",
                   "marital_status_5","individual_id","ind_id_eth"):=
                   marital_status_race_dt[.SD, c(list(race),list(ethnicity),list(age_range),list(age),list(sex),
                                                 list(spouse_present),list(separated),list(place_born),
                                                 list(marital_status),list(marital_status_5),
                                                 list(individual_id),list(ind_id_eth)), on = .(race3_sr_id)]]
    marital_status_race_dt[age>64&is.na(missing_race),("missing_race"):=
                             sr_relations[.SD, c(list(race)), on = .(race3_sr_id)]] 
    #test3 - 
    #nrow(sr_relations[is.na(race)])==0
    #nrow(sr_relations)==length(unique(sr_relations$individual_id))
    #nrow(sr_relations)==length(unique(sr_relations$ind_id_eth))
    #test <- table(marital_status_race_dt[age>64]$tract,
    #              marital_status_race_dt[age>64]$sex,
    #              marital_status_race_dt[age>64]$race,
    #              marital_status_race_dt[age>64]$ethnicity,
    #              marital_status_race_dt[age>64]$place_born,
    #              marital_status_race_dt[age>64]$marital_status,
    #              marital_status_race_dt[age>64]$marital_status_5
    #              )==table(
    #                sr_relations$tract,
    #                sr_relations$sex,
    #                sr_relations$race,
    #                sr_relations$ethnicity,
    #                sr_relations$place_born,
    #                sr_relations$marital_status,
    #                sr_relations$marital_status_5
    #              )
    #length(test[test==F])==0
    
    #to match sr_relations$relative - 1472 folks who are >= 65 are Child of Householder in original; make sure to get old HH
    hh_relations_dt[,("relative_sr"):=case_when(
      relative=="Householder" ~ "Householder",
      relative=="Spouse" ~ "Spouse",
      relative=="Nonrelatives" ~ "Nonrelatives",
      relative=="Parent" ~ "Parent",
      relative=="Parent-in-law" ~ "Parent-in-law",
      relative=="Child" ~ "Child of householder",
      relative=="Other relatives" ~ "Other relatives",
      TRUE ~ relative
    )]
    sr_relations[,("age_range_3"):=list("65 years and over")] 
    sr_relations[relative=="Householder"&family_or_non=="In family households",("living_alone"):="Not living alone"]
    hh_relations_dt[group_or_hh=="In households",("rel_sr_id"):=
                      paste0(tract,group_or_hh,family_or_non,relative_sr,sex,living_alone,
                             as.character(1000000+sample(.N))),
                    by=.(tract,group_or_hh,family_or_non,relative_sr,sex,living_alone)]
    sr_relations[group_or_hh=="In households",("rel_sr_id"):=
                   paste0(tract,group_or_hh,family_or_non,relative,sex,living_alone,
                          as.character(1000000+sample(.N))),
                 by=.(tract,group_or_hh,family_or_non,relative,sex,living_alone)]
    hh_relations_dt[group_or_hh=="In households",
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","individual_id","ind_id_eth"):=
                      sr_relations[.SD, c(list(race),list(ethnicity),
                                          list(age_range_3),list(age_range),list(age),list(sex),
                                          list(spouse_present),list(separated),list(place_born),
                                          list(marital_status),list(marital_status_5),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr_id)]]
    anti_sr_rel <- as.data.table(anti_join(sr_relations,hh_relations_dt,by="rel_sr_id"))
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    ("rel_sr3_id"):=
                      paste0(tract,relative_sr,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relative_sr)]
    anti_sr_rel[group_or_hh=="In households",("rel_sr3_id"):=
                   paste0(tract,relative,
                          as.character(1000000+sample(.N))),
                 by=.(tract,relative)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","individual_id","ind_id_eth"):=
                      anti_sr_rel[.SD, c(list(race),list(ethnicity),
                                         list(age_range_3),list(age_range),list(age),list(sex),
                                         list(spouse_present),list(separated),list(place_born),
                                         list(marital_status),list(marital_status_5),
                                         list(individual_id),list(ind_id_eth)), 
                                  on = .(rel_sr3_id)]]
    
    anti_sr2_rel <- as.data.table(anti_join(anti_sr_rel,hh_relations_dt,by="rel_sr3_id"))
    anti_sr2_rel[group_or_hh=="In households",("relative"):="Parent"]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    ("rel_sr4_id"):=
                      paste0(tract,relative_sr,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relative_sr)]
    anti_sr2_rel[group_or_hh=="In households",("rel_sr4_id"):=
                  paste0(tract,relative,
                         as.character(1000000+sample(.N))),
                by=.(tract,relative)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","individual_id","ind_id_eth"):=
                      anti_sr2_rel[.SD, c(list(race),list(ethnicity),
                                          list(age_range_3),list(age_range),list(age),list(sex),
                                          list(spouse_present),list(separated),list(place_born),
                                          list(marital_status),list(marital_status_5),
                                          list(individual_id),list(ind_id_eth)), 
                                  on = .(rel_sr4_id)]]
    anti_sr3_rel <- as.data.table(anti_join(anti_sr2_rel,hh_relations_dt,by="rel_sr4_id")) #just 582, all listed as Parent
    anti_sr3_rel[group_or_hh=="In households",("relative"):="Nonrelatives"]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    ("rel_sr5_id"):=
                      paste0(tract,relative_sr,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relative_sr)]
    anti_sr3_rel[group_or_hh=="In households",("rel_sr5_id"):=
                   paste0(tract,relative,
                          as.character(1000000+sample(.N))),
                 by=.(tract,relative)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","individual_id","ind_id_eth"):=
                      anti_sr3_rel[.SD, c(list(race),list(ethnicity),
                                          list(age_range_3),list(age_range),list(age),list(sex),
                                          list(spouse_present),list(separated),list(place_born),
                                          list(marital_status),list(marital_status_5),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr5_id)]]
    anti_sr5_rel <- as.data.table(anti_join(anti_sr3_rel,hh_relations_dt,by="rel_sr5_id"))
    
   
    #OCD Dan catches last 6 missing!!
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    ("rel_sr6a_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    anti_sr5_rel[group_or_hh=="In households",("rel_sr6a_id"):=
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","individual_id","ind_id_eth"):=
                      anti_sr5_rel[.SD, c(list(race),list(ethnicity),
                                          list(age_range_3),list(age_range),list(age),list(sex),
                                          list(spouse_present),list(separated),list(place_born),
                                          list(marital_status),list(marital_status_5),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr6a_id)]]
    #move group_quarter info over
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range),
                    ("rel_sr7_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    sr_relations[group_or_hh=="In group quarters",("rel_sr7_id"):=
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","individual_id","ind_id_eth"):=
                      sr_relations[.SD, c(list(race),list(ethnicity),
                                          list(age_range_3),list(age_range),list(age),list(sex),
                                          list(spouse_present),list(separated),list(place_born),
                                          list(marital_status),list(marital_status_5),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr7_id)]]
    #test no dups:
    #nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))-1 #NAs count as one
    #4 tracts from sr_relations not in hh_relations
    #test3a 
    #test<-table(hh_relations_dt[!is.na(age_range)]$tract,
    #            hh_relations_dt[!is.na(age_range)]$race,
    #            hh_relations_dt[!is.na(age_range)]$sex,
    #            hh_relations_dt[!is.na(age_range)]$marital_status,
    #            hh_relations_dt[!is.na(age_range)]$place_born,
    #            #hh_relations_dt[!is.na(age_range)]$living_alone, #one missing???
    #            hh_relations_dt[!is.na(age_range)]$age_range
    #        )==
    #  table(sr_relations$tract,
    #        sr_relations$race,
    #        sr_relations$sex,
    #        sr_relations$marital_status,
    #        sr_relations$place_born,
    #        #sr_relations$living_alone,
    #        sr_relations$age_range
    #        )
    #length(test[test==F])==0
    rm(sr_relations)
    rm(list=ls(pattern="^anti"))
    hh_relations_dt[,c("rel_sr3_id","rel_sr4_id","rel_sr5_id","rel_sr7_id","rel_sr6a_id","role_id","relative_sr","rel_sr_id"):=NULL]
    
    #now do adults - giving all a possible race and age - just by sample within each tract
    #good example of different ways of filling in not married
    adults_relations[age_range_3!="65 years and over",("marital_match"):=case_when(
      relation_hh=="Householder living with spouse or spouse of householder" ~ "Now married",
      relation_hh=="Lives alone" ~ "Not married",
      #relation_hh=="Other relatives" ~ sample(c("Not married","Now married"),size=.N,prob = c(.8,.2),replace = TRUE),
      #relation_hh=="Other nonrelatives" ~ sample(c("Not married","Now married"),size=.N,prob = c(.9,.1),replace = TRUE),
      #relation_hh=="Child of householder" ~ sample(c("Not married","Now married"),size=.N,prob = c(.9,.1),replace = TRUE),
      TRUE ~ "Unknown")]
    
    adults_relations[age_range_3=="18 to 34 years",("race_adults_id"):=
                   paste0(tract,marital_match,as.character(1000000+sample(.N))),
                 by=.(tract,marital_match)]
    marital_status_race_dt[age>17&age<35,("race_adults_id"):=
                   paste0(tract,marital_match,as.character(1000000+sample(.N))),
                 by=.(tract,marital_match)]
    adults_relations[age_range_3=="18 to 34 years",c("race","ethnicity","age_range","age","sex",
                                                     "spouse_present","separated","place_born","marital_status",
                                                     "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                       marital_status_race_dt[.SD, c(list(race),list(ethnicity),list(age_range),list(age),list(sex),
                                                     list(spouse_present),list(separated),list(place_born),
                                                     list(marital_status),list(marital_status_5),list(pregnant),
                                                     list(individual_id),list(ind_id_eth)), on = .(race_adults_id)]]
    marital_status_race_dt[age>17&age<35,("miss_adults"):=
                             adults_relations[.SD,age,on = .(race_adults_id)]]
    #and rest by sample - some unmarried partners would be divorced, etc., from a previous spouse...
    adults_relations[age_range_3=="18 to 34 years"&is.na(race),("race_adults1_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    marital_status_race_dt[age>17&age<35&is.na(miss_adults),("race_adults1_id"):=
                             paste0(tract,as.character(1000000+sample(.N))),
                           by=.(tract)]
    adults_relations[age_range_3=="18 to 34 years"&is.na(race),c("race","ethnicity","age_range","age","sex",
                                                     "spouse_present","separated","place_born","marital_status",
                                                     "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                       marital_status_race_dt[.SD, c(list(race),list(ethnicity),list(age_range),list(age),list(sex),
                                                     list(spouse_present),list(separated),list(place_born),
                                                     list(marital_status),list(marital_status_5),list(pregnant),
                                                     list(individual_id),list(ind_id_eth)), on = .(race_adults1_id)]]
    marital_status_race_dt[age>17&age<35&is.na(miss_adults),("miss_adults"):=
                             adults_relations[.SD,age,on = .(race_adults1_id)]]
    #rest of marital_status_race is gq - have to get their id over to hh_relations
    #marital_status_race_dt[age>17&age<35&is.na(miss_adults),("group_or_hh"):="In group quarters"]
    
    #same for 35 to 64 y.o.
    adults_relations[age_range_3=="35 to 64 years",("race_adults2_id"):=
                       paste0(tract,marital_match,as.character(1000000+sample(.N))),
                     by=.(tract,marital_match)]
    marital_status_race_dt[age>34&age<65,("race_adults2_id"):=
                             paste0(tract,marital_match,as.character(1000000+sample(.N))),
                           by=.(tract,marital_match)]
    adults_relations[age_range_3=="35 to 64 years",c("race","ethnicity","age_range","age","sex",
                                                     "spouse_present","separated","place_born","marital_status",
                                                     "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                       marital_status_race_dt[.SD, c(list(race),list(ethnicity),list(age_range),list(age),list(sex),
                                                     list(spouse_present),list(separated),list(place_born),
                                                     list(marital_status),list(marital_status_5),list(pregnant),
                                                     list(individual_id),list(ind_id_eth)), on = .(race_adults2_id)]]
    marital_status_race_dt[age>34&age<65,("miss_adults"):=
                             adults_relations[.SD,age,on = .(race_adults2_id)]]
    #and rest by sample - some unmarried partners would be divorced, etc., from a previous spouse...
    adults_relations[age_range_3=="35 to 64 years"&is.na(race),("race_adults3_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    marital_status_race_dt[age>34&age<65&is.na(miss_adults),("race_adults3_id"):=
                             paste0(tract,as.character(1000000+sample(.N))),
                           by=.(tract)]
    adults_relations[age_range_3=="35 to 64 years"&is.na(race),c("race","ethnicity","age_range","age","sex",
                                                                 "spouse_present","separated","place_born","marital_status",
                                                                 "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                       marital_status_race_dt[.SD, c(list(race),list(ethnicity),list(age_range),list(age),list(sex),
                                                     list(spouse_present),list(separated),list(place_born),
                                                     list(marital_status),list(marital_status_5),list(pregnant),
                                                     list(individual_id),list(ind_id_eth)), on = .(race_adults3_id)]]
    marital_status_race_dt[age>34&age<65&is.na(miss_adults),("miss_adults"):=
                             adults_relations[.SD,age,on = .(race_adults3_id)]]
    #rest of marital_status_race is gq - have to get their id over to hh_relations
    #marital_status_race_dt[age>34&age<65&is.na(miss_adults),("group_or_hh"):="In group quarters"]
    marital_status_race_dt[,("group_or_hh"):=if_else(is.na(miss_adults)&age>17&age<65,"In group quarters","In households")]
    
    #test3b
    #test<-table(marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$tract,
    #            marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$sex,
    #            marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$age,
    #            marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$marital_status_5,
    #            marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$place_born,
    #            marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$pregnant,
    #            marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$ethnicity,
    #            marital_status_race_dt[age>17&age<65&group_or_hh=="In households"]$race)==
    #  table(adults_relations[age_range_3!="65 years and over"]$tract,
    #        adults_relations[age_range_3!="65 years and over"]$sex,
    #        adults_relations[age_range_3!="65 years and over"]$age,
    #        adults_relations[age_range_3!="65 years and over"]$marital_status_5,
    #        adults_relations[age_range_3!="65 years and over"]$place_born,
    #        adults_relations[age_range_3!="65 years and over"]$pregnant,
    #        adults_relations[age_range_3!="65 years and over"]$ethnicity,
    #        adults_relations[age_range_3!="65 years and over"]$race)
    #length(test[test==F])==0
    
    #now move to hh_relations
    #start with group quarters from marital
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range),
                    ("rel_gq_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    marital_status_race_dt[group_or_hh=="In group quarters",("rel_gq_id"):=
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      marital_status_race_dt[.SD, c(list(race),list(ethnicity),
                                          list(age_range_3),list(age_range),list(age),list(sex),
                                          list(spouse_present),list(separated),list(place_born),
                                          list(marital_status),list(marital_status_5),list(pregnant),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_gq_id)]]
    #test3c
    #test <- table(#hh_relations_dt[group_or_hh=="In group quarters" & !is.na(age_range),tract],
    #              hh_relations_dt[group_or_hh=="In group quarters" & !is.na(age_range),pregnant])==
    #  table(#marital_status_race_dt[group_or_hh=="In group quarters",tract],
    #        marital_status_race_dt[group_or_hh=="In group quarters",pregnant])
    #length(test[test==F])==0
    #move rest over from adults
    #something above erased the sex on the hh_relations not in the seniors - only known for households
    hh_relations_dt[is.na(age_range),("sex"):=case_when(
      role_family=="Female" ~ role_family,
      role_family=="Male" ~ role_family,
      TRUE ~ sex
    )]
    hh_relations_dt[group_or_hh=="In households",("relation_hh"):=case_when(
      relative=="Child" | relative=="Grandchild" ~ "Child of householder", #but only for adults; Foster child is non-relative
      #relative=="Spouse" ~ "Householder living with spouse or spouse of householder",
      #role_in_family=="Unmarried partner" ~ "Householder living with unmarried partner or unmarried partner of householder",
      relative=="Nonrelatives" | relative=="Parent-in-law" ~ "Other nonrelatives",
      relative=="Other relatives" | relative =="Parent" | relative=="Brother or sister" |
        relative=="Son-in-law or daughter-in-law"~ "Other relatives",
      TRUE ~ "Householder" 
    )]
    adults_relations[,("living_alone"):=if_else(relation_hh=="Lives alone","Living alone","Not living alone")] #still have hh not living alone
    adults_relations[,("relation_hh_hh"):=case_when(
      relation_hh=="Householder living with unmarried partner or unmarried partner of householder" |
        relation_hh=="Lives alone" |
        relation_hh=="Householder living with spouse or spouse of householder" ~ "Householder",
      TRUE ~ relation_hh
    )]
    
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    ("rel_adults_id"):=
                      paste0(tract,relation_hh,sex,living_alone,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relation_hh,sex,living_alone)]
    adults_relations[age_range_3!="65 years and over",
                     ("rel_adults_id"):=
                       paste0(tract,relation_hh_hh,sex,living_alone,
                              as.character(1000000+sample(.N))),
                     by=.(tract,relation_hh_hh,sex,living_alone)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    c("race","ethnicity","age_range_3","age_range","age",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(race),list(ethnicity),
                                              list(age_range_3),list(age_range),list(age),
                                              list(spouse_present),list(separated),list(place_born),
                                              list(marital_status),list(marital_status_5),list(pregnant),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults_id)]]
    adults_relations[age_range_3!="65 years and over",
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults_id)]]
    #this should give you all the hh, but for some reason 50k do not match
    #matching on living alone, without sex, but having sex write over from marital (through adults)
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    ("rel_adults1_id"):=
                      paste0(tract,relation_hh,living_alone,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relation_hh,living_alone)]
    adults_relations[age_range_3!="65 years and over" & is.na(adults_matched),
                     ("rel_adults1_id"):=
                       paste0(tract,relation_hh_hh,living_alone,
                              as.character(1000000+sample(.N))),
                     by=.(tract,relation_hh_hh,living_alone)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(race),list(ethnicity),
                                              list(age_range_3),list(age_range),list(age),list(sex),
                                              list(spouse_present),list(separated),list(place_born),
                                              list(marital_status),list(marital_status_5),list(pregnant),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults1_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults1_id)]]
    #matching without living alone, without sex, but having sex write over from marital (through adults)
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    ("rel_adults1a_id"):=
                      paste0(tract,relation_hh,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relation_hh)]
    adults_relations[age_range_3!="65 years and over" & is.na(adults_matched),
                     ("rel_adults1a_id"):=
                       paste0(tract,relation_hh_hh,
                              as.character(1000000+sample(.N))),
                     by=.(tract,relation_hh_hh)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(race),list(ethnicity),
                                              list(age_range_3),list(age_range),list(age),list(sex),
                                              list(spouse_present),list(separated),list(place_born),
                                              list(marital_status),list(marital_status_5),list(pregnant),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults1a_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults1a_id)]]
    #test nodups #nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))-1
    #and get rest of marital for 15 yro!!
    hh_relations_dt[,("marital_match"):=if_else(relation_hh=="Householder","Now married","Not married")]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    ("rel_kids_mar_id"):=
                      paste0(tract,marital_match,
                             as.character(1000000+sample(.N))),
                    by=.(tract,marital_match)]
    marital_status_race_dt[group_or_hh=="In households"&age<18,
                           ("rel_kids_mar_id"):=
                             paste0(tract,marital_match,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,marital_match)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      marital_status_race_dt[.SD, c(list(race),list(ethnicity),
                                                    list(age_range_3),list(age_range),list(age),list(sex),
                                                    list(spouse_present),list(separated),list(place_born),
                                                    list(marital_status),list(marital_status_5),list(pregnant),
                                                    list(individual_id),list(ind_id_eth)), 
                                             on = .(rel_kids_mar_id)]]
    marital_status_race_dt[group_or_hh=="In households"&age<18,
                           ("miss_kids_marital"):=
                             hh_relations_dt[.SD, group_or_hh,on = .(rel_kids_mar_id)]]
    #163 left - move over to group quarters
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range),
                    ("rel_kids_mar1_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    marital_status_race_dt[group_or_hh=="In households"&age<18&is.na(miss_kids_marital),
                           ("rel_kids_mar1_id"):=
                             paste0(tract,
                                    as.character(1000000+sample(.N))),
                           by=.(tract)]
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      marital_status_race_dt[.SD, c(list(race),list(ethnicity),
                                                    list(age_range_3),list(age_range),list(age),list(sex),
                                                    list(spouse_present),list(separated),list(place_born),
                                                    list(marital_status),list(marital_status_5),list(pregnant),
                                                    list(individual_id),list(ind_id_eth)), 
                                             on = .(rel_kids_mar1_id)]]
    marital_status_race_dt[group_or_hh=="In households"&age<18&is.na(miss_kids_marital),
                           ("miss_kids_marital"):=
                             hh_relations_dt[.SD, group_or_hh,on = .(rel_kids_mar1_id)]]
    #do for rest of relatives, except grandkids 
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    ("rel_adults2_id"):=
                      paste0(tract,relation_hh,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relation_hh)]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     ("rel_adults2_id"):=
                       paste0(tract,relation_hh_hh,
                              as.character(1000000+sample(.N))),
                     by=.(tract,relation_hh_hh)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(race),list(ethnicity),
                                              list(age_range_3),list(age_range),list(age),list(sex),
                                              list(spouse_present),list(separated),list(place_born),
                                              list(marital_status),list(marital_status_5),list(pregnant),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults2_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults2_id)]]
    #test no dups:
    #nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))-1
    #assign rest by sample at tract level, first without "ild"
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & !str_detect(role_in_family,"ild"), 
                    ("rel_adults3_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched) & !str_detect(relation_hh,"Child of householder"), #in this case all children have been assigned, but put in just in case...
                     ("rel_adults3_id"):=
                       paste0(tract,
                              as.character(1000000+sample(.N))),
                     by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range)  & !str_detect(role_in_family,"ild"),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(race),list(ethnicity),
                                              list(age_range_3),list(age_range),list(age),list(sex),
                                              list(spouse_present),list(separated),list(place_born),
                                              list(marital_status),list(marital_status_5),list(pregnant),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults3_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched) & !str_detect(relation_hh,"Child of householder"),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults3_id)]]
    #test no dups:
    #nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))-1
    #and with ild match
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range), 
                    ("rel_adults4_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     ("rel_adults4_id"):=
                       paste0(tract,
                              as.character(1000000+sample(.N))),
                     by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(race),list(ethnicity),
                                              list(age_range_3),list(age_range),list(age),list(sex),
                                              list(spouse_present),list(separated),list(place_born),
                                              list(marital_status),list(marital_status_5),list(pregnant),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults4_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults4_id)]]
    #13 left in adults_relations, all from 210100!! - GQ issue??
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range), 
                    ("rel_adults4g_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     ("rel_adults4g_id"):=
                       paste0(tract,
                              as.character(1000000+sample(.N))),
                     by=.(tract)]
    hh_relations_dt[group_or_hh=="In group quarters" & is.na(age_range),
                    c("race","ethnicity","age_range_3","age_range","age","sex",
                      "spouse_present","separated","place_born","marital_status",
                      "marital_status_5","pregnant","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(race),list(ethnicity),
                                              list(age_range_3),list(age_range),list(age),list(sex),
                                              list(spouse_present),list(separated),list(place_born),
                                              list(marital_status),list(marital_status_5),list(pregnant),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults4g_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults4g_id)]]
    #test no dups:
    #nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))-1
    #test4 #there are 55 kids who didn't match a tract from marital!
    #test <- table(hh_relations_dt[age_range!="15 to 17 years"]$tract,
    #              hh_relations_dt[age_range!="15 to 17 years"]$race,
    #              hh_relations_dt[age_range!="15 to 17 years"]$ethnicity,
    #              hh_relations_dt[age_range!="15 to 17 years"]$sex,
    #              hh_relations_dt[age_range!="15 to 17 years"]$pregnant,
    #              hh_relations_dt[age_range!="15 to 17 years"]$place_born,
    #              hh_relations_dt[age_range!="15 to 17 years"]$marital_status_5,
    #              hh_relations_dt[age_range!="15 to 17 years"]$age_range)==
    #  table(marital_status_race_dt[age_range!="15 to 17 years"]$tract,
    #        marital_status_race_dt[age_range!="15 to 17 years"]$race,
    #        marital_status_race_dt[age_range!="15 to 17 years"]$ethnicity,
    #        marital_status_race_dt[age_range!="15 to 17 years"]$sex,
    #        marital_status_race_dt[age_range!="15 to 17 years"]$pregnant,
    #        marital_status_race_dt[age_range!="15 to 17 years"]$place_born,
    #        marital_status_race_dt[age_range!="15 to 17 years"]$marital_status_5,
    #        marital_status_race_dt[age_range!="15 to 17 years"]$age_range)
    #length(test[test==F])==0
    #fix up a bit - but breaks the test...
    hh_relations_dt[age<18&marital_status=="Widowed",c("marital_status","marital_status_5"):="Never married"]
    hh_relations_dt[age<18&relative=="Parent",("role_in_family"):="Child"]
    hh_relations_dt[age<18&relative=="Parent-in-law",("role_in_family"):="Son-in-law or daughter-in-law"]
    hh_relations_dt[age<18&relative=="Parent",("relative"):="Child"]
    hh_relations_dt[age<18&relative=="Parent-in-law",("relative"):="Son-in-law or daughter-in-law"]
    
    #length(unique(hh_relations_dt$individual_id))-nrow(sex_age_race[age>14]) #-54
    rm(adults_relations)

    #now do kids - giving all a possible race/eth and age - matching for family_size later...
    #marital status 15-17 already on... hh_type_kids excludes householders, spouses, and unmarried partners...
    #match hh_type_kids to existing hh_relations for 15-17? Spouse is the one to match first, then rest - put age_range back on hh_type_kids
    hh_relations_dt[is.na(age),("age"):=-1]
    hh_relations_dt[is.na(marital_status)&age<18,("marital_status"):="no status given"]
    hh_relations_dt[age<18,("marital_match"):=if_else(marital_status!="Now married","Not","Now married")]
    hh_relations_dt[age<18&family_or_non=="In family households"&marital_match!="Now married",
                    ("unmarried_kid_id"):=
                      paste0(tract,as.character(1000000+sample(.N))),
                    by=.(tract)]
    hh_type_kids[,
                 ("unmarried_kid_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[age<18&family_or_non=="In family households"&marital_match!="Now married",
                    c("in_family_type"):=
                      hh_type_kids[.SD,in_family_type,
                                   on=.(unmarried_kid_id)]]
    hh_type_kids[,
                 ("relative"):=
                   hh_relations_dt[.SD,relative,
                                   on=.(unmarried_kid_id)]]
    #should be kids in gq and non_family left... 
    hh_relations_dt[age<18&family_or_non=="In family households"&is.na(in_family_type),
                    ("in_family_type"):="In married-couple family"]
    #all the in_family_type stuff for the kids is more like a suggestion - it will be better specified later and replaced
    #pick up age, etc. straight from sar/e
    antij_sar <- anti_join(sex_age_race,hh_relations_dt,by="individual_id")
    antij_sar[,("ajr_id"):=paste0(tract,as.character(1000000+sample(.N))),
              by=.(tract)] #gets shallow copy warning
    hh_relations_dt[is.na(individual_id),("ajr_id"):=paste0(tract,as.character(1000000+sample(.N))),
                    by=.(tract)]
    hh_relations_dt[is.na(individual_id),c("race","sex","age_range","individual_id"):=
                      antij_sar[.SD,c(list(race),list(sex),list(age_range),list(individual_id)),on=.(ajr_id)]]
    #nrow(hh_relations_dt[is.na(individual_id)])==0
    #nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt$individual_id)) #no NAs
    antij_sae <- anti_join(sex_by_age_eth,hh_relations_dt,by="ind_id_eth")
    antij_sae[,("aje_id"):=paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
              by=.(tract,sex,age_range)] #gets shallow copy warning
    hh_relations_dt[is.na(ind_id_eth),
                    ("aje_id"):=paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                    by=.(tract,sex,age_range)]
    hh_relations_dt[is.na(ind_id_eth),c("ethnicity","age","ind_id_eth"):=
                      antij_sae[.SD,c(list(ethnicity),list(age),list(ind_id_eth)),on=.(aje_id)]]
    #nrow(hh_relations_dt[is.na(ind_id_eth)])==0
    #nrow(hh_relations_dt[!is.na(ind_id_eth)])==length(unique(hh_relations_dt$ind_id_eth))
    #test4a 
    #test <- table(sex_age_race$tract,
    #              sex_age_race$sex,
    #              sex_age_race$age_range,
    #              sex_age_race$race
    #              )==table(
    #                hh_relations_dt$tract,
    #                hh_relations_dt$sex,
    #                hh_relations_dt$age_range,
    #                hh_relations_dt$race
    #              )
    #length(test[test==F])==0
    #test <- table(sex_by_age_eth$tract,
    #              sex_by_age_eth$sex,
    #              sex_by_age_eth$age_range,
    #              sex_by_age_eth$ethnicity
    #)==table(
    #  hh_relations_dt$tract,
    #  hh_relations_dt$sex,
    #  hh_relations_dt$age_range,
    #  hh_relations_dt$ethnicity
    #)
    #length(test[test==F])==0
    
    #now do place_born as a separate thread
    #sample for some, in order to get factor names to match; will over-write later
    sex_nativity_age_dt[,("age_range_14"):=case_when(
      age_range=="Under 5 years" ~ "0  to  4 years",
      age_range=="5 to 9 years" ~ "05 to  9 years",
      age_range=="15 to 19 years" ~ sample(c("15 to 17 years","18 to 19 years"),.N,prob = c(.6,.4),replace = T),
      age_range=="35 to 39 years" ~ "35 to 44 years",
      age_range=="40 to 44 years" ~ "35 to 44 years",
      age_range=="45 to 49 years" ~ "45 to 54 years",
      age_range=="50 to 54 years" ~ "45 to 54 years",
      age_range=="55 to 59 years" ~ "55 to 64 years",
      age_range=="60 to 64 years" ~ "55 to 64 years",
      age_range=="65 to 69 years" ~ "65 to 74 years",
      age_range=="70 to 74 years" ~ "65 to 74 years",
      age_range=="75 to 79 years" ~ "75 to 84 years",
      age_range=="80 to 84 years" ~ "75 to 84 years",
      age_range=="85 years and over" ~ "85 to 94 years",
      TRUE ~ age_range
    )]
    place_born_age_full_dt[,("age_range_14"):=case_when(  
      age_range=="05 to 17 years" ~ sample(c("05 to  9 years","10 to 14 years","15 to 17 years"),.N,prob = c(.35,.35,.3),replace = T),
      age_range=="18 to 24 years" ~ sample(c("18 to 19 years","20 to 24 years"),.N,prob = c(.35,.65),replace = T),
      age_range=="25 to 34 years" ~ sample(c("25 to 29 years","30 to 34 years"),.N,prob = c(.5,.5),replace = T),
      age_range=="55 to 59 years" ~ "55 to 64 years",
      age_range=="60 to 62 years" ~ "55 to 64 years",
      age_range=="62 to 64 years" ~ "55 to 64 years",
      age_range=="75 to 89 years" ~ sample(c("75 to 84 years","85 to 94 years"),.N,prob = c(.7,.3),replace = T),
      TRUE ~ age_range
    )]
    #doing foreign born parts where we have more info
    
    
    #put citizen data on sex_place_when and keep_date_entered from place_period_citizen
    place_period_citizen_dt[,("fb_origin_country"):=origin_country]
    place_period_citizen_dt[origin_country=="Mexico" | origin_country=="Other Central America",
                            ("origin_country"):="Central America"]
    place_period_citizen_dt[,("date_entered_match"):=
                              if_else(date_entered=="Entered 1990 to 1999" | date_entered=="Entered before 1990",
                                      "Entered before 2000",date_entered)]
    sex_place_when_dt[,("pb_citizen_id"):= 
                        paste0(tract,date_entered,origin_country,
                               as.character(1000000+sample(.N))),
                      by=.(tract,date_entered,origin_country)]
    place_period_citizen_dt[,("pb_citizen_id"):= 
                              paste0(tract,date_entered_match,origin_country,
                                     as.character(1000000+seq.int(1:.N))),
                            by=.(tract,date_entered_match,origin_country)]
    sex_place_when_dt[,c("fb_origin_place","fb_date_entered","fb_citizen"):= 
                        place_period_citizen_dt[.SD, c(list(fb_origin_country),
                                                       list(date_entered),list(citizen)), 
                                                on = .(pb_citizen_id)]]
    #test 6b
    #test <- table(place_period_citizen_dt$tract,
    #              place_period_citizen_dt$date_entered,
    #              place_period_citizen_dt$fb_origin_country,
    #              place_period_citizen_dt$citizen)==
    #  table(sex_place_when_dt$tract,
    #        sex_place_when_dt$fb_date_entered,
    #        sex_place_when_dt$fb_origin_place,
    #        sex_place_when_dt$fb_citizen)
    #length(test[test==F])==0
    #sex_nativity gives only sex and age_range - move to sex_place_when by age group
    #by youngest first
    sex_place_when_dt[fb_date_entered=="Entered 2010 or later",
                      ("sn_age1_id"):= 
                        paste0(tract,sex,
                               as.character(1000000+sample(.N))),
                      by=.(tract,sex)]
    sex_nativity_age_dt[substr(age_range_14,1,2)<10,("sn_age1_id"):= 
                          paste0(tract,sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(tract,sex)]
    sex_place_when_dt[fb_date_entered=="Entered 2010 or later",
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age1_id)]]
    sex_nativity_age_dt[substr(age_range_14,1,2)<10,
                      c("missed_sn"):= 
                        sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age1_id)]]
    #test 6c
    #test <- nrow(sex_place_when_dt[!is.na(sn_age_range)])==nrow(sex_nativity_age_dt[substr(age_range_14,1,2)<10])
    #looks like 5 tracts fewer in sex_place_when had matches than came out of sex_nativity. - ~400 folks
    #so do without tract
    sex_place_when_dt[fb_date_entered=="Entered 2010 or later" & is.na(sn_age_range),
                      ("sn_age1a_id"):= 
                        paste0(sex,
                               as.character(1000000+sample(.N))),
                      by=.(sex)]
    sex_nativity_age_dt[substr(age_range_14,1,2)<10 & is.na(missed_sn),
                        ("sn_age1a_id"):= 
                          paste0(sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(sex)]
    sex_place_when_dt[fb_date_entered=="Entered 2010 or later" & is.na(sn_age_range),
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age1a_id)]]
    sex_nativity_age_dt[substr(age_range_14,1,2)<10 & is.na(missed_sn),
                        c("missed_sn"):= 
                          sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age1a_id)]]
    #test 6c again, this time T
    #second youngest
    sex_place_when_dt[str_detect(fb_date_entered,"20") & is.na(sn_age_range),
                      ("sn_age2_id"):= 
                        paste0(tract,sex,
                               as.character(1000000+sample(.N))),
                      by=.(tract,sex)]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<20,
                        ("sn_age2_id"):= 
                          paste0(tract,sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(tract,sex)]
    sex_place_when_dt[str_detect(fb_date_entered,"20") & is.na(sn_age_range),
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age2_id)]]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<20,
                        c("missed_sn"):= 
                          sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age2_id)]]
    #do without tract
    sex_place_when_dt[str_detect(fb_date_entered,"20") & is.na(sn_age_range),
                      ("sn_age2a_id"):= 
                        paste0(sex,
                               as.character(1000000+sample(.N))),
                      by=.(sex)]
    sex_nativity_age_dt[substr(age_range_14,1,2)<20 & is.na(missed_sn),
                        ("sn_age2a_id"):= 
                          paste0(sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(sex)]
    sex_place_when_dt[str_detect(fb_date_entered,"20") & is.na(sn_age_range),
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age2a_id)]]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<20,
                        c("missed_sn"):= 
                          sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age2a_id)]]
    
    #third youngest
    sex_place_when_dt[fb_date_entered!="Entered before 1990" & is.na(sn_age_range),
                      ("sn_age3_id"):= 
                        paste0(tract,sex,
                               as.character(1000000+sample(.N))),
                      by=.(tract,sex)]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<30,
                        ("sn_age3_id"):= 
                          paste0(tract,sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(tract,sex)]
    sex_place_when_dt[fb_date_entered!="Entered before 1990" & is.na(sn_age_range),
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age3_id)]]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<30,
                        c("missed_sn"):= 
                          sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age3_id)]]
    #do without tract
    sex_place_when_dt[fb_date_entered!="Entered before 1990" & is.na(sn_age_range),
                      ("sn_age3a_id"):= 
                        paste0(sex,
                               as.character(1000000+sample(.N))),
                      by=.(sex)]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<30,
                        ("sn_age3a_id"):= 
                          paste0(sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(sex)]
    sex_place_when_dt[fb_date_entered!="Entered before 1990" & is.na(sn_age_range),
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age3a_id)]]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<30,
                        c("missed_sn"):= 
                          sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age3a_id)]]
    #rest
    sex_place_when_dt[is.na(sn_age_range),
                      ("sn_age4_id"):= 
                        paste0(tract,sex,
                               as.character(1000000+sample(.N))),
                      by=.(tract,sex)]
    sex_nativity_age_dt[is.na(missed_sn),
                        ("sn_age4_id"):= 
                          paste0(tract,sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(tract,sex)]
    sex_place_when_dt[is.na(sn_age_range),
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age4_id)]]
    sex_nativity_age_dt[is.na(missed_sn),
                        c("missed_sn"):= 
                          sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age4_id)]]
    #do without tract 
    sex_place_when_dt[is.na(sn_age_range),
                      ("sn_age4a_id"):= 
                        paste0(sex,
                               as.character(1000000+sample(.N))),
                      by=.(sex)]
    sex_nativity_age_dt[is.na(missed_sn),
                        ("sn_age4a_id"):= 
                          paste0(sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(sex)]
    sex_place_when_dt[is.na(sn_age_range),
                      c("sn_age_range","age_range_14"):= 
                        sex_nativity_age_dt[.SD, c(list(age_range),list(age_range_14)), 
                                            on = .(sn_age4a_id)]]
    sex_nativity_age_dt[is.na(missed_sn),
                        c("missed_sn"):= 
                          sex_place_when_dt[.SD, list(sn_age_range), 
                                            on = .(sn_age4a_id)]]
    #test 6c1
    #test <- table(sex_place_when_dt$tract,
    #              sex_place_when_dt$sn_age_range,
    #              sex_place_when_dt$age_range_14,
    #              sex_place_when_dt$sex)==
    #  table(sex_nativity_age_dt$tract,
    #        sex_nativity_age_dt$age_range,
    #        sex_nativity_age_dt$age_range_14,
    #        sex_nativity_age_dt$sex)
    #length(test[test==F])/length(test) < .002 #it's a little under 2% off, because of that secondary matching above, but unavoidable and right totals.
    
    #origin_data - I think the numbers per region are safer from sex_place_when - had to sample from expand_from_census 
    origin_data_dt[,("origin_match"):=case_when(
      origin_continent=="Africa" | origin_continent=="Oceania" | 
        origin_area=="Northern America" ~ "Other areas",
      origin_region=="Caribbean" ~ "Caribbean",
      origin_region=="Central America" ~ "Central America",
      origin_region=="South America" ~ "South America",
      TRUE ~ origin_continent #gets  Asia, Europe
    )]
    #once to get exact matches, then to redistribute by sampling from Caribbean, Central American, and South American
    origin_data_dt[,
                   ("orig_sn_id"):= 
                     paste0(tract,origin_match,
                            as.character(1000000+sample(.N))),
                   by=.(tract,origin_match)]
    sex_place_when_dt[,
                      ("orig_sn_id"):= 
                        paste0(tract,origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,origin_region)]
    sex_place_when_dt[,c("fb_origin_continent","fb_origin_area","fb_origin_region","fb_origin_country"):=
      origin_data_dt[.SD, c(list(origin_continent),list(origin_area),list(origin_region),list(origin_country)), 
                     on = .(orig_sn_id)]]
    sex_place_when_dt[is.na(fb_origin_continent),
                      ("orig_sn2_id"):= 
                        paste0(tract,origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,origin_region)]
    origin_data_dt[,#b/c more of each of missing is here, they should fill
                   ("orig_sn2_id"):= 
                     paste0(tract,origin_match,
                            as.character(1000000+sample(.N))),
                   by=.(tract,origin_match)]
    sex_place_when_dt[is.na(fb_origin_continent),
                      c("fb_origin_continent","fb_origin_area","fb_origin_region","fb_origin_country"):=
                        origin_data_dt[.SD, c(list(origin_continent),list(origin_area),list(origin_region),list(origin_country)), 
                                       on = .(orig_sn2_id)]]
    #about 3k still left, so pick up without tract
    sex_place_when_dt[is.na(fb_origin_continent),
                      ("orig_sn3_id"):= 
                        paste0(origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(origin_region)]
    origin_data_dt[,
                   ("orig_sn3_id"):= 
                     paste0(origin_match,
                            as.character(1000000+sample(.N))),
                   by=.(origin_match)]
    sex_place_when_dt[is.na(fb_origin_continent),
                      c("fb_origin_continent","fb_origin_area","fb_origin_region","fb_origin_country"):=
                        origin_data_dt[.SD, c(list(origin_continent),list(origin_area),list(origin_region),list(origin_country)), 
                                       on = .(orig_sn3_id)]]
    #test 6d
    #test <- nrow(sex_place_when_dt[is.na(fb_origin_continent)])==0
    
    #add expanded fb to place_born_age
    place_born_age_full_dt[,("pbagefull_id"):=paste0(tract,as.character(100000000+sample(.N))),
                           by=.(tract)]
    place_born_age_full_dt[place_born=="Foreign born",
                           ("pb_age_fb_id"):=paste0(tract,age_range_14,as.character(100000000+sample(.N))),
                           by=.(tract,age_range_14)]
    sex_place_when_dt[,("pb_age_fb_id"):=paste0(tract,age_range_14,as.character(100000000+sample(.N))),
                      by=.(tract,age_range_14)]
    place_born_age_full_dt[place_born=="Foreign born",
                           c("age_range_18","fb_sex","fb_citizen","fb_origin_continent",
                              "fb_origin_area","fb_origin_place","fb_origin_country","fb_date_entered"):=
                             sex_place_when_dt[.SD, c(list(sn_age_range),list(sex),list(fb_citizen),
                                                      list(fb_origin_continent),list(fb_origin_area),
                                                      list(fb_origin_place),list(fb_origin_country),
                                                      list(fb_date_entered)), 
                                on = .(pb_age_fb_id)]]
    sex_place_when_dt[,("missed_fb_id"):=
                        place_born_age_full_dt[.SD,list(fb_date_entered),
                                               on = .(pb_age_fb_id)]]
    place_born_age_full_dt[place_born=="Foreign born" & is.na(fb_date_entered),
                           ("pb_age2_fb_id"):=paste0(tract,as.character(100000000+sample(.N))),
                           by=.(tract,age_range_14)]
    sex_place_when_dt[is.na(missed_fb_id),("pb_age2_fb_id"):=paste0(tract,as.character(100000000+sample(.N))),
                      by=.(tract)]
    #using this age_range_14 because place_born doesn't match sar in any case...
    place_born_age_full_dt[place_born=="Foreign born" & is.na(fb_date_entered),
                           c("age_range_14","age_range_18","fb_sex","fb_citizen","fb_origin_continent",
                             "fb_origin_area","fb_origin_place","fb_origin_country","fb_date_entered"):=
                             sex_place_when_dt[.SD, c(list(age_range_14),list(sn_age_range),list(sex),list(fb_citizen),
                                                      list(fb_origin_continent),list(fb_origin_area),
                                                      list(fb_origin_place),list(fb_origin_country),
                                                      list(fb_date_entered)), 
                                               on = .(pb_age2_fb_id)]]
    #test 7
    #nrow(sex_place_when_dt[!is.na(fb_date_entered)])==nrow(place_born_age_full_dt[!is.na(fb_date_entered)])
    #test<-table(sex_place_when_dt$tract,
    #            sex_place_when_dt$fb_origin_continent)-
    #  table(place_born_age_full_dt[place_born=="Foreign born",tract],
    #        place_born_age_full_dt[place_born=="Foreign born",fb_origin_continent])
    #max(abs(test[,1:5])) < 35
    #lose less than 1% on others, but they don't match at tract level
    
    
    #add ethnicity from latinx
    latinx_dt[,("ethnicity"):=if_else(latinx=="Hispanic or Latino","I",NULL)]
    #match will only be for foreign born, but latinx is for whole pop and includes lots of latinx from non-majority latin countries
    place_born_age_full_dt[,("family_origin"):=case_when(
      fb_origin_country=="Argentina" ~ "Argentinean",
      fb_origin_country=="Belize" ~ "Other Central American",
      fb_origin_country=="Bolivia" ~ "Bolivian",
      fb_origin_country=="Brazil" ~ "Other South American",
      fb_origin_country=="Chile" ~ "Chilean",
      fb_origin_country=="Columbia" ~ "Columbian",
      fb_origin_country=="Costa Rica" ~ "Costa Rican",
      fb_origin_country=="Cuba" ~ "Cuban",
      fb_origin_country=="Dominican Republic" ~ "Dominican (Dominican Republic)",
      fb_origin_country=="Ecuador" ~ "Ecuadorian",
      fb_origin_country=="Guatemala" ~ "Guatemalan",
      fb_origin_country=="Honduras" ~ "Honduran",
      fb_origin_country=="Mexico" ~ "Mexican",
      fb_origin_country=="Nicaragua" ~ "Nicaraguan",
      fb_origin_country=="Panama" ~ "Panamanian", #Paraguayan was not picked up
      fb_origin_country=="Peru" ~ "Peruvian", #Puerto Rican is a category from other places since it is U.S.
      fb_origin_country=="El Salvador" ~ "Salvadoran",
      fb_origin_country=="Spain" ~ "Spaniard",
      fb_origin_country=="Uruguay" ~ "Uruguayan",
      fb_origin_country=="Venezuela" ~ "Venezuelan",
    )]
    place_born_age_full_dt[,
                           ("latinx_fb_id"):=paste0(tract,family_origin,as.character(100000000+sample(.N))),
                           by=.(tract,family_origin)]
    latinx_dt[,("latinx_fb_id"):=paste0(tract,origin_country,as.character(100000000+sample(.N))),
                      by=.(tract,origin_country)]
    place_born_age_full_dt[,c("latinx_family_origin","latinx","ethnicity"):=
                             latinx_dt[.SD, c(list(origin_country),list(latinx),list(ethnicity)), 
                                               on = .(latinx_fb_id)]]
    latinx_dt[,c("miss_latinx"):=
                place_born_age_full_dt[.SD, list(latinx_family_origin), 
                                       on = .(latinx_fb_id)]]
    #get the last 67k fb_Latin America
    place_born_age_full_dt[is.na(latinx_family_origin) & fb_origin_area=="Latin America",
                           ("latinx_fb2_id"):=paste0(tract,as.character(100000000+sample(.N))),
                           by=.(tract)]
    latinx_dt[is.na(miss_latinx),
              ("latinx_fb2_id"):=paste0(tract,as.character(100000000+sample(.N))),
              by=.(tract)]
    place_born_age_full_dt[is.na(latinx_family_origin) & fb_origin_area=="Latin America",
                           c("latinx_family_origin","latinx","ethnicity"):=
                             latinx_dt[.SD, c(list(origin_country),list(latinx),list(ethnicity)), 
                                       on = .(latinx_fb2_id)]]
    latinx_dt[is.na(miss_latinx),
                    c("miss_latinx"):=
                place_born_age_full_dt[.SD, list(latinx_family_origin), 
                                       on = .(latinx_fb2_id)]]
    #put in rest of place born by sampling
    place_born_age_full_dt[is.na(latinx_family_origin),
                           ("latinx_fb_id"):=paste0(tract,as.character(100000000+sample(.N))),
                           by=.(tract)]
    latinx_dt[is.na(miss_latinx),("latinx_fb_id"):=paste0(tract,as.character(100000000+sample(.N))),
              by=.(tract)]
    place_born_age_full_dt[is.na(latinx_family_origin),
                           c("latinx_family_origin","latinx","ethnicity"):=
                             latinx_dt[.SD, c(list(origin_country),list(latinx),list(ethnicity)), 
                                       on = .(latinx_fb_id)]]
    
    #get match on ethnicity from latinx first
    place_born_eth_dt[ethnicity=="I",("pbea_id"):= 
                        paste0(tract,place_born,
                               as.character(1000000+sample(.N))),
                      by=.(tract,place_born)]
    place_born_age_full_dt[latinx=="Hispanic or Latino",("pbea_id"):= 
                             paste0(tract,place_born,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,place_born)]
    place_born_eth_dt[ethnicity=="I",c("age_range_14","fb_sex","fb_citizen",
                         "fb_origin_continent","fb_origin_area",
                         "fb_origin_place","fb_origin_country",
                         "fb_date_entered","family_origin","latinx_family_origin",
                         "latinx"):=
                        place_born_age_full_dt[.SD, c(list(age_range_14),
                                                      list(fb_sex),list(fb_citizen),list(fb_origin_continent),
                                                      list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                                      list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                                      list(latinx)), 
                                               on = .(pbea_id)]]
    place_born_age_full_dt[latinx=="Hispanic or Latino",c("missed_eth"):=
                             place_born_eth_dt[.SD, list(ethnicity), 
                                               on = .(pbea_id)]]
    
    #full group
    place_born_eth_dt[is.na(age_range_14),("pbea1_id"):= 
                        paste0(tract,place_born,
                               as.character(1000000+sample(.N))),
                      by=.(tract,place_born)]
    place_born_age_full_dt[is.na(missed_eth),("pbea1_id"):= 
                             paste0(tract,place_born,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,place_born)]
    place_born_eth_dt[is.na(age_range_14),c("age_range_14","fb_sex","fb_citizen",
                         "fb_origin_continent","fb_origin_area",
                         "fb_origin_place","fb_origin_country",
                         "fb_date_entered","family_origin","latinx_family_origin",
                         "latinx"):=
                        place_born_age_full_dt[.SD, c(list(age_range_14),
                                                      list(fb_sex),list(fb_citizen),list(fb_origin_continent),
                                                      list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                                      list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                                      list(latinx)), 
                                               on = .(pbea1_id)]]
    place_born_age_full_dt[is.na(missed_eth),c("missed_eth"):=
                             place_born_eth_dt[.SD, list(ethnicity), 
                                               on = .(pbea1_id)]]
    #test 8 
    #test <- table(place_born_age_full_dt$tract,
    #              place_born_age_full_dt$place_born,
    #              place_born_age_full_dt$age_range_14
    #              )==
    #  table(place_born_eth_dt$tract,
    #        place_born_eth_dt$place_born,
    #        place_born_eth_dt$age_range_14
    #        )
    #length(test[test==F]) == 0
    
    #put place_born stuff on pb_eth/race  
    place_born_age_full_dt[,c("race_sorter"):=case_when( 
      fb_origin_area=="Fiji" | fb_origin_area=="Oceania n.e.c." ~ 1,
      str_detect(fb_origin_area,"Asia") ~ 2,
      str_detect(fb_origin_area,"Africa") ~ 3,
      str_detect(fb_origin_area,"Europe") | fb_origin_area=="Northern America" |
        fb_origin_area=="Australia and New Zealand Subregion" ~ 4,
      fb_origin_area=="Latin America" | ethnicity=="I" ~ 5,
      fb_origin_area=="Northern Africa" ~ 6
    )]
    
    place_born_age_full_dt[order(race_sorter),
                      ("pbra_id"):=paste0(tract,place_born,
                                             as.character(1000000+seq.int(1:.N))),
                      by=.(tract,place_born)]
    place_born_race_dt[order(match(race,c("E","D","B","A","G","F","C"))),
                       ("pbra_id"):=paste0(tract,place_born,
                                              as.character(1000000+seq.int(1:.N))),
                       by=.(tract,place_born)]
    place_born_race_dt[,c("age_range_14","fb_sex","fb_citizen",
                          "fb_origin_continent","fb_origin_area",
                          "fb_origin_place","fb_origin_country",
                          "fb_date_entered","family_origin","latinx_family_origin",
                          "latinx","ethnicity"):=     #putting eth here should make join easier later...
                         place_born_age_full_dt[.SD, c(list(age_range_14),
                                                       list(fb_sex),list(fb_citizen),list(fb_origin_continent),
                                                       list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                                       list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                                       list(latinx),list(ethnicity)), 
                                                on = .(pbra_id)]]
    #test 8b
    #test <- table(place_born_age_full_dt$tract,
    #              place_born_age_full_dt$place_born,
    #              place_born_age_full_dt$ethnicity,
    #              place_born_age_full_dt$age_range_14)==
    #  table(place_born_race_dt$tract,
    #        place_born_race_dt$place_born,
    #        place_born_race_dt$ethnicity,
    #        place_born_race_dt$age_range_14)
    #length(test[test==F])==0   
    
    ####START HERE WITH FULL COMPLEMENT OF WHAT'S NEEDED FOR DIVIDING...
    #divide up eth/race 
    hh_relations_eth <- hh_relations_dt[,c("ind_id_eth","tract","ethnicity","sex","age","age_range","age_range_3",
                                           "group_or_hh","family_or_non","relative","relation_hh","role_in_family","living_alone",
                                           "spouse_present","separated","place_born","marital_status","marital_status_5",
                                           "pregnant","in_family_type","race")] #carry race around for first match with pb, below
    hh_relations_race <- hh_relations_dt[,c("individual_id","tract","race","sex","age","age_range","age_range_3",
                                            "group_or_hh","family_or_non","relative","relation_hh","role_in_family","living_alone",
                                            "spouse_present","separated","place_born","marital_status","marital_status_5",
                                            "pregnant","in_family_type")]
    
    hh_relations_eth[,("hh_relrele_id"):= 
                       paste0(tract,ethnicity,sex,age_range,place_born,as.character(10000000+seq.int(.N))),
                     by=.(tract,ethnicity,sex,age_range,place_born)]     
    place_born_eth_dt[,("hh_relrele_id"):=
                                 paste0(tract,ethnicity,fb_sex,age_range_14,place_born,as.character(10000000+seq.int(.N))),
                               by=.(tract,ethnicity,fb_sex,age_range_14,place_born)]
    hh_relations_eth[,c("fb_citizen",
                        "fb_origin_continent","fb_origin_area",
                        "fb_origin_place","fb_origin_country",
                        "fb_date_entered","family_origin","latinx_family_origin",
                        "latinx"):=
                       place_born_eth_dt[.SD,c(list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                                  on = .(hh_relrele_id)]]
    place_born_eth_dt[,c("missed_hhre","sex","race"):=
                        hh_relations_eth[.SD,c(list(latinx),list(sex),list(race)),
                                         on = .(hh_relrele_id)]]
    #nrow(hh_relations_eth[!is.na(fb_citizen)])==nrow(place_born_eth_dt[!is.na(missed_hhre)])
    #nrow(place_born_eth_dt[!is.na(fb_citizen)])-nrow(hh_relations_eth[!is.na(fb_citizen)]) #445146 of 1119188 of 1174879 (2/3 matched)
    #pick up again, but writing over place_born, from here on out - originally from marital, and only has over 14yo (56691 fb_kids; need to match)
    hh_relations_eth[is.na(fb_citizen),("hh_relrele1_id"):= 
                       paste0(tract,ethnicity,sex,age_range,as.character(10000000+sample(.N))),
                     by=.(tract,ethnicity,sex,age_range)]     
    place_born_eth_dt[is.na(missed_hhre),("hh_relrele1_id"):=
                        paste0(tract,ethnicity,fb_sex,age_range_14,as.character(10000000+sample(.N))),
                      by=.(tract,ethnicity,fb_sex,age_range_14)]
    hh_relations_eth[is.na(fb_citizen),c("place_born","fb_citizen",
                        "fb_origin_continent","fb_origin_area",
                        "fb_origin_place","fb_origin_country",
                        "fb_date_entered","family_origin","latinx_family_origin",
                        "latinx"):=
                       place_born_eth_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele1_id)]]
    place_born_eth_dt[is.na(missed_hhre),c("missed_hhre","sex","race"):=
                        hh_relations_eth[.SD,c(list(latinx),list(sex),list(race)),
                                         on = .(hh_relrele1_id)]]
    #nrow(hh_relations_eth[!is.na(fb_citizen)])==nrow(place_born_eth_dt[!is.na(missed_hhre)])
    #6288 short of matching all fb over 14 (1119188-1112900)
    
    #second time without sex - to pick up non fb
    hh_relations_eth[is.na(latinx),("hh_relrele2_id"):= 
                       paste0(tract,ethnicity,age_range,as.character(10000000+sample(.N))),
                     by=.(tract,ethnicity,age_range)]     
    place_born_eth_dt[is.na(missed_hhre),("hh_relrele2_id"):=
                        paste0(tract,ethnicity,age_range_14,as.character(10000000+sample(.N))),
                      by=.(tract,ethnicity,age_range_14)]
    hh_relations_eth[is.na(latinx),c("place_born","fb_citizen",
                                         "fb_origin_continent","fb_origin_area",
                                         "fb_origin_place","fb_origin_country",
                                         "fb_date_entered","family_origin","latinx_family_origin",
                                         "latinx"):=
                       place_born_eth_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele2_id)]]
    place_born_eth_dt[is.na(missed_hhre),c("missed_hhre","sex","race"):=
                        hh_relations_eth[.SD,c(list(latinx),list(sex),list(race)),
                                         on = .(hh_relrele2_id)]]
    #nrow(hh_relations_eth)-nrow(hh_relations_eth[!is.na(latinx)]) #690035 ~85% matched 
    #try just on ethnicity, with the idea that you're sampling from available age_range inside each tract, still
    hh_relations_eth[is.na(latinx),("hh_relrele3_id"):= 
                       paste0(tract,ethnicity,as.character(10000000+sample(.N))),
                     by=.(tract,ethnicity)]     
    place_born_eth_dt[is.na(missed_hhre),("hh_relrele3_id"):=
                        paste0(tract,ethnicity,as.character(10000000+sample(.N))),
                      by=.(tract,ethnicity)]
    hh_relations_eth[is.na(latinx),c("place_born","fb_citizen",
                                     "fb_origin_continent","fb_origin_area",
                                     "fb_origin_place","fb_origin_country",
                                     "fb_date_entered","family_origin","latinx_family_origin",
                                     "latinx"):=
                       place_born_eth_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele3_id)]]
    place_born_eth_dt[is.na(missed_hhre),c("missed_hhre","sex","race","age_range_14"):=
                        hh_relations_eth[.SD,c(list(latinx),list(sex),list(race),list(age_range)),
                                         on = .(hh_relrele3_id)]]
    #test9
    #table(hh_relations_eth[!is.na(latinx),place_born])-table(place_born_eth_dt$place_born)==c(0,0,0,0)
    #test <- table(hh_relations_eth$tract,
    #              hh_relations_eth$fb_citizen,
    #              hh_relations_eth$fb_origin_place,
    #              hh_relations_eth$fb_origin_country,
    #              hh_relations_eth$latinx_family_origin,
    #              hh_relations_eth$latinx,
    #              hh_relations_eth$place_born
    #              )==table(
    #                place_born_eth_dt$tract,
    #                place_born_eth_dt$fb_citizen,
    #                place_born_eth_dt$fb_origin_place,
    #                place_born_eth_dt$fb_origin_country,
    #                place_born_eth_dt$latinx_family_origin,
    #                place_born_eth_dt$latinx,
    #                place_born_eth_dt$place_born
    #              )
    #length(test[test==F])==0
    #do race
    #place_born_race age_ranges don't match on totals by age by tract with any of the others!
    place_born_eth_dt[,("pb_race_age_id"):=paste0(tract,age_range_14,sex,race,place_born,as.character(10000000+sample(.N))),
                      by=.(tract,age_range_14,sex,race,place_born)]
    place_born_race_dt[,("pb_race_age_id"):=paste0(tract,age_range_14,fb_sex,race,place_born,as.character(10000000+sample(.N))),
                       by=.(tract,age_range_14,fb_sex,race,place_born)]
    place_born_race_dt[,("age_range"):=
                         place_born_eth_dt[.SD,age_range_14,on=.(pb_race_age_id)]]
    place_born_eth_dt[,("miss_pbr"):=
                        place_born_race_dt[.SD,age_range,on=.(pb_race_age_id)]]
    #for non-fb without sex, but still age_range
    place_born_eth_dt[is.na(miss_pbr),("pb_race_age1_id"):=paste0(tract,age_range_14,race,place_born,as.character(10000000+sample(.N))),
                      by=.(tract,age_range_14,race,place_born)]
    place_born_race_dt[is.na(age_range),("pb_race_age1_id"):=paste0(tract,age_range_14,race,place_born,as.character(10000000+sample(.N))),
                       by=.(tract,age_range_14,race,place_born)]
    place_born_race_dt[is.na(age_range),("age_range"):=
                         place_born_eth_dt[.SD,age_range_14,on=.(pb_race_age1_id)]]
    place_born_eth_dt[is.na(miss_pbr),("miss_pbr"):=
                        place_born_race_dt[.SD,age_range,on=.(pb_race_age1_id)]]
    #and for the ones where age_range didn't match (70% matched)
    place_born_eth_dt[is.na(miss_pbr),("pb_race_age2_id"):=paste0(tract,race,place_born,as.character(10000000+sample(.N))),
                      by=.(tract,race,place_born)]
    place_born_race_dt[is.na(age_range),("pb_race_age2_id"):=paste0(tract,race,place_born,as.character(10000000+sample(.N))),
                       by=.(tract,race,place_born)]
    place_born_race_dt[is.na(age_range),("age_range"):=
                         place_born_eth_dt[.SD,age_range_14,on=.(pb_race_age2_id)]]
    place_born_eth_dt[is.na(miss_pbr),("miss_pbr"):=
                        place_born_race_dt[.SD,age_range,on=.(pb_race_age2_id)]]
    #89% matched - distribute remaining age_range by sample, and then do more merging later
    place_born_eth_dt[is.na(miss_pbr),("pb_race_age3_id"):=paste0(tract,race,as.character(10000000+sample(.N))),
                      by=.(tract,race)]
    place_born_race_dt[is.na(age_range),("pb_race_age3_id"):=paste0(tract,race,as.character(10000000+sample(.N))),
                       by=.(tract,race)]
    place_born_race_dt[is.na(age_range),("age_range"):=
                         place_born_eth_dt[.SD,age_range_14,on=.(pb_race_age3_id)]]
    place_born_eth_dt[is.na(miss_pbr),("miss_pbr"):=
                        place_born_race_dt[.SD,age_range,on=.(pb_race_age3_id)]]
    
    #test9b
    #test <- table(place_born_race_dt$tract,
    #              place_born_race_dt$race,
    #              place_born_race_dt$age_range
    #              )==table(
    #                place_born_eth_dt$tract,
    #                place_born_eth_dt$race,
    #                place_born_eth_dt$age_range
    #              )
    #length(test[test==F])==0
    
    #move corrected place_born_race over to hh_relations_race
    hh_relations_race[,("hh_relrele_id"):= 
                       paste0(tract,race,sex,age_range,place_born,as.character(10000000+seq.int(.N))),
                     by=.(tract,race,sex,age_range,place_born)]     
    place_born_race_dt[,("hh_relrele_id"):=
                        paste0(tract,race,fb_sex,age_range_14,place_born,as.character(10000000+seq.int(.N))),
                      by=.(tract,race,fb_sex,age_range_14,place_born)]
    hh_relations_race[,c("fb_citizen",
                        "fb_origin_continent","fb_origin_area",
                        "fb_origin_place","fb_origin_country",
                        "fb_date_entered","family_origin","latinx_family_origin",
                        "latinx"):=
                       place_born_race_dt[.SD,c(list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele_id)]]
    place_born_race_dt[,c("missed_hhre"):=
                        hh_relations_race[.SD,list(latinx),
                                         on = .(hh_relrele_id)]]
    #nrow(hh_relations_race[!is.na(fb_citizen)])==nrow(place_born_race_dt[!is.na(missed_hhre)])
    #nrow(place_born_race_dt[!is.na(fb_citizen)])-nrow(hh_relations_race[!is.na(fb_citizen)]) #536796 of 1119188 of 1174879 (1/2 matched)
    #pick up again, but writing over place_born, from here on out - originally from marital, and only has over 14yo (56691 fb_kids; need to match)
    hh_relations_race[is.na(fb_citizen),("hh_relrele1_id"):= 
                       paste0(tract,race,sex,age_range,as.character(10000000+sample(.N))),
                     by=.(tract,race,sex,age_range)]     
    place_born_race_dt[is.na(missed_hhre),("hh_relrele1_id"):=
                        paste0(tract,race,fb_sex,age_range_14,as.character(10000000+sample(.N))),
                      by=.(tract,race,fb_sex,age_range_14)]
    hh_relations_race[is.na(fb_citizen),c("place_born","fb_citizen",
                                         "fb_origin_continent","fb_origin_area",
                                         "fb_origin_place","fb_origin_country",
                                         "fb_date_entered","family_origin","latinx_family_origin",
                                         "latinx"):=
                       place_born_race_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele1_id)]]
    place_born_race_dt[is.na(missed_hhre),c("missed_hhre"):=
                        hh_relations_race[.SD,list(latinx),
                                         on = .(hh_relrele1_id)]]
    #nrow(hh_relations_race[!is.na(fb_citizen)])==nrow(place_born_race_dt[!is.na(missed_hhre)])
    #150k short of matching all fb over 14 (1119188-968376)
    
    #second time without sex - to pick up non fb
    hh_relations_race[is.na(latinx),("hh_relrele2_id"):= 
                       paste0(tract,race,age_range,as.character(10000000+sample(.N))),
                     by=.(tract,race,age_range)]     
    place_born_race_dt[is.na(missed_hhre),("hh_relrele2_id"):=
                        paste0(tract,race,age_range_14,as.character(10000000+sample(.N))),
                      by=.(tract,race,age_range_14)]
    hh_relations_race[is.na(latinx),c("place_born","fb_citizen",
                                     "fb_origin_continent","fb_origin_area",
                                     "fb_origin_place","fb_origin_country",
                                     "fb_date_entered","family_origin","latinx_family_origin",
                                     "latinx"):=
                       place_born_race_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele2_id)]]
    place_born_race_dt[is.na(missed_hhre),c("missed_hhre"):=
                        hh_relations_race[.SD,list(latinx),
                                         on = .(hh_relrele2_id)]]
    #nrow(hh_relations_race)-nrow(hh_relations_race[!is.na(latinx)]) #1083289 ~76% matched 
    #try just on race, with the idea that you're sampling from available age_range inside each tract, still
    hh_relations_race[is.na(latinx),("hh_relrele3_id"):= 
                       paste0(tract,race,as.character(10000000+sample(.N))),
                     by=.(tract,race)]     
    place_born_race_dt[is.na(missed_hhre),("hh_relrele3_id"):=
                        paste0(tract,race,as.character(10000000+sample(.N))),
                      by=.(tract,race)]
    hh_relations_race[is.na(latinx),c("place_born","fb_citizen",
                                     "fb_origin_continent","fb_origin_area",
                                     "fb_origin_place","fb_origin_country",
                                     "fb_date_entered","family_origin","latinx_family_origin",
                                     "latinx"):=
                       place_born_race_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele3_id)]]
    place_born_race_dt[is.na(missed_hhre),c("missed_hhre"):=
                        hh_relations_race[.SD,list(latinx),
                                         on = .(hh_relrele3_id)]]
    #test10
    #table(hh_relations_race[!is.na(latinx),place_born])-table(place_born_race_dt$place_born)==c(0,0,0,0)
    #test <- table(hh_relations_race$tract,
    #              hh_relations_race$fb_citizen,
    #              hh_relations_race$fb_origin_place,
    #              hh_relations_race$fb_origin_country,
    #              hh_relations_race$latinx_family_origin,
    #              hh_relations_race$latinx,
    #              hh_relations_race$place_born
    #              )==table(
    #                place_born_race_dt$tract,
    #                place_born_race_dt$fb_citizen,
    #                place_born_race_dt$fb_origin_place,
    #                place_born_race_dt$fb_origin_country,
    #                place_born_race_dt$latinx_family_origin,
    #                place_born_race_dt$latinx,
    #                place_born_race_dt$place_born
    #              )
    #length(test[test==F])==0
    
    #add in_family_type from household_relatives
    #matches to household_relatives for in_family_type only
    hh_relations_race[group_or_hh=="In households" & family_or_non=="In family households",
                      ("relative_or_non"):=case_when(
                        role_in_family=="Biological child" | role_in_family=="Grandchild" | role_in_family=="Spouse" |
                          role_in_family=="Brother or sister" |
                          role_in_family=="Adopted child" | 
                          role_in_family=="Stepchild" | role_in_family=="Unmarried partner" | 
                          role_in_family=="Other relatives" | role_in_family =="Parent" ~ "Relatives", 
                        role_in_family=="Other nonrelatives" | role_in_family=="Parent-in-law" | 
                          role_in_family=="Son-in-law or daughter-in-law" | role_in_family=="Foster child" | 
                          role_in_family=="Housemate or roommate" | role_in_family=="Roomer or boarder" ~ "Nonrelatives",
                        TRUE ~ "Relatives" 
                      )]
    hh_relations_eth[group_or_hh=="In households" & family_or_non=="In family households",
                     ("relative_or_non"):=case_when(
                       role_in_family=="Biological child" | role_in_family=="Grandchild" | role_in_family=="Spouse" |
                         role_in_family=="Brother or sister" |
                         role_in_family=="Adopted child" | 
                         role_in_family=="Stepchild" | role_in_family=="Unmarried partner" | 
                         role_in_family=="Other relatives" | role_in_family =="Parent" ~ "Relatives", 
                       role_in_family=="Other nonrelatives" | role_in_family=="Parent-in-law" | 
                         role_in_family=="Son-in-law or daughter-in-law" | role_in_family=="Foster child" | 
                         role_in_family=="Housemate or roommate" | role_in_family=="Roomer or boarder" ~ "Nonrelatives",
                       TRUE ~ "Relatives" 
                     )]
    #this was an imperfect match - off by about 3668, and only for family households
    #adding to have what in_family_type they are
    
    #assign in_family_type to ones that are known but not yet labeled (only kids have it now)
    #less than 2k outliers, but fix out of OCD
    
    hh_relations_eth[role_in_family=="Spouse",c("in_family_type"):="In married-couple family"] 
    hh_relations_race[role_in_family=="Spouse",c("in_family_type"):="In married-couple family"]
#need to remix unmarried partners and parents on age_range and sex
    #    hh_relations_eth[role_in_family=="Unmarried partner",c("in_family_type"):="In married-couple family"]
    
#    In male householder no wife present family
#    In female householder no husband present family
    
    #put in_family_type on as many as possible by race and relative_or_non 
    #first with in_family_type, then without - have to match to get which ones to take out of household_relatives
    hh_relations_eth[,("hh_relat_id"):=
                 paste0(tract,ethnicity,family_or_non,relative_or_non,in_family_type,
                        as.character(10000000+seq.int(.N))),
               by=.(tract,ethnicity,family_or_non,relative_or_non,in_family_type)]
    household_relatives_eth_dt[,("hh_relat_id"):=
                       paste0(tract,ethnicity,family_or_non,relative_or_non,in_family_type,
                              as.character(10000000+seq.int(.N))),
                     by=.(tract,ethnicity,family_or_non,relative_or_non,in_family_type)]
    hh_relations_race[,("hh_relat_id"):=
                       paste0(tract,race,family_or_non,relative_or_non,in_family_type,
                              as.character(10000000+seq.int(.N))),
                     by=.(tract,race,family_or_non,relative_or_non,in_family_type)]
    household_relatives_race_dt[,("hh_relat_id"):=
                                 paste0(tract,race,family_or_non,relative_or_non,in_family_type,
                                        as.character(10000000+seq.int(.N))),
                               by=.(tract,race,family_or_non,relative_or_non,in_family_type)]
    hh_relations_race[,("match_in_family_type"):=
                        household_relatives_race_dt[.SD,list(in_family_type),
                                                    on=.(hh_relat_id)]]
    hh_relations_eth[,("match_in_family_type"):=
                       household_relatives_eth_dt[.SD,list(in_family_type),
                                                on=.(hh_relat_id)]]
    household_relatives_race_dt[,("missing_ift"):=
                       hh_relations_race[.SD,list(in_family_type),
                                                   on=.(hh_relat_id)]]
    household_relatives_eth_dt[,("missing_ift"):=
                       hh_relations_eth[.SD,list(in_family_type),
                                                  on=.(hh_relat_id)]]
    #match for those without in_family_type
    hh_relations_eth[is.na(in_family_type),("hh_relat2_id"):=
                       paste0(tract,ethnicity,family_or_non,relative_or_non,
                              as.character(10000000+seq.int(.N))),
                     by=.(tract,ethnicity,family_or_non,relative_or_non)]
    household_relatives_eth_dt[is.na(missing_ift),("hh_relat2_id"):=
                                 paste0(tract,ethnicity,family_or_non,relative_or_non,
                                        as.character(10000000+seq.int(.N))),
                               by=.(tract,ethnicity,family_or_non,relative_or_non)]
    hh_relations_race[is.na(in_family_type),("hh_relat2_id"):=
                        paste0(tract,race,family_or_non,relative_or_non,
                               as.character(10000000+seq.int(.N))),
                      by=.(tract,race,family_or_non,relative_or_non)]
    household_relatives_race_dt[is.na(missing_ift),("hh_relat2_id"):=
                                  paste0(tract,race,family_or_non,relative_or_non,
                                         as.character(10000000+seq.int(.N))),
                                by=.(tract,race,family_or_non,relative_or_non)]
    hh_relations_race[is.na(in_family_type),("in_family_type"):=
                        household_relatives_race_dt[.SD,list(in_family_type),
                                                    on=.(hh_relat2_id)]]
    hh_relations_eth[is.na(in_family_type),("in_family_type"):=
                       household_relatives_eth_dt[.SD,list(in_family_type),
                                                  on=.(hh_relat2_id)]]
    household_relatives_race_dt[is.na(missing_ift),("missing_ift"):=
                                  hh_relations_race[.SD,list(in_family_type),
                                                    on=.(hh_relat2_id)]]
    household_relatives_eth_dt[is.na(missing_ift),("missing_ift"):=
                                 hh_relations_eth[.SD,list(in_family_type),
                                                  on=.(hh_relat2_id)]]
  
    #collect stragglers
    hh_relations_eth[is.na(in_family_type),("hh_relat3_id"):=
                       paste0(tract,ethnicity,family_or_non,
                              as.character(10000000+seq.int(.N))),
                     by=.(tract,ethnicity,family_or_non)]
    household_relatives_eth_dt[is.na(missing_ift),("hh_relat3_id"):=
                                 paste0(tract,ethnicity,family_or_non,
                                        as.character(10000000+seq.int(.N))),
                               by=.(tract,ethnicity,family_or_non)]
    hh_relations_race[is.na(in_family_type),("hh_relat3_id"):=
                        paste0(tract,race,family_or_non,
                               as.character(10000000+seq.int(.N))),
                      by=.(tract,race,family_or_non)]
    household_relatives_race_dt[is.na(missing_ift),("hh_relat3_id"):=
                                  paste0(tract,race,family_or_non,
                                         as.character(10000000+seq.int(.N))),
                                by=.(tract,race,family_or_non)]
    hh_relations_race[is.na(in_family_type),("in_family_type"):=
                        household_relatives_race_dt[.SD,list(in_family_type),
                                                    on=.(hh_relat3_id)]]
    hh_relations_eth[is.na(in_family_type),("in_family_type"):=
                       household_relatives_eth_dt[.SD,list(in_family_type),
                                                  on=.(hh_relat3_id)]]
    household_relatives_race_dt[is.na(missing_ift),("missing_ift"):=
                                  hh_relations_race[.SD,list(in_family_type),
                                                    on=.(hh_relat3_id)]]
    household_relatives_eth_dt[is.na(missing_ift),("missing_ift"):=
                                 hh_relations_eth[.SD,list(in_family_type),
                                                  on=.(hh_relat3_id)]]
    
    
    #align date_entered with language - this will break using it for other years, so need to check when using elsewhere
    hh_relations_eth[order(match(ethnicity,c("H","_","I")),
                           match(fb_date_entered,c("Entered before 1990","Entered 1990 to 1999",
                                                   "Entered 2000 to 2009","Entered 2010 or later"))),
                    ("pbl_id"):= 
                      paste0(tract,place_born,
                             as.character(1000000+seq.int(1:.N))),
                    by=.(tract,place_born)]
    hh_relations_race[order(match(race,c("A","D","C","B","G","F","E")),
      match(fb_date_entered,c("Entered before 1990","Entered 1990 to 1999",
                                                   "Entered 2000 to 2009","Entered 2010 or later"))),
                     ("pbl_id"):= 
                       paste0(tract,place_born,
                              as.character(1000000+seq.int(1:.N))),
                     by=.(tract,place_born)]
    place_born_language_dt[order(English_proficiency),("pbl_id"):= 
                            paste0(tract,place_born,
                                   as.character(1000000+seq.int(1:.N))),
                          by=.(tract,place_born)]
    hh_relations_eth[,c("fb_language_at_home","English_proficiency"):=
                      place_born_language_dt[.SD, c(list(language_at_home),list(English_proficiency)), 
                                            on = .(pbl_id)]]
    hh_relations_race[,c("fb_language_at_home","English_proficiency"):=
                       place_born_language_dt[.SD, c(list(language_at_home),list(English_proficiency)), 
                                              on = .(pbl_id)]]
    #test 11
    #test <- table(place_born_language_dt$tract,
    #              place_born_language_dt$English_proficiency,
    #              place_born_language_dt$language_at_home)==
    #  table(hh_relations_race$tract,
    #        hh_relations_race$English_proficiency,
    #        hh_relations_race$fb_language_at_home)
    #length(test[test==F])==0
    #test <- table(place_born_language_dt$tract,
    #              place_born_language_dt$English_proficiency,
    #              place_born_language_dt$language_at_home)==
    #  table(hh_relations_eth$tract,
    #        hh_relations_eth$English_proficiency,
    #        hh_relations_eth$fb_language_at_home)
    #length(test[test==F])==0
    #there are some surprising things in the original data, like 140051 born here who speak English less than very well
    #I copied it over - perhaps should tie to ethnicity and language? have to see how much the tract differences help!!!
    
    #only moving race and eth together on things that have 100% match - not solving the simultaneous equations completely, although table does
    hh_relations_eth[,
               ("race_eth_id"):= 
                 paste0(tract,sex,age_range,
                        marital_status_5,pregnant,place_born,fb_origin_place,fb_origin_area,
                        group_or_hh,family_or_non,role_in_family,
                        as.character(1000000+sample(1:.N))),
               by=.(tract,sex,age_range,
                    marital_status_5,pregnant,place_born,fb_origin_place,fb_origin_area,
                    group_or_hh,family_or_non,role_in_family)]
    hh_relations_race[,
                ("race_eth_id"):= 
                  paste0(tract,sex,age_range,
                         marital_status_5,pregnant,place_born,fb_origin_place,fb_origin_area,
                         group_or_hh,family_or_non,role_in_family,
                         as.character(1000000+sample(1:.N))),
                by=.(tract,sex,age_range,
                     marital_status_5,pregnant,place_born,fb_origin_place,fb_origin_area,
                     group_or_hh,family_or_non,role_in_family)]
    hh_relations_race[,c("ethnicity","ind_id_eth"):=
                        hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth)),
                             on = .(race_eth_id)]]
    hh_relations_eth[,c("race","individual_id"):=
                 hh_relations_race[.SD,c(list(race),list(individual_id)),on=.(race_eth_id)]]
    #nrow(hh_relations_race[is.na(ethnicity)])==nrow(hh_relations_eth[is.na(race)])
    #nrow(hh_relations_race[is.na(ethnicity)])/nrow(hh_relations_race) ~19%  #old way had about half match
    
    hh_relations_eth[is.na(individual_id),
                     ("race_eth1_id"):= 
                       paste0(tract,sex,age_range,
                              marital_status_5,#pregnant,place_born,fb_origin_place,fb_origin_area,
                              group_or_hh,family_or_non,#role_in_family,
                              as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,age_range,
                          marital_status_5,#pregnant,place_born,fb_origin_place,fb_origin_area,
                          group_or_hh,family_or_non)]
    hh_relations_race[is.na(ind_id_eth),
                      ("race_eth1_id"):= 
                        paste0(tract,sex,age_range,
                               marital_status_5,#pregnant,place_born,fb_origin_place,fb_origin_area,
                               group_or_hh,family_or_non,#role_in_family,
                               as.character(1000000+sample(1:.N))),
                      by=.(tract,sex,age_range,
                           marital_status_5,#pregnant,place_born,fb_origin_place,fb_origin_area,
                           group_or_hh,family_or_non)]
    hh_relations_race[is.na(ind_id_eth),c("ethnicity","ind_id_eth"):=
                        hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth)),
                                         on = .(race_eth1_id)]]
    hh_relations_eth[is.na(individual_id),c("race","individual_id"):=
                       hh_relations_race[.SD,c(list(race),list(individual_id)),on=.(race_eth1_id)]]
    #nrow(hh_relations_race[is.na(ethnicity)])==nrow(hh_relations_eth[is.na(race)])
    #nrow(hh_relations_race[is.na(ethnicity)])==0
    #nrow(hh_relations_eth[is.na(race)])==0
    
    saveRDS(hh_relations_eth,file = paste0(housingdir, vintage, "/hh_relations_eth_",Sys.Date(),".RDS"))
    saveRDS(hh_relations_race,file = paste0(housingdir, vintage, "/hh_relations_race_",Sys.Date(),".RDS"))
  }
}


