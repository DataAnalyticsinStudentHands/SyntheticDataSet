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
    
    #start by putting place_born on marital_status to match later
    place_born_marital_dt[,("marital_status_1"):=if_else(str_detect(marital_status,"eparated"),"Now married",marital_status)]
    marital_status_age_dt[,("marriedpb_id1"):=paste0(tract,marital_status,
                                                   as.character(1000000+sample(1:.N))),
                          by=.(tract,marital_status)]
    place_born_marital_dt[,("marriedpb_id1"):=paste0(tract,marital_status_1,
                                          as.character(1000000+sample(1:.N))),
                 by=.(tract,marital_status_1)]
    marital_status_age_dt[,c("place_born","marital_status"):=  #because place_born has 5 categories for marital_status
                            place_born_marital_dt[.SD, c(list(place_born),list(marital_status)), on = .(marriedpb_id1)]]
    #looks like some of hh_relations drops off a few of the place_borns - so this can help re-establish, too
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
    marital_status_age_dt[,c("ethnicity","ind_id_eth"):=
                            sex_by_age_eth[.SD, c(list(ethnicity),list(ind_id_eth)), on = .(married_id1)]]
    sex_age_race$married_id1 <- NULL
    sex_by_age_eth$married_id1 <- NULL
    #test2b
    #test <- table(sex_age_race[age>14]$tract,sex_age_race[age>14]$sex,sex_age_race[age>14]$age_range,sex_age_race[age>14]$race)==
    #  table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range,marital_status_age_dt$race)
    #test <- table(sex_by_age_eth[age>14]$tract,sex_by_age_eth[age>14]$sex,sex_by_age_eth[age>14]$age_range,sex_by_age_eth[age>14]$ethnicity)==
    #   table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range,marital_status_age_dt$ethnicity)
    #length(test[test==F])==0
    
    #put an age on marital_status_race/eth
    marital_status_race_dt[,("married_id2"):=
                             paste0(tract,marital_status,sex,race,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,marital_status,sex,race)]
    marital_status_age_dt[,("married_id2"):=
                            paste0(tract,marital_status_4,sex,race,
                                   as.character(1000000+sample(.N))),
                          by=.(tract,marital_status_4,sex,race)]
    marital_status_race_dt[,c("age_range","spouse_present","separated","place_born",
                              "marital_status_5","individual_id"):= #spouse_present and separated exist but aren't on it correctly
                             marital_status_age_dt[.SD, c(list(age_range),list(spouse_present),list(separated),
                                                          list(place_born),list(marital_status),list(individual_id)), 
                                                   on = .(married_id2)]]
    #
    anti_msr <- as.data.table(anti_join(marital_status_race_dt,marital_status_age_dt,by="married_id2"))
    #nrow(anti_msr)/nrow(marital_status_race_dt) = 0.07409486
    anti_msa <- as.data.table(anti_join(marital_status_age_dt,marital_status_race_dt,by="married_id2"))
    anti_msa[,("married2_id"):=  #letting it vary on marital status, so we can do that match
               paste0(tract,sex,race,
                      as.character(1000000+sample(.N))),
             by=.(tract,sex,race)]
    anti_msr[,("married2_id"):=  
                paste0(tract,sex,race,
                       as.character(1000000+sample(.N))),
              by=.(tract,sex,race)]
    anti_msr[,("marital_status"):=
               anti_msa[.SD,list(marital_status_4),on=.(married2_id)]]
    anti_msr[,("married_id2b"):=
               paste0(tract,marital_status,sex,race,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status,sex,race)]
    anti_msa[,("married_id2b"):=
               paste0(tract,marital_status_4,sex,race,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status_4,sex,race)]
    anti_msr[,c("age_range","spouse_present","separated","place_born","marital_status","marital_status_5","individual_id"):= 
               anti_msa[.SD, c(list(age_range),list(spouse_present),list(separated),
                               list(place_born),list(marital_status_4),list(marital_status),list(individual_id)), 
                         on = .(married_id2b)]]
    marital_status_race_dt[is.na(age_range),c("age_range","spouse_present","separated","place_born","marital_status",
                                              "marital_status_5","individual_id"):= 
                             anti_msr[.SD, c(list(age_range),list(spouse_present),list(separated),
                                             list(place_born),list(marital_status),list(marital_status_5),list(individual_id)), 
                                      on = .(married_id2)]]
    #separate line developed for ethnicity
    marital_status_eth_dt[,("married_id3"):=
                             paste0(tract,marital_status,sex,ethnicity,
                                                   as.character(1000000+sample(.N))),
                           by=.(tract,marital_status,sex,ethnicity)]
    marital_status_age_dt[,("married_id3"):=
                            paste0(tract,marital_status_4,sex,ethnicity,
                                   as.character(1000000+sample(.N))),
                          by=.(tract,marital_status_4,sex,ethnicity)]
    marital_status_eth_dt[,c("age_range","spouse_present","separated","place_born","marital_status_5","ind_id_eth"):= #spouse_present and separated exist but aren't on it correctly
                            marital_status_age_dt[.SD, c(list(age_range),list(spouse_present),list(separated),
                                                         list(place_born),list(marital_status),list(ind_id_eth)), 
                                                  on = .(married_id3)]]
    anti_mse <- as.data.table(anti_join(marital_status_eth_dt,marital_status_age_dt,by="married_id3"))
    anti_msa2 <- as.data.table(anti_join(marital_status_age_dt,marital_status_eth_dt,by="married_id3"))
    anti_msa2[,("married3_id"):=  #letting it vary on marital status, so we can do that match
               paste0(tract,sex,ethnicity,
                      as.character(1000000+sample(.N))),
             by=.(tract,sex,ethnicity)]
    anti_mse[,("married3_id"):=  
               paste0(tract,sex,ethnicity,
                      as.character(1000000+sample(.N))),
             by=.(tract,sex,ethnicity)]
    anti_mse[,("marital_status"):=
               anti_msa2[.SD,list(marital_status_4),on=.(married3_id)]]
    anti_mse[,("married_id3b"):=
               paste0(tract,marital_status,sex,ethnicity,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status,sex,ethnicity)]
    anti_msa2[,("married_id3b"):=
               paste0(tract,marital_status_4,sex,ethnicity,
                      as.character(1000000+sample(.N))),
             by=.(tract,marital_status_4,sex,ethnicity)]
    anti_mse[,c("age_range","spouse_present","separated","place_born","marital_status","marital_status_5","ind_id_eth"):= 
               anti_msa2[.SD, c(list(age_range),list(spouse_present),list(separated),list(place_born),
                                list(marital_status_4),list(marital_status),list(ind_id_eth)), 
                        on = .(married_id3b)]]
    marital_status_eth_dt[is.na(age_range),c("age_range","spouse_present","separated","place_born","marital_status",
                                             "marital_status_5","ind_id_eth"):= 
                             anti_mse[.SD, c(list(age_range),list(spouse_present),list(separated),
                                             list(place_born),list(marital_status),list(marital_status_5),list(ind_id_eth)), 
                                      on = .(married_id3)]]
    
    
    #hypothesis is that letting it vary freely will allow for the matching to fall out in the match back; 
    #there are two competing strategies; one would be to take the same matching algorithm and run it on the subset (anti_msr, etc.), but
    #with a new sample - and just run it until it all matched.
    #the second is to match the sample back to the original and then back to the target again.
    #the married_id2b step unties the remainder, and then lets an age be written that comes from the marital_age 
    
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
    preg_race_dt[,c("preg_age_range"):=
                   preg_age_dt[.SD, list(preg_age_range), on = .(preg_id)]]
    preg_eth_dt[,c("preg_age_range"):=
                  preg_age_dt[.SD, list(preg_age_range), on = .(preg_id)]]
    preg_race_dt[,("preg_id"):=NULL]
    preg_eth_dt[,("preg_id"):=NULL]
    
    #put pregnant on marital status on race/eth, sex, and ordered by ascending age
    marital_status_race_dt[order(age_range),
                           ("married_preg_id"):=paste0(tract,race,sex,if_else(str_detect(marital_status,"Now"),"Now","Not_now"),
                                                       as.character(1000000+seq.int(1:.N))),
                           by=.(tract,race,sex,str_detect(marital_status,"Now"))]
    preg_race_dt[order(preg_age_range),
                 ("married_preg_id"):=paste0(tract,race,sex,if_else(str_detect(married,"Now"),"Now","Not_now"),
                                             as.character(1000000+seq.int(1:.N))),
                 by=.(tract,race,sex,str_detect(married,"Now"))]
    marital_status_race_dt[,c("pregnant"):=
                             preg_race_dt[.SD, list(birth_label), on = .(married_preg_id)]]
    
    marital_status_eth_dt[order(age_range),
                          ("married_preg_id"):=paste0(tract,ethnicity,sex,if_else(str_detect(marital_status,"Now"),"Now","Not_now"),
                                                      as.character(1000000+seq.int(1:.N))),
                          by=.(tract,ethnicity,sex,str_detect(marital_status,"Now"))]
    preg_eth_dt[order(preg_age_range),
                ("married_preg_id"):=paste0(tract,ethnicity,sex,if_else(str_detect(married,"Now"),"Now","Not_now"),
                                            as.character(1000000+seq.int(1:.N))),
                by=.(tract,ethnicity,sex,str_detect(married,"Now"))]
    marital_status_eth_dt[,c("pregnant"):=
                            preg_eth_dt[.SD, list(birth_label), on = .(married_preg_id)]]
    #saveRDS(marital_status_eth_dt,file = paste0(housingdir, vintage, "/marital_status_eth_dt_",Sys.Date(),".RDS"))
    #saveRDS(marital_status_race_dt,file = paste0(housingdir, vintage, "/marital_status_race_dt_",Sys.Date(),".RDS"))
    #test2e
    #test <- nrow(marital_status_eth_dt[!is.na(pregnant)]) == nrow(preg_eth_dt)
    #test <- nrow(marital_status_race_dt[!is.na(pregnant)]) == nrow(preg_race_dt)
    rm(list=ls(pattern="^preg"))
    
    #put an age/race on seniors, to recalibrate at end and to store references to group_quarters
    #test <- table(sr_relations$tract)==table(sex_age_race[age>64]$tract) #sex is only for married couples
    sr_relations[group_or_hh=="In households",("race_sr_id"):=
                   paste0(tract,sex,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex)]
    sex_age_race[age>64,("race_sr_id"):=
                   paste0(tract,sex,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex)]
    sr_relations[group_or_hh=="In households",c("race","age_range","individual_id"):=
                   sex_age_race[.SD, c(list(race),list(age_range),list(individual_id)), on = .(race_sr_id)]]
    sr_relations[group_or_hh=="In households",("eth_sr_id"):=
                   paste0(tract,sex,age_range,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex,age_range)]
    sex_by_age_eth[age>64,("eth_sr_id"):=
                   paste0(tract,sex,age_range,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex,age_range)]
    sr_relations[group_or_hh=="In households",c("ethnicity","ind_id_eth"):=  #does not match what final ethnicity will be v. race
                   sex_by_age_eth[.SD, c(list(ethnicity),list(ind_id_eth)), on = .(eth_sr_id)]]
    sex_age_race[age>64,("missing_race"):=
                   sr_relations[.SD, c(list(race)), on = .(race_sr_id)]] 
    sex_by_age_eth[age>64,("missing_eth"):=
                   sr_relations[.SD, c(list(ethnicity)), on = .(eth_sr_id)]]
    #then rest (that didn't have sex listed) - with less than 75 first in households, then hh and gq for rest
    sr_relations[group_or_hh=="In households" & is.na(race),("race_sr2_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_age_race[age>64 & age<75 & is.na(missing_race),("race_sr2_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sr_relations[group_or_hh=="In households" & is.na(race),c("race","sex","age_range","individual_id"):=
                   sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), 
                                on = .(race_sr2_id)]]
    
    sr_relations[group_or_hh=="In households" & is.na(ethnicity),("eth_sr2_id"):=
                   paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                 by=.(tract,sex,age_range)]
    
    sex_by_age_eth[age>64 & age<75 & is.na(missing_eth),("eth_sr2_id"):=
                     paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                   by=.(tract,sex,age_range)]
    sr_relations[group_or_hh=="In households" & is.na(ethnicity),c("ethnicity","sex","age_range","ind_id_eth"):=
                   sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), 
                                  on = .(eth_sr2_id)]]
    sex_age_race[age>64 & age<75 & is.na(missing_race),c("missing_race","gq_race_wrk"):=  #all in households now
                   sr_relations[.SD, c(list(race),list(group_or_hh)), on = .(race_sr2_id)]]
    sex_by_age_eth[age>64 & age<75 & is.na(missing_eth),c("missing_eth","gq_eth_wrk"):=
                     sr_relations[.SD, c(list(ethnicity),list(group_or_hh)), on = .(eth_sr2_id)]]
    #for gq and hh all ages
    sr_relations[is.na(race),("race_sr3_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_age_race[age>64 & is.na(missing_race),("race_sr3_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sr_relations[is.na(race),c("race","sex","age_range","individual_id"):=
                   sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), 
                                on = .(race_sr3_id)]]
    
    sr_relations[is.na(ethnicity),("eth_sr3_id"):=
                   paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                 by=.(tract,sex,age_range)]
    
    sex_by_age_eth[age>64 & is.na(missing_eth),("eth_sr3_id"):=
                     paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                   by=.(tract,sex,age_range)]
    #sr_relations[,("race_sex_relations"):=sex_sr_relations]
    sr_relations[is.na(ethnicity),c("ethnicity","sex","age_range","ind_id_eth"):=
                   sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), 
                                  on = .(eth_sr3_id)]]
    sex_age_race[age>64 & is.na(missing_race),c("missing_race","gq_race_wrk"):=
                   sr_relations[.SD, c(list(race),list(group_or_hh)), on = .(race_sr3_id)]]
    sex_age_race[age>64 & gq_race_wrk=="In group quarters",("gq_race"):=gq_race_wrk]
    #sex_age_race[age>64 & is.na(missing_race),("gq_race"):=list("In group quarters")]
    sex_by_age_eth[age>64 & is.na(missing_eth),c("missing_eth","gq_eth_wrk"):=
                     sr_relations[.SD, c(list(ethnicity),list(group_or_hh)), on = .(eth_sr3_id)]]
    sex_by_age_eth[age>64 & gq_eth_wrk=="In group quarters",("gq_eth"):=gq_eth_wrk]
    #sex_by_age_eth[age>64 & is.na(missing_eth),("gq_eth"):=list("In group quarters")]
    #should test to see if they match the ones that are listed as gq in seniors
    sex_age_race$race_sr_id <-NULL
    sex_age_race$race_sr2_id <-NULL
    sex_age_race$race_sr3_id <-NULL
    sex_age_race$gq_race_wrk <-NULL
    sex_by_age_eth$eth_sr_id <-NULL
    sex_by_age_eth$eth_sr2_id <-NULL
    sex_by_age_eth$eth_sr3_id <-NULL
    sex_by_age_eth$gq_eth_wrk <-NULL
    #test3
    #test<-table(sex_age_race[age>64]$tract,sex_age_race[age>64]$sex,sex_age_race[age>64]$race,sex_age_race[age>64]$age_range)==
    #  table(sr_relations$tract,sr_relations$sex,sr_relations$race,sr_relations$age_range)
    #length(test[test==F])==0
    #test<-table(sex_by_age_eth[age>64]$tract,sex_by_age_eth[age>64]$sex,sex_by_age_eth[age>64]$ethnicity,sex_by_age_eth[age>64]$age_range)==
    #  table(sr_relations$tract,sr_relations$sex,sr_relations$ethnicity,sr_relations$age_range)
    #length(test[test==F])==0
    #nrow(sex_age_race[!is.na(missing_race)])==nrow(sr_relations)
    #nrow(sex_age_race[!is.na(missing_race)])==nrow(sex_age_race[age>64])
    
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
    hh_relations_dt[group_or_hh=="In households",c("age_range_3","sex","ethnicity","age_range",
                       "race","individual_id","ind_id_eth"):=
                      sr_relations[.SD, c(list(age_range_3),list(sex),
                                          list(ethnicity),list(age_range),
                                          list(race),
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
                    c("age_range_3","ethnicity","age_range","sex",
                                         "race","individual_id","ind_id_eth"):=
                      anti_sr_rel[.SD, c(list(age_range_3),
                                         list(ethnicity),list(age_range),
                                         list(sex),list(race),
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
                    c("age_range_3","ethnicity","age_range","sex",
                                         "race","individual_id","ind_id_eth"):=
                      anti_sr2_rel[.SD, c(list(age_range_3),
                                         list(ethnicity),list(age_range),
                                         list(sex),list(race),
                                         list(individual_id),list(ind_id_eth)), 
                                  on = .(rel_sr4_id)]]
    anti_sr3_rel <- as.data.table(anti_join(anti_sr2_rel,hh_relations_dt,by="rel_sr4_id"))
    anti_sr3_rel[group_or_hh=="In households",("relative"):="Parent-in-law"]
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
                    c("age_range_3","ethnicity","age_range","sex",
                                         "race","individual_id","ind_id_eth"):=
                      anti_sr3_rel[.SD, c(list(age_range_3),
                                          list(ethnicity),list(age_range),
                                          list(sex),list(race),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr5_id)]]
    anti_sr4_rel <- as.data.table(anti_join(anti_sr3_rel,hh_relations_dt,by="rel_sr5_id"))
    anti_sr4_rel[group_or_hh=="In households",("relative"):="Nonrelatives"]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    ("rel_sr6_id"):=
                      paste0(tract,relative_sr,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relative_sr)]
    anti_sr4_rel[group_or_hh=="In households",("rel_sr6_id"):=
                   paste0(tract,relative,
                          as.character(1000000+sample(.N))),
                 by=.(tract,relative)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("age_range_3","ethnicity","age_range","sex",
                                         "race","individual_id","ind_id_eth"):=
                      anti_sr4_rel[.SD, c(list(age_range_3),
                                          list(ethnicity),list(age_range),
                                          list(sex),list(race),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr6_id)]]
    #OCD Dan catches last 6 missing!!
    anti_sr5_rel <- as.data.table(anti_join(anti_sr4_rel,hh_relations_dt,by="rel_sr6_id"))
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
                    c("age_range_3","ethnicity","age_range","sex",
                      "race","individual_id","ind_id_eth"):=
                      anti_sr5_rel[.SD, c(list(age_range_3),
                                          list(ethnicity),list(age_range),
                                          list(sex),list(race),
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
                    c("age_range_3","ethnicity","age_range","sex",
                      "race","individual_id","ind_id_eth"):=
                      sr_relations[.SD, c(list(age_range_3),
                                          list(ethnicity),list(age_range),
                                          list(sex),list(race),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr7_id)]]
    #test no dups:
    nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))
    #4 tracts from sr_relations not in hh_relations
    #test3a - need to work this out - not working now
    #test<-table(hh_relations_dt[!is.na(age_range)]$tract,
    #            hh_relations_dt[!is.na(age_range)]$race,
    #            hh_relations_dt[!is.na(age_range)]$sex,
    #            #hh_relations_dt[!is.na(age_range)]$living_alone, #one missing???
    #            hh_relations_dt[!is.na(age_range)]$age_range
    #        )==
    #  table(sr_relations$tract,
    #        sr_relations$race,
    #        sr_relations$sex,
    #        #sr_relations$living_alone,
    #        sr_relations$age_range
    #        )
    #length(test[test==F])==0
    rm(sr_relations)
    rm(list=ls(pattern="^anti"))
    hh_relations_dt[,c("rel_sr3_id","rel_sr4_id","rel_sr5_id","rel_sr6_id","rel_sr7_id","rel_sr6a_id","role_id","relative_sr","rel_sr_id"):=NULL]
    
    #now do adults - giving all a possible race and age - just by sample within each tract
    adults_relations[age_range_3=="18 to 34 years",("race_adults_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_age_race[age>17&age<35,("race_adults_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    adults_relations[age_range_3=="18 to 34 years",c("race","sex","age_range","individual_id"):=
                       sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), on = .(race_adults_id)]]
    adults_relations[age_range_3=="18 to 34 years",("eth_adults_id"):=
                       paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                     by=.(tract,sex,age_range)]
    sex_by_age_eth[age>17&age<35,("eth_adults_id"):=
                     paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                   by=.(tract,sex,age_range)]
    #add ethnicity, but not going to line up with final - divide out below
    adults_relations[age_range_3=="18 to 34 years",c("ethnicity","ind_id_eth"):=
                       sex_by_age_eth[.SD, c(list(ethnicity),list(ind_id_eth)), on = .(eth_adults_id)]]
    #nrow(adults_relations[!is.na(ethnicity)])==nrow(adults_relations[!is.na(race)])
    #nrow(adults_relations[!is.na(race)])==nrow(adults_relations[age_range_3=="18 to 34 years"])
    #nrow(sex_age_race[age>17&age<35])-nrow(adults_relations[age_range_3=="18 to 34 years"]) #21231, presumably in GQ
    sex_age_race[age>17&age<35,("missing_race"):=
                   adults_relations[.SD, list(race), on = .(race_adults_id)]]
    sex_age_race[age>17&age<35&is.na(missing_race),("gq_race"):=list("In group quarters")]
    sex_by_age_eth[age>17&age<35,("missing_eth"):=
                     adults_relations[.SD, list(ethnicity), on = .(eth_adults_id)]]
    sex_by_age_eth[age>17&age<35&is.na(missing_eth),("gq_eth"):=list("In group quarters")]
    sex_age_race$race_adults_id <-NULL
    sex_by_age_eth$eth_adults_id <-NULL
    #now do 35 to 64
    adults_relations[age_range_3=="35 to 64 years",("race2_adults_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    sex_age_race[age>34&age<65,("race2_adults_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    adults_relations[age_range_3=="35 to 64 years",c("race","sex","age_range","individual_id"):=
                       sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), 
                                    on = .(race2_adults_id)]]
    adults_relations[age_range_3=="35 to 64 years",("eth2_adults_id"):=
                       paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                     by=.(tract,sex,age_range)]
    sex_by_age_eth[age>34&age<65,("eth2_adults_id"):=
                     paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                   by=.(tract,sex,age_range)]
    #add ethnicity, but not going to line up with final - divide out below
    adults_relations[age_range_3=="35 to 64 years",c("ethnicity","ind_id_eth"):=
                       sex_by_age_eth[.SD, c(list(ethnicity),list(ind_id_eth)), 
                                      on = .(eth2_adults_id)]]
    #nrow(adults_relations[!is.na(ethnicity)])==nrow(adults_relations[!is.na(race)])
    #nrow(adults_relations[!is.na(race)])==nrow(adults_relations[age_range_3=="18 to 34 years" | age_range_3=="35 to 64 years"])
    #nrow(sex_age_race[age>34&age<65])-nrow(adults_relations[age_range_3=="35 to 64 years"]) #11184, presumably in GQ
    sex_age_race[age>34&age<65&is.na(missing_race),("missing_race"):=
                   adults_relations[.SD, list(race), on = .(race2_adults_id)]]
    sex_age_race[age>34&age<65&is.na(missing_race),("gq_race"):=list("In group quarters")]
    sex_by_age_eth[age>34&age<65&is.na(missing_eth),("missing_eth"):=
                     adults_relations[.SD, list(ethnicity), on = .(eth2_adults_id)]]
    sex_by_age_eth[age>34&age<65&is.na(missing_eth),("gq_eth"):=list("In group quarters")]
    sex_age_race$race2_adults_id <-NULL
    sex_by_age_eth$eth2_adults_id <-NULL
    #test3b
    #test<-table(sex_age_race[age>17&age<65&is.na(gq_race)]$tract,
    #            sex_age_race[age>17&age<65&is.na(gq_race)]$sex,
    #            sex_age_race[age>17&age<65&is.na(gq_race)]$age_range,
    #            sex_age_race[age>17&age<65&is.na(gq_race)]$race)==
    #  table(adults_relations[age_range_3!="65 years and over"]$tract,
    #        adults_relations[age_range_3!="65 years and over"]$sex,
    #        adults_relations[age_range_3!="65 years and over"]$age_range,
    #        adults_relations[age_range_3!="65 years and over"]$race)
    #length(test[test==F])==0
    #test<-table(sex_by_age_eth[age>17&age<65&is.na(gq_eth)]$tract,
    #            sex_by_age_eth[age>17&age<65&is.na(gq_eth)]$sex,
    #            sex_by_age_eth[age>17&age<65&is.na(gq_eth)]$age_range,
    #            sex_by_age_eth[age>17&age<65&is.na(gq_eth)]$ethnicity)==
    #  table(adults_relations[age_range_3!="65 years and over"]$tract,
    #        adults_relations[age_range_3!="65 years and over"]$sex,
    #        adults_relations[age_range_3!="65 years and over"]$age_range,
    #        adults_relations[age_range_3!="65 years and over"]$ethnicity)
    #length(test[test==F])==0
    
    hh_relations_dt[group_or_hh=="In households",("relation_hh"):=case_when(
      relative=="Child" | relative=="Grandchild" ~ "Child of householder", #but only for adults; Foster child is non-relative
      #relative=="Spouse" ~ "Householder living with spouse or spouse of householder",
      #role_in_family=="Unmarried partner" ~ "Householder living with unmarried partner or unmarried partner of householder",
      relative=="Nonrelatives" | relative=="Parent-in-law" ~ "Other nonrelatives",
      relative=="Other relatives" | relative =="Parent" | relative=="Brother or sister" |
        relative=="Son-in-law or daughter-in-law"~ "Other relatives",
      TRUE ~ "Householder" 
    )]
    adults_relations[relation_hh=="Lives alone",("relation_hh"):="Householder"] #still have hh not living alone
    adults_relations[,("relation_hh_hh"):=case_when(
      relation_hh=="Householder living with unmarried partner or unmarried partner of householder" |
        relation_hh=="Householder living with spouse or spouse of householder" ~ "Householder",
      TRUE ~ relation_hh
    )]
    #matching hh_relations and adults_relations at highest level, because there's not much to go on and it's not exact
    #sex is only known for Householders
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    ("rel_adults_id"):=
                      paste0(tract,relation_hh,sex,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relation_hh,sex)]
    adults_relations[age_range_3!="65 years and over",
                     ("rel_adults_id"):=
                       paste0(tract,relation_hh_hh,sex,
                              as.character(1000000+sample(.N))),
                     by=.(tract,relation_hh_hh,sex)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative!="Grandchild",
                    c("age_range_3","ethnicity","age_range",
                      "race","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(age_range_3),
                                              list(ethnicity),list(age_range),
                                              list(race),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults_id)]]
    adults_relations[age_range_3!="65 years and over",
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults_id)]]
    #this should give you all the hh, 
    #using relative=="Householder" not relation_hh=="Householder" because it includes Spouse, etc.
    #looks like 15908 hh under 18?
    #do for rest of relatives
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
                    c("age_range_3","sex","ethnicity","age_range",
                      "race","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(age_range_3),list(sex),
                                              list(ethnicity),list(age_range),
                                              list(race),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults2_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults2_id)]]
    #test no dups:
    nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))
    #clean up on non-relatives vs. relatives
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & !str_detect(role_in_family,"ild"), 
                    ("rel_adults3_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     ("rel_adults3_id"):=
                       paste0(tract,
                              as.character(1000000+sample(.N))),
                     by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range)  & !str_detect(role_in_family,"ild"),
                    c("age_range_3","sex","ethnicity","age_range",
                      "race","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(age_range_3),list(sex),
                                              list(ethnicity),list(age_range),
                                              list(race),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults3_id)]]
    adults_relations[age_range_3!="65 years and over"&is.na(adults_matched),
                     c("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults3_id)]]
    #test no dups:
    nrow(hh_relations_dt[!is.na(individual_id)])==length(unique(hh_relations_dt[,individual_id]))
    #nrow(hh_relations_dt[!is.na(age_range_3)])-nrow(adults_relations) # = -464 not at all sure what to do and if it's worth it... because sr_relations had gq!!!
    #clean up ~ 600 edge cases? - test again!!
    hh_relations_dt[is.na(age_range_3) & str_detect(relative,"Parent"),("role_in_family") :="Son-in-law or daughter-in-law"]
    hh_relations_dt[is.na(age_range_3) & str_detect(relative,"Parent"),("relative") :="Son-in-law or daughter-in-law"]
    #length(unique(hh_relations_dt$individual_id))-nrow(sex_age_race[age>17]) #-48638
    #rm(adults_relations)
    
    #now do kids - giving all a possible race and age - for gq, it's a small number, but could be that we should have it skew older???
    hh_type_kids[,("race_kids_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    sex_age_race[age<18,("race_kids_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_type_kids[,c("race","sex","age_range","individual_id"):=
                   sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), 
                                on = .(race_kids_id)]]
    hh_type_kids[,("eth_kids_id"):=
                   paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                 by=.(tract,sex,age_range)]
    sex_by_age_eth[age<18,("eth_kids_id"):=
                     paste0(tract,sex,age_range,as.character(1000000+sample(.N))),
                   by=.(tract,sex,age_range)]
    hh_type_kids[,c("ethnicity","sex","age_range","ind_id_eth"):=
                   sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), on = .(eth_kids_id)]]
    sex_age_race[age<18,("missing_race"):=
                   hh_type_kids[.SD, list(sex), on = .(race_kids_id)]]
    sex_age_race[age<18&is.na(missing_race)&is.na(gq_race),("gq_race"):=list("In group quarters")]
    sex_by_age_eth[age<18,("missing_eth"):=
                   hh_type_kids[.SD, list(sex), on = .(eth_kids_id)]]
    sex_by_age_eth[age<18&is.na(missing_eth)&is.na(gq_eth),("gq_eth"):=list("In group quarters")]
    #test 
    #test <- table(sex_age_race[age<18&is.na(gq_race),sex],
    #              sex_age_race[age<18&is.na(gq_race),race],
    #              sex_age_race[age<18&is.na(gq_race),age_range]
    #              )==table(hh_type_kids$sex,
    #                       hh_type_kids$race,
    #                       hh_type_kids$age_range
    #                       )
    #length(test[test==F])==0
    #test <- table(sex_by_age_eth[age<18&is.na(gq_eth),sex],
    #              sex_by_age_eth[age<18&is.na(gq_eth),ethnicity],
    #              sex_by_age_eth[age<18&is.na(gq_eth),age_range]
    #)==table(hh_type_kids$sex,
    #         hh_type_kids$ethnicity,
    #         hh_type_kids$age_range
    #)
    #length(test[test==F])==0
    
    #hh_type_kids includes grandkids, but excludes hh, spouses, partners, and is everyone under 18 otherwise - marital should give that extra - could be less than 1k
    #hh_type_kids[family_or_non=="In nonfamily households",("in_family_type"):="In nonfamily households"]
    
    hh_relations_dt[!is.na(age_range_3),("age_range_4"):=age_range_3]
    hh_type_kids[,("age_range_4"):=list("0  to 17 years")]  #seems to need list type for later stuff
    hh_type_kids[age_range=="15 to 17 years",("rel_kidsb4_id"):= 
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range_3) & relative=="Householder",
                    ("rel_kidsb4_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range_3) & relative=="Householder", #1409 match this
                    c("age_range","in_family_type","family_role_3","age_range_4",
                      "ethnicity","sex","race","individual_id","ind_id_eth"):= 
                      hh_type_kids[.SD, c(list(age_range),list(in_family_type),list(family_role_3),list(age_range_4),
                                          list(ethnicity),list(sex),list(race),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_kidsb4_id)]]
    hh_type_kids[age_range=="15 to 17 years",c("matched_17","matched_fam"):=
                   hh_relations_dt[.SD,c(list(group_or_hh),list(family_or_non)),on = .(rel_kidsb4_id)]]
    #for spouses 15-17 - when married-couple hh, the spouse is the female by census rules up to 2010, and still usually it looks like
    hh_type_kids[age_range=="15 to 17 years"&sex=="Female",("sp_kidsb4_id"):= 
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative=="Householder",
                    ("sp_kidsb4_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range) & relative=="Householder", #1409 match this
                    c("age_range","in_family_type","family_role_3","age_range_4",
                      "ethnicity","sex","race","individual_id","ind_id_eth"):= 
                      hh_type_kids[.SD, c(list(age_range),list(in_family_type),list(family_role_3),list(age_range_4),
                                          list(ethnicity),list(sex),list(race),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(sp_kidsb4_id)]]
    #test 
    #nrow(hh_relations_dt[age_range=="15 to 17 years"])==nrow(hh_relations_dt[group_or_hh=="In households" & is.na(age_range_3) & relative=="Householder"])
    hh_type_kids[age_range=="15 to 17 years"&is.na(matched_17),c("matched_17","matched_fam"):=
                   hh_relations_dt[.SD,c(list(group_or_hh),list(family_or_non)),on = .(sp_kidsb4_id)]]
    hh_type_kids[is.na(matched_17),("rel_kids_id"):= 
                   paste0(tract,family_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,family_or_non)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    ("rel_kids_id"):=
                      paste0(tract,family_or_non,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,family_or_non)]
    
    #family_role_3 is a rewrite of in_family_type and matches sam_hh categories, but isn't only for Householders
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range),
                    c("in_family_type","family_role_3","age_range_4",
                      "ethnicity","sex","age_range",
                      "race","individual_id","ind_id_eth"):= 
                             hh_type_kids[.SD, c(list(in_family_type),list(family_role_3),list(age_range_4),
                                                 list(ethnicity),list(sex),list(age_range),
                                                 list(race),list(individual_id),list(ind_id_eth)), 
                                                         on = .(rel_kids_id)]]
    #p/u last 22k
    hh_type_kids[is.na(matched_fam),c("matched_17","matched_fam"):=
                   hh_relations_dt[.SD,c(list(group_or_hh),list(family_or_non)),on = .(rel_kids_id)]]
    hh_type_kids[is.na(matched_17),("rel_kids2_id"):= 
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range_4),
                    ("rel_kids2_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range_4),
                    c("in_family_type","family_role_3","age_range_4",
                      "ethnicity","sex","age_range",
                      "race","individual_id","ind_id_eth"):= 
                      hh_type_kids[.SD, c(list(in_family_type),list(family_role_3),list(age_range_4),
                                          list(ethnicity),list(sex),list(age_range),
                                          list(race),list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_kids2_id)]]
    #p/u last 9k - get younger kids first, so that older ones are the ones in group_quarters??
    #7 listed as Spouse and 90 Nonrelatives, still - just moving them into Child
    ##hh_relations_dt[relative=="Spouse" | relative=="Nonrelatives",("relative"):="Child"]
    ###hh_type_kids[is.na(matched_fam),c("matched_17","matched_fam"):=
    ###               hh_relations_dt[.SD,c(list(group_or_hh),list(family_or_non)),on = .(rel_kids2_id)]]
    ###hh_type_kids[is.na(matched_17),("rel_kids3_id"):= 
    ###               paste0(tract,
    ###                      as.character(1000000+sample(.N))),
    ###             by=.(tract)]
    ###hh_relations_dt[group_or_hh=="In households" & is.na(age_range_4),
    ###                ("rel_kids3_id"):=
    ###                  paste0(tract,
    ###                         as.character(1000000+sample(.N))),
    ###                by=.(tract)]
    ###hh_relations_dt[group_or_hh=="In households" & is.na(age_range_4),
    ###                c("in_family_type","family_role_3","age_range_4",
    ###                  "ethnicity","sex","age_range",
    ###                  "race","individual_id","ind_id_eth"):= 
    ###                  hh_type_kids[.SD, c(list(in_family_type),list(family_role_3),list(age_range_4),
    ###                                      list(ethnicity),list(sex),list(age_range),
    ###                                      list(race),list(individual_id),list(ind_id_eth)), 
    ###                               on = .(rel_kids3_id)]]
    ###
    #DO THE TESTS ON WHAT HASN'T BEEN FILLED, AND IT SHOULD ONLY BE GROUP_QUARTERS - BELOW
    #test 4a
    #test <- table(hh_relations_dt$age_range,hh_relations_dt$age_range_4)
    #sum(colSums(test==0/nrow(test)*100))==42
    tester <- hh_relations_dt[tract%in%unique(hh_type_kids$tract) & age_range_4=="0  to 17 years"]
    test <- table(#tester$tract,
                  #tester$age_range,
                  tester$sex#,
                  #tester$ethnicity,
                  #tester$race#,
                  #tester$in_family_type
                  )==
      table(#hh_type_kids$tract,
            #hh_type_kids$age_range,
            hh_type_kids$sex#,
            #hh_type_kids$ethnicity,
            #hh_type_kids$race#,
            #hh_type_kids$in_family_type
            )
    length(test[test==F])==0
        
    #length(unique(hh_relations_dt$individual_id))-nrow(sex_age_race) #~50k (~1%) - mostly folks in group quarters, but 9k in hh
    #get missing from hh_relations, ethnicity side
    #add tract back in from sex_age_race/eth with that as final source for each
    
    #get Group quarters matches
    sex_age_race[!is.na(gq_race),("gq_race_id"):= 
                   paste0(tract,
                          as.character(1000000+sample(.N))),
                 by=.(tract)]
    hh_relations_dt[group_or_hh=="In group quarters",
                    ("gq_race_id"):=
                      paste0(tract,
                             as.character(1000000+sample(.N))),
                    by=.(tract)]
    hh_relations_dt[group_or_hh=="In group quarters",
                    c("race","sex","age_range","individual_id"):= 
                      sex_age_race[.SD, c(list(race),list(sex),list(age_range),
                                          list(individual_id)), 
                                   on = .(gq_race_id)]]
    sex_by_age_eth[!is.na(gq_eth),("gq_eth_id"):= 
                   paste0(tract,sex,age_range,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex,age_range)]
    hh_relations_dt[group_or_hh=="In group quarters",
                    ("gq_eth_id"):=
                      paste0(tract,sex,age_range,
                             as.character(1000000+sample(.N))),
                    by=.(tract,sex,age_range)]
    hh_relations_dt[group_or_hh=="In group quarters",
                    c("ethnicity","sex","age_range","individual_id"):= 
                      sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),
                                          list(individual_id)), 
                                   on = .(gq_eth_id)]]
    
    
    anti_sae_rel <- as.data.table(anti_join(sex_by_age_eth,hh_relations_dt,by="ind_id_eth"))
    #put trackers on b/c we haven't separated out race/eth yet
    hh_relations_dt[,("miss_id"):=paste0(tract,as.character(10000000+sample(.N))),by=.(tract)]
    hh_relations_dt[,("missr_id"):=paste0(tract,as.character(10000000+sample(.N))),by=.(tract)]
    hh_rel_work <- as.data.table(anti_join(hh_relations_dt,sex_by_age_eth,by="ind_id_eth"))

    #missing_eth on anti_sae is group_or_hh - close but not exact match - age_ranges aren't matching
    #just under 2% didn't match, mostly group quartered
    anti_sae_rel[,
                 ("group_left_id"):=paste0(tract,missing_eth,as.character(10000000+sample(.N))),
                 by=.(tract,missing_eth)]
    hh_rel_work[,("group_left_id"):=paste0(tract,group_or_hh,as.character(10000000+sample(.N))),
                by=.(tract,group_or_hh)]
    hh_rel_work[,c("age_range_4","ethnicity","eth_sex_relations",
                  "eth_age_range","ind_id_eth"):=
                  anti_sae_rel[.SD,c(list(age_range_4),
                                       list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), 
                                 on = .(group_left_id)]]
    anti_sae2_rel <- as.data.table(anti_join(anti_sae_rel,hh_rel_work,by="ind_id_eth"))
    hh_rel2_work <- hh_rel_work[is.na(ind_id_eth)]
    #495 left, almost all children, but a few Householders - making them the older folks
    anti_sae2_rel[order(age_range_29),("group_left2_id"):=paste0(tract,as.character(10000000+seq.int(.N))),
                  by=.(tract)]     
    hh_rel2_work[order(relative),("group_left2_id"):=paste0(tract,as.character(10000000+seq.int(.N))),
                 by=.(tract)]
    hh_rel2_work[,c("age_range_4","ethnicity","eth_sex_relations",
                  "eth_age_range","ind_id_eth"):=
                  anti_sae2_rel[.SD,c(list(age_range_4),
                                     list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), 
                               on = .(group_left2_id)]]
    hh_rel_work[is.na(ind_id_eth),
                    c("age_range_4","ethnicity","eth_sex_relations",
                      "eth_age_range","ind_id_eth"):=
                  hh_rel2_work[.SD,c(list(age_range_4),list(ethnicity),
                                     list(eth_sex_relations),list(eth_age_range),list(ind_id_eth)), 
                                    on = .(miss_id)]]
    hh_relations_dt[is.na(ind_id_eth),
                c("age_range_4","ethnicity","eth_sex_relations",
                  "eth_age_range","ind_id_eth"):=
                  hh_rel_work[.SD,c(list(age_range_4),list(ethnicity),
                                    list(eth_sex_relations),list(eth_age_range),list(ind_id_eth)), 
                               on = .(miss_id)]]
    #same, but for race
    anti_sar_rel <- as.data.table(anti_join(sex_age_race,hh_relations_dt,by="individual_id"))
    hh_relr_work <- as.data.table(anti_join(hh_relations_dt,sex_age_race,by="individual_id"))
    anti_sar_rel[,
                 ("group_left_id"):=paste0(tract,missing_race,as.character(10000000+sample(.N))),
                 by=.(tract,missing_race)]
    hh_relr_work[,("group_left_id"):=paste0(tract,group_or_hh,as.character(10000000+sample(.N))),
                by=.(tract,group_or_hh)]
    hh_relr_work[,c("age_range_4","race","race_sex_relations",
                   "race_age_range","individual_id"):=
                  anti_sar_rel[.SD,c(list(age_range_4),
                                     list(race),list(sex),list(age_range),list(individual_id)), 
                               on = .(group_left_id)]]
    anti_sar2_rel <- as.data.table(anti_join(anti_sar_rel,hh_relr_work,by="individual_id"))
    hh_relr2_work <- hh_relr_work[is.na(individual_id)]
    #1315 left, all group quarters - just being consistent on sort
    anti_sar2_rel[order(age_range),("group_left2_id"):=paste0(tract,as.character(10000000+seq.int(.N))),
                  by=.(tract)]     
    hh_relr2_work[order(relative),("group_left2_id"):=paste0(tract,as.character(10000000+seq.int(.N))),
                 by=.(tract)]
    hh_relr2_work[,c("age_range_4","race","race_sex_relations",
                    "race_age_range","individual_id"):=
                   anti_sar2_rel[.SD,c(list(age_range_4),
                                       list(race),list(sex),list(age_range),list(individual_id)), 
                                 on = .(group_left2_id)]]
    hh_relr_work[is.na(individual_id),
                c("age_range_4","race","race_sex_relations",
                  "race_age_range","individual_id"):=
                  hh_relr2_work[.SD,c(list(age_range_4),list(race),
                                     list(race_sex_relations),list(race_age_range),list(individual_id)), 
                               on = .(missr_id)]]
    hh_relations_dt[is.na(individual_id),
                    c("age_range_4","race","race_sex_relations",
                      "race_age_range","individual_id"):=
                      hh_relr_work[.SD,c(list(age_range_4),list(race),
                                        list(race_sex_relations),list(race_age_range),list(individual_id)), 
                                  on = .(missr_id)]]
    #test 5
    #test<-nrow(hh_relations_dt[is.na(ind_id_eth)])==0
    #test<-nrow(hh_relations_dt[is.na(eth_age_range)])==0
    #test<-nrow(hh_relations_dt[is.na(individual_id)])==0
    #test<-nrow(hh_relations_dt[is.na(race_age_range)])==0
    #test<-length(unique(hh_relations_dt$ind_id_eth))==nrow(sex_by_age_eth)
    #test<-length(unique(hh_relations_dt$individual_id))==nrow(sex_age_race)
    #redo 4a
    #test<-table(hh_relations_dt$tract,hh_relations_dt$eth_age_range,hh_relations_dt$ethnicity,hh_relations_dt$eth_sex_relations)==
    #  table(sex_by_age_eth$tract,sex_by_age_eth$age_range,sex_by_age_eth$ethnicity,sex_by_age_eth$sex)
    #length(test[test==F])==0
    #test<-table(hh_relations_dt$tract,hh_relations_dt$race_age_range,hh_relations_dt$race,hh_relations_dt$race_sex_relations)==
    #  table(sex_age_race$tract,sex_age_race$age_range,sex_age_race$race,sex_age_race$sex)
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
    
    #divide up eth/race for ease (should have done earlier)
    hh_relations_eth <- hh_relations_dt[,c("ind_id_eth","tract","sex","ethnicity","eth_sex_relations","eth_age_range","age_range_3","age_range_4",
                                           "group_or_hh","family_or_non","relative","role_in_family","relation_hh",
                                           "living_alone","in_family_type")]
    hh_relations_race <- hh_relations_dt[,c("individual_id","tract","sex","race","race_sex_relations","race_age_range","age_range_3","age_range_4",
                                            "group_or_hh","family_or_non","relative","role_in_family","relation_hh",
                                            "living_alone","in_family_type")]
    hh_relations_eth[sex=="none",("sex"):=eth_sex_relations]
    hh_relations_race[sex=="none",("sex"):=race_sex_relations]
    
    #test 8  - suite
    #test <- table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)==
    #  table(hh_relations_race$tract,hh_relations_race$race_sex_relations)
    #length(test[test==F])==0
    #test <- table(sex_by_age_eth$tract,sex_by_age_eth$sex)==
    #  table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)
    #length(test[test==F])==0
    ##eth_age_range and race_age_range don't match
    #test <- table(hh_relations_eth$tract,hh_relations_eth$eth_age_range)==
    #  table(hh_relations_race$tract,hh_relations_race$race_age_range)
    #length(test[test==F])==0
    ##but these each are true, so much better
    #test <- table(sex_by_age_eth$tract,sex_by_age_eth$ethnicity,sex_by_age_eth$sex,sex_by_age_eth$age_range)==
    #  table(hh_relations_eth$tract,hh_relations_eth$ethnicity,hh_relations_eth$eth_sex_relations,hh_relations_eth$eth_age_range)
    #length(test[test==F])==0
    #test <- table(sex_age_race$tract,sex_age_race$race,sex_age_race$sex,sex_age_race$age_range)==
    #  table(hh_relations_race$tract,hh_relations_race$race,hh_relations_race$race_sex_relations,hh_relations_race$race_age_range)
    #length(test[test==F])==0
    
    
    hh_relations_eth[,("hh_relrele_id"):= #eth_age_range matches sex_by_age_eth$age_range
                       paste0(tract,ethnicity,eth_sex_relations,eth_age_range,as.character(10000000+seq.int(.N))),
                     by=.(tract,ethnicity,eth_sex_relations,eth_age_range)]     
    place_born_eth_dt[,("hh_relrele_id"):=
                                 paste0(tract,ethnicity,fb_sex,age_range_14,as.character(10000000+seq.int(.N))),
                               by=.(tract,ethnicity,fb_sex,age_range_14)]
    hh_relations_eth[,c("place_born","fb_citizen",
                        "fb_origin_continent","fb_origin_area",
                        "fb_origin_place","fb_origin_country",
                        "fb_date_entered","family_origin","latinx_family_origin",
                        "latinx"):=
                       place_born_eth_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                                  on = .(hh_relrele_id)]]
    place_born_eth_dt[,c("missed_hhre"):=
                        hh_relations_eth[.SD,list(latinx),
                                         on = .(hh_relrele_id)]]
    #second time without sex
    hh_relations_eth[is.na(latinx),("hh_relrele2_id"):= #this eth_age_range matches sex_by_age_eth
                       paste0(tract,ethnicity,eth_age_range,as.character(10000000+seq.int(.N))),
                     by=.(tract,ethnicity,eth_age_range)]     
    place_born_eth_dt[is.na(missed_hhre),("hh_relrele2_id"):=
                        paste0(tract,ethnicity,age_range_14,as.character(10000000+seq.int(.N))),
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
    place_born_eth_dt[is.na(missed_hhre),c("missed_hhre"):=
                        hh_relations_eth[.SD,list(latinx),
                                         on = .(hh_relrele2_id)]]

    #still have 132103 Latinx not properly matched (of 1910535 - 7%), so do a shuffle on age_range before picking up non_Latinx
    #hh_relations_eth matches sae on age_range_14, so use it for the matching.
    #place_born_eth_dt has information about fb_date_entered and age that needs to be respected
    #break into part so can order
    hh_rr_pb1 <- hh_relations_eth[is.na(latinx)]
    hh_rr_pb1[order(eth_age_range),("hh_relrele3_id"):= #this eth_age_range matches sex_by_age_eth
                       paste0(tract,ethnicity,as.character(10000000+seq.int(.N))),
                     by=.(tract,ethnicity)]    
    pb_rr_hh1 <- place_born_eth_dt[is.na(missed_hhre)]
    pb_rr_hh1[order(age_range_14),("hh_relrele3_id"):=
                        paste0(tract,ethnicity,as.character(10000000+seq.int(.N))),
                      by=.(tract,ethnicity)]
    pb_rr_hh1[!is.na(ethnicity),("age_range_tmp"):=
                hh_rr_pb1[.SD,list(eth_age_range),on=.(hh_relrele3_id)]]
    hh_rr_pb1[,c("place_born","fb_citizen",
                 "fb_origin_continent","fb_origin_area",
                 "fb_origin_place","fb_origin_country",
                 "fb_date_entered","family_origin","latinx_family_origin",
                 "latinx"):=
                pb_rr_hh1[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                list(latinx)), 
                          on = .(hh_relrele3_id)]]
    hh_rr_pb2 <- hh_rr_pb1[is.na(latinx)]
    pb_rr_hh2 <- pb_rr_hh1[is.na(age_range_tmp)]
    nrow(hh_rr_pb2)==0
    nrow(pb_rr_hh2)==0
#    hh_rr_pb2[order(eth_age_range),("hh_relrele4_id"):= 
#                paste0(tract,as.character(10000000+seq.int(.N))),
#              by=.(tract)]
#    pb_rr_hh2[order(age_range_14),("hh_relrele4_id"):=
#                paste0(tract,as.character(10000000+seq.int(.N))),
#              by=.(tract)]
#    hh_rr_pb2[,c("place_born","fb_citizen",
#                 "fb_origin_continent","fb_origin_area",
#                 "fb_origin_place","fb_origin_country",
#                 "fb_date_entered","family_origin","latinx_family_origin",
#                 "latinx"):=
#                pb_rr_hh2[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
#                                list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
#                                list(fb_date_entered),list(family_origin),list(latinx_family_origin),
#                                list(latinx)), 
#                          on = .(hh_relrele4_id)]]
#    hh_rr_pb1[is.na(latinx),c("place_born","fb_citizen",
#                 "fb_origin_continent","fb_origin_area",
#                 "fb_origin_place","fb_origin_country",
#                 "fb_date_entered","family_origin","latinx_family_origin",
#                 "latinx"):=
#                hh_rr_pb2[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
#                                list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
#                                list(fb_date_entered),list(family_origin),list(latinx_family_origin),
#                                list(latinx)), 
#                          on = .(hh_relrele3_id)]]
    hh_relations_eth[is.na(latinx),c("place_born","fb_citizen",
                 "fb_origin_continent","fb_origin_area",
                 "fb_origin_place","fb_origin_country",
                 "fb_date_entered","family_origin","latinx_family_origin",
                 "latinx"):=
                hh_rr_pb1[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                list(latinx)), 
                          on = .(hh_relrele2_id)]]

    #test9
    #test <- table(hh_relations_eth$tract,
    #              hh_relations_eth$eth_age_range)==
    #  table(sex_by_age_eth$tract,
    #        sex_by_age_eth$age_range)
    #length(test[test==F])==0
    
    #race
    hh_relations_race[race_age_range=="5  to  9 years",("race_age_range"):="05 to  9 years"]
    hh_relations_race[,("hh_relrele_id"):= #race_age_range matches sex_age_race$age_range
                       paste0(tract,race,race_sex_relations,race_age_range,as.character(10000000+seq.int(.N))),
                     by=.(tract,race,race_sex_relations,race_age_range)]     
    place_born_race_dt[,("hh_relrele_id"):=
                        paste0(tract,race,fb_sex,age_range_14,as.character(10000000+seq.int(.N))),
                      by=.(tract,race,fb_sex,age_range_14)]
    hh_relations_race[,c("place_born","fb_citizen",
                        "fb_origin_continent","fb_origin_area",
                        "fb_origin_place","fb_origin_country",
                        "fb_date_entered","family_origin","latinx_family_origin",
                        "latinx"):=
                       place_born_race_dt[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                               list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                               list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                               list(latinx)), 
                                         on = .(hh_relrele_id)]]
    place_born_race_dt[,c("missed_hhre"):=
                        hh_relations_race[.SD,list(latinx),
                                         on = .(hh_relrele_id)]]
    #second time without sex - using hh_relations race_sex_relations 
    hh_relations_race[is.na(latinx),("hh_relrele2_id"):= 
                       paste0(tract,race,race_age_range,as.character(10000000+seq.int(.N))),
                     by=.(tract,race,race_age_range)]     
    place_born_race_dt[is.na(missed_hhre),("hh_relrele2_id"):=
                        paste0(tract,race,age_range_14,as.character(10000000+seq.int(.N))),
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
    
    #just over 1m left
    #break into part so can order
    hh_rr_pb1r <- hh_relations_race[is.na(latinx)]
    hh_rr_pb1r[order(race_age_range),("hh_relrele3_id"):= 
                paste0(tract,race,as.character(10000000+seq.int(.N))),
              by=.(tract,race)]    
    pb_rr_hh1r <- place_born_race_dt[is.na(missed_hhre)]
    pb_rr_hh1r[order(age_range_14),("hh_relrele3_id"):=
                paste0(tract,race,as.character(10000000+seq.int(.N))),
              by=.(tract,race)]
    pb_rr_hh1r[!is.na(race),("age_range_tmp"):=
                hh_rr_pb1r[.SD,list(race_age_range),on=.(hh_relrele3_id)]]
    hh_rr_pb1r[,c("place_born","fb_citizen",
                 "fb_origin_continent","fb_origin_area",
                 "fb_origin_place","fb_origin_country",
                 "fb_date_entered","family_origin","latinx_family_origin",
                 "latinx"):=
                pb_rr_hh1r[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                list(latinx)), 
                          on = .(hh_relrele3_id)]]
    hh_rr_pb2r <- hh_rr_pb1r[is.na(latinx)]
    pb_rr_hh2r <- pb_rr_hh1r[is.na(age_range_tmp)]
    nrow(hh_rr_pb2r)==0
    nrow(pb_rr_hh2r)==0
    #should put in a trap in case not equal to zero
#    hh_rr_pb2r[order(race_age_range),("hh_relrele4_id"):= 
#                paste0(tract,as.character(10000000+seq.int(.N))),
#              by=.(tract)]
#    pb_rr_hh2r[order(age_range_14),("hh_relrele4_id"):=
#                paste0(tract,as.character(10000000+seq.int(.N))),
#              by=.(tract)]
#    hh_rr_pb2r[is.na(latinx),c("place_born","fb_citizen",
#                  "fb_origin_continent","fb_origin_area",
#                  "fb_origin_place","fb_origin_country",
#                  "fb_date_entered","family_origin","latinx_family_origin",
#                  "latinx"):=
#                 pb_rr_hh2r[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
#                                  list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
#                                  list(fb_date_entered),list(family_origin),list(latinx_family_origin),
#                                  list(latinx)), 
#                            on = .(hh_relrele4_id)]]
#    #then up to hh_relations_race    
#    hh_rr_pb1r[is.na(latinx),c("place_born","fb_citizen",
#                  "fb_origin_continent","fb_origin_area",
#                  "fb_origin_place","fb_origin_country",
#                  "fb_date_entered","family_origin","latinx_family_origin",
#                  "latinx"):=
#                 hh_rr_pb2r[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
#                                  list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
#                                  list(fb_date_entered),list(family_origin),list(latinx_family_origin),
#                                  list(latinx)), 
#                            on = .(hh_relrele3_id)]]
    hh_relations_race[is.na(latinx),c("place_born","fb_citizen",
                  "fb_origin_continent","fb_origin_area",
                  "fb_origin_place","fb_origin_country",
                  "fb_date_entered","family_origin","latinx_family_origin",
                  "latinx"):=
                 hh_rr_pb1r[.SD,c(list(place_born),list(fb_citizen),list(fb_origin_continent),
                                  list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                  list(fb_date_entered),list(family_origin),list(latinx_family_origin),
                                  list(latinx)), 
                            on = .(hh_relrele2_id)]]


    sex_age_race[age_range=="5  to  9 years",("age_range"):="05 to  9 years"]
    #test9
    #test <- table(hh_relations_race$tract,
    #              hh_relations_race$race_age_range)==
    #  table(sex_age_race$tract,
    #        sex_age_race$age_range)
    #length(test[test==F])==0
    #test <- table(hh_relations_eth$tract,
    #              hh_relations_eth$fb_origin_country)==
    #  table(place_born_eth_dt$tract,
    #        place_born_eth_dt$fb_origin_country)
    #length(test[test==F])==0
    #test <- table(hh_relations_race$tract,
    #              hh_relations_race$fb_origin_country)==
    #  table(place_born_race_dt$tract,
    #        place_born_race_dt$fb_origin_country)
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
    #    saveRDS(hh_relations_dt,file = paste0(housingdir, vintage, "/hh_relations_dt_l.701_",Sys.Date(),".RDS"))
    
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
    #test 10
    #still close to 100k off, but not worth further effort
    
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
    #there are some surprising things in the original data, like 140051 born here who speak English less than very well
    #I copied it over - perhaps should tie to ethnicity and language? have to see how much the tract differences help!!!
        

    #marital back
    #eth_age_range==race_age_range==age_range
    #table(hh_relations_race[substr(race_age_range,1,2)>14,race])==table(marital_status_race_dt$race)
    #test 12 pre
    #test<-table(hh_relations_eth[substr(eth_age_range,1,2)>14,tract],
    #            hh_relations_eth[substr(eth_age_range,1,2)>14,eth_sex_relations],
    #  hh_relations_eth[substr(eth_age_range,1,2)>14,ethnicity],
    #      hh_relations_eth[substr(eth_age_range,1,2)>14,eth_age_range])==
    #  table(marital_status_eth_dt$tract,
    #        marital_status_eth_dt$sex,
    #    marital_status_eth_dt$ethnicity,
    #        marital_status_eth_dt$age_range)
    #length(test[test==F])==0
    #test<-table(hh_relations_race[substr(race_age_range,1,2)>14,tract],
    #            hh_relations_race[substr(race_age_range,1,2)>14,race_sex_relations],
    #            hh_relations_race[substr(race_age_range,1,2)>14,race],
    #            hh_relations_race[substr(race_age_range,1,2)>14,race_age_range])==
    #  table(marital_status_race_dt$tract,
    #        marital_status_race_dt$sex,
    #        marital_status_race_dt$race,
    #        marital_status_race_dt$age_range)
    #length(test[test==F])==0
    #new def. for marital match - for eth - need to think through the separated folks!!
    marital_status_eth_dt[,("not_married"):=if_else(marital_status=="Now married","married","not married")]
    hh_relations_eth[,("not_married"):=case_when(
      role_in_family=="Spouse" | role_in_family=="Son-in-law or daughter-in-law" ~ "married",
      role_in_family=="Householder" | role_in_family=="Parent" | role_in_family=="Parent-in-law" ~ "maybe",
      in_family_type=="In married-couple family" & role_in_family=="Householder" ~ "married",
      in_family_type=="In male householder no wife present family" & role_in_family=="Householder" ~ "not married",
      in_family_type=="In female householder no husband present family" & role_in_family=="Householder" ~ "not married",
      TRUE ~ "not married"
    )]
    #catch ~1k strays:
    hh_relations_eth[role_in_family=="Son-in-law or daughter-in-law" & 
                       substr(eth_age_range,1,2)<15, ("role_in_family"):="Biological child"]
    hh_relations_eth[relative=="Son-in-law or daughter-in-law" & 
                       substr(eth_age_range,1,2)<15, ("relative"):="Child"]
    hh_relations_eth[substr(eth_age_range,1,2)<15 & not_married!="not married",("not_married"):="not married"]
    
    #for race
    marital_status_race_dt[,("not_married"):=if_else(marital_status=="Now married","married","not married")]
    hh_relations_race[,("not_married"):=case_when(
      role_in_family=="Spouse" | role_in_family=="Son-in-law or daughter-in-law" ~ "married",
      role_in_family=="Householder" | role_in_family=="Parent" | role_in_family=="Parent-in-law" ~ "maybe",
      in_family_type=="In married-couple family" & role_in_family=="Householder" ~ "married",
      in_family_type=="In male householder no wife present family" & role_in_family=="Householder" ~ "not married",
      in_family_type=="In female householder no husband present family" & role_in_family=="Householder" ~ "not married",
      TRUE ~ "not married"
    )]
    #catch ~1k strays:
    hh_relations_race[role_in_family=="Son-in-law or daughter-in-law" & 
                       substr(race_age_range,1,2)<15, ("role_in_family"):="Biological child"]
    hh_relations_race[relative=="Son-in-law or daughter-in-law" & 
                       substr(race_age_range,1,2)<15, ("relative"):="Child"]
    hh_relations_race[substr(race_age_range,1,2)<15 & not_married!="not married",("not_married"):="not married"]
    #match for complete matches, then on leftovers (place_born is not right on marital_status, but I didn't take time to fix)
    #eth first
    marital_status_eth_dt[,("marital_rel_id"):= 
                             paste0(tract,place_born,not_married,ethnicity,sex,age_range,
                                    as.character(1000000+sample(1:.N))),
                           by=.(tract,place_born,not_married,ethnicity,sex,age_range)]
    hh_relations_eth[substr(eth_age_range,1,2)>14,("marital_rel_id"):= 
                            paste0(tract,place_born,not_married,ethnicity,eth_sex_relations,eth_age_range,
                                   as.character(1000000+sample(1:.N))),
                          by=.(tract,place_born,not_married,ethnicity,eth_sex_relations,eth_age_range)]
    hh_relations_eth[substr(eth_age_range,1,2)>14,c("marital_status","marital_status_5","spouse_present","separated","pregnant"):=
                       marital_status_eth_dt[.SD, c(list(marital_status),list(marital_status_5),
                                                    list(spouse_present),list(separated),list(pregnant)), 
                                              on = .(marital_rel_id)]]
    marital_status_eth_dt[,("missed"):=hh_relations_eth[.SD,list(marital_status),
                                                        on = .(marital_rel_id)]]
    #had 1458168 match
    #nrow(marital_status_eth_dt[is.na(missed)])==nrow(hh_relations_eth[is.na(marital_status)&substr(eth_age_range,1,2)>14])
    #######pickup a few more without place_born, since it was always inexact
    #######marital_status_eth_dt[is.na(missed),("marital_rel_2_id"):= 
    #######                        paste0(tract,not_married,ethnicity,sex,age_range,
    #######                               as.character(1000000+sample(1:.N))),
    #######                      by=.(tract,not_married,ethnicity,sex,age_range)]
    #######hh_relations_eth[is.na(marital_status),("marital_rel_2_id"):= 
    #######                   paste0(tract,not_married,ethnicity,eth_sex_relations,eth_age_range,
    #######                          as.character(1000000+sample(1:.N))),
    #######                 by=.(tract,not_married,ethnicity,eth_sex_relations,eth_age_range)]
    marital_status_eth_dt[is.na(missed),("marital_rel_2_id"):= 
                            paste0(tract,place_born,not_married,
                                   as.character(1000000+sample(1:.N))),
                          by=.(tract,place_born,not_married)]
    hh_relations_eth[substr(eth_age_range,1,2)>14&is.na(marital_status),("marital_rel_2_id"):= 
                       paste0(tract,place_born,not_married,
                              as.character(1000000+sample(1:.N))),
                     by=.(tract,place_born,not_married)]
    hh_relations_eth[substr(eth_age_range,1,2)>14&is.na(marital_status),
                     c("marital_status","place_born","fb_citizen","fb_origin_continent",
                       "fb_origin_area","fb_origin_place","fb_origin_country",
                       "fb_date_entered","family_origin","latinx_family_origin","latinx",
                       "marital_status_5","spouse_present",
                       "separated","pregnant"):=
                       marital_status_eth_dt[.SD,c(list(marital_status),list(place_born),
                                                   list(fb_citizen),list(fb_origin_continent),list(fb_origin_area),
                                                   list(fb_origin_place),list(fb_origin_country),list(fb_date_entered),
                                                   list(family_origin),list(latinx_family_origin),list(latinx),
                                                   list(marital_status_5),
                                                   list(spouse_present),list(separated),
                                                   list(pregnant)),
                                             on = .(marital_rel_2_id)]]
    marital_status_eth_dt[is.na(missed),
                          ("missed"):=hh_relations_eth[.SD,list(marital_status),
                                                       on = .(marital_rel_2_id)]]
    #sex, race and age_range still match on all
    #keep place_born and relatives from hh_relations
    #take sub_parts so can order
    mse_hh <- marital_status_eth_dt[is.na(missed)]
    hh_mse <- hh_relations_eth[is.na(marital_status)&substr(eth_age_range,1,2)>14]
    #line up and assign
    mse_hh[order(ethnicity,sex,age_range,
                 match(marital_status,c("Now married","Divorced","Widowed","Never married"))),
           ("marital_rel_3_id"):= 
             paste0(tract,
                    as.character(1000000+seq.int(1:.N))),
           by=.(tract)]
    hh_mse[order(ethnicity,eth_sex_relations,eth_age_range,
                 match(role_in_family,c("Spouse","Householder","Parent-in-law","Parent","Other relatives",
                                        "Son-in-law or daughter-in-law","Roomer or boarder","Housemate or roommate",
                                        "Other nonrelatives","Unmarried partner",
                                        "Brother or sister","in_group_quarters","Foster child",
                                        "Biological child","Adopted child","Stepchild","Grandchild"))),
           ("marital_rel_3_id"):= 
             paste0(tract,
                    as.character(1000000+seq.int(1:.N))),
           by=.(tract)]
    hh_mse[,c("marital_status","place_born","fb_citizen","fb_origin_continent",
              "fb_origin_area","fb_origin_place","fb_origin_country",
              "fb_date_entered","family_origin","latinx_family_origin","latinx",
              "marital_status_5","spouse_present",
              "separated","pregnant"):=
             mse_hh[.SD,c(list(marital_status),list(place_born),
                          list(fb_citizen),list(fb_origin_continent),list(fb_origin_area),
                          list(fb_origin_place),list(fb_origin_country),list(fb_date_entered),
                          list(family_origin),list(latinx_family_origin),list(latinx),
                          list(marital_status_5),
                          list(spouse_present),list(separated),
                          list(pregnant)),
                    on = .(marital_rel_3_id)]]
    #only for testing
    #mse_hh[,("missed"):=hh_mse[.SD,list(marital_status),
    #                           on = .(marital_rel_3_id)]]
    hh_relations_eth[is.na(marital_status)&substr(eth_age_range,1,2)>14,
                     c("marital_status","place_born","fb_citizen","fb_origin_continent",
                       "fb_origin_area","fb_origin_place","fb_origin_country",
                       "fb_date_entered","family_origin","latinx_family_origin","latinx",
                       "marital_status_5","spouse_present",
                       "separated","pregnant"):=
                       hh_mse[.SD,c(list(marital_status),list(place_born),
                                    list(fb_citizen),list(fb_origin_continent),list(fb_origin_area),
                                    list(fb_origin_place),list(fb_origin_country),list(fb_date_entered),
                                    list(family_origin),list(latinx_family_origin),list(latinx),
                                    list(marital_status_5),
                                    list(spouse_present),list(separated),
                                    list(pregnant)),
                              on = .(marital_rel_2_id)]]
    #on race
    marital_status_race_dt[,("marital_rel_id"):= 
                            paste0(tract,place_born,not_married,race,sex,age_range,
                                   as.character(1000000+sample(1:.N))),
                          by=.(tract,place_born,not_married,race,sex,age_range)]
    hh_relations_race[,("marital_rel_id"):= 
                       paste0(tract,place_born,not_married,race,race_sex_relations,race_age_range,
                              as.character(1000000+sample(1:.N))),
                     by=.(tract,place_born,not_married,race,race_sex_relations,race_age_range)]
    hh_relations_race[,c("marital_status","marital_status_5","spouse_present","separated","pregnant"):=
                       marital_status_race_dt[.SD, c(list(marital_status),list(marital_status_5),
                                                    list(spouse_present),list(separated),list(pregnant)), 
                                             on = .(marital_rel_id)]]
    marital_status_race_dt[,("missed"):=hh_relations_race[.SD,list(marital_status),
                                                        on = .(marital_rel_id)]]
    #had 1394581 match
    #nrow(marital_status_race_dt[is.na(missed)])==nrow(hh_relations_race[is.na(marital_status)&substr(race_age_range,1,2)>14])
    #pickup a few more without place_born, since it was always inexact
    ######marital_status_race_dt[is.na(missed),("marital_rel_2_id"):= 
    ######                        paste0(tract,not_married,race,sex,age_range,
    ######                               as.character(1000000+sample(1:.N))),
    ######                      by=.(tract,not_married,race,sex,age_range)]
    ######hh_relations_race[is.na(marital_status),("marital_rel_2_id"):= 
    ######                   paste0(tract,not_married,race,race_sex_relations,race_age_range,
    ######                          as.character(1000000+sample(1:.N))),
    ######                 by=.(tract,not_married,race,race_sex_relations,race_age_range)]
    marital_status_race_dt[is.na(missed),("marital_rel_2_id"):= 
                             paste0(tract,place_born,not_married,race,
                                    as.character(1000000+sample(1:.N))),
                           by=.(tract,place_born,not_married,race)]
    hh_relations_race[is.na(marital_status)&substr(race_age_range,1,2)>14,
                      ("marital_rel_2_id"):= 
                        paste0(tract,place_born,not_married,race,
                               as.character(1000000+sample(1:.N))),
                      by=.(tract,place_born,not_married,race)]
    hh_relations_race[is.na(marital_status)&substr(race_age_range,1,2)>14,
                     c("marital_status","marital_status_5","spouse_present",
                       "separated","pregnant"):=
                       marital_status_race_dt[.SD,c(list(marital_status),
                                                   list(marital_status_5),
                                                   list(spouse_present),list(separated),
                                                   list(pregnant)),
                                             on = .(marital_rel_2_id)]]
    marital_status_race_dt[is.na(missed),
                          ("missed"):=hh_relations_race[.SD,list(marital_status),
                                                       on = .(marital_rel_2_id)]]
    #now match 1728245
    #nrow(marital_status_race_dt[is.na(missed)])==nrow(hh_relations_race[is.na(marital_status)&substr(race_age_range,1,2)>14])
    #sex, race and age_range still match on all
    #keep place_born and relatives from hh_relations
    #take sub_parts so can order
    msr_hh <- marital_status_race_dt[is.na(missed)]
    hh_msr <- hh_relations_race[is.na(marital_status)&substr(race_age_range,1,2)>14]
    #line up and assign
    msr_hh[order(race,sex,age_range,
                 match(marital_status,c("Now married","Divorced","Widowed","Never married"))),
           ("marital_rel_3_id"):= 
             paste0(tract,
                    as.character(1000000+seq.int(1:.N))),
           by=.(tract)]
    hh_msr[order(race,race_sex_relations,race_age_range,
                 match(role_in_family,c("Spouse","Householder","Parent-in-law","Parent","Other relatives",
                                        "Son-in-law or daughter-in-law","Roomer or boarder","Housemate or roommate",
                                        "Other nonrelatives","Unmarried partner",
                                        "Brother or sister","in_group_quarters","Foster child",
                                        "Biological child","Adopted child","Stepchild","Grandchild"))),
           ("marital_rel_3_id"):= 
             paste0(tract,
                    as.character(1000000+seq.int(1:.N))),
           by=.(tract)]
    hh_msr[,c("marital_status","place_born","fb_citizen","fb_origin_continent",
              "fb_origin_area","fb_origin_place","fb_origin_country",
              "fb_date_entered","family_origin","latinx_family_origin","latinx",
              "marital_status_5","spouse_present",
              "separated","pregnant"):=
             msr_hh[.SD,c(list(marital_status),list(place_born),
                          list(fb_citizen),list(fb_origin_continent),list(fb_origin_area),
                          list(fb_origin_place),list(fb_origin_country),list(fb_date_entered),
                          list(family_origin),list(latinx_family_origin),list(latinx),
                          list(marital_status_5),
                          list(spouse_present),list(separated),
                          list(pregnant)),
                    on = .(marital_rel_3_id)]]
    #only for testing
    #msr_hh[,("missed"):=hh_msr[.SD,list(marital_status),
    #                           on = .(marital_rel_3_id)]]
    hh_relations_race[is.na(marital_status)&substr(race_age_range,1,2)>14,
                      c("marital_status","place_born","fb_citizen","fb_origin_continent",
                        "fb_origin_area","fb_origin_place","fb_origin_country",
                        "fb_date_entered","family_origin","latinx_family_origin","latinx",
                        "marital_status_5","spouse_present",
                        "separated","pregnant"):=
                       hh_msr[.SD,c(list(marital_status),list(place_born),
                                    list(fb_citizen),list(fb_origin_continent),list(fb_origin_area),
                                    list(fb_origin_place),list(fb_origin_country),list(fb_date_entered),
                                    list(family_origin),list(latinx_family_origin),list(latinx),
                                    list(marital_status_5),
                                    list(spouse_present),list(separated),
                                    list(pregnant)),
                              on = .(marital_rel_2_id)]]
    
    
    
    #test 12 - no longer works, but to be expected because chose to stay with place_born marital and work back to sex/age.
    #test <- table(hh_relations_race[substr(race_age_range,1,2)>14,tract],
    #              hh_relations_race[substr(race_age_range,1,2)>14,race],
    #              hh_relations_race[substr(race_age_range,1,2)>14,race_sex_relations],
    #              hh_relations_race[substr(race_age_range,1,2)>14,race_age_range],
    #              hh_relations_race[substr(race_age_range,1,2)>14,marital_status])==
    #  table(marital_status_race_dt$tract,
    #        marital_status_race_dt$race,
    #        marital_status_race_dt$sex,
    #        marital_status_race_dt$age_range,
    #        marital_status_race_dt$marital_status)
    #length(test[test==F])==0 # very false
    
    #should look at crosstabs for weird outliers
    #test 13 - pre for if eth and race can be juggled to match 
    #exact matches: 
    test <- table(hh_relations_eth$tract,
                  hh_relations_eth$eth_age_range,
                  hh_relations_eth$eth_sex_relations
                  )==
      table(hh_relations_race$tract, 
            hh_relations_race$race_age_range, #sex and age match together
            hh_relations_race$race_sex_relations
      )
    length(test[test==F])==0
    test <- table(hh_relations_eth$tract,
                  hh_relations_eth$place_born,
                  hh_relations_eth$marital_status,
                  hh_relations_eth$latinx,
  #                hh_relations_eth$pregnant,
  #                #hh_relations_eth$fb_origin_continent,
  #                hh_relations_eth$fb_origin_place,
  #                hh_relations_eth$fb_origin_area,
  #                #hh_relations_eth$fb_origin_country,  #triggers "table with >= 2^31 elements" but in pieces still works, so...
  #                hh_relations_eth$group_or_hh,
  #                ###hh_relations_eth$relative#, - no info here that's not in role_in_family
  #                hh_relations_eth$relation_hh,
  #                hh_relations_eth$sr_hh_living_alone,
  #                hh_relations_eth$in_family_type,
  #                ###hh_relations_eth$role_in_family,
                  hh_relations_eth$family_or_non
                  )==
      table(hh_relations_race$tract,
            hh_relations_race$place_born,
            hh_relations_race$marital_status,  #if included, 0.25% of table have wrong total
            hh_relations_race$latinx,
 #           hh_relations_race$pregnant,
 #           #hh_relations_race$fb_origin_continent,
 #           hh_relations_race$fb_origin_place,
 #           hh_relations_race$fb_origin_area,
 #           #hh_relations_race$fb_origin_country,
 #           hh_relations_race$group_or_hh,
 #           ###hh_relations_race$relative#, #2% off by itself
 #           hh_relations_race$relation_hh, 
 #           hh_relations_race$sr_hh_living_alone,
 #           hh_relations_race$in_family_type,
 #           ###hh_relations_race$role_in_family, #.25% off with group and family or non; 1% off by itself
            hh_relations_race$family_or_non  
      )
    length(test[test==F])==0
    #
    #test <- table(hh_relations_eth$tract,
    #              hh_relations_eth$place_born,
    #              hh_relations_eth$fb_origin_continent,
    #              hh_relations_eth$fb_origin_country,
    #              hh_relations_eth$fb_origin_place,
    #              hh_relations_eth$fb_date_entered,
    #             hh_relations_eth$fb_origin_area
    #             )==
    #  table(hh_relations_race$tract,
    #        hh_relations_race$place_born, #matches by itself
    #       hh_relations_race$fb_origin_continent,
    #       hh_relations_race$fb_origin_country,
    #       hh_relations_race$fb_origin_place,
    #       hh_relations_race$fb_date_entered,
    #      hh_relations_race$fb_origin_area 
    #       )
    #length(test[test==F])==0
    
    #wtf test
    
    #drop one factor, then pick it back up for the next try - so you're slowly building it back in;
    #it's equal to shuffling if you write it over, and then include the term as you search the rest, 
    #hh_relations_eth[,#order(eth_age_range),
    #                 ("wtf8_eth_id"):= 
    #                   paste0(tract,marital_status,latinx,
    #                          as.character(1000000+seq.int(1:.N))),
    #                 by=.(tract,marital_status,latinx)]
    #hh_relations_race[,#order(race_age_range),
    #                  ("wtf8_eth_id"):= 
    #                    paste0(tract,marital_status,latinx,
    #                           as.character(1000000+seq.int(1:.N))),
    #                  by=.(tract,marital_status,latinx)]
    #hh_relations_race[,c("wtf8","wtf7_ind_id_eth"):=
    #                    hh_relations_eth[.SD,c(list(latinx),list(ind_id_eth)),
    #                                     on = .(wtf8_eth_id)]]
    #hh_relations_eth[,c("wtf8","wtf7_individual_id"):=
    #                   hh_relations_race[.SD,c(list(latinx),list(individual_id)),on=.(wtf8_eth_id)]]
    
    #only moving race and eth together on things that have 100% match - not solving the simultaneous equations completely, although table does
    hh_relations_eth[,
               ("race_eth_id"):= 
                 paste0(tract,marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                        group_or_hh,family_or_non,relation_hh,
                        sr_hh_living_alone,in_family_type,
                        eth_age_range,eth_sex_relations,
                        as.character(1000000+sample(1:.N))),
               by=.(tract,marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                    group_or_hh,family_or_non,relation_hh,
                    sr_hh_living_alone,in_family_type,eth_age_range,eth_sex_relations)]
    hh_relations_race[,
                ("race_eth_id"):= 
                  paste0(tract,marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                         group_or_hh,family_or_non,relation_hh,
                         sr_hh_living_alone,in_family_type,
                         race_age_range,race_sex_relations,
                         as.character(1000000+sample(1:.N))),
                by=.(tract,marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                     group_or_hh,family_or_non,relation_hh,
                     sr_hh_living_alone,in_family_type,race_age_range,race_sex_relations)]
    hh_relations_race[,c("ethnicity","ind_id_eth"):=
                        hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth)),
                             on = .(race_eth_id)]]
    hh_relations_eth[,c("race","individual_id"):=
                 hh_relations_race[.SD,c(list(race),list(individual_id)),on=.(race_eth_id)]]
    #nrow(hh_relations_race[is.na(ethnicity)])==nrow(hh_relations_eth[is.na(race)])
    #nrow(hh_relations_race[is.na(ethnicity)])/nrow(hh_relations_race) ~46%
    
    hh_relations_eth[is.na(individual_id),
               ("race2_eth_id"):= 
                 paste0(tract,marital_status,pregnant,#place_born,fb_origin_place,fb_origin_area,
                        group_or_hh,family_or_non,relation_hh,
                        sr_hh_living_alone,in_family_type,
                        eth_age_range,eth_sex_relations,
                        as.character(1000000+sample(1:.N))),
               by=.(tract,marital_status,pregnant,#place_born,fb_origin_place,fb_origin_area,
                    group_or_hh,family_or_non,relation_hh,
                    sr_hh_living_alone,in_family_type,eth_age_range,eth_sex_relations)]
    hh_relations_race[is.na(ind_id_eth),
               ("race2_eth_id"):= 
                 paste0(tract,marital_status,pregnant,#place_born,fb_origin_place,fb_origin_area,
                        group_or_hh,family_or_non,relation_hh,
                        sr_hh_living_alone,in_family_type,
                        race_age_range,race_sex_relations,
                        as.character(1000000+sample(1:.N))),
               by=.(tract,marital_status,pregnant,#place_born,fb_origin_place,fb_origin_area,
                    group_or_hh,family_or_non,relation_hh,
                    sr_hh_living_alone,in_family_type,race_age_range,race_sex_relations)]
    hh_relations_race[is.na(ind_id_eth),
                c("ethnicity","ind_id_eth","place_born","fb_origin_continent","fb_origin_area",
                   "fb_origin_place","fb_origin_country","fb_citizen",
                   "fb_date_entered","family_origin","latinx_family_origin",
                   "latinx","fb_language_at_home","English_proficiency"):=
                  hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth),
                                   list(place_born),list(fb_origin_continent),
                                   list(fb_origin_area),list(fb_origin_place),
                                   list(fb_origin_country),list(fb_citizen),
                                   list(fb_date_entered),list(family_origin),
                                   list(latinx_family_origin),list(latinx),
                                   list(fb_language_at_home), #not only fb!!!
                                   list(English_proficiency)),
                             on = .(race2_eth_id)]]
    hh_relations_eth[is.na(individual_id),c("race","individual_id","place_born","fb_origin_continent","fb_origin_area",
                   "fb_origin_place","fb_origin_country","fb_citizen",
                   "fb_date_entered","family_origin","latinx_family_origin",
                   "latinx","fb_language_at_home","English_proficiency"):=
                 hh_relations_race[.SD,c(list(race),list(individual_id),
                                   list(place_born),list(fb_origin_continent),
                                   list(fb_origin_area),list(fb_origin_place),
                                   list(fb_origin_country),list(fb_citizen),
                                   list(fb_date_entered),list(family_origin),
                                   list(latinx_family_origin),list(latinx),
                                   list(fb_language_at_home), #not only fb!!!
                                   list(English_proficiency)),
                             on = .(race2_eth_id)]]
    
    #on marital_status and family_type
    hh_relations_eth[is.na(individual_id),
               ("race3_eth_id"):= 
                 paste0(tract,place_born,fb_origin_place,fb_origin_area,
                        group_or_hh,family_or_non,relation_hh,
                        sr_hh_living_alone,in_family_type,
                        eth_age_range,eth_sex_relations,
                        as.character(1000000+sample(1:.N))),
               by=.(tract,place_born,fb_origin_place,fb_origin_area,
                    group_or_hh,family_or_non,relation_hh,
                    sr_hh_living_alone,in_family_type,eth_age_range,eth_sex_relations)]
    hh_relations_race[is.na(ind_id_eth),
                ("race3_eth_id"):= 
                  paste0(tract,place_born,fb_origin_place,fb_origin_area,
                         group_or_hh,family_or_non,relation_hh,
                         sr_hh_living_alone,in_family_type,
                         race_age_range,race_sex_relations,
                         as.character(1000000+sample(1:.N))),
                by=.(tract,place_born,fb_origin_place,fb_origin_area,
                     group_or_hh,family_or_non,relation_hh,
                     sr_hh_living_alone,in_family_type,race_age_range,race_sex_relations)]
    hh_relations_race[is.na(ind_id_eth),
                c("ethnicity","ind_id_eth","marital_status","marital_status_5","spouse_present",
                   "separated","pregnant"):=
                  hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth),
                                   list(marital_status),list(marital_status_5),
                                   list(spouse_present),list(separated),
                                   list(pregnant)),
                             on = .(race3_eth_id)]]
    hh_relations_eth[is.na(individual_id),
               c("race","individual_id","marital_status","marital_status_5","spouse_present",
                 "separated","pregnant"):=
                 hh_relations_race[.SD,c(list(race),list(individual_id),
                                   list(marital_status),list(marital_status_5),
                                   list(spouse_present),list(separated),
                                   list(pregnant)),
                             on = .(race3_eth_id)]]
    
    #on family_type/relations for rest
    hh_relations_eth[is.na(individual_id),
                     ("race4_eth_id"):= 
                       paste0(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                              group_or_hh,family_or_non,relation_hh,
                              sr_hh_living_alone,in_family_type,
                              eth_age_range,eth_sex_relations,
                              as.character(1000000+sample(1:.N))),
                     by=.(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                          group_or_hh,family_or_non,relation_hh,
                          sr_hh_living_alone,in_family_type,
                          eth_age_range,eth_sex_relations
                          )]
    hh_relations_race[is.na(ind_id_eth),
                      ("race4_eth_id"):= 
                        paste0(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                               group_or_hh,family_or_non,relation_hh,
                               sr_hh_living_alone,in_family_type,
                               race_age_range,race_sex_relations,
                               as.character(1000000+sample(1:.N))),
                      by=.(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                           group_or_hh,family_or_non,relation_hh,
                           sr_hh_living_alone,in_family_type,
                           race_age_range,race_sex_relations
                           )]
    hh_relations_race[is.na(ind_id_eth),
                      c("ethnicity","ind_id_eth"):=
                        hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth)),
                                         on = .(race4_eth_id)]]
    hh_relations_eth[is.na(individual_id),c("race","individual_id"):=
                       hh_relations_race[.SD,c(list(race),list(individual_id)),
                                         on = .(race4_eth_id)]]
    #and just on age/sex
    hh_relations_eth[is.na(individual_id),
                     ("race5_eth_id"):= 
                       paste0(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                              #group_or_hh,family_or_non,relation_hh,
                              #sr_hh_living_alone,in_family_type,
                              eth_age_range,eth_sex_relations,
                              as.character(1000000+sample(1:.N))),
                     by=.(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                          #group_or_hh,family_or_non,relation_hh,
                          #sr_hh_living_alone,in_family_type#,
                          eth_age_range,eth_sex_relations
                     )]
    hh_relations_race[is.na(ind_id_eth),
                      ("race5_eth_id"):= 
                        paste0(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                               #group_or_hh,family_or_non,relation_hh,
                               #sr_hh_living_alone,in_family_type,
                               race_age_range,race_sex_relations,
                               as.character(1000000+sample(1:.N))),
                      by=.(tract,#marital_status,pregnant,place_born,fb_origin_place,fb_origin_area,
                           #group_or_hh,family_or_non,relation_hh,
                           #sr_hh_living_alone,in_family_type#,
                           race_age_range,race_sex_relations
                      )]
    hh_relations_race[is.na(ind_id_eth),
                      c("ethnicity","ind_id_eth"):=
                        hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth)),
                                         on = .(race5_eth_id)]]
    hh_relations_eth[is.na(individual_id),c("race","individual_id"):=
                       hh_relations_race[.SD,c(list(race),list(individual_id)),
                                         on = .(race5_eth_id)]]
    
    #just sample last 1%
    hh_relations_eth[is.na(individual_id),
                     ("race6_eth_id"):= 
                       paste0(tract,as.character(1000000+sample(1:.N))),
                     by=.(tract)]
    hh_relations_race[is.na(ind_id_eth),
                      ("race6_eth_id"):= 
                        paste0(tract,as.character(1000000+sample(1:.N))),
                      by=.(tract)]
    hh_relations_race[is.na(ind_id_eth),
                      c("ethnicity","ind_id_eth"):=
                        hh_relations_eth[.SD,c(list(ethnicity),list(ind_id_eth)),
                                         on = .(race6_eth_id)]]
    hh_relations_eth[is.na(individual_id),c("race","individual_id"):=
                       hh_relations_race[.SD,c(list(race),list(individual_id)),
                                         on = .(race6_eth_id)]]
    saveRDS(hh_relations_eth,file = paste0(housingdir, vintage, "/hh_relations_eth_",Sys.Date(),".RDS"))
    saveRDS(hh_relations_race,file = paste0(housingdir, vintage, "/hh_relations_race_",Sys.Date(),".RDS"))
  }
}



    
 #Move next steps into apportioning.R   

