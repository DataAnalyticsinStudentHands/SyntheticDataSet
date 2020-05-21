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
    anti_msr[,c("age_range","spouse_present","separated","place_born","marital_status_5","individual_id"):= 
               anti_msa[.SD, c(list(age_range),list(spouse_present),list(separated),
                               list(place_born),list(marital_status),list(individual_id)), 
                         on = .(married_id2b)]]
    marital_status_race_dt[is.na(age_range),c("age_range","spouse_present","separated","place_born",
                                              "marital_status_5","individual_id"):= 
                             anti_msr[.SD, c(list(age_range),list(spouse_present),list(separated),
                                             list(place_born),list(marital_status),list(individual_id)), 
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
    anti_mse[,c("age_range","spouse_present","separated","place_born","marital_status_5","ind_id_eth"):= 
               anti_msa2[.SD, c(list(age_range),list(spouse_present),list(separated),list(place_born),
                                list(marital_status),list(ind_id_eth)), 
                        on = .(married_id3b)]]
    marital_status_eth_dt[is.na(age_range),c("age_range","spouse_present","separated","place_born",
                                             "marital_status_5","ind_id_eth"):= 
                             anti_mse[.SD, c(list(age_range),list(spouse_present),list(separated),
                                             list(place_born),list(marital_status),list(ind_id_eth)), 
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
    #test <- table(sr_relations$tract)==table(sex_age_race[age>64]$tract) #sex_sr_relations is only for married couples
    sr_relations[,("race_sr_id"):=
                   paste0(tract,sex_sr_relations,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex_sr_relations)]
    sex_age_race[age>64,("race_sr_id"):=
                   paste0(tract,sex,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex)]
    sex_by_age_eth[age>64,("race_sr_id"):=
                   paste0(tract,sex,
                          as.character(1000000+sample(.N))),
                 by=.(tract,sex)]
    sr_relations[,c("race","race_age_range","individual_id"):=
                  sex_age_race[.SD, c(list(race),list(age_range),list(individual_id)), on = .(race_sr_id)]]
    sex_age_race[age>64,("missing_race"):=
                   sr_relations[.SD, c(list(group_or_hh)), on = .(race_sr_id)]] #put group_quarters on for potential matching...
    sr_relations[,c("ethnicity","eth_age_range","ind_id_eth"):=
                   sex_by_age_eth[.SD, c(list(ethnicity),list(age_range),list(ind_id_eth)), on = .(race_sr_id)]]
    sex_by_age_eth[age>64,("missing_eth"):=
                   sr_relations[.SD, c(list(group_or_hh)), on = .(race_sr_id)]]
    #then rest (that didn't have sex listed)
    sr_relations[is.na(race),("race_sr2_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sr_relations[is.na(ethnicity),("eth_sr2_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_age_race[age>64 & is.na(missing_race),("race_sr2_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_by_age_eth[age>64 & is.na(missing_eth),("eth_sr2_id"):=
                     paste0(tract,as.character(1000000+sample(.N))),
                   by=.(tract)]
    sr_relations[,("race_sex_relations"):=sex_sr_relations]
    sr_relations[is.na(race),c("race","race_sex_relations","race_age_range","individual_id"):=
                   sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), on = .(race_sr2_id)]]
    sex_age_race[age>64 & is.na(missing_race),("missing_race"):=
                   sr_relations[.SD, c(list(group_or_hh)), on = .(race_sr2_id)]]
    sr_relations[,("eth_sex_relations"):=sex_sr_relations]
    sr_relations[is.na(ethnicity),c("ethnicity","eth_sex_relations","eth_age_range","ind_id_eth"):=
                   sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), on = .(eth_sr2_id)]]
    sex_by_age_eth[age>64 & is.na(missing_eth),("missing_eth"):=
                     sr_relations[.SD, c(list(group_or_hh)), on = .(eth_sr2_id)]]
    sex_age_race$race_sr_id <-NULL
    sex_age_race$race_sr2_id <-NULL
    sex_by_age_eth$race_sr_id <-NULL
    sex_by_age_eth$eth_sr2_id <-NULL
    #test3
    #test<-table(sex_age_race[age>64]$tract,sex_age_race[age>64]$sex,sex_age_race[age>64]$race,sex_age_race[age>64]$age_range)==
    #  table(sr_relations$tract,sr_relations$race_sex_relations,sr_relations$race,sr_relations$race_age_range)
    #test<-table(sex_by_age_eth[age>64]$tract,sex_by_age_eth[age>64]$sex,sex_by_age_eth[age>64]$ethnicity,sex_by_age_eth[age>64]$age_range)==
    #  table(sr_relations$tract,sr_relations$eth_sex_relations,sr_relations$ethnicity,sr_relations$eth_age_range)
    #length(test[test==F])==0
    
    #to match sr_relations$relative - 1472 folks who are >= 65 are Child of Householder in original; make sure to get old HH
    hh_relations_dt[,("relative_sr"):=case_when(
      relative=="Householder" ~ "Householder",
      relative=="Spouse" ~ "Spouse",
      relative=="Nonrelatives" ~ "Nonrelatives",
      relative=="Parent" ~ "Parent",
      relative=="Parent-in-law" ~ "Parent-in-law",
      relative=="Child" ~ "Child of householder",
      relative=="Other relatives" | relative=="Brother or sister" | 
        relative=="Grandchild" ~ "Other relatives",
      TRUE ~ relative
    )]
    sr_relations[,("age_range_3"):=list("65 years and over")] 
    hh_relations_dt[,("rel_sr_id"):=
                      paste0(tract,group_or_hh,family_or_non,relative_sr,
                             as.character(1000000+sample(.N))),
                    by=.(tract,group_or_hh,family_or_non,relative_sr)]
    sr_relations[,("rel_sr_id"):=
                   paste0(tract,group_or_hh,family_or_non,relative,
                          as.character(1000000+sample(.N))),
                 by=.(tract,group_or_hh,family_or_non,relative)]
    hh_relations_dt[,c("sex_sr_hh","sr_hh_living_alone","age_range_3","ethnicity","eth_sex_relations","eth_age_range",
                       "race","race_sex_relations","race_age_range","individual_id","ind_id_eth"):=
                      sr_relations[.SD, c(list(sex_sr_relations),list(living_alone),list(age_range_3),
                                          list(ethnicity),list(eth_sex_relations),list(eth_age_range),
                                          list(race),list(race_sex_relations),list(race_age_range),
                                          list(individual_id),list(ind_id_eth)), 
                                   on = .(rel_sr_id)]]
    #25 folks were missing - a bit OCD to get them...
    anti_sr_rel <- as.data.table(anti_join(sr_relations,hh_relations_dt,by="rel_sr_id"))
    hh_relations_dt[is.na(age_range_3),("rel_sr2_id"):=
                      paste0(tract,group_or_hh,
                             as.character(1000000+sample(.N))),
                    by=.(tract,group_or_hh)]
    anti_sr_rel[,("rel_sr2_id"):=
                   paste0(tract,group_or_hh,
                          as.character(1000000+sample(.N))),
                 by=.(tract,group_or_hh)]
    hh_relations_dt[is.na(age_range_3),c("sex_sr_hh","sr_hh_living_alone","age_range_3","ethnicity","eth_sex_relations","eth_age_range",
                                         "race","race_sex_relations","race_age_range","individual_id","ind_id_eth"):=
                      anti_sr_rel[.SD, c(list(sex_sr_relations),list(living_alone),list(age_range_3),
                                         list(ethnicity),list(eth_sex_relations),list(eth_age_range),
                                         list(race),list(race_sex_relations),list(race_age_range),
                                         list(individual_id),list(ind_id_eth)), 
                                  on = .(rel_sr2_id)]]
    
    #4 tracts from sr_relations not in hh_relations
    #test3a
    #test<-table(hh_relations_dt[tract%in%unique(sr_relations$tract)]$tract,hh_relations_dt[tract%in%unique(sr_relations$tract)]$sex_sr_hh,
    #            hh_relations_dt[tract%in%unique(sr_relations$tract)]$sr_hh_living_alone,hh_relations_dt[tract%in%unique(sr_relations$tract)]$race,
    #            hh_relations_dt[tract%in%unique(sr_relations$tract)]$ethnicity,hh_relations_dt[tract%in%unique(sr_relations$tract)]$eth_age_range)==
    #  table(sr_relations$tract,sr_relations$sex_sr_relations,sr_relations$living_alone,sr_relations$race,sr_relations$ethnicity,sr_relations$eth_age_range)
    #length(test[test==F])==0
    rm(sr_relations)

    #now do adults - giving all a possible race and age - just by sample within each tract
    adults_relations[age_range_3=="18 to 34 years",("race_adults_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_age_race[age>17&age<35,("race_adults_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_by_age_eth[age>17&age<35,("race_adults_id"):=
                     paste0(tract,as.character(1000000+sample(.N))),
                   by=.(tract)]
    adults_relations[age_range_3=="18 to 34 years",c("race","race_sex_relations","race_age_range","individual_id"):=
                   sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), on = .(race_adults_id)]]
    sex_age_race[age>17&age<35,("missing_race"):=
                   adults_relations[.SD, list(race_sex_relations), on = .(race_adults_id)]]
    sex_age_race[age>17&age<35&is.na(missing_race),("missing_race"):=list("In group quarters")]
    sex_age_race[age>17&age<35,("missing_race"):=if_else(missing_race=="Male" | missing_race=="Female","In households","In group quarters")]
    adults_relations[age_range_3=="18 to 34 years",c("ethnicity","eth_sex_relations","eth_age_range","ind_id_eth"):=
                   sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), on = .(race_adults_id)]]
    sex_by_age_eth[age>17&age<35,("missing_eth"):=
                     adults_relations[.SD, list(eth_sex_relations), on = .(race_adults_id)]]
    sex_by_age_eth[age>17&age<35&is.na(missing_eth),("missing_eth"):=list("In group quarters")]
    sex_by_age_eth[age>17&age<35,("missing_eth"):=if_else(missing_eth=="Male" | missing_eth=="Female","In households","In group quarters")]
    sex_age_race$race_adults_id <-NULL
    sex_by_age_eth$race_adults_id <-NULL
    #now do 35 to 64
    adults_relations[age_range_3=="35 to 64 years",("race_adults2_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    sex_age_race[age>34&age<65,("race_adults2_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_by_age_eth[age>34&age<65,("race_adults2_id"):=
                     paste0(tract,as.character(1000000+sample(.N))),
                   by=.(tract)]
    adults_relations[age_range_3=="35 to 64 years",c("race","race_sex_relations","race_age_range","individual_id"):=
                       sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), on = .(race_adults2_id)]]
    sex_age_race[age>34&age<65,("missing_race"):=
                   adults_relations[.SD, list(race_sex_relations), on = .(race_adults2_id)]]
    sex_age_race[age>34&age<65,("missing_race"):=if_else(missing_race=="Male" | missing_race=="Female","In households","In group quarters")]
    sex_age_race[age>34&age<65&is.na(missing_race),("missing_race"):="In group quarters"]
    adults_relations[age_range_3=="35 to 64 years",c("ethnicity","eth_sex_relations","eth_age_range","ind_id_eth"):=
                       sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), on = .(race_adults2_id)]]
    sex_by_age_eth[age>34&age<65,("missing_eth"):=
                     adults_relations[.SD, list(eth_sex_relations), on = .(race_adults2_id)]]
    sex_by_age_eth[age>34&age<65&is.na(missing_eth),("missing_eth"):="In group quarters"]
    sex_by_age_eth[age>34&age<65,("missing_eth"):=if_else(missing_eth=="Male" | missing_eth=="Female","In households","In group quarters")]
    sex_age_race$race_adults2_id <-NULL
    sex_by_age_eth$race_adults2_id <-NULL
    #test3b
    #test<-table(sex_age_race[age>17&age<65&missing_race=="In households"]$tract,sex_age_race[age>17&age<65&missing_race=="In households"]$sex,
    #            sex_age_race[age>17&age<65&missing_race=="In households"]$race,sex_age_race[age>17&age<65&missing_race=="In households"]$age_range)==
    #  table(adults_relations[age_range_3!="65 years and over"]$tract,adults_relations[age_range_3!="65 years and over"]$race_sex_relations,
    #        adults_relations[age_range_3!="65 years and over"]$race,adults_relations[age_range_3!="65 years and over"]$race_age_range)
    #test<-table(sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$tract,sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$sex,
    #            sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$ethnicity,sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$age_range)==
    #  table(adults_relations[age_range_3!="65 years and over"]$tract,adults_relations[age_range_3!="65 years and over"]$eth_sex_relations,adults_relations[age_range_3!="65 years and over"]$ethnicity,adults_relations[age_range_3!="65 years and over"]$eth_age_range)
    #length(test[test==F])==0
    
    hh_relations_dt[group_or_hh=="In households",("relation_hh"):=case_when(
      relative=="Child" | relative=="Grandchild" ~ "Child of householder", #but only for adults; Foster child is non-relative
      relative=="Spouse" ~ "Householder living with spouse or spouse of householder",
      role_in_family=="Unmarried partner" ~ "Householder living with unmarried partner or unmarried partner of householder",
      relative=="Nonrelatives" | relative=="Parent-in-law" ~ "Other nonrelatives",
      relative=="Other relatives" | relative =="Parent" | relative=="Brother or sister" |
        relative=="Son-in-law or daughter-in-law"~ "Other relatives",
      TRUE ~ "Householder" 
    )]
    #test 4 
    #nrow(hh_relations_dt[relation_hh=="Householder"])==nrow(hh_relations_dt[relative=="Householder"])
    
    #matching hh_relations and adults_relations at highest level, because there's not much to go on and it's not exact
    hh_relations_dt[is.na(age_range_3) & group_or_hh=="In households",
                    ("rel_adults_id"):=
                      paste0(tract,relation_hh,
                             as.character(1000000+sample(.N))),
                    by=.(tract,relation_hh)]
    adults_relations[age_range_3!="65 years and over",
                     ("rel_adults_id"):=
                       paste0(tract,relation_hh,
                              as.character(1000000+sample(.N))),
                     by=.(tract,relation_hh)]
    hh_relations_dt[is.na(age_range_3) & group_or_hh=="In households",
                    c("age_range_3","ethnicity","eth_sex_relations","eth_age_range",
                      "race","race_sex_relations","race_age_range","individual_id","ind_id_eth"):=
                      adults_relations[.SD, c(list(age_range_3),
                                              list(ethnicity),list(eth_sex_relations),list(eth_age_range),
                                              list(race),list(race_sex_relations),list(race_age_range),
                                              list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_adults_id)]]
    adults_relations[age_range_3!="65 years and over",
                     ("adults_matched"):=
                       hh_relations_dt[.SD,list(group_or_hh),
                                       on = .(rel_adults_id)]]
    
    #give the remaining ones (lives alone) from hh_relations the Householder again, to fill out - not sure why the originals didn't add up    
#    adults_relations[relation_hh=="Lives alone",("relation_hh"):="Householder Lives alone"]
#    hh_relations_dt[is.na(age_range_3) & str_detect(relation_hh,"Householder") & group_or_hh=="In households",
    hh_relations_dt[is.na(age_range_3) & group_or_hh=="In households" & !str_detect(relation_hh,"Child"),
                    ("rel_hh_id"):=
                      paste0(tract,as.character(1000000+sample(.N))),
                    by=.(tract)]
    #adults_relations[age_range_3!="65 years and over" & str_detect(relation_hh,"Householder"), 
    adults_relations[age_range_3!="65 years and over" & is.na(adults_matched),
                     ("rel_hh_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    hh_relations_dt[is.na(age_range_3) & str_detect(relation_hh,"Householder"),
                    c("age_range_3","ethnicity","eth_sex_relations","eth_age_range",
                      "race","race_sex_relations","race_age_range","individual_id","ind_id_eth"):= 
                      adults_relations[.SD, c(list(age_range_3),list(ethnicity),list(eth_sex_relations),
                                              list(eth_age_range),list(race),list(race_sex_relations),
                                              list(race_age_range),list(individual_id),list(ind_id_eth)), 
                                       on = .(rel_hh_id)]]
    #give age race/eth to group_quarters folks later, straight from missing_race/eth on sex_age_race/eth, although the fact that the
    #missing tracts are places where the jails are may mean we should juggle ages at the end in favor of this age group
    #test <= table(hh_relations_dt)
    
    #nrow(hh_relations_dt[!is.na(age_range_3)])-nrow(adults_relations) =~ -8k 
    #clean up ~ 1550 edge cases?
    hh_relations_dt[is.na(age_range_3) & str_detect(relative,"Parent"),("role_in_family") :="Son-in-law or daughter-in-law"]
    hh_relations_dt[is.na(age_range_3) & str_detect(relative,"Parent"),("relative") :="Son-in-law or daughter-in-law"]
    #length(unique(hh_relations_dt$individual_id))-nrow(sex_age_race[age>17]) #-48638
    #rm(adults_relations)
    
    #now do kids - giving all a possible race and age
    hh_type_kids[,("race_kids_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    hh_type_kids[,("eth_kids_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_age_race[age<18,("race_kids_id"):=
                   paste0(tract,as.character(1000000+sample(.N))),
                 by=.(tract)]
    sex_by_age_eth[age<18,("eth_kids_id"):=
                     paste0(tract,as.character(1000000+sample(.N))),
                   by=.(tract)]
    hh_type_kids[,c("race","race_sex_relations","race_age_range","individual_id"):=
                       sex_age_race[.SD, c(list(race),list(sex),list(age_range),list(individual_id)), on = .(race_kids_id)]]
    sex_age_race[age<18,("missing_race"):=
                   hh_type_kids[.SD, list(sex), on = .(race_kids_id)]]
    sex_age_race[age<18&is.na(missing_race),("missing_race"):="In group quarters"]
    sex_age_race[age<18,("missing_race"):=if_else(missing_race=="Male" | missing_race=="Female","In households","In group quarters")]
    hh_type_kids[,c("ethnicity","eth_sex_relations","eth_age_range","ind_id_eth"):=
                       sex_by_age_eth[.SD, c(list(ethnicity),list(sex),list(age_range),list(ind_id_eth)), on = .(eth_kids_id)]]
    sex_by_age_eth[age<18,("missing_eth"):=
                     hh_type_kids[.SD, list(eth_sex_relations), on = .(eth_kids_id)]]
    sex_by_age_eth[age<18&is.na(missing_eth),("missing_eth"):="In group quarters"]
    sex_by_age_eth[age<18,("missing_eth"):=if_else(missing_eth=="Male" | missing_eth=="Female","In households","In group quarters")]
    sex_age_race$race_kids_id <-NULL
    
    
    #hh_type_kids includes grandkids, but excludes hh, spouses, partners, and is everyone under 18 otherwise - marital should give that extra - could be less than 1k
    #hh_type_kids[family_or_non=="In nonfamily households",("in_family_type"):="In nonfamily households"]
    hh_type_kids[,("rel_kids_id"):= 
                   paste0(tract,family_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,family_or_non)]
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range_3),
                    ("rel_kids_id"):=
                      paste0(tract,family_or_non,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,family_or_non)]
    hh_relations_dt[!is.na(age_range_3),("age_range_4"):=age_range_3]
    hh_type_kids[,("age_range_4"):=list("0  to 17 years")]  #seems to need list type for later stuff
    #family_role_3 is a rewrite of in_family_type and matches sam_hh categories, but isn't only for Householders
    hh_relations_dt[group_or_hh=="In households" & is.na(age_range_3),
                    c("in_family_type","family_role_3","age_range_4",
                      "ethnicity","eth_sex_relations","eth_age_range",
                      "race","race_sex_relations","race_age_range","individual_id","ind_id_eth"):= 
                             hh_type_kids[.SD, c(list(in_family_type),list(family_role_3),list(age_range_4),
                                                 list(ethnicity),list(eth_sex_relations),list(eth_age_range),
                                                 list(race),list(race_sex_relations),list(race_age_range),
                                                 list(individual_id),list(ind_id_eth)), 
                                                         on = .(rel_kids_id)]]
    #test 4a
    #test <- table(hh_relations_dt$eth_age_range,hh_relations_dt$age_range_4)
    #sum(colSums(test==0/nrow(test)*100))==42
    #test <- table(hh_relations_dt$race_age_range,hh_relations_dt$age_range_4)
    #sum(colSums(test==0/nrow(test)*100))==42
        
    #length(unique(hh_relations_dt$individual_id))-nrow(sex_age_race) #57917 (~1.3%)
    #get missing from hh_relations, ethnicity side
    #add tract back in from sex_age_race/eth with that as final source for each
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
    #redo 4a
    #test<-table(hh_relations_dt$tract,hh_relations_dt$eth_age_range,hh_relations_dt$ethnicity,hh_relations_dt$eth_sex_relations)==
    #  table(sex_by_age_eth$tract,sex_by_age_eth$age_range,sex_by_age_eth$ethnicity,sex_by_age_eth$sex)
    #length(test[test==F])==0
    #test<-table(hh_relations_dt$tract,hh_relations_dt$race_age_range,hh_relations_dt$race,hh_relations_dt$race_sex_relations)==
    #  table(sex_age_race$tract,sex_age_race$age_range,sex_age_race$race,sex_age_race$sex)
    #length(test[test==F])==0
    
    #divide up eth/race for ease (should have done earlier)
    hh_relations_eth <- hh_relations_dt[,c("ind_id_eth","tract","ethnicity","eth_sex_relations","eth_age_range","age_range_3","age_range_4",
                                           "group_or_hh","family_or_non","relative","role_in_family","relation_hh",
                                           "sr_hh_living_alone","in_family_type")]
    hh_relations_race <- hh_relations_dt[,c("individual_id","tract","race","race_sex_relations","race_age_range","age_range_3","age_range_4",
                                            "group_or_hh","family_or_non","relative","role_in_family","relation_hh",
                                            "sr_hh_living_alone","in_family_type")]
    
    #now do place_born as a separate thread
    #sample for some, in order to get factor names to match; will over-write later
    sex_nativity_age_dt[,("age_range_14"):=case_when(
      age_range=="Under 5 years" ~ "0  to 4 years",
      age_range=="5 to 9 years" ~ "05  to 9 years",
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
    #looks like 5 tracts fewer in sex_place_when had matches than came out of sex_nativity. 
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
    sex_place_when_dt[str_detect(fb_date_entered,"20" & is.na(sn_age_range)),
                      ("sn_age2_id"):= 
                        paste0(tract,sex,
                               as.character(1000000+sample(.N))),
                      by=.(tract,sex)]
    sex_nativity_age_dt[is.na(missed_sn) & substr(age_range_14,1,2)<20,
                        ("sn_age2_id"):= 
                          paste0(tract,sex,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(tract,sex)]
    sex_place_when_dt[str_detect(fb_date_entered,"20" & is.na(sn_age_range)),
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
    #do without tract (last 9!)
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
    #length(test[test==F]) #it's a little under 2% off, because of that secondary matching above, but unavoidable and right totals.
    
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
                              "fb_origin_area","fb_origin_place","fb_origin_country"):=
                             sex_place_when_dt[.SD, c(list(sn_age_range),list(sex),list(fb_citizen),
                                                      list(fb_origin_continent),list(fb_origin_area),
                                                      list(fb_origin_place),list(fb_origin_country)), 
                                on = .(pb_age_fb_id)]]
    #test 7
    #test<-table(sex_place_when_dt$tract,
    #            sex_place_when_dt$fb_origin_continent)==
    #  table(place_born_age_full_dt[place_born=="Foreign born",tract],
    #        place_born_age_full_dt[place_born=="Foreign born",fb_origin_continent])
    #length(test[test==F])==2361
    
    #add ethnicity from latinx
    latinx_dt[,("ethnicity"):=if_else(latinx=="Hispanic or Latino","I",NULL)]
    #match will only be for foreign born
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
    place_born_eth_dt[!is.na(ethnicity),("pbea_id"):= 
                        paste0(tract,place_born,ethnicity,
                               as.character(1000000+sample(.N))),
                      by=.(tract,place_born,ethnicity)]
    place_born_age_full_dt[,("pbea_id"):= 
                             paste0(tract,place_born,ethnicity,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,place_born,ethnicity)]
    place_born_eth_dt[,c("age_range_14","fb_sex","fb_citizen",
                         "fb_origin_continent","fb_origin_area",
                         "fb_origin_place","fb_origin_country",
                         "family_origin","latinx_family_origin",
                         "latinx","ethnicity"):=
                        place_born_age_full_dt[.SD, c(list(age_range_14),
                                                      list(fb_sex),list(fb_citizen),list(fb_origin_continent),
                                                      list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                                      list(family_origin),list(latinx_family_origin),
                                                      list(latinx),list(ethnicity)), 
                                               on = .(pbea_id)]]
    place_born_age_full_dt[,c("missed_eth"):=
                             place_born_eth_dt[.SD, list(fb_origin_continent), 
                                               on = .(pbea_id)]]
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
                         "family_origin","latinx_family_origin",
                         "latinx","ethnicity"):=
                        place_born_age_full_dt[.SD, c(list(age_range_14),
                                                      list(fb_sex),list(fb_citizen),list(fb_origin_continent),
                                                      list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                                      list(family_origin),list(latinx_family_origin),
                                                      list(latinx),list(ethnicity)), 
                                               on = .(pbea1_id)]]
    place_born_age_full_dt[is.na(missed_eth),c("missed_eth"):=
                             place_born_eth_dt[.SD, list(age_range_14), 
                                               on = .(pbea1_id)]]
    #test 8
    #test <- table(place_born_age_full_dt$tract,
    #              place_born_age_full_dt$place_born,
    #              place_born_age_full_dt$ethnicity,
    #              place_born_age_full_dt$age_range_14)==
    #  table(place_born_eth_dt$tract,
    #        place_born_eth_dt$place_born,
    #        place_born_eth_dt$ethnicity,
    #        place_born_eth_dt$age_range_14)
    #length(test[test==F])==0
    
    #put place_born stuff on pb_eth/race  
    place_born_race_dt[,("pbra_id"):= 
                         paste0(tract,place_born,
                                as.character(1000000+sample(.N))),
                       by=.(tract,place_born)]
    place_born_age_full_dt[,("pbra_id"):= 
                             paste0(tract,place_born,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,place_born)]
    place_born_race_dt[,c("age_range_14","fb_sex","fb_citizen",
                          "fb_origin_continent","fb_origin_area",
                          "fb_origin_place","fb_origin_country",
                          "family_origin","latinx_family_origin",
                          "latinx","ethnicity"):=     #putting eth here should make join easier later...
                         place_born_age_full_dt[.SD, c(list(age_range_14),
                                                       list(fb_sex),list(fb_citizen),list(fb_origin_continent),
                                                       list(fb_origin_area),list(fb_origin_place),list(fb_origin_country),
                                                       list(family_origin),list(latinx_family_origin),
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

##                              #put eth/race builds on place_born by age
##                              #get fb sex matched by eth
##                              hh_relations_dt[,("hh_pb_age_eth_id"):= 
##                                                paste0(tract,eth_sex_relations,eth_age_range,  
##                                                       as.character(1000000+sample(.N))),
##                                              by=.(tract,eth_sex_relations,eth_age_range)]
##                              place_born_age_full_dt[!is.na(fb_sex),("hh_pb_age_eth_id"):= 
##                                                       paste0(tract,fb_sex,age_range_14,
##                                                              as.character(1000000+sample(.N))),
##                                                     by=.(tract,fb_sex,age_range_14)]
##                              hh_relations_dt[,c("place_born_eth","age_range_14_eth"):=  
##                                                place_born_age_full_dt[.SD, c(list(place_born),list(age_range_14)), 
##                                                                       on = .(hh_pb_age_eth_id)]]
##                              place_born_age_full_dt[,c("miss_eth"):= 
##                                                       hh_relations_dt[.SD, list(group_or_hh), 
##                                                                       on = .(hh_pb_age_eth_id)]]
##                              
##                              #get fb sex matched by race
##                              hh_relations_dt[,("hh_pb_age_race_id"):= 
##                                                paste0(tract,race_sex_relations,race_age_range,  
##                                                       as.character(1000000+sample(.N))),
##                                              by=.(tract,race_sex_relations,race_age_range)]
##                              place_born_age_full_dt[!is.na(fb_sex),("hh_pb_age_race_id"):= 
##                                                       paste0(tract,fb_sex,age_range_14,
##                                                              as.character(1000000+sample(.N))),
##                                                     by=.(tract,fb_sex,age_range_14)]
##                              hh_relations_dt[,c("place_born_race","age_range_14_race"):= 
##                                                place_born_age_full_dt[.SD, c(list(place_born),list(age_range_14)), 
##                                                                       on = .(hh_pb_age_race_id)]]
##                              place_born_age_full_dt[,c("miss_race"):= 
##                                  hh_relations_dt[.SD, list(group_or_hh), 
##                                                                       on = .(hh_pb_age_race_id)]]
##                              #then match non-fb, still sample
##                              #by eth
##                              hh_rel4_worke <- hh_relations_dt[is.na(place_born_eth)]
##                              pb_age_worke <- place_born_age_full_dt[is.na(miss_eth)]
##                              
##                              hh_rel4_worke[,("hh_pb_age_eth2_id"):= 
##                                                paste0(tract,eth_age_range,  
##                                                       as.character(1000000+sample(.N))),
##                                              by=.(tract,eth_age_range)]
##                              pb_age_worke[,("hh_pb_age_eth2_id"):= 
##                                                       paste0(tract,age_range_14,
##                                                              as.character(1000000+sample(.N))),
##                                                     by=.(tract,age_range_14)]
##                              hh_rel4_worke[,c("place_born_eth","age_range_14_eth"):=  
##                                              pb_age_worke[.SD, c(list(place_born),list(age_range_14)), 
##                                                                       on = .(hh_pb_age_eth2_id)]]
##                              hh_relations_dt[is.na(place_born_eth),c("place_born_eth","age_range_14_eth"):=  
##                                                hh_rel4_worke[.SD, c(list(place_born_eth),list(age_range_14_eth)), 
##                                                           on = .(ind_id_eth)]]
##                              pb_age_worke[,c("miss_eth"):= 
##                                             hh_rel4_worke[.SD, list(group_or_hh), 
##                                                                       on = .(hh_pb_age_eth2_id)]]
##                              place_born_age_full_dt[is.na(miss_eth),c("miss_eth"):= 
##                                                       pb_age_worke[.SD, list(miss_eth), 
##                                                           on = .(pbagefull_id)]]
##                              #by race
##                              hh_rel4_workr <- hh_relations_dt[is.na(place_born_race)]
##                              pb_age_workr <- place_born_age_full_dt[is.na(miss_race)]
##                              hh_rel4_workr[,("hh_pb_age_race2_id"):= 
##                                                paste0(tract,race_age_range,  
##                                                       as.character(1000000+sample(.N))),
##                                              by=.(tract,race_age_range)]
##                              pb_age_workr[,("hh_pb_age_race2_id"):= 
##                                                       paste0(tract,age_range_14,
##                                                              as.character(1000000+sample(.N))),
##                                                     by=.(tract,age_range_14)]
##                              hh_rel4_workr[,c("place_born_race","age_range_14_race"):= 
##                                              pb_age_workr[.SD, c(list(place_born),list(age_range_14)), 
##                                                                       on = .(hh_pb_age_race2_id)]]
##                              hh_relations_dt[is.na(place_born_race),c("place_born_race","age_range_14_race"):= 
##                                                       hh_rel4_workr[.SD, c(list(place_born_race),list(age_range_14_race)), 
##                                                           on = .(individual_id)]]
##                              pb_age_workr[,c("miss_race"):= 
##                                                       hh_rel4_workr[.SD, list(group_or_hh), 
##                                                                       on = .(hh_pb_age_race2_id)]]
##                              place_born_age_full_dt[is.na(miss_race),c("miss_race"):= 
##                                                       pb_age_workr[.SD, list(miss_race), 
##                                                           on = .(pbagefull_id)]]
##                              #test6a
##                              #test <- table(hh_relations_dt$eth_age_range,hh_relations_dt$age_range_14_eth)
##                              #sum(colSums(test==0/nrow(test)*100))==182
##                              #test <- table(hh_relations_dt$race_age_range,hh_relations_dt$age_range_14_race)
##                              #sum(colSums(test==0/nrow(test)*100))==169
##                              #nrow(hh_rel4_workr[is.na(place_born_race)]) #565619 
##                              #nrow(hh_rel4_worke[is.na(place_born_eth)]) #228011
##                              #test<-nrow(hh_relations_dt[is.na(place_born_eth)])==nrow(place_born_age_full_dt[is.na(miss_eth)])
##                              #test<-nrow(hh_relations_dt[is.na(place_born_race)])==nrow(place_born_age_full_dt[is.na(miss_race)])
##                              
##                              hhe_rel_tmp <- hh_relations_dt[is.na(place_born_eth)] #228011
##                              pbe_age_tmp <- place_born_age_full_dt[is.na(miss_eth)] #228011
##                              
##                              
##                              
##                              #need to get by sex - either resample by sex_age_race, etc. or by hh_relations on age_range_14?...
##                              #then match leftover, which isn't following age_range by sex in anything like a convenient way
##                              #match on tract and sex with seq.int - which will stop after fb_sex
##                              #first eth
##                              
##                              hhe_rel_tmp[order(eth_age_range),("hh_pb_age_id2"):= 
##                                            paste0(tract,eth_sex_relations,
##                                                   as.character(1000000+seq.int(1:.N))),
##                                          by=.(tract,eth_sex_relations)]
##                              pbe_age_tmp[order(age_range_14),("hh_pb_age_id2"):= 
##                                            paste0(tract,fb_sex,
##                                                   as.character(1000000+seq.int(1:.N))),
##                                          by=.(tract,fb_sex)]
##                              hhe_rel_tmp[,c("place_born_eth","age_range_14_eth"):=
##                                            pbe_age_tmp[.SD, c(list(place_born),list(age_range_14)), 
##                                                        on = .(hh_pb_age_id2)]]
##                              hh_relations_dt[is.na(place_born_eth),c("place_born_eth","age_range_14_eth"):=
##                                                hhe_rel_tmp[.SD, c(list(place_born_eth),list(age_range_14_eth)), 
##                                                            on = .(ind_id_eth)]]
##                              pbe_age_tmp[,c("miss_eth"):= 
##                                             hhe_rel_tmp[.SD, list(group_or_hh), 
##                                                           on = .(hh_pb_age_id2)]]
##                              place_born_age_full_dt[is.na(miss_eth),c("miss_eth"):= 
##                                                       pbe_age_tmp[.SD, list(miss_eth), 
##                                                                    on = .(pbagefull_id)]]
##                              #then race
##                              hhr_rel_tmp <- hh_relations_dt[is.na(place_born_race)]
##                              pbr_age_tmp <- place_born_age_full_dt[is.na(miss_race)]
##                              hhr_rel_tmp[order(race_age_range),("hh_pb_age_id3"):= 
##                                            paste0(tract,race_sex_relations,
##                                                   as.character(1000000+seq.int(1:.N))),
##                                          by=.(tract,race_sex_relations)]
##                              pbr_age_tmp[order(age_range_14),("hh_pb_age_id3"):= 
##                                            paste0(tract,fb_sex,
##                                                   as.character(1000000+seq.int(1:.N))),
##                                          by=.(tract,fb_sex)]
##                              hhr_rel_tmp[,c("place_born_race","age_range_14_race"):=
##                                            pbr_age_tmp[.SD, c(list(place_born),list(age_range_14)), 
##                                                        on = .(hh_pb_age_id3)]]
##                              hh_relations_dt[is.na(place_born_race),c("place_born_race","age_range_14_race"):=
##                                                hhr_rel_tmp[.SD, c(list(place_born_race),list(age_range_14_race)), 
##                                                            on = .(individual_id)]]
##                              pbr_age_tmp[,c("miss_race"):= 
##                                            hhr_rel_tmp[.SD, list(group_or_hh), 
##                                                        on = .(hh_pb_age_id3)]]
##                              place_born_age_full_dt[is.na(miss_race),c("miss_race"):= 
##                                                       pbr_age_tmp[.SD, list(miss_race), 
##                                                                   on = .(pbagefull_id)]]
##                              
##                              #then rest
##                              #first eth
##                              hhe2_rel_tmp <- hh_relations_dt[is.na(place_born_eth)]
##                              pbe2_age_tmp <- place_born_age_full_dt[is.na(miss_eth)]
##                              hhe2_rel_tmp[order(eth_age_range),("hh_pb_age_id4"):= 
##                                                paste0(tract,
##                                                       as.character(1000000+seq.int(1:.N))),
##                                              by=.(tract)]
##                              pbe2_age_tmp[order(age_range_14),("hh_pb_age_id4"):= 
##                                                       paste0(tract,
##                                                              as.character(1000000+seq.int(1:.N))),
##                                                     by=.(tract)]
##                              hhe2_rel_tmp[,c("place_born_eth","age_range_14_eth"):=
##                                           pbe2_age_tmp[.SD, c(list(place_born),list(age_range_14)), 
##                                                           on = .(hh_pb_age_id4)]]
##                              hh_relations_dt[is.na(place_born_eth),c("place_born_eth","age_range_14_eth"):=
##                                                hhe2_rel_tmp[.SD, c(list(place_born_eth),list(age_range_14_eth)), 
##                                                                       on = .(ind_id_eth)]]
##                              #then race
##                              hhr2_rel_tmp <- hh_relations_dt[is.na(place_born_race)]
##                              pbr2_age_tmp <- place_born_age_full_dt[is.na(miss_race)]
##                              hhr2_rel_tmp[order(race_age_range),("hh_pb_age_id5"):= 
##                                            paste0(tract,
##                                                   as.character(1000000+seq.int(1:.N))),
##                                          by=.(tract)]
##                              pbr2_age_tmp[order(age_range_14),("hh_pb_age_id5"):= 
##                                            paste0(tract,
##                                                   as.character(1000000+seq.int(1:.N))),
##                                          by=.(tract)]
##                              hhr2_rel_tmp[,c("place_born_race","age_range_14_race"):=
##                                            pbr2_age_tmp[.SD, c(list(place_born),list(age_range_14)), 
##                                                        on = .(hh_pb_age_id5)]]
##                              hh_relations_dt[is.na(place_born_race),c("place_born_race","age_range_14_race"):=
##                                                hhr2_rel_tmp[.SD, c(list(place_born_race),list(age_range_14_race)), 
##                                                           on = .(individual_id)]]
##                              #test 6b
##                              #test <- nrow(hh_relations_dt[is.na(place_born_race)])==0
##                              #test <- nrow(hh_relations_dt[is.na(place_born_eth)])==0
##                              #test <- table(hh_relations_dt$tract,hh_relations_dt$place_born_eth,hh_relations_dt$age_range_14_eth)==
##                              #  table(place_born_age_full_dt$tract,place_born_age_full_dt$place_born,place_born_age_full_dt$age_range_14)
##                              #test <- table(hh_relations_dt$tract,hh_relations_dt$place_born_race,hh_relations_dt$age_range_14_race)==
##                              #  table(place_born_age_full_dt$tract,place_born_age_full_dt$place_born,place_born_age_full_dt$age_range_14)
##                              #length(test[test==F])==0
##                              rm(pbr_age_tmp)
##                              rm(hhr_rel_tmp)
##                              rm(pbe_age_tmp)
##                              rm(hhe_rel_tmp)
##                              rm(pbr2_age_tmp)
##                              rm(hhr2_rel_tmp)
##                              rm(pbe2_age_tmp)
##                              rm(hhe2_rel_tmp)
##    #saveRDS(hh_relations_dt,file = paste0(housingdir, vintage, "/hh_relations_dt_l.701_",Sys.Date(),".RDS"))
    
     
    #add fb_origin (foreign_born_origin) onto place_born_age - origin and sex_place_when_dt are both on foreign_born, only
    #should come up with a marker for when one is losing information - this will not get at how long folks have been here, by itself.
    hh_relations_race[place_born_race=="Foreign born",("fb_origin_id"):= 
                             paste0(tract,
                                    as.character(1000000+sample(.N))),
                           by=.(tract)]
    hh_relations_eth[place_born_eth=="Foreign born",("fb_origin_id"):= 
                        paste0(tract,
                               as.character(1000000+sample(.N))),
                      by=.(tract)]
    origin_data_dt[,("fb_origin_id"):= 
                     paste0(tract,
                            as.character(1000000+sample(.N))),
                   by=.(tract)]
    hh_relations_race[place_born_race=="Foreign born",c("fb_origin_continent","fb_origin_area","fb_origin_region","fb_origin_country"):=
                             origin_data_dt[.SD, c(list(origin_continent),list(origin_area),list(origin_region),list(origin_country)), 
                                            on = .(fb_origin_id)]]
    hh_relations_eth[place_born_eth=="Foreign born",c("fb_origin_continent","fb_origin_area","fb_origin_region","fb_origin_country"):=
                        origin_data_dt[.SD, c(list(origin_continent),list(origin_area),list(origin_region),list(origin_country)), 
                                       on = .(fb_origin_id)]]
    #test 7
    #tester <- hh_relations_eth[tract%in%unique(origin_data_dt$tract)]
    ##too big#test <- table(tester$tract,tester$fb_origin_area,tester$fb_origin_continent,tester$fb_origin_region,tester$fb_origin_country)==
    ##too big#  table(origin_data_dt$tract,origin_data_dt$origin_area,origin_data_dt$origin_continent,origin_data_dt$origin_region,origin_data_dt$origin_country)
    ##too big#length(test[test==F])==0
    #test <- table(tester$tract,tester$fb_origin_area,tester$fb_origin_country)==
    #  table(origin_data_dt$tract,origin_data_dt$origin_area,origin_data_dt$origin_country)
    #length(test[test==F])==0
    #tester <- hh_relations_race[tract%in%unique(origin_data_dt$tract)]
    #test <- table(tester$tract,tester$fb_origin_area,tester$fb_origin_country)==
    #  table(origin_data_dt$tract,origin_data_dt$origin_area,origin_data_dt$origin_country)
    #length(test[test==F])==0
    
    #add when_came - lose some info
    hh_relations_eth[place_born_eth=="Foreign born",("fb_origin_region"):=case_when(
      str_detect(fb_origin_area,"Asia") ~ "Asia",
      str_detect(fb_origin_area,"Europe") ~ "Europe",
      TRUE ~ "Other areas"
    )]
    hh_relations_race[place_born_race=="Foreign born",("fb_origin_region"):=case_when(
      str_detect(fb_origin_area,"Asia") ~ "Asia",
      str_detect(fb_origin_area,"Europe") ~ "Europe",
      TRUE ~ "Other areas"
    )]
    #origin_area works for sex_place_when and place_period, but needed to match hh_relations
    sex_place_when_dt[,("fb_origin_region"):=case_when(  
      origin_region=="Asia"  ~ "Asia",
      origin_region=="Europe" ~ "Europe",
      TRUE ~ "Other areas"
    )]
    place_period_citizen_dt[,("fb_origin_region"):=case_when(
      origin_area=="Asia"  ~ "Asia",
      origin_area=="Europe" ~ "Europe",
      TRUE ~ "Other areas"
    )]
    
    
    
    #matches to household_relatives
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

    #test 8  
    #test <- table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)==
    #  table(hh_relations_race$tract,hh_relations_race$race_sex_relations)
    #length(test[test==F])==0
    #test <- table(sex_by_age_eth$tract,sex_by_age_eth$sex)==
    #  table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)
    #length(test[test==F])==0
    #test <- table(hh_relations_eth$tract,hh_relations_eth$age_range_14_eth)==
    #  table(hh_relations_race$tract,hh_relations_race$age_range_14_race)
    #length(test[test==F])==0
    ###the crosstab is false - ugh!!! I dropped it somewhere!!
    #test <- table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations,hh_relations_eth$age_range_14_eth)==
    #  table(hh_relations_race$tract,hh_relations_race$race_sex_relations,hh_relations_race$age_range_14_race)
    #length(test[test==F])/length(test)#55% of tracts are off by just a handful - can be explained by sampling to age_range_14
    ##but these each are true, so much better
    #test <- table(sex_by_age_eth$tract,sex_by_age_eth$ethnicity,sex_by_age_eth$sex,sex_by_age_eth$age_range)==
    #  table(hh_relations_eth$tract,hh_relations_eth$ethnicity,hh_relations_eth$eth_sex_relations,hh_relations_eth$eth_age_range)
    #length(test[test==F])==0
    #test <- table(sex_age_race$tract,sex_age_race$race,sex_age_race$sex,sex_age_race$age_range)==
    #  table(hh_relations_race$tract,hh_relations_race$race,hh_relations_race$race_sex_relations,hh_relations_race$race_age_range)
    #length(test[test==F])==0

    #put when_entered for eth/race
    sex_place_when_dt[,("when_total_id"):=paste0(tract,as.character(1000000+sample(.N))),
                      by=.(tract)]
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(eth_age_range,1,2))<18,
                     ("wheny_id"):= 
                       paste0(tract,eth_sex_relations,fb_origin_region,
                              as.character(1000000+sample(.N))),
                     by=.(tract,eth_sex_relations,fb_origin_region)]
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(race_age_range,1,2))<18,
                      ("wheny_id"):= 
                        paste0(tract,race_sex_relations,fb_origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,race_sex_relations,fb_origin_region)]
    sex_place_when_dt[fb_date_entered=="Entered 2010 or later",
                      ("wheny_id"):= 
                        paste0(tract,sex,fb_origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,sex,fb_origin_region)]
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(eth_age_range,1,2))<18,
                     c("fb_date_entered","when_origin_country","fb_citizen"):= 
                       sex_place_when_dt[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                         on = .(wheny_id)]]
    sex_place_when_dt[fb_date_entered=="Entered 2010 or later",("missing_eth"):=
                        hh_relations_eth[.SD, c(fb_date_entered), on = .(wheny_id)]]
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(race_age_range,1,2))<18,
                      c("fb_date_entered","when_origin_country","fb_citizen"):= 
                        sex_place_when_dt[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                          on = .(wheny_id)]]
    sex_place_when_dt[fb_date_entered=="Entered 2010 or later",("missing_race"):=
                        hh_relations_race[.SD, c(fb_date_entered), on = .(wheny_id)]]
    #test <- nrow(hh_relations_eth[!is.na(fb_date_entered)])==nrow(sex_place_when_dt[!is.na(missing_eth)])
    #test <- nrow(hh_relations_race[!is.na(fb_date_entered)])==nrow(sex_place_when_dt[!is.na(missing_race)])
    #up through 30
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(eth_age_range,1,2))<31
                     & as.numeric(substr(eth_age_range,1,2))>17,
                    ("when_id"):= 
                      paste0(tract,eth_sex_relations,fb_origin_region,
                             as.character(1000000+sample(.N))),
                    by=.(tract,eth_sex_relations,fb_origin_region)]
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(race_age_range,1,2))<31
                      & as.numeric(substr(race_age_range,1,2))>17,
                     ("whenr_id"):= 
                       paste0(tract,race_sex_relations,fb_origin_region,
                              as.character(1000000+sample(.N))),
                     by=.(tract,race_sex_relations,fb_origin_region)]
    sex_place_when_dt[!str_detect(fb_date_entered,"1990") & is.na(missing_eth),  
                      ("when_id"):= 
                          paste0(tract,sex,fb_origin_region,
                                 as.character(1000000+sample(.N))),
                        by=.(tract,sex,fb_origin_region)]
    sex_place_when_dt[!str_detect(fb_date_entered,"1990") & is.na(missing_race),  
                      ("whenr_id"):= 
                        paste0(tract,sex,fb_origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,sex,fb_origin_region)]
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(eth_age_range,1,2))<31
                     & as.numeric(substr(eth_age_range,1,2))>17,
                    c("fb_date_entered","when_origin_country","fb_citizen"):= 
                      sex_place_when_dt[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                          on = .(when_id)]]
    sex_place_when_dt[!str_detect(fb_date_entered,"1990") & is.na(missing_eth),("missing_eth"):=
                        hh_relations_eth[.SD, c(fb_date_entered), on = .(when_id)]]
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(race_age_range,1,2))<31
                      & as.numeric(substr(race_age_range,1,2))>17,
                     c("fb_date_entered","when_origin_country","fb_citizen"):= 
                       sex_place_when_dt[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                         on = .(whenr_id)]]
    sex_place_when_dt[!str_detect(fb_date_entered,"1990") & is.na(missing_race),("missing_race"):=
                        hh_relations_race[.SD, c(fb_date_entered), on = .(whenr_id)]]
    #test <- nrow(hh_relations_eth[!is.na(fb_date_entered)])==nrow(sex_place_when_dt[!is.na(missing_eth)])
    #test <- nrow(hh_relations_race[!is.na(fb_date_entered)])==nrow(sex_place_when_dt[!is.na(missing_race)])
    #test 8b
    #nrow(hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(race_age_range,1,2))<30])-
    #    nrow(hh_relations_race[!is.na(fb_date_entered)])  #~8k of ~295k
    #nrow(hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(eth_age_range,1,2))<30])-
    #  nrow(hh_relations_eth[!is.na(fb_date_entered)])  #~8k of ~295k
    #get older crowd - ethnicity
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(eth_age_range,1,2))>30,
                     ("when_eth1_id"):= 
                      paste0(tract,eth_sex_relations,fb_origin_region,
                             as.character(1000000+sample(.N))),
                    by=.(tract,eth_sex_relations,fb_origin_region)]
    sex_place_when_dt[is.na(missing_eth),
                      ("when_eth1_id"):= 
                        paste0(tract,sex,fb_origin_region,
                               as.character(1000000+seq.int(1:.N))),
                      by=.(tract,sex,fb_origin_region)]
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(eth_age_range,1,2))>30,
                    c("fb_date_entered","when_origin_country","fb_citizen"):= 
                      sex_place_when_dt[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                        on = .(when_eth1_id)]]
    sex_place_when_dt[is.na(missing_eth),("missing_eth"):=  #this should also capture ones that were NA going other way
                        hh_relations_eth[.SD, c(fb_date_entered), on = .(when_eth1_id)]]
    #and on race
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(race_age_range,1,2))>30,("when_race1_id"):= 
                       paste0(tract,race_sex_relations,fb_origin_region,
                              as.character(1000000+sample(.N))),
                     by=.(tract,race_sex_relations,fb_origin_region)]
    sex_place_when_dt[is.na(missing_race),("when_race1_id"):= 
                        paste0(tract,sex,fb_origin_region,
                               as.character(1000000+seq.int(1:.N))),
                      by=.(tract,sex,fb_origin_region)]
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(race_age_range,1,2))>30,
                     c("fb_date_entered","when_origin_country","fb_citizen"):= #raw numbers don't match in census files!!! need to check and choose!!
                       sex_place_when_dt[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                         on = .(when_race1_id)]]
    sex_place_when_dt[is.na(missing_race),("missing_race"):=
                        hh_relations_race[.SD, c(fb_date_entered), on = .(when_race1_id)]]
    #test <- nrow(hh_relations_eth[!is.na(fb_date_entered)])==nrow(sex_place_when_dt[!is.na(missing_eth)])
    #test <- nrow(hh_relations_race[!is.na(fb_date_entered)])==nrow(sex_place_when_dt[!is.na(missing_race)])
    #take remainders and rematch on sex_place_when to rejoin
    hh_pwr <- hh_relations_race[is.na(fb_date_entered) & place_born_race=="Foreign born"] #]# get all for match#
    spwr <- sex_place_when_dt[is.na(missing_race)]
    hh_pwe <- hh_relations_eth[is.na(fb_date_entered) & place_born_eth=="Foreign born"]
    spwe <- sex_place_when_dt[is.na(missing_eth)]
    #test 8c
    #test <- table(hh_pwr$tract,hh_pwr$fb_origin_region) == table(spwr$tract,spwr$fb_origin_region)
    #test <- table(hh_pwe$tract,hh_pwe$fb_origin_region) == table(spwe$tract,spwe$fb_origin_region)
    #length(test[test==F])==0
    
    #rematch so that sex_place_when is respected on date_entered; really just getting sex back in place at this point
    hh_pwr[order(match(place_born_race,c("Foreign born","Native; born outside the United States",
                                         "Born in other state in the United States","Born in state of residence"))),
           ("pwr_id"):=paste0(tract,race_sex_relations,
                               as.character(1000000+seq.int(1:.N))),
           by=.(tract,race_sex_relations)]
    spwr[,("pwr_id"):=paste0(tract,sex,
                               as.character(1000000+seq.int(1:.N))),
           by=.(tract,sex)]
    hh_pwr[,("fb_origin_region"):=spwr[.SD,list(origin_region),on=.(pwr_id)]]
    hh_pwe[order(match(place_born_eth,c("Foreign born","Native; born outside the United States",
                                         "Born in other state in the United States","Born in state of residence"))),
           ("pwe_id"):=paste0(tract,eth_sex_relations,
                               as.character(1000000+seq.int(1:.N))),
           by=.(tract,eth_sex_relations)]
    spwe[,("pwe_id"):=paste0(tract,sex,
                             as.character(1000000+seq.int(1:.N))),
         by=.(tract,sex)]
    hh_pwe[,("fb_origin_region"):=spwe[.SD,list(fb_origin_region),on=.(pwe_id)]]
    #this didn't help as much as I had hoped, but gets several thousand redone
    #then redo above >29 <30 
    hh_pwe[as.numeric(substr(eth_age_range,1,2))<31,
                     ("when2_id"):= 
                       paste0(tract,fb_origin_region,
                              as.character(1000000+sample(.N))),
                     by=.(tract,fb_origin_region)]
    hh_pwr[as.numeric(substr(race_age_range,1,2))<31,
                      ("when2_id"):= 
                        paste0(tract,fb_origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,fb_origin_region)]
    spwe[!str_detect(fb_date_entered,"1990"),("when2_id"):= 
                        paste0(tract,fb_origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,fb_origin_region)]
    spwr[!str_detect(fb_date_entered,"1990"),("when2_id"):= 
           paste0(tract,fb_origin_region,
                  as.character(1000000+sample(.N))),
         by=.(tract,fb_origin_region)]
    hh_pwe[as.numeric(substr(eth_age_range,1,2))<31,
                     c("fb_date_entered","when_origin_country","fb_citizen"):= 
             spwe[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                         on = .(when2_id)]]
    spwe[!str_detect(fb_date_entered,"1990"),("missing_eth"):=
           hh_pwe[.SD, c(fb_date_entered), on = .(when2_id)]]
    hh_pwr[as.numeric(substr(race_age_range,1,2))<31,
                      c("fb_date_entered","when_origin_country","fb_citizen"):= 
             spwr[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                          on = .(when2_id)]]
    spwr[!str_detect(fb_date_entered,"1990"),("missing_race"):=
           hh_pwr[.SD, list(fb_date_entered), on = .(when2_id)]]
    #get older crowd - ethnicity
    hh_pwe[as.numeric(substr(eth_age_range,1,2))>30,
           ("when_eth2_id"):= 
                       paste0(tract,fb_origin_region,
                              as.character(1000000+sample(.N))),
                     by=.(tract,fb_origin_region)]
    spwe[is.na(missing_eth),("when_eth2_id"):= 
                        paste0(tract,fb_origin_region,
                               as.character(1000000+seq.int(1:.N))),
                      by=.(tract,fb_origin_region)]
    hh_pwe[as.numeric(substr(eth_age_range,1,2))>30,
                     c("fb_date_entered","when_origin_country","fb_citizen"):= 
             spwe[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                         on = .(when_eth2_id)]]
    spwe[is.na(missing_eth),("missing_eth"):=
                        hh_pwe[.SD, c(fb_date_entered), on = .(when_eth2_id)]]
    #and on race
    hh_pwr[as.numeric(substr(race_age_range,1,2))>30,("when_race2_id"):= 
                        paste0(tract,fb_origin_region,
                               as.character(1000000+sample(.N))),
                      by=.(tract,fb_origin_region)]
    spwr[is.na(missing_race),("when_race2_id"):= 
                        paste0(tract,fb_origin_region,
                               as.character(1000000+seq.int(1:.N))),
                      by=.(tract,fb_origin_region)]
    hh_pwr[as.numeric(substr(race_age_range,1,2))>30,
                      c("fb_date_entered","when_origin_country","fb_citizen"):= 
             spwr[.SD, c(list(fb_date_entered),list(origin_country),list(fb_citizen)), 
                                          on = .(when_race2_id)]]
    spwr[is.na(missing_race),("missing_race"):=
                        hh_pwr[.SD, c(fb_date_entered), on = .(when_race2_id)]]
    #then add back on individual_id and ind_id_eth
    hh_relations_race[place_born_race=="Foreign born" & is.na(fb_date_entered),
                      c("fb_date_entered","when_origin_country","fb_citizen"):= 
                        hh_pwr[.SD, c(list(fb_date_entered),list(when_origin_country),list(fb_citizen)), 
                               on = .(individual_id)]]
    sex_place_when_dt[is.na(missing_race),("missing_race"):=
                        spwr[.SD, c(missing_race), on = .(when_total_id)]]
    hh_relations_eth[place_born_eth=="Foreign born" & is.na(fb_date_entered),
                      c("fb_date_entered","when_origin_country","fb_citizen"):= 
                        hh_pwe[.SD, c(list(fb_date_entered),list(when_origin_country),list(fb_citizen)), 
                               on = .(ind_id_eth)]]
    sex_place_when_dt[is.na(missing_eth),("missing_eth"):=
                        spwe[.SD, c(missing_eth), on = .(when_total_id)]]
    #test <- nrow(hh_relations_eth[!is.na(fb_date_entered)])-nrow(sex_place_when_dt[!is.na(missing_eth)])==0
    #test <- nrow(hh_relations_race[!is.na(fb_date_entered)])-nrow(sex_place_when_dt[!is.na(missing_race)])==0
    
    #p/u missing as ordered seq.int
    leftover_hh <- hh_relations_race[place_born_race=="Foreign born" & is.na(fb_date_entered)]
    leftover_hhe <- hh_relations_eth[place_born_eth=="Foreign born" & is.na(fb_date_entered)]
    leftover_spw <- sex_place_when_dt[is.na(missing_race)]
    leftover_spw[,("d_e"):=if_else(fb_date_entered=="Entered before 1990","1990",fb_date_entered)]
    leftover_spwe <- sex_place_when_dt[is.na(missing_eth)]
    leftover_spwe[,("d_e"):=if_else(fb_date_entered=="Entered before 1990","1990",fb_date_entered)]
    leftover_hh[order(race_sex_relations,race_age_range),("leftov_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),
                by=.(tract)]
    leftover_hhe[order(eth_sex_relations,eth_age_range),("leftov_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),
                by=.(tract)]
    leftover_spw[order(sex,d_e),("leftov_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),
                by=.(tract)]
    leftover_spwe[order(sex,d_e),("leftov_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),
                by=.(tract)]
    leftover_hh[,c("fb_date_entered","when_origin_country","fb_citizen"):= 
                  leftover_spw[.SD, c(list(fb_date_entered),list(when_origin_country),list(fb_citizen)), 
                               on = .(leftov_id)]]
    leftover_hhe[,c("fb_date_entered","when_origin_country","fb_citizen"):= 
                  leftover_spwe[.SD, c(list(fb_date_entered),list(when_origin_country),list(fb_citizen)), 
                               on = .(leftov_id)]]
    hh_relations_race[place_born_race=="Foreign born" & is.na(fb_date_entered),
                      c("fb_date_entered","when_origin_country","fb_citizen"):= 
                        leftover_hh[.SD, c(list(fb_date_entered),list(when_origin_country),list(fb_citizen)), 
                                    on = .(individual_id)]]
    hh_relations_eth[place_born_eth=="Foreign born" & is.na(fb_date_entered),
                     c("fb_date_entered","when_origin_country","fb_citizen"):= 
                       leftover_hhe[.SD, c(list(fb_date_entered),list(when_origin_country),list(fb_citizen)), 
                                    on = .(ind_id_eth)]]
    #test 8d
    #test <- nrow(hh_relations_eth[!is.na(fb_date_entered)])==nrow(sex_place_when_dt[!is.na(fb_date_entered)])
    #fix a few outliers #make number to move and put older folks in earlier date_entered matching the ones moved out below??? (10k total?)
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(age_range_14_eth,1,2))<15,
                     ("fb_date_entered"):="Entered 2010 or later"]
    hh_relations_eth[place_born_eth=="Foreign born" & as.numeric(substr(age_range_14_eth,1,2))<25 &
                       str_detect(fb_date_entered,"90"),
                     ("fb_date_entered"):=sample(c("Entered 2000 to 2009","Entered 2010 or later"),.N,prob = c(.6,.4),replace = T)]
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(age_range_14_race,1,2))<15,
                     ("fb_date_entered"):="Entered 2010 or later"]
    hh_relations_race[place_born_race=="Foreign born" & as.numeric(substr(age_range_14_race,1,2))<25 &
                       str_detect(fb_date_entered,"90"),
                     ("fb_date_entered"):=sample(c("Entered 2000 to 2009","Entered 2010 or later"),.N,prob = c(.6,.4),replace = T)]
    
    
    #nrow(hh_relations_eth[!is.na(in_family_type)])-nrow(hh_type_kids[family_or_non=="In family households"]) - missing about 5317 / remember 7468 in non-family hh
    
    #align date_entered with language - this will break using it for other years, so need to check when using elsewhere
    hh_relations_eth[order(match(fb_date_entered,c("Entered before 1990","Entered 1990 to 1999",
                                                   "Entered 2000 to 2009","Entered 2010 or later"))),
                    ("pbl_id"):= 
                      paste0(tract,place_born_eth,
                             as.character(1000000+seq.int(1:.N))),
                    by=.(tract,place_born_eth)]
    hh_relations_race[order(match(fb_date_entered,c("Entered before 1990","Entered 1990 to 1999",
                                                   "Entered 2000 to 2009","Entered 2010 or later"))),
                     ("pbl_id"):= 
                       paste0(tract,place_born_race,
                              as.character(1000000+seq.int(1:.N))),
                     by=.(tract,place_born_race)]
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
    #test <- table(place_born_language_dt$fb_language_at_home)==table(hh_relations_dt$fb_language_at_home)
    #there are some surprising things in the original data, like 140051 born here who speak English less than very well
    #I copied it over - perhaps should tie to ethnicity and language? have to see how much the tract differences help!!!
    #saveRDS(hh_relations_eth,file = paste0(housingdir, vintage, "/hh_relations_eth_l.1247_",Sys.Date(),".RDS"))
    #saveRDS(hh_relations_race,file = paste0(housingdir, vintage, "/hh_relations_race_l.1248_",Sys.Date(),".RDS"))
    

    #add relatives info, but keep race/eth from place_born - na = group_quarters
    
    #finishing the rest of in_family_type by matching hh_relations to household_relatives, then matching with pb
    #first with in_family_type, to pick up those matches from kids, then the rest
    hh_relations_race[group_or_hh=="In households",("ift_id"):=
                        paste0(tract,race,family_or_non,in_family_type,
                               as.character(1000000+sample(.N))),
                      by=.(tract,race,family_or_non,in_family_type)]
    household_relatives_race_dt[,("ift_id"):=
                        paste0(tract,race,family_or_non,in_family_type,
                               as.character(1000000+sample(.N))),
                      by=.(tract,race,family_or_non,in_family_type)]
    household_relatives_race_dt[,c("matched"):=
                                  hh_relations_race[.SD, list(family_or_non), 
                                               on = .(ift_id)]]
    hh_relations_eth[group_or_hh=="In households",("ift_id"):=
                        paste0(tract,ethnicity,family_or_non,in_family_type,
                               as.character(1000000+sample(.N))),
                      by=.(tract,ethnicity,family_or_non,in_family_type)]
    household_relatives_eth_dt[,("ift_id"):=
                        paste0(tract,ethnicity,family_or_non,in_family_type,
                               as.character(1000000+sample(.N))),
                      by=.(tract,ethnicity,family_or_non,in_family_type)]
    household_relatives_eth_dt[,c("matched"):=
                                  hh_relations_eth[.SD, list(family_or_non), 
                                                    on = .(ift_id)]]
    #see if having all the in_family_type helps - 
    
    #then do without in_family_type
    hh_relations_race[is.na(in_family_type) & group_or_hh=="In households",("ift2_id"):=
                        paste0(tract,race,family_or_non,
                               as.character(1000000+sample(.N))),
                      by=.(tract,race,family_or_non)]
    household_relatives_race_dt[is.na(matched),("ift2_id"):=
                                  paste0(tract,race,family_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,race,family_or_non)]
    hh_relations_race[is.na(in_family_type) & group_or_hh=="In households",
                      c("in_family_type"):=
                        household_relatives_race_dt[.SD,list(in_family_type),
                                                    on = .(ift2_id)]]
    household_relatives_race_dt[is.na(matched),
                                c("matched"):=
                                 hh_relations_race[.SD, list(family_or_non), 
                                                  on = .(ift2_id)]]
    hh_relations_eth[is.na(in_family_type) & group_or_hh=="In households",
                     ("ift2_id"):=
                        paste0(tract,ethnicity,family_or_non,
                               as.character(1000000+sample(.N))),
                      by=.(tract,ethnicity,family_or_non)]
    household_relatives_eth_dt[is.na(matched),("ift2_id"):=
                                  paste0(tract,ethnicity,family_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,ethnicity,family_or_non)]
    hh_relations_eth[is.na(in_family_type) & group_or_hh=="In households",
                     c("in_family_type"):=
                        household_relatives_eth_dt[.SD,list(in_family_type),
                                                    on = .(ift2_id)]]
    household_relatives_eth_dt[is.na(matched),c("matched"):=
                                 hh_relations_eth[.SD, list(family_or_non), 
                                                  on = .(ift2_id)]]
    #just like marital, rematch so that race/eth is rewritten from household_relatives, etc.
    #the numbers don't quite match, so don't get a perfect match, but closer
    anti_householdrr <- household_relatives_race_dt[is.na(matched)]
    anti_hh_r <- hh_relations_race[is.na(in_family_type) & group_or_hh=="In households"]
    anti_householdrr[!is.na(in_family_type),("anti_rel_id"):=  #letting it vary on race, so we can do that match
               paste0(tract,family_or_non,
                      as.character(1000000+sample(.N))),
             by=.(tract,family_or_non)]
    anti_hh_r[,("anti_rel_id"):=  
               paste0(tract,family_or_non,
                      as.character(1000000+sample(.N))),
             by=.(tract,family_or_non)]
    anti_hh_r[,c("race","in_family_type"):=
               anti_householdrr[.SD,c(list(race),list(in_family_type)),on=.(anti_rel_id)]]
    #now back to whole
    hh_relations_race[is.na(in_family_type) & group_or_hh=="In households",
                      c("race","in_family_type"):=
                        anti_hh_r[.SD,c(list(race),list(in_family_type)),on=.(ift2_id)]]
    
    
    
    
 
    anti_householdre <- household_relatives_eth_dt[is.na(matched)]
    anti_hh_e <- hh_relations_eth[is.na(in_family_type) & group_or_hh=="In households"]
    
    
    #give all stragglers with only family_or_non - about 100k left to assign
    hh_relations_race[is.na(in_family_type) & group_or_hh=="In households",("ift3_id"):=
                        paste0(tract,family_or_non,
                               as.character(1000000+sample(.N))),
                      by=.(tract,family_or_non)]
    household_relatives_race_dt[is.na(matched),("ift3_id"):=
                                  paste0(tract,family_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,family_or_non)]
    hh_relations_race[is.na(in_family_type) & group_or_hh=="In households",c("in_family_type"):=
                        household_relatives_race_dt[.SD,list(in_family_type),
                                                    on = .(ift3_id)]]
    household_relatives_race_dt[is.na(matched),c("matched"):=
                                  hh_relations_race[.SD, list(family_or_non), 
                                                    on = .(ift3_id)]]
    hh_relations_eth[is.na(in_family_type) & group_or_hh=="In households",("ift3_id"):=
                       paste0(tract,family_or_non,
                              as.character(1000000+sample(.N))),
                     by=.(tract,family_or_non)]
    household_relatives_eth_dt[is.na(matched),("ift3_id"):=
                                 paste0(tract,family_or_non,
                                        as.character(1000000+sample(.N))),
                               by=.(tract,family_or_non)]
    hh_relations_eth[is.na(in_family_type) & group_or_hh=="In households",c("in_family_type"):=
                       household_relatives_eth_dt[.SD,list(in_family_type),
                                                  on = .(ift3_id)]]
    household_relatives_eth_dt[is.na(matched),c("matched"):=
                                 hh_relations_eth[.SD, list(family_or_non), 
                                                  on = .(ift3_id)]]
    
    #match on race/eth and age_range for place_born
    place_born_race_dt[,("pbrel_id"):= 
                         paste0(tract,race,age_range,
                                as.character(1000000+sample(.N))),
                       by=.(tract,race,age_range)]
    place_born_eth_dt[,("pbrel_id"):= 
                        paste0(tract,ethnicity,age_range,
                               as.character(1000000+sample(.N))),
                      by=.(tract,ethnicity,age_range)]
    hh_relations_race[,("pbrel_id"):= 
                             paste0(tract,race,age_range_14,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,race,age_range_14)]
    hh_relations_eth[,("pbrel_id"):= 
                                  paste0(tract,ethnicity,age_range_14,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,ethnicity,age_range_14)]
    place_born_race_dt[,c("group_or_hh","family_or_non","in_family_type","relative_or_non"):=
                         hh_relations_race[.SD, c(list(group_or_hh),list(family_or_non),list(in_family_type),list(relative_or_non)), 
                                                on = .(pbrel_id)]]
    place_born_eth_dt[,c("group_or_hh","family_or_non","in_family_type","relative_or_non"):=
                        hh_relations_eth[.SD, c(list(group_or_hh),list(family_or_non),list(in_family_type),list(relative_or_non)), 
                                               on = .(pbrel_id)]]
    #11% don't match on eth/race and age: nrow(place_born_eth_dt[is.na(family_or_non)])/(nrow(place_born_eth_dt)-nrow(hh_relations_dt[group_or_hh=="In group quarters"]))
    #pick up stragglers / remember nrow(place_born_race_dt[is.na(family_or_non)])==nrow(hh_relations_dt[group_or_hh=="In group quarters])
    anti_pbr <- as.data.table(anti_join(hh_relations_race,place_born_race_dt,by="pbrel_id"))
    anti_pbe <- as.data.table(anti_join(hh_relations_eth,place_born_eth_dt,by="pbrel_id"))
    #after first time, need to move to seq.int, because of how the sample works in the group?
    anti_pbe[,("pbrel2_id"):= 
               paste0(tract,ethnicity,as.character(1000000+sample(.N))),
             by=.(tract,ethnicity)]
    anti_pbr[,("pbrel2_id"):= 
               paste0(tract,race,as.character(1000000+sample(.N))),
             by=.(tract,race)]
    place_born_eth_dt[is.na(group_or_hh),("pbrel2_id"):= 
                        paste0(tract,ethnicity,as.character(1000000+sample(.N))),
                      by=.(tract,ethnicity)]
    place_born_race_dt[is.na(group_or_hh),("pbrel2_id"):= 
                         paste0(tract,race,as.character(1000000+sample(.N))),
                       by=.(tract,race)]
    place_born_race_dt[is.na(group_or_hh),c("group_or_hh","family_or_non","in_family_type",
                                            "relative_or_non","age_range"):=
                         anti_pbr[.SD, c(list(group_or_hh),list(family_or_non),list(in_family_type),
                                         list(relative_or_non),list(age_range_14)), 
                                  on = .(pbrel2_id)]]
    place_born_eth_dt[is.na(group_or_hh),c("group_or_hh","family_or_non","in_family_type",
                                           "relative_or_non","age_range"):=
                        anti_pbe[.SD, c(list(group_or_hh),list(family_or_non),list(in_family_type),
                                        list(relative_or_non),list(age_range_14)), 
                                 on = .(pbrel2_id)]]
    #catch last ~1% and pull the race/eth/age off of hh_relations thread
    anti_pbr1 <- as.data.table(anti_join(anti_pbr,place_born_race_dt,by="pbrel2_id"))
    anti_pbe1 <- as.data.table(anti_join(anti_pbe,place_born_eth_dt,by="pbrel2_id"))
    anti_pbe1[,("pbrel3_id"):= 
               paste0(tract,age_range_14,as.character(1000000+sample(.N))),
             by=.(tract,age_range_14)]
    anti_pbr1[,("pbrel3_id"):= 
               paste0(tract,age_range_14,as.character(1000000+sample(.N))),
             by=.(tract,age_range_14)]
    place_born_eth_dt[is.na(group_or_hh),("pbrel3_id"):= 
                        paste0(tract,age_range,as.character(1000000+sample(.N))),
                      by=.(tract,age_range)]
    place_born_race_dt[is.na(group_or_hh),("pbrel3_id"):= 
                         paste0(tract,age_range,as.character(1000000+sample(.N))),
                       by=.(tract,age_range)]
    place_born_race_dt[is.na(group_or_hh),c("group_or_hh","family_or_non","in_family_type","relative_or_non",
                                            "race","age_range"):=
                         anti_pbr1[.SD, c(list(group_or_hh),list(family_or_non),list(in_family_type),
                                          list(relative_or_non),list(race),list(age_range_14)), 
                                  on = .(pbrel3_id)]]
    place_born_eth_dt[is.na(group_or_hh),c("group_or_hh","family_or_non","in_family_type","relative_or_non",
                                           "ethnicity","age_range"):=
                        anti_pbe1[.SD, c(list(group_or_hh),list(family_or_non),list(in_family_type),
                                         list(relative_or_non),list(ethnicity),list(age_range_14)), 
                                 on = .(pbrel3_id)]]
    #nrow(place_born_race_dt[is.na(family_or_non)])==nrow(hh_relations_dt[group_or_hh=="In group quarters"])
    #presumably missing ones line up with group_quarters - weirdness around not good matches on race/eth with sex_age_race and others
#move hh_relation broken into parts here, with pb stuff pulled in??
    
    #CHECK IF GROUP OR HH lets you move folks into right categories and fixes totals in_family, too - could fix it sooner that way
    
    ##in_family_type now doesn't agree
    test <- table(hh_relations_race$tract,hh_relations_race$place_born,
                  hh_relations_race$age_range_14,hh_relations_race$in_family_type)==
      table(place_born_race_dt$tract,place_born_race_dt$place_born,place_born_race_dt$age_range,place_born_race_dt$in_family_type)
    
    #get place_born_race/eth to match with hh_relation race/eth - idea is that we match with the availables that are in the right category, then pull over
    
    #add pb_eth/race to hh_relations, etc., then still break up as below - add latin_x to the ethnicity thread and race from origin_area 
    hh_relations_race[,c("race_sorter"):=case_when( 
      fb_origin_area=="Fiji" | fb_origin_area=="Oceania n.e.c." ~ 1,
      str_detect(fb_origin_area,"Asia") ~ 2,
      str_detect(fb_origin_area,"Africa") ~ 3,
      str_detect(fb_origin_area,"Europe") | fb_origin_area=="Northern America" |
        fb_origin_area=="Australia and New Zealand Subregion" ~ 4,
      fb_origin_area=="Latin America" ~ 5,
      fb_origin_area=="Northern Africa" ~ 6
    )]
    #
    hh_relations_race[order(race_sorter),
                      ("pb_race_id"):=paste0(tract,place_born,age_range_14,group_or_hh,family_or_non,in_family_type,relative_or_non,
                                              as.character(1000000+seq.int(1:.N))),
                      by=.(tract,place_born,age_range_14,group_or_hh,family_or_non,in_family_type,relative_or_non)]
    place_born_race_dt[order(match(race,c("E","D","B","A","G","F","C"))),
                       ("pb_race_id"):=paste0(tract,place_born,age_range,group_or_hh,family_or_non,in_family_type,relative_or_non,
                                               as.character(1000000+seq.int(1:.N))),
                       by=.(tract,place_born,age_range,group_or_hh,family_or_non,in_family_type,relative_or_non)]
    
    test <- table(hh_relations_race$tract,hh_relations_race$place_born,
                  hh_relations_race$age_range_14,hh_relations_race$in_family_type)==
      table(place_born_race_dt$tract,place_born_race_dt$place_born,place_born_race_dt$age_range,place_born_race_dt$in_family_type)
    
    hh_relations_race[,c("race"):=
                        place_born_race_dt[.SD,list(race),
                                           on = .(pb_race_id)]]
    hh_relations_eth$race <- NULL #can't figure this out - seems like once you've done this they separate fully
    hh_relations_eth[,
                     ("pb_eth_id"):=paste0(tract,place_born,age_range_14,family_or_non,in_family_type,relative_or_non,
                                            as.character(1000000+sample(.N))),
                     by=.(tract,place_born,age_range_14,family_or_non,in_family_type,relative_or_non)]
    place_born_eth_dt[order(match(ethnicity,c("H","_","I"))),
                      ("pb_eth_id"):=paste0(tract,place_born,age_range,family_or_non,in_family_type,relative_or_non,
                                             as.character(1000000+sample(.N))),
                      by=.(tract,place_born,age_range,family_or_non,in_family_type,relative_or_non)]
    hh_relations_eth[,c("ethnicity"):=   
                       place_born_eth_dt[.SD,list(ethnicity),
                                         on = .(pb_eth_id)]]
    #nrow(hh_relations_race[is.na(race)])/nrow(hh_relations_race)  =~ 67%
    #nrow(hh_relations_eth[is.na(ethnicity)])/nrow(hh_relations_eth)
    #pick up non-kids [matched on age_range_4 properly, but some weirdness on age_range_14]
    
    anti_rpbr <- as.data.table(anti_join(place_born_race_dt,hh_relations_race,by="pb_race_id"))
    anti_rpbe <- as.data.table(anti_join(place_born_eth_dt,hh_relations_eth,by="pb_eth_id"))
    hh_relations_race[is.na(race),
                      ("pb_race2_id"):=paste0(tract,place_born,age_range_14,family_or_non,relative_or_non,
                                             as.character(1000000+seq.int(1:.N))),
                      by=.(tract,place_born,age_range_14,family_or_non,relative_or_non)]
    anti_rpbr[,("pb_race2_id"):=paste0(tract,place_born,age_range,family_or_non,relative_or_non,
                                       as.character(1000000+seq.int(1:.N))),
                       by=.(tract,place_born,age_range,family_or_non,relative_or_non)]
    hh_relations_race[is.na(race),c("race","in_family_type"):=
                        anti_rpbr[.SD,c(list(race),list(in_family_type)),
                                           on = .(pb_race2_id)]]
    hh_relations_eth[is.na(ethnicity),("pb_eth2_id"):=paste0(tract,place_born,age_range_14,family_or_non,relative_or_non,
                                             as.character(1000000+sample(.N))),
                     by=.(tract,place_born,age_range_14,family_or_non,relative_or_non)]
    anti_rpbe[,("pb_eth2_id"):=paste0(tract,place_born,age_range,family_or_non,relative_or_non,
                                            as.character(1000000+sample(.N))),
                      by=.(tract,place_born,age_range,family_or_non,relative_or_non)]
    hh_relations_eth[is.na(ethnicity),c("ethnicity","in_family_type"):=   
                       anti_rpbe[.SD,c(list(ethnicity),list(in_family_type)),
                                         on = .(pb_eth2_id)]]
    #nrow(hh_relations_race[is.na(race)])/nrow(hh_relations_race)  =~ 9%
    #nrow(hh_relations_eth[is.na(ethnicity)])/nrow(hh_relations_eth)
    #get last 10% that don't match on age
    anti_rpbr2 <- as.data.table(anti_join(anti_rpbr,hh_relations_race,by="pb_race2_id"))
    anti_rpbe2 <- as.data.table(anti_join(anti_rpbe,hh_relations_eth,by="pb_eth2_id"))
    hh_relations_race[is.na(race),
                      ("pb_race3_id"):=paste0(tract,place_born,family_or_non,relative_or_non,
                                              as.character(1000000+seq.int(1:.N))),
                      by=.(tract,place_born,family_or_non,relative_or_non)]
    anti_rpbr2[,("pb_race3_id"):=paste0(tract,place_born,family_or_non,relative_or_non,
                                       as.character(1000000+seq.int(1:.N))),
              by=.(tract,place_born,family_or_non,relative_or_non)]
    hh_relations_race[is.na(race),c("race","in_family_type"):=
                        anti_rpbr2[.SD,c(list(race),list(in_family_type)),
                                  on = .(pb_race3_id)]]
    hh_relations_eth[is.na(ethnicity),("pb_eth3_id"):=paste0(tract,place_born,family_or_non,relative_or_non,
                                                             as.character(1000000+sample(.N))),
                     by=.(tract,place_born,family_or_non,relative_or_non)]
    anti_rpbe2[,("pb_eth3_id"):=paste0(tract,place_born,family_or_non,relative_or_non,
                                      as.character(1000000+sample(.N))),
              by=.(tract,place_born,family_or_non,relative_or_non)]
    hh_relations_eth[is.na(ethnicity),c("ethnicity","in_family_type"):=   
                       anti_rpbe2[.SD,c(list(ethnicity),list(in_family_type)),
                                 on = .(pb_eth3_id)]]
    #nrow(hh_relations_race[is.na(race)])/nrow(hh_relations_race)  =~ 4%
    #nrow(hh_relations_eth[is.na(ethnicity)])/nrow(hh_relations_eth)
    #and last 5%
    anti_rpbr3 <- as.data.table(anti_join(anti_rpbr2,hh_relations_race,by="pb_race3_id"))
    anti_rpbe3 <- as.data.table(anti_join(anti_rpbe2,hh_relations_eth,by="pb_eth3_id"))
    hh_relations_race[is.na(race) & family_or_non=="In family households",
                      ("pb_race4_id"):=paste0(tract,place_born,
                                              as.character(1000000+seq.int(1:.N))),
                      by=.(tract,place_born)]
    anti_rpbr3[family_or_non=="In family households",("pb_race4_id"):=paste0(tract,place_born,
                                        as.character(1000000+seq.int(1:.N))),
               by=.(tract,place_born)]
    hh_relations_race[is.na(race) & family_or_non=="In family households",c("race","in_family_type"):=
                        anti_rpbr3[.SD,c(list(race),list(in_family_type)),
                                   on = .(pb_race4_id)]]
    hh_relations_eth[is.na(ethnicity) & family_or_non=="In family households",("pb_eth4_id"):=paste0(tract,place_born,
                                                             as.character(1000000+sample(.N))),
                     by=.(tract,place_born)]
    anti_rpbe3[family_or_non=="In family households",("pb_eth4_id"):=paste0(tract,place_born,
                                       as.character(1000000+sample(.N))),
               by=.(tract,place_born)]
    hh_relations_eth[is.na(ethnicity) & family_or_non=="In family households",
                     c("ethnicity","in_family_type"):=   
                       anti_rpbe3[.SD,c(list(ethnicity),list(in_family_type)),
                                  on = .(pb_eth4_id)]]
    #nrow(hh_relations_race[is.na(race)])/nrow(hh_relations_race)  =~ 3%
    #nrow(hh_relations_eth[is.na(ethnicity)])/nrow(hh_relations_eth)
    #stragglers
    anti_rpbr4 <- as.data.table(anti_join(anti_rpbr3,hh_relations_race,by="pb_race4_id"))
    anti_rpbe4 <- as.data.table(anti_join(anti_rpbe3,hh_relations_eth,by="pb_eth4_id"))
    hh_relations_race[is.na(race),("pb_race5_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),
                     by=.(tract)]
    anti_rpbr4[,("pb_race5_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),
               by=.(tract)]
    hh_relations_race[is.na(race),c("race"):=   
                       anti_rpbr4[.SD,c(list(race)),
                                  on = .(pb_race5_id)]]
    hh_relations_eth[is.na(ethnicity),("pb_eth5_id"):=paste0(tract,
                                                             as.character(1000000+sample(.N))),
                     by=.(tract)]
    anti_rpbe4[,("pb_eth5_id"):=paste0(tract,
                                       as.character(1000000+sample(.N))),
               by=.(tract)]
    hh_relations_eth[is.na(ethnicity),c("ethnicity"):=   
                       anti_rpbe4[.SD,c(list(ethnicity)),
                                  on = .(pb_eth5_id)]]
    #nrow(hh_relations_race[is.na(race)])/nrow(hh_relations_race)  =~ .2% (9k)
    #nrow(hh_relations_eth[is.na(ethnicity)])/nrow(hh_relations_eth)
#fails these tests, so write over with final values...
    #test <- table(hh_relations_race$tract,hh_relations_race$race)==table(sex_age_race$tract,sex_age_race$race)
    #test <- table(hh_relations_eth$tract,hh_relations_eth$ethnicity)==table(sex_by_age_eth$tract,sex_by_age_eth$ethnicity)
    #length(test[test==F])==0
    
    #add latinx - fb is a subset - a bit tricky
    #add only to ethnicity side and then join back after other ethnicity stuff (marital and transportation?)
    latinx_dt[,("ethnicity"):=if_else(latinx=="Hispanic or Latino","I",NULL)]
    #match will only be for foreign born
    hh_relations_eth[,("origin_family"):=case_when(
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
    
    #only foreign born have origin_country in hh_relations, won't get complete
    hh_relations_eth[ethnicity=="I",("latinx_match_id"):=paste0(tract,origin_country,
                                            as.character(1000000+sample(.N))),
                     by=.(tract,origin_country)]
    latinx_dt[ethnicity=="I",("latinx_match_id"):=paste0(tract,origin_country,
                                     as.character(1000000+sample(.N))),
              by=.(tract,origin_country)]
    hh_relations_eth[ethnicity=="I",("latinx_family_origin_country"):=
                      latinx_dt[.SD,list(origin_country),
                                on = .(latinx_match_id)]]
    latinx_dt[ethnicity=="I",("missing"):=hh_relations_eth[.SD,list(latinx_family_origin_country), on = .(latinx_match_id)]]
    #add rest of origin_country 
    hh_relations_eth[is.na(latinx_family_origin_country) & ethnicity=="I",
                     ("latinx_match2_id"):=paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    latinx_dt[is.na(missing) & ethnicity=="I",
              ("latinx_match2_id"):=paste0(tract,as.character(1000000+sample(.N))),
              by=.(tract)]
    hh_relations_eth[is.na(latinx_family_origin_country) & ethnicity=="I",
                     ("latinx_family_origin_country"):=
                       latinx_dt[.SD,list(origin_country),
                                 on = .(latinx_match2_id)]]
    
    
    
    #DO MARITAL RACE/ETH WITH THE MISSING ONES PULLED OVER, don't forget to use place_born as part of match
    #order by age_range, but only for the ones over 14, then match on sex, race, eth - with a created variable from
    #hh_relations that lines up with marital_status possibilities...
    
    
    #join marital data to sex_age_race/eth
    sar_marital <- sex_age_race[age>14]
    sar_marital[order(age),
                 ("married_join_id"):=paste0(tract,race,sex,
                                             as.character(1000000+seq.int(1:.N))),
                 by=.(tract,race,sex)]
    marital_status_race_dt[order(as.numeric(substr(age_range,1,2))),
                           ("married_join_id"):=paste0(tract,race,sex,
                                                       as.character(1000000+seq.int(1:.N))),
                           by=.(tract,race,sex)]
    sar_marital[,c("marital_status","spouse_present","separated","pregnant"):=
                   marital_status_race_dt[.SD, c(list(marital_status),list(spouse_present),list(separated),list(pregnant)), 
                                          on = .(married_join_id)]]
    sex_age_race[,c("marital_status","spouse_present","separated","pregnant"):=
                   sar_marital[.SD, c(list(marital_status),list(spouse_present),list(separated),list(pregnant)), 
                               on = .(individual_id)]]
    sae_marital <- sex_by_age_eth[age>14]
    sae_marital[order(age),
                   ("married_join_id"):=paste0(tract,ethnicity,sex,
                                               as.character(1000000+seq.int(1:.N))),
                   by=.(tract,ethnicity,sex)]
    marital_status_eth_dt[order(as.numeric(substr(age_range,1,2))),
                          ("married_join_id"):=paste0(tract,ethnicity,sex,
                                                      as.character(1000000+seq.int(1:.N))),
                          by=.(tract,ethnicity,sex)]
    sae_marital[,c("marital_status","spouse_present","separated","pregnant"):=
                     marital_status_eth_dt[.SD, c(list(marital_status),list(spouse_present),list(separated),list(pregnant)), 
                                           on = .(married_join_id)]]
    sex_by_age_eth[,c("marital_status","spouse_present","separated","pregnant"):=
                   sae_marital[.SD, c(list(marital_status),list(spouse_present),list(separated),list(pregnant)), 
                               on = .(individual_id)]]
    #test <- nrow(sex_by_age_eth[!is.na(pregnant)]) == nrow(preg_eth_dt)
    #test <- nrow(sex_age_race[!is.na(pregnant)]) == nrow(preg_race_dt)
    #test <- table(sex_age_race$tract,sex_age_race$marital_status,sex_age_race$spouse_present)==
    # table(marital_status_race_dt$tract,marital_status_race_dt$marital_status,marital_status_race_dt$spouse_present)
    #test <- table(sex_age_race$tract,sex_age_race$marital_status,sex_age_race$spouse_present)==
    #   table(sex_by_age_eth$tract,sex_by_age_eth$marital_status,sex_by_age_eth$spouse_present)
    #length(test[test==F])==0
    #for comparison's sake:
    #test <- table(sex_age_race$tract,sex_age_race$marital_status_tmp,sex_age_race$spouse_present)==
    #   table(sex_by_age_eth$tract,sex_by_age_eth$marital_status_tmp,sex_by_age_eth$spouse_present)
    #length(test[test==F])/length(test) #96% false

    #catch the ones we know (HH, etc) - 
    sex_age_race[age>14,("mar_match"):=case_when(
      marital_status=="Now married except separated" ~ "In married-couple family",
      sex=="Female" & !str_detect(marital_status,"eparated") ~ "In female householder no husband present family",
      sex=="Male" & !str_detect(marital_status,"eparated") ~ "In male householder no wife present family"
    )]
    
    sex_by_age_eth[age>14,("mar_match"):=case_when(
      marital_status=="Now married except separated" ~ "In married-couple family",
      sex=="Female" & !str_detect(marital_status,"eparated") ~ "In female householder no husband present family",
      sex=="Male" & !str_detect(marital_status,"eparated") ~ "In male householder no wife present family"
    )]

    
    
    #join back into sex_age_race - if you also match on pregnant, you get some missing - not sure what's going on so pulled pregnant from eth
    #should pregnant count as having one child - maybe give them birthdates??
    sex_age_race[order(match(race,c("A","F","G","C","B","E","D"))), #
                 ("join_race_id"):=paste0(tract,sex,marital_status,spouse_present,separated,pregnant,
                                          as.character(1000000+seq.int(1:.N))),
                 by=.(tract,sex,marital_status,spouse_present,separated,pregnant)]
    sex_by_age_eth[order(match(ethnicity,c("H","I","_"))),
                   ("join_race_id"):=paste0(tract,sex,marital_status,spouse_present,separated,pregnant,
                                            as.character(1000000+seq.int(1:.N))),
                   by=.(tract,sex,marital_status,spouse_present,separated,pregnant)]
    sex_age_race[,c("ethnicity","age"):=  #the age on eth was generated from age_29, and should be more granular
                   sex_by_age_eth[.SD, c(list(ethnicity),list(age)), on = .(join_race_id)]]
    
    #pick up spares - 81699, or 1.8%
    s_a_r <- sex_age_race[is.na(ethnicity)]
    s_a_e <- as.data.table(anti_join(sex_by_age_eth,sex_age_race,by="join_race_id"))
    s_a_r[order(match(race,c("A","F","G","C","B","E","D"))), #
          ("join_race_id2"):=paste0(tract,sex,marital_status,spouse_present,separated,
                                    as.character(1000000+seq.int(1:.N))),
          by=.(tract,sex,marital_status,spouse_present,separated)]
    s_a_e[order(match(ethnicity,c("H","I","_"))),
          ("join_race_id2"):=paste0(tract,sex,marital_status,spouse_present,separated,
                                    as.character(1000000+seq.int(1:.N))),
          by=.(tract,sex,marital_status,spouse_present,separated)]
    s_a_r[,c("ethnicity","age","pregnant"):=  #overwrite mismatching pregnant
            s_a_e[.SD, c(list(ethnicity),list(age),list(pregnant)), on = .(join_race_id2)]]
    sex_age_race[is.na(ethnicity),c("ethnicity","age","pregnant"):=  
                   s_a_r[.SD, c(list(ethnicity),list(age),list(pregnant)), on = .(join_race_id)]]
    sex_age_race[,("join_race_id"):=NULL]
    #table(sex_age_race$race,sex_age_race$ethnicity) has about .06% in categories they shouldn't be in - H being the weird one
    #could try to go longer on the separate builds, like in sam_hh, or could try to nudge it later - or just hand-tune (haven't done that yet)
    #could put in published stuff correlating preg and educ, but I think the tract level stuff is carrying more info
    
  }
}
    
 #Move next steps into apportioning.R   
    
    
    
    
    #add place_born_education to sex_age_educ, then match to sex_age_race...
    #population of 25 and over: 2923369 folks - count all people over 25 in place_born_age you get 2860024 (63345 too many in educ?)
    
    #add educ to sex_age_race, in order to match better with hh - definitely missing granularity, but matching on educ / family type or marital not available in census itself
    sex_age_race[order(age),
                 ("educ_id"):=paste0(tract,sex,if_else(age>17,"age_fill","nomatch"),
                                     as.character(1000000+sample(.N))),
                 by=.(tract,sex,age>17)]
    sex_age_educ_dt[order(age),
                    ("educ_id"):=paste0(tract,sex,"age_fill",
                                        as.character(1000000+sample(.N))),
                    by=.(tract,sex)]
    sex_age_race[,c("education"):=
                   sex_age_educ_dt[.SD, list(education_level), on = .(educ_id)]]
    sex_age_race[,("educ_id"):=NULL]
    
    #saveRDS(sex_age_race,file = paste0(housingdir, vintage, "/sex_age_race_l.189_",Sys.Date(),".RDS"))
    #get householders into and marked on sex_age_race - sex, age, race, eth, marital/family_type, education? - may have to do HH first
    
    #make congruent categories
    sex_age_race[,("education_4") := case_when(
      education=="Less than 9th grade" ~ "Less than high school graduate",
      education=="9th to 12th grade no diploma" ~ "Less than high school graduate",
      education=="High school graduate (includes equivalency)" ~ "High school graduate (including equivalency)",
      education=="Some college no degree" ~ "Some college or associate's degree",
      education=="Associate's degree" ~ "Some college or associate's degree",
      education=="Bachelor's degree" ~ "Bachelor's degree or higher",
      education=="Graduate or professional degree" ~ "Bachelor's degree or higher"
    )]
    sex_age_race[age>14,("householder_age_9") := case_when(
      age < 25 ~ "Householder 15 to 24 years",
      age > 24 & age < 35 ~ "Householder 25 to 34 years",
      age > 34 & age < 45 ~ "Householder 35 to 44 years",
      age > 44 & age < 55 ~ "Householder 45 to 54 years",
      age > 54 & age < 59 ~ "Householder 55 to 59 years",
      age > 59 & age < 65 ~ "Householder 60 to 64 years",
      age > 64 & age < 75 ~ "Householder 65 to 74 years",
      age > 74 & age < 85 ~ "Householder 75 to 84 years",
      age > 84 ~ "Householder 85 years and over",
    )]
    #saveRDS(sex_age_race,file = paste0(housingdir, vintage, "/sex_age_race_l.231_",Sys.Date(),".RDS"))
    #add ids for matching
    #do the sam_hh where the sex is known (only about 78k).
    sex_age_race[,("hh_join_id"):=paste0(tract,sex,race,ethnicity,householder_age_9,education_4,
                                         as.character(1000000+sample(.N))),
                 by=.(tract,sex,race,ethnicity,householder_age_9,education_4)]
    sam_hh[,("hh_join_id"):=paste0(tract,if_else(is.na(sex),"none",sex),race,ethnicity,householder_age_9,hh_education_level,
                                   as.character(1000000+sample(.N))),
           by=.(tract,is.na(sex),race,ethnicity,householder_age_9,hh_education_level)]
    #move to each side - householder_id, individual_id, some info for later matching...
    sam_hh[,c("individual_id","marital_status","spouse_present","separated") := 
             sex_age_race[.SD, c(list(individual_id),list(marital_status),list(spouse_present),list(separated)), on = .(hh_join_id)]]
    
    sex_age_race[age>14,
                 c("household_id","hh_income_level","kids_by_age","number_workers_in_hh","number_vehicles_in_hh") := 
                   sam_hh[.SD, c(list(household_id),list(hh_income_level),list(kids_by_age),list(number_workers_in_hh),list(number_vehicles_in_hh)), 
                          on = .(hh_join_id)]]
    sam_hh$hh_join_id <- NULL
    sex_age_race$hh_join_id <- NULL
    #then pick up others, with bias toward putting males as householders (following census; but not absolute)
    
    sex_age_race_1_hh <- sex_age_race[is.na(household_id) & age>14] #since I can't get the i to order and to subset
    sex_age_race_1_hh[order(-sex), #want male to be first, since the census usually treats them as householders in shared houses (but not always)
                      ("second_join_id"):=paste0(tract,race,ethnicity,householder_age_9,education_4,
                                                 as.character(1000000+seq.int(1:.N))),
                      by=.(tract,race,ethnicity,householder_age_9,education_4)]
    sam_hh[is.na(individual_id),("second_join_id"):=paste0(tract,race,ethnicity,householder_age_9,hh_education_level,
                                                           as.character(1000000+sample(.N))),
           by=.(tract,race,ethnicity,householder_age_9,hh_education_level)]
    sam_hh[is.na(individual_id),c("individual_id","sex","marital_status","spouse_present","separated") := 
             sex_age_race_1_hh[.SD, c(list(individual_id),list(sex),
                                      list(marital_status),list(spouse_present),list(separated)), on = .(second_join_id)]]
    
    sex_age_race[age>14 & is.na(household_id),
                 c("household_id","hh_income_level","kids_by_age","number_workers_in_hh","number_vehicles_in_hh") := 
                   sam_hh[.SD, c(list(household_id),list(hh_income_level),list(kids_by_age),list(number_workers_in_hh),list(number_vehicles_in_hh)), 
                          on = .(individual_id)]]
    rm(sex_age_race_1_hh)
    sam_hh$second_join_id <- NULL
    #nrow(sam_hh[is.na(individual_id)]) - 272054 or ~17.4%
    #try without education and order on age
    sex_age_race_1_hh <- sex_age_race[is.na(household_id) & age>14]
    sex_age_race_1_hh[order(-sex,age),
                      ("third_join_id"):=paste0(tract,race,ethnicity,
                                                as.character(1000000+seq.int(.N))),
                      by=.(tract,race,ethnicity)]
    sam_1_hh <- sam_hh[is.na(individual_id)]
    sam_1_hh[order(householder_age_9),("third_join_id"):=paste0(tract,race,ethnicity,
                                                                as.character(1000000+seq.int(.N))),
             by=.(tract,race,ethnicity)]
    sam_1_hh[,c("individual_id","sex","marital_status","spouse_present","separated") := 
               sex_age_race_1_hh[.SD, c(list(individual_id),list(sex),
                                        list(marital_status),list(spouse_present),list(separated)), on = .(third_join_id)]]
    sam_hh[is.na(individual_id),c("individual_id","sex","marital_status","spouse_present","separated") := 
             sam_1_hh[.SD, c(list(individual_id),list(sex),
                             list(marital_status),list(spouse_present),list(separated)), on = .(household_id)]]
    sex_age_race[age>14 & is.na(household_id),
                 c("household_id","hh_income_level","kids_by_age","number_workers_in_hh","number_vehicles_in_hh") := 
                   sam_1_hh[.SD, c(list(household_id),list(hh_income_level),list(kids_by_age),list(number_workers_in_hh),list(number_vehicles_in_hh)), 
                            on = .(individual_id)]]
    rm(sex_age_race_1_hh)
    rm(sam_1_hh)
    #sex_age_race$third_join_id <- NULL
    #nrow(sam_hh[is.na(individual_id)]) - 14062 or ~0.89% of hh
    #test <- nrow(sex_age_race[!is.na(household_id)])==nrow(sam_hh)
    sex_age_race[age>14 & is.na(household_id),
                 ("fourth_join_id"):=paste0(tract,education_4,
                                            as.character(1000000+sample(.N))),
                 by=.(tract,education_4)]
    sam_hh[is.na(individual_id),("fourth_join_id"):=paste0(tract,hh_education_level,
                                                           as.character(1000000+sample(.N))),
           by=.(tract,hh_education_level)]
    sam_hh[is.na(individual_id),c("individual_id","sex","marital_status","spouse_present","separated") := 
             sex_age_race[.SD, c(list(individual_id),list(sex),
                                 list(marital_status),list(spouse_present),list(separated)), on = .(fourth_join_id)]]
    
    sex_age_race[age>14 & is.na(household_id),
                 c("household_id","hh_income_level","kids_by_age","number_workers_in_hh","number_vehicles_in_hh") := 
                   sam_hh[.SD, c(list(household_id),list(hh_income_level),list(kids_by_age),list(number_workers_in_hh),list(number_vehicles_in_hh)), 
                          on = .(individual_id)]]
    sam_hh$fourth_join_id <- NULL
    sex_age_race$fourth_join_id <- NULL
    #have to think through how to reconcile this very last part - 7,856 who don't match on ethnicity and race - about the number in bad categories
    #table(sam_hh$race,sam_hh$ethnicity) and table(sex_age_race$race,sex_age_race$ethnicity) interesting - maybe do more with sex_age_race / eth separate before joining?
    #test nrow(sam_hh[is.na(individual_id)])==0 ; nrow(sex_age_race[!is.na(household_id)])==nrow(sam_hh)
    
    #get relations info back and forth with sam_hh
    sex_age_race[!is.na(household_id),("relation_householder"):="self"]
    
    #saveRDS(sex_age_race,file = paste0(housingdir, vintage, "/sex_age_race_l.319_",Sys.Date(),".RDS"))
    #saveRDS(sam_hh,file = paste0(housingdir, vintage, "/sam_hh_l.320_",Sys.Date(),".RDS"))
    
    
    
    saveRDS(sam_hh,file = paste0(housingdir, vintage, "/sam_hh_l.568",Sys.Date(),".RDS")) 
    #    rm(list = setdiff(ls(),"sam_hh")) # or just the ones that are in the list that is passed from expand_hh_from_census
    #when go through and set break if error points, also remove after using - just to clean up space
    #END BASE_HH_GEN here
  }   
}





#instead of below, add straight into sex_age_race / sam
ages_all_dt <- rbindlist(list(kids_ages_dt,adults_relations[relation_age_range!="65 years and over"],sr_relations),fill = TRUE)

ages_dt <- rbindlist(list(kids_family_age_data,household_adults_relation_data),fill = TRUE)

hh_relations_dt[relative=="Householder" | relative=="Spouse" | family_role=="Unmarried partner",
                ("relations_merged"):= "Householder_partner"]
hh_ages_dt[relation_hh=="Householder living with spouse or spouse of householder" |
             relation_hh=="Householder living with unmarried partner or unmarried partner of householder" |
             relation_hh=="Lives alone", #pick up and separate when merging with sam_hh
           ("relations_merged"):= "Householder_partner"]
hh_relations_dt[relative=="Child",("relations_merged"):="Child"]
hh_ages_dt[relation_hh=="Child of householder",("relations_merged"):="Child"] #but over 18!
hh_relations_dt[relative=="Nonrelatives",("relations_merged"):="Other nonrelatives"]
hh_ages_dt[relation_hh=="Other nonrelatives",("relations_merged"):="Other nonrelatives"]
hh_relations_dt[relative=="Brother or sister" | relative=="Grandchild" | 
                  str_detect(relative, "in-law"),
                ("relations_merged"):="Younger relatives"]
hh_relations_dt[relative=="Brother or sister" | relative=="Parent" |
                  str_detect(relative, "in-law"),
                ("relations_merged"):="Other relatives"]
hh_ages_dt[relation_hh=="Other relatives",("relations_merged"):="Other relatives"]
hh_ages_dt[!is.na(kid_age),("relations_merged"):="Younger relatives"]
sr_relations[relative=="Householder" | relative=="Spouse",
             ("relations_merged"):= "Householder_partner"]
sr_relations[relative=="Parent" | relative=="Parent-in-law",
             ("relations_merged"):= "Other relatives"]
sr_relations[group_or_hh=="In group quarters",("relations_merged"):="Group Quarters"] #just for sorting
sr_relations[order(relations_merged),("sr_relations_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),by=.(tract)]
hh_ages_dt[order(relations_merged) & relation_age_range=="65 years and over",
           ("sr_relations_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),by=.(tract)]
hh_sr_ages <- hh_ages_dt[sr_relations,on="sr_relations_id"]

#hh_sr_ages <- sr_relations[hh_ages_dt,on="sr_relations_id"]
hh_sr_ages[is.na(kid_age),("age_range"):=relation_age_range]
hh_sr_ages[!is.na(kid_age),("age_range"):=kid_age]
hh_ages_exp <- rbindlist(list(hh_sr_ages,hh_relations_dt[family_role=="Adopted child" |
                                                           family_role=="Foster child" | 
                                                           family_role=="Grandchild" | 
                                                           family_role=="Stepchild"]),fill = TRUE)
hh_ages_exp[is.na(age_range),("age_range"):="0 to 17 years"] #add to all moved over from hh_relations
hh_ages_exp[is.na(age_range),("relations_merged"):="Younger relatives"]

#hh_relations_dt[(group_quarters),("relations_merged"):="Group Quarters"]
hh_ages_exp[is.na(tract),("tract"):=i.tract]
hh_ages_exp[is.na(relations_merged),("relations_merged"):=i.relations_merged]
hh_ages_exp[order(match(relations_merged,c("Child","Younger relatives","Householder_partner","Other nonrelatives","Other relatives"))),
            ("ages_exp_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),by=.(tract)]
hh_relations_dt[order(match(relations_merged,c("Child","Younger relatives","Householder_partner","Other nonrelatives","Other relatives"))),
                ("ages_exp_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),by=.(tract)]

##could we put them all together, and then pick the ones that respect original distribution on family_roles??
##just do the count thing before assigning the ages_exp_id and keep the family_roles from relations.????
full_relations_dt <- hh_relations_dt[hh_ages_exp,on=("ages_exp_id")]
relations_dt <- full_relations_dt[!is.na(i.tract),
                                  c("i.tract","group_or_hh","group_quarters","relative",
                                    "family_or_non","family_role","age_range")]
#then count on the ones that were in relations to begin with? 
#    relations_dt <- hh_ages_exp[hh_relations_dt,on=("ages_exp_id")]

#relations_dt[is.na(tract),("tract"):=if_else(is.na(i.tract),i.tract.1,i.tract)]
relations_dt[,("tract"):=i.tract.1]
relations_dt[is.na(family_role),c("group_or_hh","family_or_non","relative","family_role"):=
               c(list(i.group_or_hh),list(i.family_or_non),list(i.relative),list(i.family_role))]
relations_dt[is.na(age_range) & family_role=="Foster child",("age_range"):="0 to 17 years"]  
relations_dt[is.na(age_range),("age_range"):="18 to 64 years"]







#old version:



#starting with households, but cleaned up.
#need to source Census_Data and get a few variables from workflow. The censuskey is kept in households

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

#census does a process of modeling, based on weighting of responses. Hard to find exactly where they're doing the projections.
#cf. Appendix B, starting at esp. around 103: https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf
#re-cast small cells to be more plausible
#get groupings for tracts by larger categories from the HCAD_b4_expand (save as an RDS from other)
#maybe some financial stuff to get a direction?? look at what's available... Brownfields? neighborhood quality? 
#because it's all the parcels, they break out differently according to different geo markers
tracts_by_geos <- HCAD_b4_expand[,c("City","Zip","ISD","Superneighborhood","HISD_Elem_School","HISD_Middle_School","HISD_Elementary_School","min_h_distance","min_f_distance")] 
#what levels do I think I can trust? sex_by_age_race?? then make the others assign...
#are we assigning by distance from median on the eigs? sample and match according to a sense of lots... 
#add rows of sam_sex_age, with the tract average for the HCAD values, and the likelihood of belonging within based on the sums (not quite right for apts)
#then do one by zip, one by city/sn, one by isd/HighSchool, one by Tract - then calculating ... then I get distances from the whole and do the matching trick.
#don't put in the stuff about the smallest cells at all? or put them in as totals at a larger level?....
#so, if I had city-wide, 
#with means for the missing...
#and then put them in as a lot-draw with 
#percentages from the distances calculated like preg - but for each layer, and then normalized back?

##make sam with all race categories
sex_by_age_race_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B01001")
sex_by_age_race_data <- sex_by_age_race_data_from_census %>%
 # mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%  
  pivot_longer(4:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams") %>%    
  mutate(label = str_remove_all(label,"Estimate!!Total!!"),#clean up label
         race = substr(name,7,7),
         white = if_else(race=="A",1,0),  #add these for PCA dimensions #white includes latino and hispanic, as do parts of other categories
         black = if_else(race=="B",1,0),
         american_indian = if_else(race=="C",1,0),
         asian = if_else(race=="D",1,0),
         pacific_islander = if_else(race=="E",1,0),
         other_race = if_else(race=="F",1,0),
         bi_racial = if_else(race=="G",1,0)
  ) %>%
  separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE) %>%  #NA in rows with only one
  mutate(age_range = str_replace(age_range,"Under 5 years","0  to  5 years"), #have to do something funky...
         age_range = str_replace(age_range,"5 to 9 years","5  to  9 years"),
         age_range = str_replace(age_range,"18 and 19 years","18 to 19 years"),
         age_range = str_replace(age_range,"85 years and over","85 to 94 years"),  #have to norm it when calculating...
         first_age = as.numeric(substr(age_range,1,2)),
         last_age = as.numeric(substr(age_range,7,8)),
         age_range_length = last_age-first_age+1
         ) %>%
  rename(census_group_name = name) %>%
  filter(number_sams != 0, !is.na(age_range),race!="_",age_range!="Total") %>%
  rowwise() %>%
  mutate(
    age=as.numeric(sample(as.character(first_age:last_age),1,prob = rep(1/age_range_length,age_range_length),replace = FALSE)),
    age=if_else(age_range=="85 to 94 years",as.numeric(sample(85:105,size=1,replace=TRUE,prob = rep(2/(8:28)^5/sum(2/(8:28)^5)))),age) 
  ) %>%
  uncount(number_sams,.id = "sams_id") 

sex_age_race_dt <- as.data.table(sex_by_age_race_data) # a couple of things for sampling work better as dt




sex_age_race_dt[,("white_not_latinx_num") := nrow(.SD[race=="H"]),by=.(tract,sex,age_range)] #get number of white_not_latinx
sex_age_race_dt[,("white_and_latinx_num") := nrow(.SD[race=="A"])-nrow(.SD[race=="H"]),by=.(tract,sex,age_range)]
#sex_age_race_dt[,("latinx_not_white_num") := nrow(.SD[race=="I"])-white_and_latinx_num,by=.(tract,age_range)]
sex_age_race_dt[race=="A",("latinx") := c(rep(as.integer(1),as.integer(white_and_latinx_num[1])),rep(as.integer(0),as.integer(white_not_latinx_num[1]))),
                   by=.(tract,sex,age_range)] #just assigns, since there's no matching besides on tract and place born within A
sex_age_race_dt[,("remaining_latinx") := (nrow(.SD[race=="I"]) - white_and_latinx_num),by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0,("other_latinx_num") := round(nrow(.SD[race=="F"]) * .88, digits = 0),by=.(tract,sex,age_range)]
sex_age_race_dt[,("other_latinx_num") := if_else(other_latinx_num > remaining_latinx,as.numeric(remaining_latinx),other_latinx_num),by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0,("other_not_latinx_num") := nrow(.SD[race=="F"]) - other_latinx_num,by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0 & race=="F",("latinx") := c(rep(as.integer(1),as.numeric(other_latinx_num[1])),rep(as.integer(0),as.integer(other_not_latinx_num[1]))),
                   by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0,("remaining_latinx") := nrow(.SD[race=="I"]) - as.integer(white_and_latinx_num) - as.integer(other_latinx_num),by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0,("two_latinx_num") := round(nrow(.SD[race=="G"]) * .87, digits = 0),by=.(tract,sex,age_range)]
sex_age_race_dt[,("two_latinx_num") := if_else(two_latinx_num > remaining_latinx,as.numeric(remaining_latinx),two_latinx_num),by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0,("two_not_latinx_num") := nrow(.SD[race=="G"]) - two_latinx_num,by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0 & race=="G",("latinx") := c(rep(as.integer(1),as.integer(two_latinx_num[1])),rep(as.integer(0),as.integer(two_not_latinx_num[1]))),
                   by=.(tract,sex,age_range)]
sex_age_race_dt[,("remaining_latinx") := nrow(.SD[race=="I"]) - as.integer(white_and_latinx_num) - as.integer(other_latinx_num) - as.integer(two_latinx_num),by=.(tract,sex,age_range)]
sex_age_race_dt[remaining_latinx>0,("black_latinx_num") := round(nrow(.SD[race=="B" & age_range=="Foreign born"]) * .8, digits = 0),by=.(tract,sex,age_range)]
sex_age_race_dt[,("black_latinx_num") := if_else(black_latinx_num > remaining_latinx,as.numeric(remaining_latinx),as.numeric(black_latinx_num))]
sex_age_race_dt[black_latinx_num>=0,("black_not_latinx_num") := nrow(.SD[race=="B" & age_range=="Foreign born"]) - black_latinx_num,by=.(tract,sex,age_range)]
sex_age_race_dt[,("black_not_latinx_num") := if_else(black_not_latinx_num>0,black_not_latinx_num,0)]
sex_age_race_dt[race=="B" & age_range=="Foreign born"  & black_not_latinx_num>0,("latinx") := 
                     c(rep(as.integer(1),as.integer(black_latinx_num[1])),rep(as.integer(0),as.integer(black_not_latinx_num[1]))),
                   by=.(tract,sex,age_range)]
#now reassign to get native born black_latinx
sex_age_race_dt[,("remaining_latinx") := nrow(.SD[race=="I"]) - as.integer(white_and_latinx_num) - 
                  as.integer(other_latinx_num) - as.integer(two_latinx_num) - as.integer(black_latinx_num),by=.(tract,sex,age_range)]
#sex_age_race_dt[is.na(remaining_latinx),("remaining_latinx") := max(remaining_latinx),by=.(tract,age_range)]#work around because of weirdness in if_else
sex_age_race_dt[remaining_latinx>0,("rem_black_latinx_num") := round(nrow(.SD[race=="B" & age_range!="Foreign born"]) - remaining_latinx, digits = 0),by=.(tract,sex,age_range)]
sex_age_race_dt[,("rem_black_latinx_num") := if_else(rem_black_latinx_num > remaining_latinx,as.numeric(remaining_latinx),as.numeric(rem_black_latinx_num))]
sex_age_race_dt[rem_black_latinx_num>=0,("rem_black_not_latinx_num") := nrow(.SD[race=="B" & age_range!="Foreign born"]) - rem_black_latinx_num,by=.(tract,sex,age_range)]
sex_age_race_dt[race=="B" & age_range!="Foreign born"  & rem_black_not_latinx_num>0 & rem_black_latinx_num>0,("latinx") := 
                     c(rep(as.integer(1),as.integer(rem_black_latinx_num[1])),rep(as.integer(0),as.integer(rem_black_not_latinx_num[1]))),
                   by=.(tract,sex,age_range)]
sex_age_race_latinx_dt <- sex_age_race_dt[!is.na(race) & race %in% acs_race_codes] # should equal 4525519 per B10001 row 166 total in 2017; 4602523 in 2018;



#unique(place_born) - "Born in state of residence" "Born in other state in the United States" "Native; born outside the United States"   "Foreign born" 
place_born_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06004")
place_born_race_data <- place_born_race_from_census %>% #right total - 4525519
  mutate(label = str_remove_all(label,"Estimate!!Total!!"),#clean up label
         race = substr(name,7,7),
         white = if_else(race=="A",1,0),  #add these for PCA dimensions #white includes latino and hispanic, as do parts of other categories
         black = if_else(race=="B",1,0),
         american_indian = if_else(race=="C",1,0),
         asian = if_else(race=="D",1,0),
         pacific_islander = if_else(race=="E",1,0),
         other_race = if_else(race=="F",1,0),
         bi_racial = if_else(race=="G",1,0)
  ) %>%
  filter(label != "Estimate!!Total") %>%
  rename(place_born = label) %>%
  pivot_longer(4:ncol(place_born_race_from_census),names_to = "tract", values_to = "number_sams") %>% 
  group_by(tract) %>%
  filter(number_sams > 0) %>% # & !is.na(race) & race %in% acs_race_codes
  uncount(as.numeric(number_sams),.id = "place_born_race_id") 
place_born_race_dt <- as.data.table(place_born_race_data) # a couple of things for sampling work better as dt


#the distribution by subcategories is flaky - I'm guessing it's the result of the statistical process of casting individuals so there are the same number
#by race in each tract. I'm thinking that each of the categories is trustworthy, but the distribution into cells introduces weirdness, even with larger cells.

#if we do the euc_distances on place_born race and place_born age, and then join them together with the HCAD so that the variation given by housing conditions
#structures the distribution???

#assign race and age from census to tract level, not individuals, in HCAD - thena PCA????
#if treat each tract as having certain characteristics, instead of the individual, and then see the variance within the tract as a way of placing individuals? save

