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

#' createHouseholds
#'
#' This function simulates individual people based on census data (age, sex, race, as of April 1, vintage year).
#' If simulated data already exists, it will read it from an RDS file.
#'Input should include exp_census from expand_from_census.R, which is a list of data.tables
#' @return sam_residents A dataframe of simulated people.
createHouseholds <- function() {
  
  sam_residents_data_file <- paste0(censusdir, vintage,"/sam_hh.RDS") 
  #Create or read in individual sam residents
  if(file.exists(sam_residents_data_file)) {
    # import saved sam residents from RDS file
    sam_residents <- readRDS(sam_residents_data_file)
    print(sprintf("Done reading sam residents RDS from %s", sam_residents_data_file ))
  } else {
    #join all by race, then by ethnicity, then join both together - from expand_from_census.R
#    hh_type_race_dt # "race" "family"  "family_type" "family_role" "single_hh_sex"  "tract" 
#    occupied_race_dt #"own_rent" "ethnicity" "tract"
#    occup_type_dt #"own_rent"  "family" "family_type" "partner_present" "householder_age"   "tract"
#    housing_units_race_dt #ethnicity, housing_units, tract
#    housing_units_rent_dt #own_rent housing_units, tract
#    hh_age_dt - own_rent householder_age_9
    ####
    #join hh_age_dt (householder_age_9) to occup_type_dt by own_rent and age - there's minimum info lost, just detail on age assigned at random inside category and tract
    #test <- table(hh_age_dt$tract,hh_age_dt$own_rent)==table(occup_type_dt$tract,occup_type_dt$own_rent)
###Perhaps a way of writing it up is to say that it's a problem of counting the edges of a hypercube. 
    #You sort like long sticks, where you only see the point, then you go to the other side and sort by that - and as you go along, 
    #you get to the state where the individuals are determined by the sorting.
    #category theory is the thought that these views on the hypercube are a metric - the categories for what it means to be
    #an individual, as categorized by the census, etc., and the transformations are the new ways you can look at the cube.
    #what if there's a transformation of the metric, and not just the view??

  ###NEED TO BUILD ETHNICITY UP ALONG WITH RACE TO COORDINATE AS WE BUILD -- DRAW IT OUT... go back into sam_eth and then rename...
    #there's a chance it won't resolve back, so could test and then run again hoping the samples fall in the right race categories
    #per ethnicity....
###    occupied_race_dt with occup_type_dt 
    sam_race_hh <- occupied_race_dt[,c("tract","race","own_rent")]
    sam_eth_hh <- occupied_eth_dt[,c("tract","ethnicity","own_rent")]
#    rm(occupied_race_dt)
#    rm(occupied_eth_dt)
    #sample inside group for id generation, in case there's oddness - set.seed(seed = seed) doesn't seem worth repeating every time
    #rule is to match until have a degree of freedom, sample in that group on each and then designate - ideal is to add only one
    #think of it as a sort of triangulation repeated
    #coordinate family_type back to own_rent, so that hh_type_race has an own_rent on it to match later
    occup_type_dt[order(match(family_type,c("Householder living alone","Householder not living alone",
                                            "Married-couple family","Other family")),
                        match(single_hh_sex,c("Female householder no husband present",
                                              "Male householder no wife present"))),
                  ("rent_type_id"):=paste0(tract,family_type,single_hh_sex,as.character(1000000+sample(.N))),
                  by=.(tract,family_type,single_hh_sex)]
    hh_type_race_dt[order(match(family_type,c("Householder living alone","Householder not living alone",
                                              "Married-couple family","Other family")),
                          match(single_hh_sex,c("Female householder no husband present",
                                                "Male householder no wife present"))),
                    ("rent_type_id"):=paste0(tract,family_type,single_hh_sex,as.character(1000000+sample(.N))),
                    by=.(tract,family_type,single_hh_sex)]
    hh_type_race_dt[,c("own_rent"):=
                  occup_type_dt[.SD, list(own_rent), on = .(rent_type_id)]]
    hh_type_race_dt[,("rent_type_id"):=NULL]
    hh_type_eth_dt[order(match(family_type,c("Householder living alone","Householder not living alone",
                                             "Married-couple family","Other family")),
                         match(single_hh_sex,c("Female householder no husband present",
                                               "Male householder no wife present"))),
                   ("rent_type_id"):=paste0(tract,family_type,single_hh_sex,as.character(1000000+sample(.N))),
                   by=.(tract,family_type,single_hh_sex)]
    hh_type_eth_dt[,c("own_rent"):=
                      occup_type_dt[.SD, list(own_rent), on = .(rent_type_id)]]
    hh_type_eth_dt[,("rent_type_id"):=NULL]
    
    #add family_type info to sam_race/eth_hh
    sam_race_hh[order(race),
             ("race_id"):=paste0(tract,race,as.character(1000000+sample(.N))),by=.(tract,race)]
    hh_type_race_dt[order(race),
                    ("race_id"):=paste0(tract,race,as.character(1000000+sample(.N))),by=.(tract,race)]
    sam_race_hh[,c("own_rent","family","family_type","family_role","single_hh_sex"):=
                  hh_type_race_dt[.SD, c(list(own_rent),list(family),list(family_type),list(family_role),list(single_hh_sex)), 
                                  on = .(race_id)]]
    sam_race_hh[,("race_id"):=NULL]
    sam_eth_hh[order(ethnicity),
             ("ethnicity_id"):=paste0(tract,ethnicity,as.character(1000000+sample(.N))),by=.(tract,ethnicity)]
    hh_type_eth_dt[order(ethnicity),
               ("ethnicity_id"):=paste0(tract,ethnicity,as.character(1000000+sample(.N))),by=.(tract,ethnicity)]
    sam_eth_hh[,c("own_rent","family","family_type","family_role","single_hh_sex"):=
                 hh_type_eth_dt[.SD, c(list(own_rent),list(family),list(family_type),list(family_role),list(single_hh_sex)), 
                                on = .(ethnicity_id)]]
    sam_eth_hh[,("ethnicity_id"):=NULL]
    #test <- table(sam_race_hh$tract,sam_race_hh$race,sam_race_hh$family,sam_race_hh$family_type,sam_race_hh$single_hh_sex)==
    #  table(hh_type_race_dt$tract,hh_type_race_dt$race,hh_type_race_dt$family,hh_type_race_dt$family_type,hh_type_race_dt$single_hh_sex)
    #length(test[test==FALSE])/length(test)
    
#add householder_age_9 to occup_type_dt - (just a specification inside householder age, here with own_rent)
    occup_type_dt[order(match(own_rent,c("Owner occupied","Renter occupied")),
                        match(householder_age,c("Householder 15 to 34 years","Householder 35 to 64 years", "Householder 65 years and over"))),
                  ("num_type_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    hh_age_dt[order(match(own_rent,c("Owner occupied","Renter occupied")),
                    match(householder_age_9,c("Householder 15 to 24 years","Householder 25 to 34 years","Householder 35 to 44 years",
                                              "Householder 45 to 54 years","Householder 55 to 59 years","Householder 60 to 64 years",
                                              "Householder 65 to 74 years","Householder 75 to 84 years"))),
              ("num_type_id"):=paste0(tract,own_rent,as.character(1000000++seq.int(1:.N))),by=.(tract,own_rent)]
    occup_type_dt[,c("householder_age_9"):=hh_age_dt[.SD, list(householder_age_9), on = .(num_type_id)]]
    #test <- table(hh_age_dt$tract,hh_age_dt$householder_age_9)==table(occup_type_dt$tract,occup_type_dt$householder_age_9)
    
#add householder_age (3 labels) and householder_age_9 to sam
    sam_race_hh[order(match(own_rent,c("Owner occupied","Renter occupied")),
                      match(family_type,c("Householder living alone","Householder not living alone",
                                          "Married-couple family","Other family")),
                      match(single_hh_sex,c("Female householder no husband present",
                                            "Male householder no wife present"))),
             ("rent_type_id"):=paste0(tract,own_rent,family_type,single_hh_sex,as.character(1000000+sample(.N))),
             by=.(tract,own_rent,family_type,single_hh_sex)]
    occup_type_dt[order(match(own_rent,c("Owner occupied","Renter occupied")),
                      match(family_type,c("Householder living alone","Householder not living alone",
                                          "Married-couple family","Other family")),
                      match(single_hh_sex,c("Female householder no husband present",
                                            "Male householder no wife present"))),
                ("rent_type_id"):=paste0(tract,own_rent,family_type,single_hh_sex,as.character(1000000+sample(.N))),
                by=.(tract,own_rent,family_type,single_hh_sex)]
    sam_race_hh[,c("householder_age_3","householder_age_9"):=
                  occup_type_dt[.SD, c(list(householder_age),list(householder_age_9)), on = .(rent_type_id)]]
    sam_race_hh[,("rent_type_id"):=NULL]
    sam_eth_hh[order(match(own_rent,c("Owner occupied","Renter occupied")),
                     match(family_type,c("Householder living alone","Householder not living alone",
                                         "Married-couple family","Other family")),
                     match(single_hh_sex,c("Female householder no husband present",
                                           "Male householder no wife present"))),
               ("rent_type_id"):=paste0(tract,own_rent,family_type,single_hh_sex,as.character(1000000+sample(.N))),
               by=.(tract,own_rent,family_type,single_hh_sex)]
    sam_eth_hh[,c("householder_age_3","householder_age_9"):=
                  occup_type_dt[.SD, c(list(householder_age),list(householder_age_9)), on = .(rent_type_id)]]
    sam_eth_hh[,("rent_type_id"):=NULL]
#    test <- table(sam_race_hh$tract,sam_race_hh$own_rent,sam_race_hh$householder_age_9)==
#            table(occup_type_dt$tract,occup_type_dt$own_rent,occup_type_dt$householder_age_9)
 #   length(test[test==FALSE])/length(test)
    

#add family type for housing units to housing units by rent_own and by race - giving units as much to match on as possible by building a set, aligned with sam as we build when there's plenty of space for multiple solutions
    #start with own_rent, since it has lots of freedom
    #fix labeling on unit naming
    housing_units_rent_dt[,("num_structures"):=case_when(
      housing_units=="1 attached" | housing_units=="1 detached" ~ "1-unit structures",
      housing_units=="Boat RV van etc." | housing_units=="Mobile home" ~ "Mobile homes and all other types of units",
      TRUE ~ "2-or-more-unit structures"
    )]
    sam_race_hh[,("family_role_4"):=if_else(family_role=="Householder living alone" | family_role=="Householder not living alone",
                                 "Nonfamily households",family_role)]
    sam_eth_hh[,("family_role_4"):=if_else(family_role=="Householder living alone" | family_role=="Householder not living alone",
                                 "Nonfamily households",family_role)]
  #add race/eth and family_role_4 by own_rent from sam_race/eth_hh to the hh_units build
    sam_race_hh[order(match(own_rent,c("Owner occupied","Renter occupied"))),
                ("rent_type_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),
                by=.(tract,own_rent)]
    housing_units_rent_dt[order(match(own_rent,c("Owner occupied","Renter occupied"))),
                  ("rent_type_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),
                  by=.(tract,own_rent)]
    hh_units_rent_race <- housing_units_rent_dt[,c("race","family_role_4"):=
                                                  sam_race_hh[.SD, c(list(race),list(family_role_4)), 
                                                              on = .(rent_type_id)]]
    sam_race_hh[,("rent_type_id"):=NULL]
    sam_eth_hh[order(match(own_rent,c("Owner occupied","Renter occupied"))),
               ("rent_type_id"):=paste0(tract,own_rent,as.character(1000000+sample(.N))),
               by=.(tract,own_rent)]
    hh_units_rent_eth <- housing_units_rent_dt[,c("ethnicity","family_role_4"):=
                                                  sam_eth_hh[.SD, c(list(ethnicity),list(family_role_4)), 
                                                             on = .(rent_type_id)]]
    sam_eth_hh[,("rent_type_id"):=NULL]
    
    #add units by family_role_4 and num_structures to hh_units build
    hh_type_units_dt[order(match(family_role_4,c("Female householder no husband present",
                                                 "Male householder no wife present",
                                                 "Married-couple family","Nonfamily households")),
                           match(num_structures,c("1-unit structures","2-or-more-unit structures",
                                                  "Mobile homes and all other types of units"))),
                     ("hh_type_units_id"):=paste0(tract,family_role_4,num_structures,as.character(1000000+seq.int(1:.N))),
                     by=.(tract,family_role_4,num_structures)]
    hh_units_rent_race[order(match(family_role_4,c("Female householder no husband present",
                                                      "Male householder no wife present",
                                                      "Married-couple family","Nonfamily households")),
                                match(num_structures,c("1-unit structures","2-or-more-unit structures",
                                                       "Mobile homes and all other types of units"))),
                     ("hh_type_units_id"):=paste0(tract,family_role_4,num_structures,as.character(1000000+seq.int(1:.N))),
                     by=.(tract,family_role_4,num_structures)]
    hh_units_rent_eth[order(match(family_role_4,c("Female householder no husband present",
                                                   "Male householder no wife present",
                                                   "Married-couple family","Nonfamily households")),
                             match(num_structures,c("1-unit structures","2-or-more-unit structures",
                                                    "Mobile homes and all other types of units"))),
                       ("hh_type_units_id"):=paste0(tract,family_role_4,num_structures,as.character(1000000+seq.int(1:.N))),
                       by=.(tract,family_role_4,num_structures)]
    hh_units_rent_race[,c("family_role_4"):=
                    hh_type_units_dt[.SD,list(family_role_4), on = .(hh_type_units_id)]]
    hh_units_rent_eth[,c("family_role_4"):=
                         hh_type_units_dt[.SD,list(family_role_4), on = .(hh_type_units_id)]]
    
    #now put units back onto sam_race/eth_hh using race/ethnicity, own_rent, family_role_4
    hh_units_rent_race[order(race,
                            match(own_rent,c("Owner occupied","Renter occupied")),
                            match(family_role_4,c("Female householder no husband present",
                                                  "Male householder no wife present",
                                                  "Married-couple family","Nonfamily households"))),
                      ("hh_units_id"):=paste0(tract,race,own_rent,family_role_4,as.character(1000000+seq.int(1:.N))),
                      by=.(tract,race,own_rent,family_role_4)]
    sam_race_hh[order(race,
                      match(own_rent,c("Owner occupied","Renter occupied")),
                      match(family_role_4,c("Female householder no husband present",
                                            "Male householder no wife present",
                                            "Married-couple family","Nonfamily households"))),
                ("hh_units_id"):=paste0(tract,race,own_rent,family_role_4,as.character(1000000+seq.int(1:.N))),
                by=.(tract,race,own_rent,family_role_4)]
    sam_race_hh[,c("housing_units"):=
                  hh_units_rent_race[.SD,list(housing_units), on = .(hh_units_id)]]
    sam_race_hh[,("hh_units_id"):=NULL]
    
    hh_units_rent_eth[order(ethnicity,
                            match(own_rent,c("Owner occupied","Renter occupied")),
                            match(family_role_4,c("Female householder no husband present",
                                                  "Male householder no wife present",
                                                  "Married-couple family","Nonfamily households"))),
                      ("hh_units_id"):=paste0(tract,ethnicity,own_rent,family_role_4,as.character(1000000+seq.int(1:.N))),
                      by=.(tract,ethnicity,own_rent,family_role_4)]
    sam_eth_hh[order(ethnicity,
                     match(own_rent,c("Owner occupied","Renter occupied")),
                     match(family_role_4,c("Female householder no husband present",
                                           "Male householder no wife present",
                                           "Married-couple family","Nonfamily households"))),
               ("hh_units_id"):=paste0(tract,ethnicity,own_rent,family_role_4,as.character(1000000+seq.int(1:.N))),
               by=.(tract,ethnicity,own_rent,family_role_4)]
    
    sam_eth_hh[,c("housing_units"):=
                 hh_units_rent_eth[.SD,list(housing_units), on = .(hh_units_id)]]
    sam_eth_hh[,("hh_units_id"):=NULL]
#    test <- table(sam_eth_hh$tract,sam_eth_hh$ethnicity,sam_eth_hh$own_rent,sam_eth_hh$family_role_4,sam_eth_hh$housing_units)==
 #     table(hh_units_rent_eth$tract,hh_units_rent_eth$ethnicity,hh_units_rent_eth$own_rent,hh_units_rent_eth$family_role_4,hh_units_rent_eth$housing_units)
    
    #bring sam_eth_hh and sam_race_hh together again
#    sam_race_hh[order(match(own_rent,c("Owner occupied","Renter occupied")),
#                      match(family_type,c("Householder living alone","Householder not living alone",
#                                          "Married-couple family","Other family")),
#                      match(single_hh_sex,c("Female householder no husband present",
#                                            "Male householder no wife present"))),#trying without specifying order for householder_age_9
#                ("join_race_id"):=paste0(tract,own_rent,family_type,single_hh_sex,householder_age_9,
#                                         housing_units,as.character(1000000+sample(.N))),
#                by=.(tract,own_rent,family_type,single_hh_sex,householder_age_9,housing_units)]
#    sam_eth_hh[order(match(own_rent,c("Owner occupied","Renter occupied")),
#                     match(family_type,c("Householder living alone","Householder not living alone",
#                                         "Married-couple family","Other family")),
#                     match(single_hh_sex,c("Female householder no husband present",
#                                           "Male householder no wife present"))),
#               ("join_race_id"):=paste0(tract,own_rent,family_type,single_hh_sex,householder_age_9,
#                                        housing_units,as.character(1000000+sample(.N))),
#               by=.(tract,own_rent,family_type,single_hh_sex,householder_age_9,housing_units)]
#    sam_race_hh[,c("ethnicity"):=
#                  sam_eth_hh[.SD, list(ethnicity), on = .(join_race_id)]]
#    sam_race_hh[,("join_race_id"):=NULL]
    #test <- table(sam_eth_hh$tract,sam_eth_hh$ethnicity,sam_eth_hh$family_type)==table(sam_race_hh$tract,sam_race_hh$ethnicity,sam_race_hh$family_type)
    
    #separate into sam_race_hh and sam_eth_hh to track the other stuff easier (they are just subsets, so could be done together, but want them to run separately until consumed all the ones that have them separate)
#    sam_eth_hh <- sam_race_hh
#    sam_eth_hh[,("race"):=NULL]
#    sam_race_hh[,("ethnicity"):=NULL]
    
    #per_room has race, own_rent, and age - build from matching with sam_hh, then match per_room
    
#then add per_room  with own_rent, race and age_range_3 to match - 
    #put own_rent by race on housing_per_room_race, then match back on num_per and own_rent
    sam_race_hh[order(race),
                ("race_id"):=paste0(tract,race,as.character(1000000+sample(.N))),
                by=.(tract,race)]
    housing_per_room_race_dt[order(race),
                             ("race_id"):=paste0(tract,race,as.character(1000000+sample(.N))),
                             by=.(tract,race)]
    housing_per_room_race_dt[,c("own_rent"):=
                               sam_race_hh[.SD, list(own_rent), 
                                           on = .(race_id)]]
    sam_race_hh[,("race_id"):=NULL]
    sam_eth_hh[order(ethnicity),
               ("eth_id"):=paste0(tract,ethnicity,as.character(1000000+sample(.N))),
               by=.(tract,ethnicity)]
    housing_per_room_eth_dt[order(ethnicity),
                             ("eth_id"):=paste0(tract,ethnicity,as.character(1000000+sample(.N))),
                             by=.(tract,ethnicity)]
    housing_per_room_eth_dt[,c("own_rent"):=
                              sam_eth_hh[.SD, list(own_rent), 
                                         on = .(eth_id)]]
    sam_eth_hh[,("eth_id"):=NULL]
    #own_rent is broken out from race, so add it back in per_room eth/race 
    housing_per_room_race_dt[order(num_per,
                                   match(own_rent,c("Owner occupied","Renter occupied"))),
                             ("num_per_type_id"):=paste0(tract,num_per,as.character(1000000+sample(.N))),
                             by=.(tract,num_per)]
    housing_per_room_rent_dt[order(num_per,
                                   match(own_rent,c("Owner occupied","Renter occupied"))),
                             ("num_per_type_id"):=paste0(tract,num_per,as.character(1000000+sample(.N))),
                             by=.(tract,num_per)]
    housing_per_room_race_dt[,c("num_per_room_5"):= 
                               housing_per_room_rent_dt[.SD, list(num_per_room_5),
                                                        on = .(num_per_type_id)]]
    housing_per_room_eth_dt[order(num_per,
                                  match(own_rent,c("Owner occupied","Renter occupied"))),
                             ("num_per_type_id"):=paste0(tract,num_per,as.character(1000000+sample(.N))),
                             by=.(tract,num_per)]
    housing_per_room_eth_dt[,c("num_per_room_5"):= 
                               housing_per_room_rent_dt[.SD, list(num_per_room_5), 
                                                        on = .(num_per_type_id)]]
    #then add householder_age_3 from sam to here by own_rent and race
    sam_race_hh[order(race,
                      match(own_rent,c("Owner occupied","Renter occupied"))),
                ("age_per_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(.N))),
                by=.(tract,race,own_rent)]
    housing_per_room_race_dt[order(race,
                                   match(own_rent,c("Owner occupied","Renter occupied"))),
                             ("age_per_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(.N))),
                             by=.(tract,race,own_rent)]
    housing_per_room_race_dt[,c("householder_age_3"):=
                               sam_race_hh[.SD, list(householder_age_3), 
                                           on = .(age_per_id)]]
    sam_race_hh[,("age_per_id"):=NULL]
    sam_eth_hh[order(ethnicity,
                     match(own_rent,c("Owner occupied","Renter occupied"))),
               ("age_per_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(.N))),
               by=.(tract,ethnicity,own_rent)]
    housing_per_room_eth_dt[order(ethnicity,
                                   match(own_rent,c("Owner occupied","Renter occupied"))),
                             ("age_per_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(.N))),
                             by=.(tract,ethnicity,own_rent)]
    housing_per_room_eth_dt[,c("householder_age_3"):=
                               sam_eth_hh[.SD, list(householder_age_3), 
                                           on = .(age_per_id)]]
    sam_eth_hh[,("age_per_id"):=NULL]
    
    #then add to sam_hh by age, own_rent and eth/race...
    housing_per_room_race_dt[order(race,
                                   match(own_rent,c("Owner occupied","Renter occupied")),
                                   match(householder_age_3,c("Householder 15 to 34 years",
                                                           "Householder 35 to 64 years", 
                                                           "Householder 65 years and over"))),
                             ("num_per_id"):=paste0(tract,race,own_rent,householder_age_3,as.character(1000000+sample(.N))),
                             by=.(tract,race,own_rent,householder_age_3)]
    sam_race_hh[order(race,
                      match(own_rent,c("Owner occupied","Renter occupied")),
                      match(householder_age_3,c("Householder 15 to 34 years",
                                              "Householder 35 to 64 years", 
                                              "Householder 65 years and over"))),
                ("num_per_id"):=paste0(tract,race,own_rent,householder_age_3,as.character(1000000+sample(.N))),
                by=.(tract,race,own_rent,householder_age_3)]
    sam_race_hh[,c("people_per_room"):= 
                  housing_per_room_race_dt[.SD, list(num_per_room_5),
                                          on = .(num_per_id)]]
    sam_race_hh[,("num_per_id"):=NULL]
    housing_per_room_eth_dt[order(ethnicity,
                                  match(own_rent,c("Owner occupied","Renter occupied")),
                                  match(householder_age_3,c("Householder 15 to 34 years",
                                                          "Householder 35 to 64 years", 
                                                          "Householder 65 years and over"))),
                            ("num_per_id"):=paste0(tract,ethnicity,own_rent,householder_age_3,as.character(1000000+sample(.N))),
                            by=.(tract,ethnicity,own_rent,householder_age_3)]
    sam_eth_hh[order(ethnicity,
                     match(own_rent,c("Owner occupied","Renter occupied")),
                     match(householder_age_3,c("Householder 15 to 34 years",
                                             "Householder 35 to 64 years", 
                                             "Householder 65 years and over"))),
               ("num_per_id"):=paste0(tract,ethnicity,own_rent,householder_age_3,as.character(1000000+sample(.N))),
               by=.(tract,ethnicity,own_rent,householder_age_3)]
    sam_eth_hh[,c("people_per_room"):= 
                  housing_per_room_eth_dt[.SD, list(num_per_room_5),
                                           on = .(num_per_id)]]
    sam_eth_hh[,("num_per_id"):=NULL]

#    test <- table(sam_eth_hh$tract,sam_eth_hh$own_rent,sam_eth_hh$family_type,sam_eth_hh$single_hh_sex,sam_eth_hh$householder_age_9,
#                  sam_eth_hh$housing_units,sam_eth_hh$people_per_room)==
#      table(sam_race_hh$tract,sam_race_hh$own_rent,sam_race_hh$family_type,sam_race_hh$single_hh_sex,sam_race_hh$householder_age_9,
#            sam_race_hh$housing_units,sam_race_hh$people_per_room)
#    length(test[test==FALSE])/length(test) #0.007968927 - have a few cells with fairly large number discrepancy
    
#    saveRDS(sam_eth_hh,file = paste0(housingdir, vintage, "/sam_eth_hh.419",Sys.Date(),".RDS"))
#    saveRDS(sam_race_hh,file = paste0(housingdir, vintage, "/sam_race_hh.420",Sys.Date(),".RDS"))

#the complicated reorders don't do any better than match on race by order given here - which is interesting...
#bring sam_eth_hh and sam_race_hh together again (colnames for each...)
    sam_race_hh[order(match(race,c("A","F","G","C","B","E","D"))), #
                ("join_race_id"):=paste0(tract,own_rent,family_type,single_hh_sex,householder_age_3,
                                         as.character(1000000+seq.int(1:.N))),
                by=.(tract,own_rent,family_type,single_hh_sex,householder_age_3)]
    #sam_eth_hh[ethnicity=="_",("ethnicity"):="none applied"]
    sam_eth_hh[order(match(ethnicity,c("H","I","_"))),
               ("join_race_id"):=paste0(tract,own_rent,family_type,single_hh_sex,householder_age_3,
                                        as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent,family_type,single_hh_sex,householder_age_3)]
    sam_race_hh[,c("ethnicity"):=
                  sam_eth_hh[.SD, list(ethnicity), on = .(join_race_id)]]
    sam_race_hh[,("join_race_id"):=NULL]
    #have to use a nrow.SD to move over to keep overall straight??
    #clean up strays - a few hundred in mutually exclusive categories? my guess is that they are places where there are more H than A, etc.
    #sam_race_hh[ethnicity==H & race==B,("ethnicity"):=]
    #test <- table(sam_eth_hh$tract,sam_eth_hh$ethnicity,sam_eth_hh$family_type)==table(sam_race_hh$tract,sam_race_hh$ethnicity,sam_race_hh$family_type)
    
    sam_hh <- sam_race_hh #adding hh characteristics that don't have race/ethnicity broken up
    
#add hh_size to occup
    #add family and family_type to occup_size by matching with sam_hh on own_rent
    sam_hh[order(-people_per_room),("hh_size_or_id"):=
             paste0(tract,own_rent,if_else(family_type=="Householder living alone","A","B"),as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent,family_type=="Householder living alone")]
    hh_occup_size_dt[order(hh_size),("hh_size_or_id"):=
                       paste0(tract,own_rent,if_else(family_type=="Householder living alone","A","B"),
                                               as.character(1000000+seq.int(1:.N))),
                     by=.(tract,own_rent,family_type=="Householder living alone")]
    hh_occup_size_dt[,c("family_or_non","family_type"):=
                 sam_hh[.SD, c(list(family),list(family_type)), 
                                  on = .(hh_size_or_id)]]
    sam_hh[,("hh_size_or_id"):=NULL]
    #test<-table(hh_occup_size_dt$tract,hh_occup_size_dt$own_rent,hh_occup_size_dt$family)==table(sam_hh$tract,sam_hh$own_rent,sam_hh$family)
    #add own_rent to hh_size_dt from sam
    sam_hh[,("hh_size_fam_id"):=paste0(tract,family,as.character(1000000+sample(.N))),
           by=.(tract,family)]
    hh_size_dt[,("hh_size_fam_id"):=paste0(tract,family_or_non,as.character(1000000+sample(.N))),
                     by=.(tract,family_or_non)]
    hh_size_dt[,c("own_rent"):=
                       sam_hh[.SD, list(own_rent), 
                              on = .(hh_size_fam_id)]]
    sam_hh[,("hh_size_fam_id"):=NULL]
    #add family_type to hh_size, but the match on hh_size has hh_size and family
    #add own_rent and family_type to hh_size_dt by matching to hh_occup_size on family and hh_size
    hh_size_dt[,("hh_size_id"):=paste0(tract,own_rent,family_or_non,as.character(1000000+sample(.N))),
                by=.(tract,own_rent,family_or_non)]
    hh_occup_size_dt[,("hh_size_id"):=paste0(tract,own_rent,family_or_non,as.character(1000000+sample(.N))),
                             by=.(tract,own_rent,family_or_non)]
    hh_size_dt[,c("family_type"):=
                 hh_occup_size_dt[.SD, list(family_type), 
                                           on = .(hh_size_id)]]
    hh_size_dt[,("hh_size_id"):=NULL]
    #trying combinations - using own_rent, too, gets 24808 not matching so took it out
    sam_hh[,("hh_size_id"):=paste0(tract,family,if_else(family_type=="Householder living alone","A","B"),as.character(1000000+sample(.N))),
           by=.(tract,family,family_type=="Householder living alone")]
    hh_size_dt[,("hh_size_id"):=paste0(tract,family_or_non,if_else(hh_size=="1-person household","A","B"),as.character(1000000+sample(.N))),
               by=.(tract,family_or_non,hh_size=="1-person household")]
    sam_hh[,c("hh_size"):=
             hh_size_dt[.SD, list(hh_size), 
                        on = .(hh_size_id)]]
    sam_hh[,("hh_size_id"):=NULL]
    
#do rooms and bedrooms by person_per_room and hh_size
    sam_hh[,("calc_rooms") := as.numeric(substr(hh_size,1,1))*as.numeric(substr(people_per_room,1,3))]
    sam_hh[order(calc_rooms),("rooms_id") := paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
           by=.(tract,own_rent)]
    hh_occup_rooms_dt[order(substr(num_rooms,1,1)),
                      ("rooms_id") := paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                      by=.(tract,own_rent)]
    sam_hh[,c("num_rooms"):=
             hh_occup_rooms_dt[.SD, list(num_rooms),
                               on = .(rooms_id)]]
    
    hh_occup_bedrooms_dt[order(substr(num_bedrooms,1,1)),
                         ("rooms_id") := paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                         by=.(tract,own_rent)]
    sam_hh[,c("num_bedrooms"):=
             hh_occup_bedrooms_dt[.SD, list(num_bedrooms),
                               on = .(rooms_id)]]
    
    sam_hh[,("rooms_id"):=NULL]
    sam_hh[,("calc_rooms"):=NULL]
    
#then start making individuals to then match with hh, and to line up with other income and employment info not provided by hh    
#idea is that you can add individuals to figure out origin, married, workers, then expand from hh_size
        
    #put sex on sam_hh for matching
    sam_hh[str_detect(family_role,"Female"),("sex"):="Female"]
    sam_hh[family_role=="Householder living alone",("sex"):=sample(c("Male","Female"),.N,replace = TRUE)]
    sam_hh[is.na(sex),("sex"):="Male"]
    
    #give all households an id
    sam_hh[,("household_id"):=paste0(tract,as.character(1000000+sample(.N))),by=.(tract)]
    #ADD ALL THE OTHER HH STUFF, LIKE BEFORE, THEN GO TO HH
    
    
    
    
    #merge the ones with race first then the ones with ethnicity, then do a merge on all those factors for ethnicity back to race...
    sex_age_race[,"individual_id":=paste0(tract,as.character(2000000+sample(.N))),by=.(tract)]
    
    
#put marital_status temp on sex_age_race by sex and race; put age on marital_status_race; then use that to match with marital_status_age, then put final marital_status, etc. on
    #add more age categories and race to marital_status_age and preg_age, for better matching
    #move preg data to sex_age_race - sort by age, since it's not even on age_ranges, and sampling for age isn't exact for sex_age_race
    sex_age_race[age>14 & order(age),
                 ("married_id"):=paste0(tract,sex,race,
                                        as.character(1000000+seq.int(1:.N))),
                 by=.(tract,sex,race)]
    marital_status_race_dt[,
                           ("married_id"):=paste0(tract,sex,race,
                                                  as.character(1000000+sample(.N))),
                           by=.(tract,sex,race)]
    sex_age_race[,c("marital_status_tmp"):=
                  marital_status_race_dt[.SD, list(marital_status), on = .(married_id)]]
    marital_status_race_dt[,c("age"):=
                             sex_age_race[.SD, list(age), on = .(married_id)]]
    sex_by_age_eth[age>14 & order(age),
                 ("married_id"):=paste0(tract,sex,ethnicity,
                                        as.character(1000000+seq.int(1:.N))),
                 by=.(tract,sex,ethnicity)]
    marital_status_eth_dt[,
                           ("married_id"):=paste0(tract,sex,ethnicity,
                                                  as.character(1000000+sample(.N))),
                           by=.(tract,sex,ethnicity)]
    sex_by_age_eth[,c("marital_status_tmp"):=
                   marital_status_eth_dt[.SD, list(marital_status), on = .(married_id)]]
#age is a mess on eth - many different categories for some reason    
    marital_status_eth_dt[,c("age"):=
                            sex_by_age_eth[.SD, list(substr(age_range,1,2)), on = .(married_id)]] 
    sex_age_race$married_id <- NULL
    sex_by_age_eth$married_id <- NULL
    
    #add race/eth to marital_status_age from sex_age_race/eth
    marital_status_age_dt[order(age_range_marital),
                          ("married_id1"):=paste0(tract,sex,marital_status,
                                                 as.character(1000000+seq.int(1:.N))),
                          by=.(tract,sex,marital_status)]
    sex_age_race[order(age) & !is.na(marital_status_tmp),
                          ("married_id1"):=paste0(tract,sex,marital_status_tmp,
                                                  as.character(1000000+seq.int(1:.N))),
                          by=.(tract,sex,marital_status_tmp)]
    marital_status_age_dt[,c("race"):=
                            sex_age_race[.SD, list(race), on = .(married_id1)]]
    sex_by_age_eth[order(as.numeric(substr(age_range,1,2))) & !is.na(marital_status_tmp),
                 ("married_id1"):=paste0(tract,sex,marital_status_tmp,
                                         as.character(1000000+seq.int(1:.N))),
                 by=.(tract,sex,marital_status_tmp)]
    marital_status_age_dt[,c("ethnicity"):=
                            sex_by_age_eth[.SD, list(ethnicity), on = .(married_id1)]]
    sex_age_race$married_id1 <- NULL
    sex_by_age_eth$married_id1 <- NULL
    
#put together preg and marital_status by race and age - slightly more women by age in oldest group for pregnant_age women, b/c of how sampling is done on sex_age_race and non-overlapping categories
    marital_status_race_dt[order(age),("married2_id"):=paste0(tract,sex,marital_status,race,
                                      as.character(1000000+seq.int(1:.N))),
                 by=.(tract,sex,marital_status,race)]
    marital_status_eth_dt[order(age),("married3_id"):=paste0(tract,sex,marital_status,ethnicity,
                                     as.character(1000000+seq.int(1:.N))),
                by=.(tract,sex,marital_status,ethnicity)]
    marital_status_age_dt[order(age_range_marital),("married2_id"):=paste0(tract,sex,marital_status,race,
                                     as.character(1000000+seq.int(1:.N))),
                by=.(tract,sex,marital_status,race)]
    marital_status_age_dt[order(age_range_marital),("married3_id"):=paste0(tract,sex,marital_status,ethnicity,
                                                                           as.character(1000000+seq.int(1:.N))),
                          by=.(tract,sex,marital_status,ethnicity)]
    marital_status_race_dt[,c("spouse_present","separated","age_range_marital"):=
                             marital_status_age_dt[.SD, c(list(spouse_present),list(separated),list(age_range_marital)), on = .(married2_id)]]
    marital_status_eth_dt[,c("spouse_present","separated","age_range_marital"):=
                  marital_status_age_dt[.SD, c(list(spouse_present),list(separated),list(age_range_marital)), on = .(married3_id)]]
    marital_status_age_dt[,
                          c("missing"):=
                            marital_status_eth_dt[.SD, list(age_range_marital), on = .(married3_id)]]
    marital_status_eth_dt[is.na(age_range_marital),
                          ("married4_id"):=paste0(tract,sex,marital_status,
                                                  as.character(1000000+seq.int(1:.N))),
                          by=.(tract,sex,marital_status)]
    marital_status_age_dt[is.na(missing),
                          ("married4_id"):=paste0(tract,sex,marital_status,
                                                  as.character(1000000+seq.int(1:.N))),
                          by=.(tract,sex,marital_status)]
    marital_status_eth_dt[is.na(age_range_marital),c("spouse_present","separated","age_range_marital"):=
                            marital_status_age_dt[.SD, c(list(spouse_present),list(separated),list(age_range_marital)), on = .(married4_id)]]
    marital_status_race_dt[,("married2_id"):=NULL]
    marital_status_eth_dt[,("married3_id"):=NULL]
    marital_status_eth_dt[,("married4_id"):=NULL]
    #test <- table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$marital_status,marital_status_age_dt$spouse_present,marital_status_age_dt$separated,marital_status_age_dt$age_range_marital)==
     # table(marital_status_race_dt$tract,marital_status_race_dt$sex,marital_status_race_dt$marital_status,marital_status_race_dt$spouse_present,marital_status_race_dt$separated,marital_status_race_dt$age_range_marital)
    
    preg_race_dt[,("preg_id"):=paste0(tract,birth_label,married,
                                      as.character(1000000+seq.int(1:.N))),
                 by=.(tract,birth_label,married)]
    preg_eth_dt[,("preg_id"):=paste0(tract,birth_label,married,
                                     as.character(1000000+seq.int(1:.N))),
                by=.(tract,birth_label,married)]
    preg_age_dt[,("preg_id"):=paste0(tract,birth_label,married,
                                     as.character(1000000+seq.int(1:.N))),
                by=.(tract,birth_label,married)]
    preg_race_dt[,c("preg_age_range"):=
                   preg_age_dt[.SD, list(preg_age_range), on = .(preg_id)]]
    preg_eth_dt[,c("preg_age_range"):=
                  preg_age_dt[.SD, list(preg_age_range), on = .(preg_id)]]
    preg_race_dt[,("preg_id"):=NULL]
    preg_eth_dt[,("preg_id"):=NULL]
    
    
    marital_status_race_dt[order(age_range_marital),
                           ("married_preg_id"):=paste0(tract,race,sex,if_else(str_detect(marital_status,"Now"),"Now","Not_now"),
                                                       as.character(1000000+seq.int(1:.N))),
                           by=.(tract,race,sex,str_detect(marital_status,"Now"))]
    preg_race_dt[order(preg_age_range),
                           ("married_preg_id"):=paste0(tract,race,sex,if_else(str_detect(married,"Now"),"Now","Not_now"),
                                                       as.character(1000000+seq.int(1:.N))),
                           by=.(tract,race,sex,str_detect(married,"Now"))]
    marital_status_race_dt[,c("pregnant"):=
                             preg_race_dt[.SD, list(birth_label), on = .(married_preg_id)]]
    
    marital_status_eth_dt[order(age_range_marital),
                          ("married_preg_id"):=paste0(tract,ethnicity,sex,if_else(str_detect(marital_status,"Now"),"Now","Not_now"),
                                                      as.character(1000000+seq.int(1:.N))),
                          by=.(tract,ethnicity,sex,str_detect(marital_status,"Now"))]
    preg_eth_dt[order(preg_age_range),
                 ("married_preg_id"):=paste0(tract,ethnicity,sex,if_else(str_detect(married,"Now"),"Now","Not_now"),
                                             as.character(1000000+seq.int(1:.N))),
                 by=.(tract,ethnicity,sex,str_detect(married,"Now"))]
    marital_status_eth_dt[,c("pregnant"):=
                             preg_eth_dt[.SD, list(birth_label), on = .(married_preg_id)]]
    
    #join marital data to sex_age_race/eth
    sex_age_race[order(as.numeric(substr(age_range,1,2))),
                 ("married_join_id"):=paste0(tract,race,sex,
                                                      as.character(1000000+seq.int(1:.N))),
                          by=.(tract,race,sex)]
    marital_status_race_dt[order(age_range_marital),
                ("married_join_id"):=paste0(tract,race,sex,
                                            as.character(1000000+seq.int(1:.N))),
                by=.(tract,race,sex)]
    sex_age_race[,c("marital_status","spouse_present","separated","pregnant"):=
        marital_status_race_dt[.SD, c(list(marital_status),list(spouse_present),list(separated),list(pregnant)), 
                              on = .(married_join_id)]]
    
    sex_by_age_eth[order(as.numeric(substr(age_range,1,2))),
                   ("married_join_id"):=paste0(tract,ethnicity,sex,
                                               as.character(1000000+seq.int(1:.N))),
                   by=.(tract,ethnicity,sex)]
    marital_status_eth_dt[order(age_range_marital),
                          ("married_join_id"):=paste0(tract,ethnicity,sex,
                                                      as.character(1000000+seq.int(1:.N))),
                          by=.(tract,ethnicity,sex)]
    sex_by_age_eth[,c("marital_status","spouse_present","separated","pregnant"):=
                     marital_status_eth_dt[.SD, c(list(marital_status),list(spouse_present),list(separated),list(pregnant)), 
                                           on = .(married_join_id)]]
    
    
    #join back into sex_age_race - if you also match on pregnant, you get some missing - not sure what's going on so pulled pregnant from eth
    sex_age_race[order(match(race,c("A","F","G","C","B","E","D"))), #
                ("join_race_id"):=paste0(tract,sex,marital_status,spouse_present,separated,
                                         as.character(1000000+seq.int(1:.N))),
                by=.(tract,sex,marital_status,spouse_present,separated)]
    sex_by_age_eth[order(match(ethnicity,c("H","I","_"))),
               ("join_race_id"):=paste0(tract,sex,marital_status,spouse_present,separated,
                                        as.character(1000000+seq.int(1:.N))),
               by=.(tract,sex,marital_status,spouse_present,separated)]
    sex_age_race[,c("ethnicity","pregnant"):=
                   sex_by_age_eth[.SD, c(list(ethnicity),list(pregnant)), on = .(join_race_id)]]
    sex_age_race[,("join_race_id"):=NULL]
    
    
    ##hh_workers, below, matches hh and on hh_size is pretty reasonable
    ##
 

#    saveRDS(sam_hh,file = paste0(housingdir, vintage, "/sam_hh_l.427",Sys.Date(),".RDS"))
    

    
    #concept: TENURE BY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) - 1562813hh
    housing_occup_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25118") #of occup, own or rent by income
    housing_occup_income_data <- housing_occup_income_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_income_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_income_level"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(income_low=case_when(
                hh_income_level == "Less than $5 000" ~ as.integer(1),
                hh_income_level == "$5 000 to $9 999" ~ as.integer(5000),
                hh_income_level == "$10 000 to $14 999" ~ as.integer(10000),
                hh_income_level == "$15 000 to $19 999" ~ as.integer(15000),
                hh_income_level == "$20 000 to $24 999" ~ as.integer(20000),
                hh_income_level == "$25 000 to $34 999" ~ as.integer(25000),
                hh_income_level == "$35 000 to $49 999" ~ as.integer(35000),
                hh_income_level == "$50 000 to $74 999" ~ as.integer(50000),
                hh_income_level == "$75 000 to $99 999" ~ as.integer(75000),
                hh_income_level == "$100 000 to $149 999" ~ as.integer(100000),
                hh_income_level == "$150 000 or more" ~ as.integer(150000)),
            income_high=case_when(
              hh_income_level == "Less than $5 000" ~ as.integer(4999),
              hh_income_level == "$5 000 to $9 999" ~ as.integer(9999),
              hh_income_level == "$10 000 to $14 999" ~ as.integer(14999),
              hh_income_level == "$15 000 to $19 999" ~ as.integer(19999),
              hh_income_level == "$20 000 to $24 999" ~ as.integer(24999),
              hh_income_level == "$25 000 to $34 999" ~ as.integer(34999),
              hh_income_level == "$35 000 to $49 999" ~ as.integer(49999),
              hh_income_level == "$50 000 to $74 999" ~ as.integer(74999),
              hh_income_level == "$75 000 to $99 999" ~ as.integer(99999),
              hh_income_level == "$100 000 to $149 999" ~ as.integer(149999),
              hh_income_level == "$150 000 or more" ~ as.integer(500000)) #have to think about skew on this one... ~.006 make more than 500000
      ) %>%
      filter(!is.na(hh_income_level) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hhincome_id",.remove = TRUE)
    hh_income_dt <- as.data.table(housing_occup_income_data)
    
    #concept: TENURE BY EDUCATIONAL ATTAINMENT OF HOUSEHOLDER 1562813hh
    housing_occup_educ_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25013") #of occup, own or rent by educ attainment
    housing_occup_educ_data <- housing_occup_educ_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_educ_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_education_level"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_education_level) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hheduc_id",.remove = TRUE)
    hh_educ_dt <- as.data.table(housing_occup_educ_data)
    #make same variable categories with sam_hh
    hh_educ_dt[str_detect(own_rent,"Owner"),("own_rent"):="Owner occupied"]
    hh_educ_dt[str_detect(own_rent,"Renter"),("own_rent"):="Renter occupied"]
    sam_hh[order(income_low),
           ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    hh_educ_dt[order(match(hh_education_level,c("Less than high school graduate","High school graduate (including equivalency)",
                                                "Some college or associate's degree","Bachelor's degree or higher"))),
                     ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    sam_hh[,c("hh_education_level") := 
             hh_educ_dt[.SD, list(hh_education_level), on = .(educ_occup_id)]]
    #test: table(sam_hh$tract,sam_hh$hh_education_level)==table(hh_educ_dt$tract,hh_educ_dt$hh_education_level)


                        hh_workers[number_workers_in_hh=="3 workers",c("number_workers_in_hh"):="3 or more workers"] #I think they made a mistake
                        #number in hh_size_dt has 7 factors; in workers, there's only 4 so collapse
                        #sam_workers[,("hh_size_4"):=if_else(as.numeric(substr(hh_size,1,1))>3,"4-or-more-person household",hh_size)]
                        #create ids, just following order of how many in hh, so they can match
                        #the number of tracts aren't the same, so line them up so that it runs out on 3 or more workers
                        sam_hh[order(hh_size_4,-householder_age),
                               ("num_workers_id"):=paste0(tract,hh_size_4,as.character(1000000+seq.int(1:.N))),by=.(tract,hh_size_4)]
                        hh_workers[order(hh_size_4,match(
                          number_workers_in_hh,c("No workers","1 worker","2 workers","3 or more workers"))),
                          ("num_workers_id"):=paste0(tract,hh_size_4,as.character(1000000+seq.int(1:.N))),by=.(tract,hh_size_4)]
                        sam_hh[,c("number_workers_in_hh") := hh_workers[.SD, list(number_workers_in_hh), on = .(num_workers_id)]]
                        #clean up strays
                        sam_hh[hh_size=="1-person household" & number_workers_in_hh=="2 workers",("number_workers_in_hh"):="1 worker"]
                        sam_hh[hh_size=="1-person household" & number_workers_in_hh=="3 or more workers",("number_workers_in_hh"):="1 worker"]
                        sam_hh[hh_size=="2-person household" & number_workers_in_hh=="3 or more workers",("number_workers_in_hh"):="2 workers"]
    
    #equals "Renter occupied" on own_rent in Sam
    #gross rent is contract plus estimate for utilities, etc.
    #concept: GROSS RENT
    gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25063")
    gross_rent_data <- gross_rent_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(gross_rent_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("rent_cash","gross_rent"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(gross_rent=if_else(rent_cash=="No cash rent", "Less than $100", gross_rent)) %>%
      filter(!is.na(gross_rent) & number_sams > 0) %>%
      mutate(gross_rent_low=case_when(
        gross_rent == "Less than $100" ~ as.integer(1),
        gross_rent == "$100 to $149" ~ as.integer(100),
        gross_rent == "$150 to $199" ~ as.integer(150),
        gross_rent == "$200 to $249" ~ as.integer(200),
        gross_rent == "$250 to $299" ~ as.integer(250),
        gross_rent == "$300 to $349" ~ as.integer(300),
        gross_rent == "$350 to $399" ~ as.integer(350),
        gross_rent == "$400 to $449" ~ as.integer(400),
        gross_rent == "$450 to $499" ~ as.integer(450),
        gross_rent == "$500 to $549" ~ as.integer(500),
        gross_rent == "$550 to $599" ~ as.integer(550),
        gross_rent == "$600 to $649" ~ as.integer(600),
        gross_rent == "$650 to $699" ~ as.integer(650),
        gross_rent == "$700 to $749" ~ as.integer(700),
        gross_rent == "$750 to $799" ~ as.integer(750),
        gross_rent == "$800 to $899" ~ as.integer(800),
        gross_rent == "$900 to $999" ~ as.integer(900),
        gross_rent == "$1 000 to $1 249" ~ as.integer(1000),
        gross_rent == "$1 250 to $1 499" ~ as.integer(1250),
        gross_rent == "$1 500 to $1 999" ~ as.integer(1500),
        gross_rent == "$2 000 to $2 499" ~ as.integer(2000),
        gross_rent == "$2 500 to $2 999" ~ as.integer(2500),
        gross_rent == "$3 000 to $3 499" ~ as.integer(3000),
        gross_rent == "$3 500 or more" ~ as.integer(3500)),
        gross_rent_high=case_when(
          gross_rent == "Less than $100" ~ as.integer(99),
          gross_rent == "$100 to $149" ~ as.integer(149),
          gross_rent == "$150 to $199" ~ as.integer(199),
          gross_rent == "$200 to $249" ~ as.integer(249),
          gross_rent == "$250 to $299" ~ as.integer(299),
          gross_rent == "$300 to $349" ~ as.integer(349),
          gross_rent == "$350 to $399" ~ as.integer(399),
          gross_rent == "$400 to $449" ~ as.integer(449),
          gross_rent == "$450 to $499" ~ as.integer(499),
          gross_rent == "$500 to $549" ~ as.integer(549),
          gross_rent == "$550 to $599" ~ as.integer(599),
          gross_rent == "$600 to $649" ~ as.integer(649),
          gross_rent == "$650 to $699" ~ as.integer(699),
          gross_rent == "$700 to $749" ~ as.integer(749),
          gross_rent == "$750 to $799" ~ as.integer(799),
          gross_rent == "$800 to $899" ~ as.integer(899),
          gross_rent == "$900 to $999" ~ as.integer(999),
          gross_rent == "$1 000 to $1 249" ~ as.integer(1249),
          gross_rent == "$1 250 to $1 499" ~ as.integer(1499),
          gross_rent == "$1 500 to $1 999" ~ as.integer(1999),
          gross_rent == "$2 000 to $2 499" ~ as.integer(2499),
          gross_rent == "$2 500 to $2 999" ~ as.integer(2999),
          gross_rent == "$3 000 to $3 499" ~ as.integer(3449),
          gross_rent == "$3 500 or more" ~ as.integer(6000)), #have to think about skew on this one
        gross_rent_high2=case_when(
          gross_rent == "Less than $100" ~ as.character(99),
          gross_rent == "$100 to $149" ~ as.character(199),
          gross_rent == "$150 to $199" ~ as.character(199),
          gross_rent == "$200 to $249" ~ as.character(299),
          gross_rent == "$250 to $299" ~ as.character(299),
          gross_rent == "$300 to $349" ~ as.character(399),
          gross_rent == "$350 to $399" ~ as.character(399),
          gross_rent == "$400 to $449" ~ as.character(499),
          gross_rent == "$450 to $499" ~ as.character(499),
          gross_rent == "$500 to $549" ~ as.character(599),
          gross_rent == "$550 to $599" ~ as.character(599),
          gross_rent == "$600 to $649" ~ as.character(699),
          gross_rent == "$650 to $699" ~ as.character(699),
          gross_rent == "$700 to $749" ~ as.character(799),
          gross_rent == "$750 to $799" ~ as.character(799),
          gross_rent == "$800 to $899" ~ as.character(899),
          gross_rent == "$900 to $999" ~ as.character(999),
          gross_rent == "$1 000 to $1 249" ~ as.character(1249),
          gross_rent == "$1 250 to $1 499" ~ as.character(1499),
          gross_rent == "$1 500 to $1 999" ~ as.character(1999),
          gross_rent == "$2 000 to $2 499" ~ as.character(6000),
          gross_rent == "$2 500 to $2 999" ~ as.character(6000),
          gross_rent == "$3 000 to $3 499" ~ as.character(6000),
          gross_rent == "$3 500 or more" ~ as.character(6000))
      ) %>%
      uncount(as.numeric(number_sams),.id = "gross_rent_id",.remove = TRUE)
    gross_rent_hh <- as.data.table(gross_rent_data)
    
    #matches gross_rent for with cash rent only... - add it before gross_rent
    income_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25122")
    income_gross_rent_data <- income_gross_rent_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(income_gross_rent_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("hh_income_title","hh_income_renters","gross_rent_title","gross_rent"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(gross_rent) & number_sams > 0) %>%
      mutate(
        gross_rent_high2=case_when(
          gross_rent == "Less than $100" ~ as.character(99),
          gross_rent == "$100 to $199" ~ as.character(199),
          gross_rent == "$200 to $299" ~ as.character(299),
          gross_rent == "$300 to $399" ~ as.character(399),
          gross_rent == "$400 to $499" ~ as.character(499),
          gross_rent == "$500 to $599" ~ as.character(599),
          gross_rent == "$600 to $699" ~ as.character(699),
          gross_rent == "$700 to $799" ~ as.character(799),
          gross_rent == "$800 to $899" ~ as.character(899),
          gross_rent == "$900 to $999" ~ as.character(999),
          gross_rent == "$1 000 to $1 249" ~ as.character(1249),
          gross_rent == "$1 250 to $1 499" ~ as.character(1499),
          gross_rent == "$1 500 to $1 999" ~ as.character(1999),
          gross_rent == "$2 000 or more" ~ as.character(6000)) #have to think about skew on this one
      ) %>%
      uncount(as.numeric(number_sams),.id = "income_gross_rent_id",.remove = TRUE)
    income_gross_rent_hh <- as.data.table(income_gross_rent_data)
                      gross_rent_hh[rent_cash=="With cash rent",("gross_inc_id"):=
                                      paste0(tract,gross_rent_high2,as.character(1000000+seq.int(1:.N))),by=.(tract,gross_rent_high2)]
                      income_gross_rent_hh[,("gross_inc_id"):=
                                             paste0(tract,gross_rent_high2,as.character(1000000+seq.int(1:.N))),by=.(tract,gross_rent_high2)]
                      setkey(income_gross_rent_hh,gross_inc_id)
                      setkey(gross_rent_hh,gross_inc_id)
                      gross_rent_income <- income_gross_rent_hh[gross_rent_hh,]
                      
                      #just correlating with income - could get more complicated in future iterations
                      sam_hh[order(-own_rent,income_low),
                             ("gross_rent_hh_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract)]
                      gross_rent_income[order(gross_rent_high2),("gross_rent_hh_id"):=paste0(tract,"Renter occupied",as.character(1000000+seq.int(1:.N))),by=.(tract)]
                      setkey(sam_hh,gross_rent_hh_id)
                      setkey(gross_rent_income,gross_rent_hh_id)
                      sam_rent <- gross_rent_income[sam_hh,]
                      #test: table(sam_rent[own_rent=="Renter occupied"]$tract,sam_rent[own_rent=="Renter occupied"]$gross_rent)==table(gross_rent_hh$tract,gross_rent_hh$gross_rent)
                      #perhaps looking at systematically, there's good but appropriately not perfect correlation: table(sam_rent$i.gross_rent,sam_rent$gross_rent)
                      #keep only gross_rent with 25 categories
                      sam_rent[,c("rent_gross","rent_gross_high","rent_gross_low"):=c(list(i.gross_rent),list(i.gross_rent_high2),list(gross_rent_low))]
                      sam_rent[is.na(tract),("tract"):=i.tract.1]
                      sam_hh <- sam_rent[,c("tract","sex","race","ethnicity","family","family_type","hh_role","family_role","hh_size","hh_size_4",
                                            "householder_age","householder_age_9","own_rent","housing_units","person_per_room","SNAP",
                                            "rent_cash","rent_gross","rent_gross_high","rent_gross_low","hh_income_renters", #eventually assign numbers to rent and income
                                            "hh_income_level","income_high","income_low","hh_education_level","number_vehicles_in_hh",
                                            "number_workers_in_hh")]
    
    #SNAP - should follow rules for sub-setting that have to do with requirements by feds...
    #do food_stamps as last with ethnicity and race separate for households (others, below, on full sam)
    #have background sorting include broad indicators of income, but not too much - all you get on this is race/eth and food_stamps
    sam_race_hh[order(people_per_room,match(own_rent,c("Owner occupied","Renter occupied"))),
                ("join_snap_id"):=paste0(tract,race,as.character(1000000+seq.int(1:.N))),
                by=.(tract,race)]
    food_stamps_race_dt[,
                        ("join_snap_id"):=paste0(tract,race,as.character(1000000+seq.int(1:.N))),
                        by=.(tract,race)]
    sam_race_hh[,c("SNAP"):=
                  food_stamps_race_dt[.SD, list(food_stamps), on = .(join_snap_id)]]
    sam_race_hh[,("join_snap_id"):=NULL]
    sam_eth_hh[order(people_per_room,match(own_rent,c("Owner occupied","Renter occupied"))),
               ("join_snap_id"):=paste0(tract,ethnicity,as.character(1000000+seq.int(1:.N))),
               by=.(tract,ethnicity)]
    food_stamps_eth_dt[,
                       ("join_snap_id"):=paste0(tract,ethnicity,as.character(1000000+seq.int(1:.N))),
                       by=.(tract,ethnicity)]
    sam_eth_hh[,c("SNAP"):=
                 food_stamps_eth_dt[.SD, list(food_stamps), on = .(join_snap_id)]]
    sam_eth_hh[,("join_snap_id"):=NULL]
    
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
    hh_partner_dt[is.na(partner_type),("partner_type") := "Not a partner household"] 
    #make partner_ids, with Other family from family_type
    #frustrating because some partner households are listed as married couples - I think it's just the numbers game. I chose to put them as unmarried partners
    hh_partner_dt[order(match(unmarried,c("Unmarried-partner households","All other households"))),
                  ("partner_type_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),by=.(tract)]
    sam_hh[order(match(family_type,c("Other family","Householder not living alone","Householder living alone","Married-couple family"))),
           ("partner_type_id"):=paste0(tract,as.character(1000000+sample(.N))),by=.(tract)]
    setkey(sam_hh,partner_type_id)
    setkey(hh_partner_dt,partner_type_id)
    sam_partners <- hh_partner_dt[sam_hh,]
    #matches are very slightly off - same total, distributed on edges differently
    sam_partners[is.na(partner_type),("partner_type"):="Not a partner household"]
    sam_partners[partner_type == "Female householder and female partner",("sex_partner") := "Female"]
    sam_partners[partner_type == "Male householder and male partner",("sex_partner") := "Male"]
    sam_partners[partner_type == "Male householder and female partner",("sex_partner") := "Female"]
    sam_partners[partner_type == "Female householder and male partner",("sex_partner") := "Male"]
    sam_partners[partner_type == "Female householder and female partner",("sex") := "Female"]
    sam_partners[partner_type == "Male householder and male partner",("sex") := "Male"]
    sam_partners[partner_type == "Male householder and female partner",("sex") := "Male"]
    sam_partners[partner_type == "Female householder and male partner",("sex") := "Female"]

    sam_hh <- sam_partners[,c("tract","sex","race","ethnicity","family","family_type","hh_role","family_role","hh_size","hh_size_4",
                              "householder_age","householder_age_9","own_rent","housing_units","person_per_room","SNAP",
                              "rent_cash","rent_gross","rent_gross_high","rent_gross_low","hh_income_renters", #eventually assign numbers to rent and income
                              "hh_income_level","income_high","income_low","hh_education_level","number_vehicles_in_hh",
                              "number_workers_in_hh","partner_type","sex_partner")]
    
    
    #concept: HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP gives exact number (4525519) - it's a mess in the mutate; should be able to fix
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
    hh_relations_dt <- as.data.table(household_type_relation_data)
    
    #this gets you adults and seniors, too - slightly different totals from one with just seniors, for some reason, by 3 part age_range
    household_adults_relation_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09021")
    household_adults_relation_data <- household_adults_relation_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(household_adults_relation_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("relation_age_range","relation_hh"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(relation_hh)) %>%
      uncount(as.numeric(number_sams),.id = "family_id",.remove = TRUE) 
    
    #1223249 matches age_sex_race under 18
    #concept#HOUSEHOLD TYPE FOR CHILDREN UNDER 18 YEARS IN HOUSEHOLDS (EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS)
    household_type_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09005")
    household_type_kids_data <- household_type_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "In family households") %>%
      pivot_longer(4:ncol(household_type_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family_or_non","family_type"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "type_kids_id")
    hh_type_kids <- as.data.table(household_type_kids_data)
    
    #nrow(sex_age_race[age_range=="0  to  5 years" | age_range=="5  to  9 years" | age_range=="10 to 14 years" | age_range=="15 to 17 years"]) - 1225059
    #concept is:"OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE" - each kid, not each hh - 1065925
    kids_family_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09002")
    kids_family_age_data <- kids_family_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_family_age_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("family","family_type","kid_age"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(kid_age = if_else(family=="In married-couple families",family_type,kid_age),
             kid_age = if_else(kid_age=="Under 3 years","0 to 3 years",kid_age)) %>%
      filter(!is.na(kid_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "kids_age_id",.remove = TRUE)
    
    #GRANDCHILDREN UNDER 18 YEARS LIVING WITH A GRANDPARENT HOUSEHOLDER BY AGE OF GRANDCHILD - 103908 
    #nrow(hh_relations_dt[family_role=="Grandchild"]) - 126406 - this matches on role of grandparent with kids, some will have > 1 (22,500)
    kids_age_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10001")
    kids_grandparents_age_data <- kids_age_grandparents_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>% 
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(kids_age_grandparents_from_census),names_to = "tract", values_to = "number_sams") %>% 
      #some may live 2 or more to a house
      rename(grandkid_age = label) %>%
      uncount(as.numeric(number_sams),.id = "kids_grandparents_id",.remove = TRUE)
    kids_grand_age <- as.data.table(kids_grandparents_age_data)
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
      separate(label, c("group_or_hh","family_or_non","relative","sex_sr_relations","living_alone"), sep = "!!", remove = F, convert = FALSE) %>%
      uncount(as.numeric(number_sams),.id = "sr_role_id",.remove = TRUE)
    sr_relations_dt <- as.data.table(household_seniors_relation_data)
    kids_ages_dt <- as.data.table(kids_family_age_data)
    adults_relations <- as.data.table(household_adults_relation_data)
    hh_ages_all_dt <- rbindlist(list(kids_ages_dt,adults_relations[relation_age_range!="65 years and over"],sr_relations),fill = TRUE)
    
    hh_ages_dt <- rbindlist(list(kids_family_age_data,household_adults_relation_data),fill = TRUE)
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
    #lost information because tracts were not right in original provided by census (780??)
    hh_kids[order(family_role,match(kid_age,c("No children","Under 6 years only","6 to 17 years only","Under 6 years and 6 to 17 years"))),
      ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_hh[family=="Family households" & order(family_role,hh_size),
           ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_hh[family=="Family households",c("kids_by_age") := hh_kids[.SD, list(kid_age), on = .(hh_kids_id)]] #may have more than 1 in other categories
    
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
    
    #household_id
    sam_hh[,c("household_id") := list(paste0(state,county,tract,as.character(2000000+sample(.N)))),by=.(tract)] #sample defaults to replace=FALSE
    sam_hh[,c("hh_1_role") := "self"]
    sam_hh[,c("householder") := TRUE]
    sam_hh[,("age_range"):=householder_age_9]
    relations_dt[,c("relations_id"):=list(paste0(family_role,tract,as.character(3000000+sample(.N)))),by=.(tract)]
    
#build household matches from edge cases back to middle
    #add group quarters - look up GQ_sam for jail stuff and nursing homes from sr_relations
    gq1 <- sr_relations[(group_or_hh=="In group quarters"),c("tract","group_or_hh")]
    gq2 <- relations_dt[(i.group_quarters)]
    gq3 <- rbindlist(list(gq2,gq1),fill = TRUE)
    gq3[is.na(tract),("tract"):=if_else(is.na(i.tract),i.tract.1,i.tract)]
    gq3[,("tract_tot"):=nrow(.SD[!is.na(i.tract.1)]),by=.(tract)] 
    gq3[order(match(age_range,c("65 years and over","18 to 64 years"))),("tokeep"):=seq.int(1:.N)<=tract_tot,by=.(tract)]
    gq4 <- gq3[(tokeep),c("tract","age_range","relations_id")]
    gq4[,("group_quarters"):=TRUE]
    relations_dt_no_GQ <- relations_dt_no_hh[!relations_id %in% unique(gq2[,relations_id])]
    sam_hh_gq <- rbindlist(list(sam_hh,gq4),fill = TRUE)
    #can add the rest of logic on jail - for some reason, went down to 40911 (from 41220) - I think there are weird cases on tracts...
    
    #add hh_size == 8, children
    sam_hh_gq[as.numeric(substr(hh_size,1,1))==7,
           ("hh_size"):=sample(c("7-person household","8-person household"),.N,prob=c(.7,.3), replace = TRUE)]
    sam_hh_gq[as.numeric(substr(hh_size,1,1))>7 & kids_by_age!="No children" & family_role=="Householder",
              c("child_8_match_id"):=list(paste0(tract,"child8",as.character(2000000+sample(.N)))),by=.(tract)]
    relations_dt_no_GQ[,c("child_8_match_id"):=list(paste0(tract,"child8",as.character(2000000+sample(.N)))),by=.(tract)]
    sam_hh_gq[as.numeric(substr(hh_size,1,1))>7 & kids_by_age!="No children",
              c("hh_8_id","hh_8_role"):=relations_dt_no_GQ[.SD, c(list(relations_id),list(family_role)),on=c("child_8_match_id")]]
    
    #adults - 
                    #use number of workers and number of vehicles to match as expand
                    
    
    #take them out of relations_dt and expand sam
    relations_dt_no_GQ <- relations_dt_no_hh[!relations_id %in% unique(gq2[,relations_id])]
    
    
    #clean up sam - if .id of expand == 2, then householder == FALSE - and add other logic
    
    #do last
    sam_hh[as.numeric(substr(hh_size,1,1))==1 & partner_type != "Not a partner household",
           ("hh_size"):=sample(c("2-person household","3-person household"),.N,replace = TRUE)]
    sam_hh[partner_type!="Not a partner household",("family_type"):="Other family"]
    sam_hh[as.numeric(substr(hh_size,1,1))==1 & family_type=="Married-couple family",
           ("hh_size"):=sample(c("2-person household","3-person household"),.N,replace = TRUE)]
    sam_hh[partner_type!="Not a partner household", 
           ("hh_2_role"):="Unmarried partner"]
    sam_hh[hh_2_role=="Unmarried partner" & as.numeric(substr(hh_size,1,1))==1,("hh_size"):="2-person household"]
    sam_hh[hh_2_role=="Unmarried partner" & as.numeric(substr(hh_size_4,1,1))==1,("hh_size_4"):="2-person household"]
    sam_hh[family_type=="Married-couple family",
           ("hh_2_role"):="Spouse"] #will be only Female
     #so that the spouses and the hh will be close in age, once we randomize
    
    
    
#    saveRDS(sam_hh,file = paste0(housingdir, vintage, "/sam_hh_l.973",Sys.Date(),".RDS")) #"2020-04-15"
#    saveRDS(relations_dt,file = paste0(housingdir, vintage, "/relations_dt_",Sys.Date(),".RDS")) #"2020-04-13" 
    
    sam_hh[order(-age_range),c("hh_match_id"):=list(paste0(tract,"hh",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt[family_role=="Householder" & order(-age_range), 
                           c("hh_match_id"):=list(paste0(tract,"hh",as.character(2000000+seq.int(1:.N)))),by=.(tract)]

    sam_hh[,c("hh_1_id","relations_age_range"):=relations_dt[.SD, c(list(relations_id),list(age_range)),on=c("hh_match_id")]]
    sam_hh[is.na(hh_1_id),("hh1_match_id"):=list(paste0(tract,"hh1",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt[relative=="Nonrelatives" & age_range!="0 to 17 years",("hh1_match_id"):=list(paste0(tract,"hh1",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    sam_hh[is.na(hh_1_id),c("hh_1_id","relations_age_range"):=relations_dt[.SD, c(list(relations_id),list(age_range)),on=c("hh1_match_id")]]
    sam_hh[is.na(hh_1_id),("hh_rest_match_id"):=list(paste0("hh2",as.character(2000000+seq.int(1:.N))))]
    #trying to pick up last 2k, and help balance for Spouse, below
    relations_dt[family_role=="Housemate or roommate" | family_role== "Roomer or boarder",("hh_rest_match_id"):=list(paste0("hh2",as.character(2000000+seq.int(1:.N))))]
    sam_hh[is.na(hh_1_id),c("hh_1_id","relations_age_range"):=relations_dt[.SD, c(list(relations_id),list(age_range)),on=c("hh_rest_match_id")]]
    relations_dt_no_hh <- relations_dt[!relations_id %in% unique(sam_hh[,hh_1_id])]
    #still missing over 1k HH - should come out in the wash later...
    
    #add group quarters folks 
    

#START WITH     sam_hh_l.973, above
#WHY DOESN'T IT LET ME PUT AN ID ON spouse_partner_id==1, if it lets me do it on 2?    #DO THE MATCH, THEN THE EXPAND!!! #make sure only expanding original householder
#and should start with hh_7_id and hh_7_role
    
    
        #using rbindlist instead of bind_rows and as.data.table a second time, prevent the invalid .internal.selfref problem
    
    #give all the 7 and above to the 8s - using same size as 7 above 

    
    
    
    
    relations_dt_no_5kids <- relations_dt_no_4adults[!relations_id %in% unique(exp_sam[,hh_5_id])]
    
    exp_sam[as.numeric(substr(hh_size,1,1))>4 & number_workers_in_hh=="3 or more workers",
            ("hh_5_role"):="adult"]  
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_5_role) & hh_5_role=="adult",2,1),.id="adult_5_id",.remove = TRUE)])
    exp_sam[hh_5_role=="adult",("employed"):= sample(c(TRUE,FALSE))]
    exp_sam[adult_5_id==2 & hh_5_role=="adult",
            c("working_5_id"):=list(paste0(tract,"adult5",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_5kids[age_range=="18 to 34 years" | age_range=="35 to 64 years" | age_range=="18 to 64 years", 
                          c("working_5_id"):=list(paste0(tract,"adult5",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[adult_5_id==2 & hh_5_role=="adult",
            c("hh_5_id","hh_5_role"):=relations_dt_no_5kids[.SD, c(list(relations_id),list(family_role)),on=c("working_5_id")]]
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_5_role) & hh_5_role=="child",2,1),.id="child_5_id",.remove = TRUE)])
    relations_dt_no_5adults <- relations_dt_no_5kids[!relations_id %in% unique(exp_sam[,hh_5_id])]
    
    
    
    
    
    exp_sam[spouse_partner_id==2 & hh_2_role=="Spouse",
            c("spouse_id"):=list(paste0(tract,"spouse",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_GQ[family_role=="Spouse",
                       c("spouse_id"):=list(paste0(tract,"spouse",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam <- as.data.table(sam_hh_gq[,uncount(.SD,if_else(!is.na(hh_2_role),2,1),.id="spouse_partner_id",.remove = TRUE)])
    exp_sam[spouse_partner_id==2 & !is.na(sex_partner),("sex"):=sex_partner]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Spouse",c("sex"):="Female"]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Spouse",c("family_role"):="Spouse"]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Spouse",c("relation_hh_1"):="Spouse"]
    
    exp_sam[spouse_partner_id==2 & hh_2_role=="Spouse",("hh_2_id"):=relations_dt_no_GQ[.SD, list(relations_id),on=c("spouse_id")]]

    #exp_sam[hh_2_role=="Spouse",("hh_2_id"):=.SD[.N,hh_2_id],by=.(household_id)]
    
    exp_sam[spouse_partner_id==2 & hh_2_role=="Spouse",c("hh_1_role"):="Spouse"] 
    exp_sam[spouse_partner_id==2 & hh_2_role=="Spouse",c("hh_2_role"):="self"] 
    relations_dt_no_spouse <- relations_dt_no_GQ[!relations_id %in% unique(exp_sam[,hh_2_id])]
    
    exp_sam[spouse_partner_id==2 & hh_2_role=="Unmarried partner",c("family_role"):="Unmarried partner"]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Unmarried partner",c("hh_1_role"):="Unmarried partner"]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Unmarried partner",
            c("unmarried_partner_id"):=list(paste0(tract,"unm_p",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_spouse[family_role=="Unmarried partner", 
                       c("unmarried_partner_id"):=list(paste0(tract,"unm_p",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Unmarried partner",
            ("hh_2_id"):=relations_dt_no_spouse[.SD, list(relations_id),on=c("unmarried_partner_id")]]
    #exp_sam[hh_2_role=="Unmarried partner",("hh_2_id"):=.SD[.N,hh_2_id],by=.(household_id)]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Unmarried partner",c("hh_1_role"):="Unmarried partner"]
    exp_sam[spouse_partner_id==2 & hh_2_role=="Unmarried partner",c("hh_2_role"):="self"]
    relations_dt_no_partners <- relations_dt_no_spouse[!relations_id %in% unique(exp_sam[,hh_2_id])]
    
    #a lot of disagreement over unmarried partner, which was much higher in relations than in hh_partners - went for the higher number, but ended up taking some away from Spouse
    
    
#    saveRDS(exp_sam,file = paste0(housingdir, vintage, "/exp_sam_l.1033_",Sys.Date(),".RDS")) #"2020-04-15"
#    saveRDS(relations_dt_no_hh,file = paste0(housingdir, vintage, "/relations_dt_no_hh_",Sys.Date(),".RDS")) #"2020-04-15"
#    saveRDS(relations_dt_no_partners,file = paste0(housingdir, vintage, "/relations_dt_no_partners_",Sys.Date(),".RDS"))

    #do for hh_2_id, with a few tweaks from others
    #logic for adults, no kids and workers
    #if hh_size has room and no kids add workers - for other adults...
    exp_sam[hh_size=="1-person household",("employed"):= if_else(number_workers_in_hh=="1 worker",TRUE,FALSE)] 
    #NEED TO FIX THIS
    exp_sam[hh_size=="2-person household" & number_workers_in_hh=="1 worker",("employed"):= sample(c(TRUE,FALSE),size=.N,replace = TRUE)]
    exp_sam[hh_size=="2-person household" & number_workers_in_hh=="2 workers",("employed"):= TRUE]
    exp_sam[hh_size=="2-person household" & number_workers_in_hh=="No workers",("employed"):= FALSE]
    exp_sam[as.numeric(substr(hh_size,1,1))>=2 & is.na(hh_2_role) & kids_by_age=="No children",
            ("hh_2_role"):="adult"] #hh_2_role will be replaced by matching 
    #the uncount screws with something about the dt
    exp_sam2 <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_2_role) & hh_2_role=="adult",2,1),.id="adult_2_id",.remove = TRUE)])
    
    exp_sam2[adult_2_id==2 & hh_2_role=="adult",
            c("working_adult_id"):=list(paste0(tract,"adult1",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_partners[age_range=="18 to 34 years" | age_range=="35 to 64 years" | age_range=="18 to 64 years", 
                           c("working_adult_id"):=list(paste0(tract,"adult1",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam2[adult_2_id==2 & hh_2_role=="adult",
            c("hh_2_id","hh_2_role"):=relations_dt_no_partners[.SD, c(list(relations_id),list(family_role)),on=c("working_adult_id")]]
    relations_dt_no_2workers <- relations_dt_no_partners[!relations_id %in% unique(exp_sam2[,hh_2_id])]
    
    #kids_by_age works off of family=="Family households" 
    exp_sam2[kids_by_age!="No children" & as.numeric(substr(hh_size,1,1))>=2 & is.na(hh_2_role),("hh_2_role"):="child"]
    exp_sam3 <- as.data.table(exp_sam2[,uncount(.SD,if_else(!is.na(hh_2_role) & hh_2_role=="child",2,1),.id="child_2_id",.remove = TRUE)])
    exp_sam3[child_2_id==2 & hh_2_role=="child",
            c("child_2_match_id"):=list(paste0(tract,"child2",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_2workers[, 
                             c("child_2_match_id"):=list(paste0(tract,"child2",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam3[child_2_id==2 & hh_2_role=="child",
            c("hh_2_id","hh_2_role"):=relations_dt_no_2workers[.SD, c(list(relations_id),list(family_role)),on=c("child_2_match_id")]]
    relations_dt_no_2kids <- relations_dt_no_2workers[!relations_id %in% unique(exp_sam3[,hh_2_id])]
    exp_sam3[!is.na(hh_2_role),c("hh_2_id"):=shift(.SD[,c(hh_2_id)],n=1L,type = "lead")]
#    exp_sam3[!is.na(hh_2_role),c("hh_2_id","hh_2_role"):=.SD[.N,c(hh_2_id,hh_2_role)],by=.(household_id)]
    
    exp_sam <- exp_sam3 #reducing clutter
#do same for hh_3_id
    #but kids first
    exp_sam[kids_by_age!="No children" & as.numeric(substr(hh_size,1,1))>2,("hh_3_role"):="child"]
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_3_role) & hh_3_role=="child",2,1),.id="child_3_id",.remove = TRUE)])
    exp_sam[child_3_id==2 & hh_3_role=="child",
            c("child_3_match_id"):=list(paste0(tract,"child3",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_2kids[,c("child_3_match_id"):=list(paste0(tract,"child3",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[child_3_id==2 & hh_3_role=="child",
            c("hh_3_id","hh_3_role"):=relations_dt_no_2kids[.SD, c(list(relations_id),list(family_role)),on=c("child_3_match_id")]]
    relations_dt_no_3kids <- relations_dt_no_2kids[!relations_id %in% unique(exp_sam[,hh_3_id])]
    
    exp_sam[as.numeric(substr(hh_size,1,1))>2 & number_workers_in_hh=="3 or more workers",
            ("hh_3_role"):="adult"] #hh_2_role will be replaced by matching 
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_3_role) & hh_3_role=="adult",2,1),.id="adult_3_id",.remove = TRUE)])
    exp_sam[hh_3_role=="adult",("employed"):= TRUE]
    exp_sam[adult_3_id==2 & hh_3_role=="adult",
            c("working_3_id"):=list(paste0(tract,"adult3",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_3kids[age_range=="18 to 34 years" | age_range=="35 to 64 years" | age_range=="18 to 64 years", 
                             c("working_3_id"):=list(paste0(tract,"adult3",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[adult_3_id==2 & hh_3_role=="adult",
            c("hh_3_id","hh_3_role"):=relations_dt_no_3kids[.SD, c(list(relations_id),list(family_role)),on=c("working_3_id")]]
    relations_dt_no_3adults <- relations_dt_no_3kids[!relations_id %in% unique(exp_sam[,hh_3_id])]
#    exp_sam[!is.na(hh_3_role),c("hh_3_id","hh_3_role"):=.SD[.N,c(hh_3_id,hh_3_role)],by=.(household_id)]
    

    saveRDS(exp_sam,file = paste0(housingdir, vintage, "/exp_sam_l.1102_",Sys.Date(),".RDS")) #"2020-04-15"
    saveRDS(relations_dt_no_3adults,file = paste0(housingdir, vintage, "/relations_dt_no_3adults_",Sys.Date(),".RDS")) #"2020-04-15"
    
    #check how many 3 or more workers are left, and then just move to adults only - in general, should add automatically through hh_size==5
    #could add vehicles by worker here and see how many workers are left??? - or just move to adults / children and do workers at end??
    #point is how to get kids and adults spread around
    
    #do same for hh_4_id
    #kids first
    exp_sam[kids_by_age!="No children" & as.numeric(substr(hh_size,1,1))>3,("hh_4_role"):="child"]
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_4_role) & hh_4_role=="child",2,1),.id="child_4_id",.remove = TRUE)])
    exp_sam[child_4_id==2 & hh_4_role=="child",
            c("child_4_match_id"):=list(paste0(tract,"child4",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_3adults[,c("child_4_match_id"):=list(paste0(tract,"child4",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[child_4_id==2 & hh_4_role=="child",
            c("hh_4_id","hh_4_role"):=relations_dt_no_3adults[.SD, c(list(relations_id),list(family_role)),on=c("child_4_match_id")]]
    relations_dt_no_4kids <- relations_dt_no_3adults[!relations_id %in% unique(exp_sam[,hh_4_id])]
    
    exp_sam[as.numeric(substr(hh_size,1,1))>3 & number_workers_in_hh=="3 or more workers",
            ("hh_4_role"):="adult"] #hh_2_role will be replaced by matching 
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_4_role) & hh_4_role=="adult",2,1),.id="adult_4_id",.remove = TRUE)])
    exp_sam[hh_4_role=="adult",("employed"):= TRUE] #better to get real number of total workers, but sample(c(TRUE,FALSE)) after this
    exp_sam[adult_4_id==2 & hh_4_role=="adult",
            c("working_4_id"):=list(paste0(tract,"adult4",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_4kids[age_range=="18 to 34 years" | age_range=="35 to 64 years" | age_range=="18 to 64 years", 
                          c("working_4_id"):=list(paste0(tract,"adult4",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[adult_4_id==2 & hh_4_role=="adult",
            c("hh_4_id","hh_4_role"):=relations_dt_no_4kids[.SD, c(list(relations_id),list(family_role)),on=c("working_4_id")]]
    relations_dt_no_4adults <- relations_dt_no_4kids[!relations_id %in% unique(exp_sam[,hh_4_id])]
#    exp_sam[!is.na(hh_4_role),c("hh_4_id","hh_4_role"):=.SD[.N,c(hh_4_id,hh_4_role)],by=.(household_id)]
    
    #do same for hh_5_id
    #kids first
    exp_sam[kids_by_age!="No children" & as.numeric(substr(hh_size,1,1))>4,("hh_5_role"):="child"]
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_5_role) & hh_5_role=="child",2,1),.id="child_5_id",.remove = TRUE)])
    exp_sam[child_5_id==2 & hh_5_role=="child",
            c("child_5_match_id"):=list(paste0(tract,"child5",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_4adults[,c("child_5_match_id"):=list(paste0(tract,"child5",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[child_5_id==2 & hh_5_role=="child",
            c("hh_5_id","hh_5_role"):=relations_dt_no_4adults[.SD, c(list(relations_id),list(family_role)),on=c("child_5_match_id")]]
    relations_dt_no_5kids <- relations_dt_no_4adults[!relations_id %in% unique(exp_sam[,hh_5_id])]
    
    exp_sam[as.numeric(substr(hh_size,1,1))>4 & number_workers_in_hh=="3 or more workers",
            ("hh_5_role"):="adult"]  
    exp_sam <- as.data.table(exp_sam[,uncount(.SD,if_else(!is.na(hh_5_role) & hh_5_role=="adult",2,1),.id="adult_5_id",.remove = TRUE)])
    exp_sam[hh_5_role=="adult",("employed"):= sample(c(TRUE,FALSE))]
    exp_sam[adult_5_id==2 & hh_5_role=="adult",
            c("working_5_id"):=list(paste0(tract,"adult5",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    relations_dt_no_5kids[age_range=="18 to 34 years" | age_range=="35 to 64 years" | age_range=="18 to 64 years", 
                          c("working_5_id"):=list(paste0(tract,"adult5",as.character(2000000+seq.int(1:.N)))),by=.(tract)]
    exp_sam[adult_5_id==2 & hh_5_role=="adult",
            c("hh_5_id","hh_5_role"):=relations_dt_no_5kids[.SD, c(list(relations_id),list(family_role)),on=c("working_5_id")]]
    relations_dt_no_5adults <- relations_dt_no_5kids[!relations_id %in% unique(exp_sam[,hh_5_id])]
#    exp_sam[!is.na(hh_5_role),c("hh_5_id","hh_5_role"):=.SD[.N,c(hh_5_id,hh_5_role)],by=.(household_id)]
    
    #then fill out final by referring back to age_race for whole
    
    
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
    
    
    
    
    #unique(place_born) - "Born in state of residence" "Born in other state in the United States" "Native; born outside the United States"   "Foreign born" 
    place_born_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B06004")
    #should do it by households, too??? would match on hh_size, then on race, etc., 
    #    place_born_race_data <- place_born_race_from_census %>% #right total - 4525519
    
    
        
    #add number of workers per household - same logic, but only has four factors for size, not seven
    
    
    


    #means of transportation to work - perhaps use for 
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
#      filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "transport_race_id",.remove = TRUE)
    transport_race_dt <- as.data.table(transport_race_data)
    transport_eth_dt <- as.data.table(transport_eth_data)
    transport_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity %in% acs_race_codes]),by=.(tract,transport_to_work)]
    transport_eth_dt[order(match(ethnicity,c("H","I",ethnicity %in% acs_race_codes))),c("cnt_ethn"):=list(1:.N),by=.(tract,transport_to_work)]
    transport_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,transport_to_work)]
    transport_eth_dt <- transport_eth_dt[(tokeep)]
    
    #concept: MEANS OF TRANSPORTATION TO WORK BY TENURE - for workers - 2135069
    transport_tenure_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B08137")
    transport_tenure_data <- transport_tenure_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(transport_tenure_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("hh_means_transport","owner_renter"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(owner_renter) & number_sams > 0) %>%
      mutate(own_rent = if_else(str_detect(owner_renter,"owner"),"Owner occupied","Renter occupied")) %>%
      uncount(as.numeric(number_sams),.id = "workers_vehicles_id",.remove = TRUE)
    

    

    
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
    
    
    
                        #concept: HOUSEHOLD TYPE BY UNITS IN STRUCTURE
                        #could use this to match with the housing_units_race?? - seems like you lose a lot of information with this one...
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
                            fam_role_units = if_else(fam_role_units=="Other family",structs,if_else(family=="Nonfamily households",family,fam_role_units))
                          ) %>% 
                          filter(!is.na(num_structures)) %>%
                          filter(number_sams > 0) %>%
                          uncount(as.numeric(number_sams),.id = "hh_units_id")
    
        
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
    #4484299 - not sure why not complete? group quarters?
    housing_occup_date_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25026") #of occup, own or rent by move in date
    housing_occup_date_data <- housing_occup_date_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total population in occupied housing units!!")) %>%
      filter(label != "Estimate!!Total population in occupied housing units") %>%
      pivot_longer(4:ncol(housing_occup_date_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","move_in_date"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(move_in_date) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_date_id",.remove = TRUE)
    
    
    #concept:PRESENCE OF OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE BY EMPLOYMENT STATUS
    #wife of a husband not in labor force is not listed as existing
    #attaches to family households, under sam_hh$family - 1062265
    family_employment_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B23007")
    family_employment_data <- family_employment_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces!!Wife in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed!!Wife in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces!!Wife in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed!!Wife in labor force") %>%
      filter(label != "With own children under 18 years!!Other family!!Male householder no wife present!!In labor force") %>%
      filter(label != "With own children under 18 years!!Other family!!Male householder no wife present") %>%
      filter(label != "With own children under 18 years!!Other family!!Female householder no husband present!!In labor force") %>%
      filter(label != "With own children under 18 years!!Other family!!Female householder no husband present") %>%
      filter(label != "No children under 18 years!!Other family!!Male householder no wife present!!In labor force") %>%
      filter(label != "No children under 18 years!!Other family!!Male householder no wife present") %>%
      filter(label != "No children under 18 years!!Other family!!Female householder no husband present!!In labor force") %>%
      filter(label != "No children under 18 years!!Other family!!Female householder no husband present") %>%
      filter(label != "With own children under 18 years!!Other family") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband not in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force") %>%
      filter(label != "With own children under 18 years!!Married-couple family") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces") %>%
      filter(label != "With own children under 18 years") %>%
      filter(label != "No children under 18 years!!Other family") %>%
      filter(label != "No children under 18 years") %>%
      filter(label != "No children under 18 years!!Married-couple family") %>%
      filter(label != "With own children under 18 years!!Married-couple family!!Husband not in labor force!!Wife in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband not in labor force!!Wife in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband not in labor force") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Unemployed") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force!!Employed or in Armed Forces") %>%
      filter(label != "No children under 18 years!!Married-couple family!!Husband in labor force") %>%
      pivot_longer(4:ncol(family_employment_data_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("own_kids","family_type","hh_worker_role","employed","husband_employ","wife_employ"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(hh_employ=if_else(hh_worker_role=="Husband in labor force",employed,husband_employ)) %>%
#     filter(family_type=="Other family" | !is.na(husband_employ) & number_sams > 0) %>%
#      filter(!is.na(husband_employ) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "family_employ_id",.remove = TRUE)
    
    
    
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
    
    ##NEED TO REDO
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
    
    sam_marital_GQ <- rbindlist(list(sam_marital,GQ_sam),fill = TRUE) #bind them so that the PCA includes GQ by tract 
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
    #Household - multigenerational B11017 all NA
    #above just says how many people below or above poverty, but number of people and type of HH is given - something... match on family_type of Head of Household, but have to already have households
    
    #  type - relatives -just tells you if they live with additional non-relatives, and gives race
    household_relatives_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B11002")
    
        
    
    #AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS FOR FAMILIES BY FAMILY TYPE -only below poverty... too complicated to unwind and explain
    agg_deficit_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17011")
    
    #by number of children and poverty B17023
    number_children_poverty_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17023")
    
    #by tenure B17019 (whether renter or not)
    household_tenure_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17019")
    
  saveRDS(sam_hh,file = paste0(censusdir, vintage,"/sam_hh.RDS"))
  #and a dated copy?
  saveRDS(sam_hh,file = paste0(censusdir, vintage, "/sam_hh_",Sys.Date(),".RDS"))
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




# poverty - type - number of persons in HH and family_type, but only below or above poverty in last 12 months
#    household_poverty_people_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17013")

#type by age of HH only above and below
#    household_poverty_age_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17017")
#type by education of HH B17018 - only above / below poverty
#    household_poverty_educ_level_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17018")

#kids_to_householder_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B09018")

kids_age_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10001")
kids_respons_grandparents_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10002")
kids_respons_grands_time_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10050")
kids_respons_grands_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10051")
kids_respons_grands_sex_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10057")
kids_respons_grands_disability_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B10052")
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


#not run                            #should we compare this with hh_size times per_person_per_room??   
#haven't done yet
housing_occup_rooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25020") #of occup, number of rooms
housing_occup_rooms_data <- housing_occup_rooms_from_census %>%
  mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
  filter(label != "Estimate!!Total") %>%
  pivot_longer(4:ncol(housing_occup_rooms_from_census),names_to = "tract", values_to = "number_sams") %>%
  separate(label, c("own_rent","num_rooms"), sep = "!!", remove = F, convert = FALSE) %>%
  filter(!is.na(num_rooms) & number_sams > 0) %>%
  uncount(as.numeric(number_sams),.id = "own_rent_num_rooms_id",.remove = TRUE)
#not run                            
housing_occup_bedrooms_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25042") #of occup, number of bedrooms
housing_occup_bedrooms_data <- housing_occup_bedrooms_from_census %>%
  mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
  filter(label != "Estimate!!Total") %>%
  pivot_longer(4:ncol(housing_occup_bedrooms_from_census),names_to = "tract", values_to = "number_sams") %>%
  separate(label, c("own_rent","num_bedrooms"), sep = "!!", remove = F, convert = FALSE) %>%
  filter(!is.na(num_bedrooms) & number_sams > 0) %>%
  uncount(as.numeric(number_sams),.id = "own_rent_num_bedrooms_id",.remove = TRUE)



