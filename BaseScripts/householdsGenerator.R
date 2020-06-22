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

  ###NEED TO BUILD ETHNICITY UP ALONG WITH RACE TO COORDINATE AS WE BUILD -- 
    #can think of the first task as walking around the polygon and sorting sides,
    #when you finish and some don't match you step up a level (gathering stragglers, below) and then start walking around again
    #the hope is that as you go up occassionally, you give the system enough freedom to settle back into place as you walk around
    #there's a chance it won't resolve back, so could test and then run again hoping the samples fall in the right race categories
    #per ethnicity....
###    occupied_race_dt with occup_type_dt 
    sam_race_hh <- occupied_race_dt[,c("tract","race","own_rent")]
    sam_eth_hh <- occupied_eth_dt[,c("tract","ethnicity","own_rent")]
    rm(occupied_race_dt)
    rm(occupied_eth_dt)
    #sample inside group for id generation, in case there's oddness - set.seed(seed = seed) doesn't seem worth repeating every time
    #rule is to match until have a degree of freedom, sample in that group on each and then designate - ideal is to add only one
    #think of it as a sort of triangulation repeated
    #add householder_age_9 to occup_type_dt - (just a specification inside householder age, here with own_rent)
    occup_type_dt[order(match(own_rent,c("Owner occupied","Renter occupied")),
                        match(householder_age,c("Householder 15 to 34 years","Householder 35 to 64 years", "Householder 65 years and over"))),
                  ("num_type_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    hh_age_dt[order(match(own_rent,c("Owner occupied","Renter occupied")),
                    match(householder_age_9,c("Householder 15 to 24 years","Householder 25 to 34 years","Householder 35 to 44 years",
                                              "Householder 45 to 54 years","Householder 55 to 59 years","Householder 60 to 64 years",
                                              "Householder 65 to 74 years","Householder 75 to 84 years"))),
              ("num_type_id"):=paste0(tract,own_rent,as.character(1000000++seq.int(1:.N))),by=.(tract,own_rent)]
    occup_type_dt[,c("householder_age_9"):=
                    hh_age_dt[.SD, list(householder_age_9), on = .(num_type_id)]]
    #test hh1a
    #test <- table(hh_age_dt$tract,hh_age_dt$householder_age_9)==table(occup_type_dt$tract,occup_type_dt$householder_age_9)
    #length(test[test==F])==0
    rm(hh_age_dt)
    #coordinate family_type back to own_rent, so that hh_type_race has an own_rent on it to match later
#the problem here is the distribution of own_rent onto race in the background is underdetermined - this provides a matching, but could have ones that will not properly resolve
    occup_type_dt[,("rent_type_id"):=paste0(tract,family_type,single_hh_sex,as.character(1000000+sample(1:.N))),
                  by=.(tract,family_type,single_hh_sex)]
    hh_type_race_dt[,("rent_type_id"):=paste0(tract,family_type,single_hh_sex,as.character(1000000+sample(1:.N))),
                    by=.(tract,family_type,single_hh_sex)]
    hh_type_race_dt[,c("own_rent","householder_age","householder_age_9"):=
                  occup_type_dt[.SD, c(list(own_rent),list(householder_age),list(householder_age_9)), 
                                on = .(rent_type_id)]]
    #hh_type_race_dt[,("rent_type_id"):=NULL] #use below for shuffle to keep age
    hh_type_eth_dt[,("rent_type_id"):=paste0(tract,family_type,single_hh_sex,as.character(1000000+sample(1:.N))),
                   by=.(tract,family_type,single_hh_sex)]
    hh_type_eth_dt[,c("own_rent","householder_age","householder_age_9"):=
                      occup_type_dt[.SD, c(list(own_rent),list(householder_age),list(householder_age_9)), 
                                    on = .(rent_type_id)]]
    #hh_type_eth_dt[,("rent_type_id"):=NULL]
    #test hh1
    #test <- table(hh_type_eth_dt$tract,hh_type_eth_dt$own_rent,hh_type_eth_dt$family_type)==table(hh_type_race_dt$tract,hh_type_race_dt$own_rent,hh_type_race_dt$family_type)
    #length(test[test==F])==0
    
    #add family_type info to sam_race/eth_hh
    sam_race_hh[,("race_id"):=paste0(tract,own_rent,race,as.character(1000000+sample(1:.N))),by=.(tract,own_rent,race)]
    hh_type_race_dt[,("race_id"):=paste0(tract,own_rent,race,as.character(1000000+sample(1:.N))),by=.(tract,own_rent,race)]
    sam_race_hh[,c("family","family_type","family_role","single_hh_sex","householder_age","householder_age_9"):= 
                  hh_type_race_dt[.SD, c(list(family),list(family_type),list(family_role),list(single_hh_sex),
                                         list(householder_age),list(householder_age_9)), 
                                  on = .(race_id)]]
    #anti_hh_type_race_dt <- as.data.table(anti_join(hh_type_race_dt,sam_race_hh,by="race_id"))
    hh_type_race_dt[,("missed_type"):=sam_race_hh[.SD,list(family),on=.(race_id)]]
    anti_hh_type_race_dt <- hh_type_race_dt[is.na(missed_type)]
    sam_race_hh[,("race_id"):=NULL]
    sam_eth_hh[,("ethnicity_id"):=
                 paste0(tract,own_rent,ethnicity,as.character(1000000+sample(1:.N))),by=.(tract,own_rent,ethnicity)]
    hh_type_eth_dt[,("ethnicity_id"):=
                     paste0(tract,own_rent,ethnicity,as.character(1000000+sample(1:.N))),by=.(tract,own_rent,ethnicity)]
    sam_eth_hh[,c("family","family_type","family_role","single_hh_sex","householder_age","householder_age_9"):=  
                 hh_type_eth_dt[.SD, c(list(family),list(family_type),list(family_role),list(single_hh_sex),
                                       list(householder_age),list(householder_age_9)), 
                                on = .(ethnicity_id)]]
    #anti_hh_type_eth_dt <- as.data.table(anti_join(hh_type_eth_dt,sam_eth_hh,by="ethnicity_id"))
    hh_type_eth_dt[,("missed_type"):=sam_eth_hh[.SD,list(family),on=.(ethnicity_id)]]
    anti_hh_type_eth_dt <- hh_type_eth_dt[is.na(missed_type)]
    sam_eth_hh[,("ethnicity_id"):=NULL]
    #and pick up stragglers (about 100k, or 6%) - drop race/eth from match, could use orig on sam_race/eth or bring over (trying bring over)
    
    #rematch remaining ~100k/6% - stuff on hh_type was assigned as possible, so re-sort it
    #test hh2 - right mix for rematch
    #test <- table(
    #  anti_hh_type_race_dt$tract,
    #  anti_hh_type_race_dt$own_rent
    #  )==table(
    #    sam_race_hh[is.na(family)]$tract,
    #    sam_race_hh[is.na(family)]$own_rent
    #  )
    #length(test[test==F])==0
    #test <- table(
    #  anti_hh_type_race_dt$tract,
    #  anti_hh_type_race_dt$race
    #)==table(
    #  sam_race_hh[is.na(family)]$tract,
    #  sam_race_hh[is.na(family)]$race
    #)
    #length(test[test==F])==0
    #test <- table(
    #  anti_hh_type_eth_dt$tract,
    #  anti_hh_type_eth_dt$own_rent
    #)==table(
    #  sam_eth_hh[is.na(family)]$tract,
    #  sam_eth_hh[is.na(family)]$own_rent
    #)
    #length(test[test==F])==0
    #test <- table(
    #  anti_hh_type_eth_dt$tract,
    #  anti_hh_type_eth_dt$ethnicity
    #)==table(
    #  sam_eth_hh[is.na(family)]$tract,
    #  sam_eth_hh[is.na(family)]$ethnicity
    #)
    #length(test[test==F])==0
    #since hh_type had race assigned, privilege sam_race - if you had put race from sam onto hh_type, it would have been one of possible
    #ways to assign it from hh_type to begin with (totals are the same); 
    #first move a new match on own rent on race to anti_hh_type_race
    sam_race_hh[is.na(family),("race1_id"):=
                  paste0(tract,race,as.character(1000000+sample(1:.N))),
                by=.(tract,race)]
    anti_hh_type_race_dt[,("race1_id"):=
                           paste0(tract,race,as.character(1000000+sample(1:.N))),
                         by=.(tract,race)]
    anti_hh_type_race_dt[,c("own_rent"):= #this own_rent shouldn't over-write connection to age
                           sam_race_hh[.SD, c(list(own_rent)), 
                                       on = .(race1_id)]]
    anti_occup_type_race_dt <- as.data.table(left_join(anti_hh_type_race_dt,occup_type_dt,by="rent_type_id"))
    anti_hh_type_race_dt[,("race1a_id"):=
                           paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                         by=.(tract,own_rent)]
    anti_occup_type_race_dt[,("race1a_id"):=
                           paste0(tract.y,own_rent.y,as.character(1000000+sample(1:.N))),
                         by=.(tract.y,own_rent.y)]
    anti_hh_type_race_dt[,c("householder_age","householder_age_9"):= #this own_rent shouldn't over-write connection to age
                           anti_occup_type_race_dt[.SD, c(list(householder_age.y),list(householder_age_9.y)), 
                                       on = .(race1a_id)]]
    
    #sam_race_hh[,("race1_id"):=NULL]
    #then bring back, including bringing back race
    sam_race_hh[is.na(family),("race2_id"):=
                  paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,race,own_rent)]
    anti_hh_type_race_dt[,("race2_id"):=
                           paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                         by=.(tract,race,own_rent)]
    sam_race_hh[is.na(family),c("family","family_type","family_role","single_hh_sex",
                                "householder_age","householder_age_9"):= 
                  anti_hh_type_race_dt[.SD, c(list(family),list(family_type),list(family_role),list(single_hh_sex),
                                              list(householder_age),list(householder_age_9)), 
                                  on = .(race2_id)]]
    #put it back to help with age match, below
    hh_type_race_dt[is.na(family),c("own_rent","householder_age","householder_age_9"):=
                     anti_hh_type_race_dt[.SD,c(list(own_rent),list(householder_age),list(householder_age_9)),
                                         on = .(race_id)]]
    #sam_race_hh[,("race2_id"):=NULL]
    sam_eth_hh[is.na(family),("ethnicity1_id"):=
                  paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                by=.(tract,ethnicity)]
    anti_hh_type_eth_dt[,("ethnicity1_id"):=
                           paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                         by=.(tract,ethnicity)]
    anti_hh_type_eth_dt[,c("own_rent"):= 
                           sam_eth_hh[.SD, c(list(own_rent)), 
                                       on = .(ethnicity1_id)]]
    anti_occup_type_eth_dt <- as.data.table(left_join(anti_hh_type_eth_dt,occup_type_dt,by="rent_type_id"))
    anti_hh_type_eth_dt[,("ethnicity1a_id"):=
                           paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                         by=.(tract,own_rent)]
    anti_occup_type_eth_dt[,("ethnicity1a_id"):=
                              paste0(tract.y,own_rent.y,as.character(1000000+sample(1:.N))),
                            by=.(tract.y,own_rent.y)]
    anti_hh_type_eth_dt[,c("householder_age","householder_age_9"):= #this own_rent shouldn't over-write connection to age
                           anti_occup_type_eth_dt[.SD, c(list(householder_age.y),list(householder_age_9.y)), 
                                       on = .(ethnicity1a_id)]]
    
    #sam_eth_hh[,("ethnicity1_id"):=NULL]
    sam_eth_hh[is.na(family),("ethnicity2_id"):=
                 paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),by=.(tract,ethnicity,own_rent)]
    anti_hh_type_eth_dt[,("ethnicity2_id"):=
                     paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),by=.(tract,ethnicity,own_rent)]
    sam_eth_hh[is.na(family),c("family","family_type","family_role","single_hh_sex",
                               "householder_age","householder_age_9"):=  
                 anti_hh_type_eth_dt[.SD, c(list(family),list(family_type),list(family_role),list(single_hh_sex),
                                            list(householder_age),list(householder_age_9)), 
                                on = .(ethnicity2_id)]]
    #put it back to help with age match, below
    hh_type_eth_dt[is.na(family),c("own_rent","householder_age","householder_age_9"):=
                     anti_hh_type_eth_dt[.SD,c(list(own_rent),list(householder_age),list(householder_age_9)),
                                         on = .(ethnicity_id)]]
    #sam_eth_hh[,("ethnicity2_id"):=NULL]
    #test hh2b 
    #test <- nrow(sam_eth_hh[is.na(family)])==0
    #test <- nrow(sam_race_hh[is.na(family)])==0
    #test <- table(
    #  sam_race_hh$tract,
    #  sam_race_hh$race,
    #  #sam_race_hh$own_rent,#with this in, it's false, because line-up would be from a diff. possible match
    #  sam_race_hh$family,
    #  sam_race_hh$family_type,
    #  sam_race_hh$single_hh_sex
    #  )==
    #  table(
    #    hh_type_race_dt$tract,
    #    hh_type_race_dt$race,
    #    #hh_type_race_dt$own_rent, 
    #    hh_type_race_dt$family,
    #    hh_type_race_dt$family_type,
    #    hh_type_race_dt$single_hh_sex
    #    )
    #length(test[test==F])==0
    #test <- table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$ethnicity,
    #  sam_eth_hh$family,
    #  sam_eth_hh$family_type,
    #  sam_eth_hh$single_hh_sex
    #  )==
    #  table(
    #    hh_type_eth_dt$tract,
    #    hh_type_eth_dt$ethnicity,
    #    hh_type_eth_dt$family,
    #    hh_type_eth_dt$family_type,
    #    hh_type_eth_dt$single_hh_sex
    #    )
    #length(test[test==F])==0
    #test <- table(
    #  sam_race_hh$tract,
    #  sam_race_hh$family,
    #  sam_race_hh$family_type,
    #  sam_race_hh$family_role,
    #  sam_race_hh$single_hh_sex
    #  )==
    #  table(
    #    sam_eth_hh$tract,
    #    sam_eth_hh$family,
    #    sam_eth_hh$family_type,
    #    sam_eth_hh$family_role,
    #    sam_eth_hh$single_hh_sex
    #    )
    #length(test[test==F])==0
    #test <- table(
    #  sam_race_hh$tract,
    #  sam_race_hh$householder_age_9,
    #  sam_race_hh$own_rent
    #)==
    #  table(
    #    sam_eth_hh$tract,
    #    sam_eth_hh$householder_age_9,
    #    sam_eth_hh$own_rent
    #  )
    #length(test[test==F])==0
    
#add family type for housing units to housing units by rent_own and by race - giving units as much to match on as possible by building a set, aligned with sam as we build when there's plenty of space for multiple solutions
    #start with own_rent, since it has lots of freedom
    
    
    housing_units_rent_dt[,("rent_race_id"):=paste0(tract,housing_units,as.character(1000000+sample(1:.N))),
                          by=.(tract,housing_units)]
    housing_units_race_dt[,("rent_race_id"):=paste0(tract,housing_units,as.character(1000000+sample(1:.N))),
                          by=.(tract,housing_units)]
    housing_units_eth_dt[,("rent_race_id"):=paste0(tract,housing_units,as.character(1000000+sample(1:.N))),
                          by=.(tract,housing_units)]
    housing_units_race_dt[,("own_rent"):=
                            housing_units_rent_dt[.SD,list(own_rent),on=.(rent_race_id)]]
    housing_units_eth_dt[,("own_rent"):=
                           housing_units_rent_dt[.SD,list(own_rent),on=.(rent_race_id)]]
    #test hh31a
    #test <- table(
    #  housing_units_race_dt$tract,
    #  housing_units_race_dt$housing_units,
    #  housing_units_race_dt$own_rent
    #)==
    #  table(
    #    housing_units_rent_dt$tract,
    #    housing_units_rent_dt$housing_units,
    #    housing_units_rent_dt$own_rent
    #  )
    #length(test[test==F])==0
    #test <- table(
    #  housing_units_eth_dt$tract,
    #  housing_units_eth_dt$housing_units,
    #  housing_units_eth_dt$own_rent
    #)==
    #  table(
    #    housing_units_rent_dt$tract,
    #    housing_units_rent_dt$housing_units,
    #    housing_units_rent_dt$own_rent
    #  )
    #length(test[test==F])==0
    ##test hh3preA - each works alone with tract, but not together
    #test <- table(
    #  housing_units_eth_dt$tract,
    #  housing_units_eth_dt$ethnicity#,
    #  #housing_units_eth_dt$own_rent
    #)==
    #  table(
    #    sam_eth_hh$tract,
    #    sam_eth_hh$ethnicity#,
    #    #sam_eth_hh$own_rent
    #  )
    #length(test[test==F])==0
  
    sam_race_hh[,("rent_type_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,race,own_rent)]
    housing_units_race_dt[,("rent_type_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,race,own_rent)]
    housing_units_race_dt[,c("family","family_type","family_role","single_hh_sex"):=
                            sam_race_hh[.SD,c(list(family),list(family_type),
                                              list(family_role),list(single_hh_sex)),
                                        on=.(rent_type_id)]]
    sam_race_hh[,("miss_fr4"):=
                  housing_units_race_dt[.SD,list(family_role),on=.(rent_type_id)]]
    sam_eth_hh[,("rent_type_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,ethnicity,own_rent)]
    housing_units_eth_dt[,("rent_type_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),
                          by=.(tract,ethnicity,own_rent)]
    housing_units_eth_dt[,c("family","family_type","family_role","single_hh_sex"):=
                            sam_eth_hh[.SD,c(list(family),list(family_type),
                                             list(family_role),list(single_hh_sex)),
                                       on=.(rent_type_id)]]
    sam_eth_hh[,("miss_fr4"):=
                  housing_units_eth_dt[.SD,list(family_role),on=.(rent_type_id)]]
    #nrow(sam_race_hh[is.na(miss_fr4)])==nrow(housing_units_race_dt[is.na(family_role)])
    #nrow(sam_eth_hh[is.na(miss_fr4)])==nrow(housing_units_eth_dt[is.na(family_role)])
    #because race/eth were randomly matched to family stuff, reshuffling how they're matched
    sam_race_leftover <- sam_race_hh[is.na(miss_fr4)]
    housing_units_race_leftover <- housing_units_race_dt[is.na(family_role)]
    sam_race_leftover[,("rent_type1_id"):=paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent)]
    housing_units_race_leftover[,("rent_type1_id"):=paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                          by=.(tract,own_rent)]
    housing_units_race_leftover[,("race"):=
                            sam_race_leftover[.SD,list(race),on=.(rent_type1_id)]]
    
    sam_eth_leftover <- sam_eth_hh[is.na(miss_fr4)]
    housing_units_eth_leftover <- housing_units_eth_dt[is.na(family_role)]
    sam_eth_leftover[,("rent_type1_id"):=paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent)]
    housing_units_eth_leftover[,("rent_type1_id"):=paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                         by=.(tract,own_rent)]
    housing_units_eth_leftover[,("ethnicity"):=
                           sam_eth_leftover[.SD,list(ethnicity),on=.(rent_type1_id)]]
    #then try last bit second time on the reshuffled bit
    sam_race_leftover[,("rent2_type_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,race,own_rent)]
    housing_units_race_leftover[,("rent2_type_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                          by=.(tract,race,own_rent)]
    housing_units_race_leftover[,c("family","family_type","family_role","single_hh_sex"):=
                            sam_race_leftover[.SD,c(list(family),list(family_type),
                                                    list(family_role),list(single_hh_sex)),
                                              on=.(rent2_type_id)]]
    sam_race_leftover[,c("miss_fr4"):=
                        housing_units_race_leftover[.SD,c(list(family_role)),on=.(rent2_type_id)]]
    sam_eth_leftover[,("rent2_type_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),
               by=.(tract,ethnicity,own_rent)]
    housing_units_eth_leftover[,("rent2_type_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),
                         by=.(tract,ethnicity,own_rent)]
    housing_units_eth_leftover[,c("family","family_type","family_role","single_hh_sex"):=
                                 sam_eth_leftover[.SD,c(list(family),list(family_type),
                                                        list(family_role),list(single_hh_sex)),
                                                  on=.(rent2_type_id)]]
    sam_eth_leftover[,c("miss_fr4"):=
                 housing_units_eth_leftover[.SD,c(list(family_role)),on=.(rent2_type_id)]]
    #and back up to whole on housing units side, ready to then match with hh_units
    housing_units_race_dt[is.na(family_role),
               c("family","family_type","family_role","single_hh_sex"):=
                 housing_units_race_leftover[.SD,c(list(family),list(family_type),list(family_role),list(single_hh_sex)),
                                             on=.(rent_type_id)]]
    housing_units_eth_dt[is.na(family_role),
               c("family","family_type","family_role","single_hh_sex"):=
                 housing_units_eth_leftover[.SD,c(list(family),list(family_type),list(family_role),list(single_hh_sex)),
                                            on=.(rent_type_id)]]
    
    #testing case_when vs. if_else b/c of weird type error
    #sam_race_hh[,("family_role_4"):=case_when(
    #  family_role=="Householder living alone" | family_role=="Householder not living alone" ~ "Nonfamily households",
    #  TRUE ~ family_role
    #)]
    #sam_eth_hh[,("family_role_4"):=case_when(
    #  family_role=="Householder living alone" | family_role=="Householder not living alone" ~ "Nonfamily households",
    #  TRUE ~ family_role
    #)]
    sam_race_hh[,("family_role_4"):=if_else(family_role=="Householder living alone" | family_role=="Householder not living alone",
                                 c("Nonfamily households"),c(family_role))]
    sam_eth_hh[,("family_role_4"):=if_else(family_role=="Householder living alone" | family_role=="Householder not living alone",
                                 c("Nonfamily households"),c(family_role))]
    housing_units_race_dt[,("family_role_4"):=if_else(family_role=="Householder living alone" | family_role=="Householder not living alone",
                                            c("Nonfamily households"),c(family_role))]
    housing_units_eth_dt[,("family_role_4"):=if_else(family_role=="Householder living alone" | family_role=="Householder not living alone",
                                           c("Nonfamily households"),c(family_role))]
    
    #only using for testing purposes:
    #fix labeling on unit naming to match with hh_type_units
    housing_units_race_dt[,("num_structures"):=case_when(
      housing_units=="1 attached" | housing_units=="1 detached" ~ "1-unit structures",
      housing_units=="Boat RV van etc." | housing_units=="Mobile home" ~ "Mobile homes and all other types of units",
      TRUE ~ "2-or-more-unit structures"
    )]
    housing_units_eth_dt[,("num_structures"):=case_when(
      housing_units=="1 attached" | housing_units=="1 detached" ~ "1-unit structures",
      housing_units=="Boat RV van etc." | housing_units=="Mobile home" ~ "Mobile homes and all other types of units",
      TRUE ~ "2-or-more-unit structures"
    )]
    
    #test hh4c - confirming distribution with hh_type_units - no need to do more
    #test <- table(
    #  housing_units_race_dt$tract,
    #  housing_units_race_dt$family,
    #  housing_units_race_dt$family_role_4
    #)==table(
    #  hh_type_units_dt$tract,
    #  hh_type_units_dt$family,
    #  hh_type_units_dt$family_role_4
    #)
    #length(test[test==F])==0
    #test <- table(
    #  housing_units_eth_dt$tract,
    #  housing_units_eth_dt$family,
    #  housing_units_eth_dt$family_role_4
    #)==table(
    #  hh_type_units_dt$tract,
    #  hh_type_units_dt$family,
    #  hh_type_units_dt$family_role_4
    #)
    #length(test[test==F])==0
    #test hh4d - confirming distribution with hh_type_units - no need to do more
    #test <- table(
    #  sam_race_hh$tract,
    #  sam_race_hh$family,
    #  sam_race_hh$family_role_4
    #)==table(
    #  hh_type_units_dt$tract,
    #  hh_type_units_dt$family,
    #  hh_type_units_dt$family_role_4
    #)
    #length(test[test==F])==0
    #test <- table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$family,
    #  sam_eth_hh$family_role_4
    #)==table(
    #  hh_type_units_dt$tract,
    #  hh_type_units_dt$family,
    #  hh_type_units_dt$family_role_4
    #)
    #length(test[test==F])==0
    
    #put housing_units in from hh_type_units - this just ensures that race/eth have a good match on housing_units
    hh_type_units_dt[,("hh_type_units_id"):=
                       paste0(tract,family,family_role_4,as.character(1000000+sample(1:.N))),
                     by=.(tract,family,family_role_4)]
    housing_units_race_dt[,("hh_type_units_id"):=
                            paste0(tract,family,family_role_4,as.character(1000000+sample(1:.N))),
                          by=.(tract,family,family_role_4)]
    housing_units_eth_dt[,("hh_type_units_id"):=
                            paste0(tract,family,family_role_4,as.character(1000000+sample(1:.N))),
                          by=.(tract,family,family_role_4)]
    #first put housing_units onto hh_type_units, so don't lose specificity
    hh_type_units_dt[,("housing_units_eth"):=
                       housing_units_eth_dt[.SD,list(housing_units),
                                            on=.(hh_type_units_id)]]
    hh_type_units_dt[,("housing_units_race"):=
                       housing_units_race_dt[.SD,list(housing_units),
                                             on=.(hh_type_units_id)]]
    #then move back as pair
    housing_units_eth_dt[,c("housing_units","num_structures"):=
                           hh_type_units_dt[.SD,c(list(housing_units_eth),list(num_structures)),
                                            on=.(hh_type_units_id)]]
    housing_units_race_dt[,c("housing_units","num_structures"):=
                           hh_type_units_dt[.SD,c(list(housing_units_race),list(num_structures)),
                                            on=.(hh_type_units_id)]]
                     
    #test hh5 - family_role and num_structures before moving over to sam_eth/race as just the units
    #test <- table(
    #  housing_units_race_dt$tract,
    #  housing_units_race_dt$family,
    #  housing_units_race_dt$family_role_4,
    #  housing_units_race_dt$num_structures
    #)==table(
    #  hh_type_units_dt$tract,
    #  hh_type_units_dt$family,
    #  hh_type_units_dt$family_role_4,
    #  hh_type_units_dt$num_structures
    #)
    #length(test[test==F])==0
    #test <- table(
    #  housing_units_eth_dt$tract,
    #  housing_units_eth_dt$family,
    #  housing_units_eth_dt$family_role_4,
    #  housing_units_eth_dt$num_structures
    #)==table(
    #  hh_type_units_dt$tract,
    #  hh_type_units_dt$family,
    #  hh_type_units_dt$family_role_4,
    #  hh_type_units_dt$num_structures
    #)
    #length(test[test==F])==0

    #now add housing_units and num_structures to sam_eth/race
    sam_race_hh[,("hh_type2_units_id"):=
                       paste0(tract,race,family,family_role,as.character(1000000+sample(1:.N))),
                     by=.(tract,race,family,family_role)]
    housing_units_race_dt[,("hh_type2_units_id"):=
                            paste0(tract,race,family,family_role,as.character(1000000+sample(1:.N))),
                          by=.(tract,race,family,family_role)]
    sam_race_hh[,c("housing_units","num_structures"):=
                  housing_units_race_dt[.SD,c(list(housing_units),list(num_structures)),
                                             on=.(hh_type2_units_id)]]
    housing_units_race_dt[,c("missing"):=
                            sam_race_hh[.SD,c(list(housing_units)),
                                        on=.(hh_type2_units_id)]]
    #nrow(sam_race_hh[is.na(housing_units)]) #25270
    #nrow(sam_race_hh[is.na(housing_units)]) == nrow(housing_units_race_dt[is.na(missing)])
    sam_eth_hh[,("hh_type2_units_id"):=
                  paste0(tract,ethnicity,family,family_role,as.character(1000000+sample(1:.N))),
                by=.(tract,ethnicity,family,family_role)]
    housing_units_eth_dt[,("hh_type2_units_id"):=
                            paste0(tract,ethnicity,family,family_role,as.character(1000000+sample(1:.N))),
                          by=.(tract,ethnicity,family,family_role)]
    sam_eth_hh[,c("housing_units","num_structures"):=
                  housing_units_eth_dt[.SD,c(list(housing_units),list(num_structures)),
                                        on=.(hh_type2_units_id)]]
    housing_units_eth_dt[,c("missing"):=
                            sam_eth_hh[.SD,c(list(housing_units)),
                                        on=.(hh_type2_units_id)]]
    #nrow(sam_eth_hh[is.na(housing_units)]) #~22k
    #nrow(sam_eth_hh[is.na(housing_units)]) == nrow(housing_units_eth_dt[is.na(missing)])
    #line up race/eth from sam to housing_units, with remaining 
    #table(sam_eth_hh[is.na(housing_units)]$ethnicity)==
    #table(housing_units_eth_dt[is.na(missing)]$ethnicity)
    #new hh_type3_units_id on family, family_role, #race/eth from sam to housing
    sam_race_hh[is.na(housing_units),("hh_type3_units_id"):=
                  paste0(tract,family,family_role,as.character(1000000+sample(1:.N))),
                by=.(tract,family,family_role)]
    housing_units_race_dt[is.na(missing),("hh_type3_units_id"):=
                            paste0(tract,family,family_role,as.character(1000000+sample(1:.N))),
                          by=.(tract,family,family_role)]
    housing_units_race_dt[is.na(missing),c("race1"):=
                            sam_race_hh[.SD,c(list(race)),
                                        on=.(hh_type3_units_id)]]
    sam_race_hh[is.na(housing_units),c("miss_race1"):=
                  housing_units_race_dt[.SD,c(list(race)),
                                        on=.(hh_type3_units_id)]]
    #housing_units_race_dt[!is.na(race1),("race"):=race1] #about 1k that didn't match for some reason
    sam_eth_hh[is.na(housing_units),("hh_type3_units_id"):=
                 paste0(tract,family,family_role,as.character(1000000+sample(1:.N))),
               by=.(tract,family,family_role)]
    housing_units_eth_dt[is.na(missing),("hh_type3_units_id"):=
                           paste0(tract,family,family_role,as.character(1000000+sample(1:.N))),
                         by=.(tract,family,family_role)]
    housing_units_eth_dt[is.na(missing),c("ethnicity1"):=
                           sam_eth_hh[.SD,c(list(ethnicity)),
                                      on=.(hh_type3_units_id)]]
    sam_eth_hh[is.na(housing_units),c("miss_eth1"):=
                  housing_units_eth_dt[.SD,c(list(ethnicity)),
                                        on=.(hh_type3_units_id)]]
    #housing_units_eth_dt[!is.na(ethnicity1),("ethnicity"):=ethnicity1] #about 1k that didn't match for some reason
    #new hh_type3a_units_id on family #race/eth from sam to housing
    sam_race_hh[is.na(housing_units)&is.na(miss_race1),("hh_type3a_units_id"):=
                  paste0(tract,family,as.character(1000000+sample(1:.N))),
                by=.(tract,family)]
    housing_units_race_dt[is.na(missing)&is.na(race1),("hh_type3a_units_id"):=
                            paste0(tract,family,as.character(1000000+sample(1:.N))),
                          by=.(tract,family)]
    housing_units_race_dt[is.na(missing)&is.na(race1),c("race1"):=
                            sam_race_hh[.SD,c(list(race)),
                                        on=.(hh_type3a_units_id)]]
    #move over to race
    #nrow(housing_units_race_dt[!is.na(race1)])==nrow(housing_units_race_dt[is.na(missing)])
    housing_units_race_dt[!is.na(race1),("race"):=race1] 
    
    sam_eth_hh[is.na(housing_units)&is.na(miss_eth1),("hh_type3a_units_id"):=
                 paste0(tract,family,as.character(1000000+sample(1:.N))),
               by=.(tract,family)]
    housing_units_eth_dt[is.na(missing)&is.na(ethnicity1),("hh_type3a_units_id"):=
                           paste0(tract,family,as.character(1000000+sample(1:.N))),
                         by=.(tract,family)]
    housing_units_eth_dt[is.na(missing)&is.na(ethnicity1),c("ethnicity1"):=
                           sam_eth_hh[.SD,c(list(ethnicity)),
                                      on=.(hh_type3a_units_id)]]
    #move rest over to ethnicity 
    #nrow(housing_units_eth_dt[!is.na(ethnicity1)])==nrow(housing_units_eth_dt[is.na(missing)])
    housing_units_eth_dt[!is.na(ethnicity1),("ethnicity"):=ethnicity1] 
    
    #then housing units from housing back to sam, based on hh_type4
    sam_race_hh[is.na(housing_units),("hh_type4_units_id"):=
                  paste0(tract,race,family,family_role,as.character(1000000+sample(1:.N))),
                by=.(tract,race,family,family_role)]
    housing_units_race_dt[is.na(missing),("hh_type4_units_id"):=
                            paste0(tract,race,family,family_role,as.character(1000000+sample(1:.N))),
                          by=.(tract,race,family,family_role)]
    sam_race_hh[is.na(housing_units),c("housing_units","num_structures"):=
                  housing_units_race_dt[.SD,c(list(housing_units),list(num_structures)),
                                        on=.(hh_type4_units_id)]]
    housing_units_race_dt[is.na(missing),c("missing"):=
                            sam_race_hh[.SD,c(list(housing_units)),
                                        on=.(hh_type4_units_id)]]
    #nrow(sam_race_hh[is.na(housing_units)])==0
    sam_eth_hh[is.na(housing_units),("hh_type4_units_id"):=
                 paste0(tract,ethnicity,family,family_role,as.character(1000000+sample(1:.N))),
               by=.(tract,ethnicity,family,family_role)]
    housing_units_eth_dt[is.na(missing),("hh_type4_units_id"):=
                           paste0(tract,ethnicity,family,family_role,as.character(1000000+sample(1:.N))),
                         by=.(tract,ethnicity,family,family_role)]
    sam_eth_hh[is.na(housing_units),c("housing_units","num_structures"):=
                 housing_units_eth_dt[.SD,c(list(housing_units),list(num_structures)),
                                      on=.(hh_type4_units_id)]]
    housing_units_eth_dt[is.na(missing),c("missing"):=
                           sam_eth_hh[.SD,c(list(housing_units)),
                                      on=.(hh_type4_units_id)]]
    #nrow(sam_eth_hh[is.na(housing_units)])==0
    
    #test hh5b - housing_units matched on family, race/eth
    #test <- table(
    #  housing_units_race_dt$tract,
    #  housing_units_race_dt$family,
    #  housing_units_race_dt$housing_units,
    #  housing_units_race_dt$race
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$family,
    #  sam_race_hh$housing_units,
    #  sam_race_hh$race
    #)
    #length(test[test==F])==0
    #test <- table(
    #  housing_units_eth_dt$tract,
    #  housing_units_eth_dt$family,
    #  housing_units_eth_dt$housing_units,
    #  housing_units_eth_dt$ethnicity
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$family,
    #  sam_eth_hh$housing_units,
    #  sam_eth_hh$ethnicity
    #)
    #length(test[test==F])==0
    
   
    #per_room has race, own_rent, and age - build from matching with sam_hh, then match per_room
    #the suite of things that reflect income will have to be adjusted as a group as we add to full_sam
    
    #then add per_room  with own_rent, race and age_range_3 to match - 
    #put own_rent by race on housing_per_room_race, then match back on num_per and own_rent
    sam_race_hh[,("race_id"):=paste0(tract,race,as.character(1000000+sample(1:.N))),
                by=.(tract,race)]
    housing_per_room_race_dt[,("race_id"):=paste0(tract,race,as.character(1000000+sample(1:.N))),
                             by=.(tract,race)]
    housing_per_room_race_dt[,c("own_rent"):=
                               sam_race_hh[.SD, list(own_rent), 
                                           on = .(race_id)]]
    sam_race_hh[,("race_id"):=NULL]
    sam_eth_hh[,("eth_id"):=paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
               by=.(tract,ethnicity)]
    housing_per_room_eth_dt[,("eth_id"):=paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                             by=.(tract,ethnicity)]
    housing_per_room_eth_dt[,c("own_rent"):=
                              sam_eth_hh[.SD, list(own_rent), 
                                         on = .(eth_id)]]
    sam_eth_hh[,("eth_id"):=NULL]
    #own_rent is broken out from race, so add it back in per_room eth/race 
    housing_per_room_race_dt[,("num_per_type_id"):=paste0(tract,num_per,as.character(1000000+sample(1:.N))),
                             by=.(tract,num_per)]
    housing_per_room_rent_dt[,("num_per_type_id"):=paste0(tract,num_per,as.character(1000000+sample(1:.N))),
                             by=.(tract,num_per)]
    housing_per_room_race_dt[,c("num_per_room_5"):= 
                               housing_per_room_rent_dt[.SD, list(num_per_room_5),
                                                        on = .(num_per_type_id)]]
    housing_per_room_eth_dt[,("num_per_type_id"):=paste0(tract,num_per,as.character(1000000+sample(1:.N))),
                             by=.(tract,num_per)]
    housing_per_room_eth_dt[,c("num_per_room_5"):= 
                               housing_per_room_rent_dt[.SD, list(num_per_room_5), 
                                                        on = .(num_per_type_id)]]
    #then add householder_age from sam to here by own_rent and race
    sam_race_hh[,("householder_age_3"):=householder_age]
    sam_eth_hh[,("householder_age_3"):=householder_age]
    sam_race_hh[,("age_per_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,race,own_rent)]
    housing_per_room_race_dt[,("age_per_id"):=paste0(tract,race,own_rent,as.character(1000000+sample(1:.N))),
                             by=.(tract,race,own_rent)]
    housing_per_room_race_dt[,c("householder_age_3"):=
                               sam_race_hh[.SD, list(householder_age_3), 
                                           on = .(age_per_id)]]
    sam_race_hh[,("age_per_id"):=NULL]
    sam_eth_hh[,("age_per_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),
               by=.(tract,ethnicity,own_rent)]
    housing_per_room_eth_dt[,("age_per_id"):=paste0(tract,ethnicity,own_rent,as.character(1000000+sample(1:.N))),
                             by=.(tract,ethnicity,own_rent)]
    housing_per_room_eth_dt[,c("householder_age_3"):=
                               sam_eth_hh[.SD, list(householder_age_3), 
                                           on = .(age_per_id)]]
    sam_eth_hh[,("age_per_id"):=NULL]
    
    #then add to sam_hh by age, own_rent and eth/race...
    housing_per_room_race_dt[,("num_per_id"):=paste0(tract,race,own_rent,householder_age_3,as.character(1000000+sample(1:.N))),
                             by=.(tract,race,own_rent,householder_age_3)]
    sam_race_hh[,("num_per_id"):=paste0(tract,race,own_rent,householder_age_3,as.character(1000000+sample(1:.N))),
                by=.(tract,race,own_rent,householder_age_3)]
    sam_race_hh[,c("people_per_room"):= 
                  housing_per_room_race_dt[.SD, list(num_per_room_5),
                                          on = .(num_per_id)]]
    sam_race_hh[,("num_per_id"):=NULL]
    housing_per_room_eth_dt[,("num_per_id"):=paste0(tract,ethnicity,own_rent,householder_age_3,as.character(1000000+sample(1:.N))),
                            by=.(tract,ethnicity,own_rent,householder_age_3)]
    sam_eth_hh[,("num_per_id"):=paste0(tract,ethnicity,own_rent,householder_age_3,as.character(1000000+sample(1:.N))),
               by=.(tract,ethnicity,own_rent,householder_age_3)]
    sam_eth_hh[,c("people_per_room"):= 
                  housing_per_room_eth_dt[.SD, list(num_per_room_5),
                                           on = .(num_per_id)]]
    sam_eth_hh[,("num_per_id"):=NULL]

    #test hh6
    #test <- table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$people_per_room
    #)==
    #  table(
    #    sam_race_hh$tract,
    #    sam_race_hh$people_per_room
    #  )
    #length(test[test==FALSE])==0

    
#    saveRDS(sam_eth_hh,file = paste0(housingdir, vintage, "/sam_eth_hh",Sys.Date(),".RDS"))
#    saveRDS(sam_race_hh,file = paste0(housingdir, vintage, "/sam_race_hh",Sys.Date(),".RDS"))
    

#add hh_size to occup, for race and eth
    #add family and family_type to occup_size by matching with sam_hh on own_rent
    
    sam_eth_hh[order(-people_per_room),("hh_size_or_id"):=
             paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent)]
    hh_occup_size_dt[order(hh_size),("hh_size_or_id"):=
                       paste0(tract,own_rent,
                              as.character(1000000+seq.int(1:.N))),
                     by=.(tract,own_rent)]
    hh_occup_size_dt[,c("family_or_non_eth","family_type_eth"):=  
                 sam_eth_hh[.SD, c(list(family),list(family_type)), 
                                  on = .(hh_size_or_id)]]
    sam_race_hh[order(-people_per_room),("hh_size_or2_id"):=
                 paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent)]
    hh_occup_size_dt[order(hh_size),("hh_size_or2_id"):=
                       paste0(tract,own_rent,
                              as.character(1000000+seq.int(1:.N))),
                     by=.(tract,own_rent)]
    hh_occup_size_dt[,c("family_or_non_race","family_type_race"):=  
                       sam_race_hh[.SD, c(list(family),list(family_type)), 
                                  on = .(hh_size_or2_id)]]
    
    #test hh7
    #test<-table(
    #  hh_occup_size_dt$tract,
    #  hh_occup_size_dt$own_rent,
    #  hh_occup_size_dt$family_or_non_eth,
    #  hh_occup_size_dt$family_type_eth
    #  )==table(
    #    sam_eth_hh$tract,
    #    sam_eth_hh$own_rent,
    #    sam_eth_hh$family,
    #    sam_eth_hh$family_type
    #    )
    #length(test[test==F])==0
    #test<-table(
    #  hh_occup_size_dt$tract,
    #  hh_occup_size_dt$own_rent,
    #  hh_occup_size_dt$family_or_non_race,
    #  hh_occup_size_dt$family_type_race
    #  )==table(
    #    sam_race_hh$tract,
    #    sam_race_hh$own_rent,
    #    sam_race_hh$family,
    #    sam_race_hh$family_type
    #    )
    #length(test[test==F])==0
    
    #add own_rent to hh_size_dt for eth/race
    sam_eth_hh[,("hh_size_fam_id"):=paste0(tract,family,as.character(1000000+sample(1:.N))),
           by=.(tract,family)]
    sam_race_hh[,("hh_size_fam_id"):=paste0(tract,family,as.character(1000000+sample(1:.N))),
               by=.(tract,family)]
    hh_size_dt[,("hh_size_fam_id"):=paste0(tract,family_or_non,as.character(1000000+sample(1:.N))),
                     by=.(tract,family_or_non)]
    hh_size_dt[,c("own_rent_eth"):=
                       sam_eth_hh[.SD, list(own_rent), 
                              on = .(hh_size_fam_id)]]
    hh_size_dt[,c("own_rent_race"):=
                 sam_race_hh[.SD, list(own_rent), 
                            on = .(hh_size_fam_id)]]
    #add family_type to hh_size, but the match on hh_size has hh_size and family
    #add own_rent and family_type to hh_size_dt by matching to hh_occup_size on family and hh_size
    hh_size_dt[,("hh_size_eth_id"):=paste0(tract,own_rent_eth,family_or_non,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent_eth,family_or_non)]
    hh_occup_size_dt[,("hh_size_eth_id"):=paste0(tract,own_rent,family_or_non_eth,as.character(1000000+sample(1:.N))),
                             by=.(tract,own_rent,family_or_non_eth)]
    hh_size_dt[,c("family_type_eth"):=
                 hh_occup_size_dt[.SD, list(family_type_eth), 
                                           on = .(hh_size_eth_id)]]
    hh_size_dt[,("hh_size_race_id"):=paste0(tract,own_rent_race,family_or_non,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent_race,family_or_non)]
    hh_occup_size_dt[,("hh_size_race_id"):=paste0(tract,own_rent,family_or_non_race,as.character(1000000+sample(1:.N))),
                     by=.(tract,own_rent,family_or_non_race)]
    hh_size_dt[,c("family_type_race"):=
                 hh_occup_size_dt[.SD, list(family_type_race), 
                                  on = .(hh_size_race_id)]]
    
    #add size to sam_eth/race_hh
    sam_eth_hh[,("hh_size_eth_id"):=
                 paste0(tract,own_rent,family,if_else(family_type=="Householder living alone","A","B"),
                        as.character(1000000+sample(1:.N))),
           by=.(tract,own_rent,family,family_type=="Householder living alone")]
    hh_size_dt[,("hh_size_eth_id"):=
                 paste0(tract,own_rent_eth,family_or_non,if_else(hh_size=="1-person household","A","B"),
                        as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent_eth,family_or_non,hh_size=="1-person household")]
    sam_eth_hh[,c("hh_size"):=
             hh_size_dt[.SD, list(hh_size), 
                        on = .(hh_size_eth_id)]]
    hh_size_dt[,c("miss_eth_hh_size"):=
                 sam_eth_hh[.SD, list(hh_size), 
                            on = .(hh_size_eth_id)]]
    sam_race_hh[,("hh_size_race_id"):=
                  paste0(tract,own_rent,family,if_else(family_type=="Householder living alone","A","B"),
                         as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,family,family_type=="Householder living alone")]
    hh_size_dt[,("hh_size_race_id"):=
                 paste0(tract,own_rent_race,family_or_non,if_else(hh_size=="1-person household","A","B"),
                        as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent_race,family_or_non,hh_size=="1-person household")]
    sam_race_hh[,c("hh_size"):=
                 hh_size_dt[.SD, list(hh_size), 
                            on = .(hh_size_race_id)]]
    hh_size_dt[,c("miss_race_hh_size"):=
                 sam_race_hh[.SD, list(hh_size), 
                            on = .(hh_size_race_id)]]
    #pickup stragglers by recasting own_rent, then picking up again
    #because there are only two possibilities, don't need to go through whole reshuffle on casting and can just assign [tables match on missing and are evenly distributed]
    hh_size_dt[is.na(miss_eth_hh_size),
               ("own_rent_eth"):=if_else(own_rent_eth=="Owner occupied","Renter occupied","Owner occupied")]
    hh_size_dt[is.na(miss_race_hh_size),
               ("own_rent_race"):=if_else(own_rent_race=="Owner occupied","Renter occupied","Owner occupied")]
    sam_eth_hh[is.na(hh_size),("hh_size_eth2_id"):=
                 paste0(tract,own_rent,family,if_else(family_type=="Householder living alone","A","B"),
                        as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,family,family_type=="Householder living alone")]
    hh_size_dt[is.na(miss_eth_hh_size),("hh_size_eth2_id"):=
                 paste0(tract,own_rent_eth,family_or_non,if_else(hh_size=="1-person household","A","B"),
                        as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent_eth,family_or_non,hh_size=="1-person household")]
    sam_eth_hh[is.na(hh_size),c("hh_size"):=
                 hh_size_dt[.SD, list(hh_size), 
                            on = .(hh_size_eth2_id)]]
    hh_size_dt[is.na(miss_eth_hh_size),c("miss_eth_hh_size"):=
                 sam_eth_hh[.SD, list(hh_size), 
                            on = .(hh_size_eth2_id)]]
    sam_race_hh[is.na(hh_size),("hh_size_race2_id"):=
                  paste0(tract,own_rent,family,if_else(family_type=="Householder living alone","A","B"),
                         as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,family,family_type=="Householder living alone")]
    hh_size_dt[is.na(miss_race_hh_size),("hh_size_race2_id"):=
                 paste0(tract,own_rent_race,family_or_non,if_else(hh_size=="1-person household","A","B"),
                        as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent_race,family_or_non,hh_size=="1-person household")]
    sam_race_hh[is.na(hh_size),c("hh_size"):=
                  hh_size_dt[.SD, list(hh_size), 
                             on = .(hh_size_race2_id)]]
    hh_size_dt[is.na(miss_race_hh_size),c("miss_race_hh_size"):=
                 sam_race_hh[.SD, list(hh_size), 
                             on = .(hh_size_race2_id)]]
    
    sam_eth_hh[,("hh_size_eth_id"):=NULL]
    sam_race_hh[,("hh_size_race_id"):=NULL]
    #because it's just the own_rent backwards on the ones that are left
    
    #test hh8
    #test<-table(
    #  hh_size_dt$tract,
    #  hh_size_dt$own_rent_eth,
    #  hh_size_dt$family_or_non,
    #  hh_size_dt$hh_size
    #  )==table(
    #    sam_eth_hh$tract,
    #    sam_eth_hh$own_rent,
    #    sam_eth_hh$family,
    #    sam_eth_hh$hh_size
    #    )
    #length(test[test==F])==0
    #test<-table(
    #  hh_size_dt$tract,
    #  hh_size_dt$own_rent_race,
    #  hh_size_dt$family_or_non,
    #  hh_size_dt$hh_size
    #  )==table(
    #    sam_race_hh$tract,
    #    sam_race_hh$own_rent,
    #    sam_race_hh$family,
    #    sam_race_hh$hh_size
    #    )
    #length(test[test==F])==0
    
#do rooms and bedrooms by own_rent and hh_size
    sam_eth_hh[,("calc_rooms") := as.numeric(substr(hh_size,1,1))*as.numeric(substr(people_per_room,1,3))]
    sam_eth_hh[order(calc_rooms),("rooms_id") := paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
           by=.(tract,own_rent)]
    sam_race_hh[,("calc_rooms") := as.numeric(substr(hh_size,1,1))*as.numeric(substr(people_per_room,1,3))]
    sam_race_hh[order(calc_rooms),("rooms_id") := paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent)]
    hh_occup_rooms_dt[order(substr(num_rooms,1,1)),
                      ("rooms_id") := paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                      by=.(tract,own_rent)]
    sam_eth_hh[,c("num_rooms"):=
             hh_occup_rooms_dt[.SD, list(num_rooms),
                               on = .(rooms_id)]]
    sam_race_hh[,c("num_rooms"):=
                 hh_occup_rooms_dt[.SD, list(num_rooms),
                                   on = .(rooms_id)]]
    
    hh_occup_bedrooms_dt[order(substr(num_bedrooms,1,1)),
                         ("rooms_id") := paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                         by=.(tract,own_rent)]
    sam_eth_hh[,c("num_bedrooms"):=
             hh_occup_bedrooms_dt[.SD, list(num_bedrooms),
                               on = .(rooms_id)]]
    sam_race_hh[,c("num_bedrooms"):=
                 hh_occup_bedrooms_dt[.SD, list(num_bedrooms),
                                      on = .(rooms_id)]]
    
    sam_eth_hh[,("rooms_id"):=NULL]
    sam_eth_hh[,("calc_rooms"):=NULL]
    sam_race_hh[,("rooms_id"):=NULL]
    sam_race_hh[,("calc_rooms"):=NULL]
    
    #test hh9
    #test<-table(
    #  hh_occup_bedrooms_dt$tract,
    #  hh_occup_bedrooms_dt$own_rent,
    #  hh_occup_bedrooms_dt$num_bedrooms
    #  )==table(
    #    sam_eth_hh$tract,
    #    sam_eth_hh$own_rent,
    #    sam_eth_hh$num_bedrooms
    #    )
    #length(test[test==F])==0
    #test<-table(
    #  hh_occup_bedrooms_dt$tract,
    #  hh_occup_bedrooms_dt$own_rent,
    #  hh_occup_bedrooms_dt$num_bedrooms
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$own_rent,
    #  sam_race_hh$num_bedrooms
    #)
    #length(test[test==F])==0
    #test<-table(
    #  hh_occup_rooms_dt$tract,
    #  hh_occup_rooms_dt$own_rent,
    #  hh_occup_rooms_dt$num_rooms
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$own_rent,
    #  sam_eth_hh$num_rooms
    #)
    #length(test[test==F])==0
    #test<-table(
    #  hh_occup_rooms_dt$tract,
    #  hh_occup_rooms_dt$own_rent,
    #  hh_occup_rooms_dt$num_rooms
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$own_rent,
    #  sam_race_hh$num_rooms
    #)
    #length(test[test==F])==0
    
    #put sex on sam_eth/race_hh for matching for ones where we know (no husband present, no wife present for in_family
    sam_eth_hh[str_detect(family_role,"Female"),("sex"):="Female"]
    sam_eth_hh[str_detect(family_role,"Male"),("sex"):="Male"]
    #have to try on this a couple of ways, but I think it follows how they do it
    sam_eth_hh[is.na(sex) & family_type=="Married-couple family",("sex"):="Male"]
    sam_race_hh[str_detect(family_role,"Female"),("sex"):="Female"]
    sam_race_hh[str_detect(family_role,"Male"),("sex"):="Male"]
    sam_race_hh[is.na(sex) & family_type=="Married-couple family",("sex"):="Male"]
#assign rest of sex from individuals side, as opposed to just giving them a sex
#    sam_hh[family_role=="Householder living alone",("sex"):=sample(c("Male","Female"),.N,replace = TRUE)]
    #test hh10
    #test <- table(
    #  sam_race_hh$tract,
    #  sam_race_hh$sex
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$sex
    #)
    #length(test[test==F])==0
    
    #give all households an id
    sam_eth_hh[,("household_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    #length(unique(sam_eth_hh$household_id))==nrow(sam_eth_hh)
    sam_race_hh[,("household_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    #length(unique(sam_race_hh$household_id))==nrow(sam_race_hh)
    
    #create a workers profile that we can match against
    #this has been a major pain, since it's hard to see how to get to the right size from expanding hh_workers
    #hh_workers[number_workers_in_hh=="3 workers",c("number_workers_in_hh"):="3 or more workers"] #I think they made a mistake, although it could be that the other should be 4 or more workers?
    hh_workers[,("hh_workers_id"):=paste0(tract,as.character(2000000+sample(1:.N))),by=.(tract)]
    hh_workers[number_workers_in_hh=="3 workers",c("number_workers_in_hh"):="4 workers"] #I think they made a mistake, although it could be that the other should be 4 or more workers?
    hh_workers[number_workers_in_hh=="No workers",("number_workers_in_hh"):="0 workers"]
    #get hh_workers to have right hh_size and then upping number of workers
    hh_workers[,("size_work_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                         by=.(tract,hh_size_4)]
    hh_size_dt[,("size_work_id") := paste0(tract,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,hh_size)]
    hh_workers[,c("hh_size_7"):=
                 hh_size_dt[.SD, list(hh_size),
                                      on = .(size_work_id)]]
    
    #only ones left are "4-person or more"
    hh_workers[is.na(hh_size_7),("size_work2_id") := paste0(tract,as.character(1000000+sample(1:.N))),
               by=.(tract)]
    hh_size_dt[as.numeric(substr(hh_size,1,1))>3,("size_work2_id") := paste0(tract,as.character(1000000+sample(1:.N))),
               by=.(tract)]
    hh_workers[is.na(hh_size_7),c("hh_size_7"):=
                 hh_size_dt[.SD, list(hh_size),
                            on = .(size_work2_id)]]
    #test hh10a
    #test <- table(
    #  hh_workers$tract,
    #  hh_workers$hh_size_7
    #)==table(
    #  hh_size_dt$tract,
    #  hh_size_dt$hh_size
    #)
    #length(test[test==F])==0
    #trying to get it large enough to fill the tracts, and then cut off the ones with large number of workers by
    hh_workers[,("hh_size_10"):=if_else(hh_size_7=="7-or-more person household",
                                        sample(c("7-person household","8-person household","9-person household","10-person household"),
                                               .N,prob = c(.4,.3,.2,.1),replace = TRUE),
                                        hh_size_7)]
    hh_workers[hh_size_10!="10-person household",("hh_size_10"):=paste0("0",hh_size_10)]
    hh_workers[hh_size_10=="09-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=sample(c("3 workers","4 workers", "5 workers","6 workers",
                                                  "7 workers","8 workers","9 workers","10 workers"),.N,
                                                prob = c(.01,.01,.01,.1,.1,.1,.1,.27),replace = TRUE)]
    hh_workers[hh_size_10=="09-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=sample(c("3 workers","4 workers", "5 workers","6 workers",
                                                  "7 workers","8 workers","9 workers"),.N,
                                                prob = c(.01,.01,.01,.1,.1,.1,.37),replace = TRUE)]
    hh_workers[hh_size_10=="08-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=sample(c("3 workers","4 workers", "5 workers","6 workers","7 workers","8 workers"),.N,
                                                prob = c(.01,.01,.01,.1,.1,.47),replace = TRUE)]
    hh_workers[hh_size_10=="07-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=sample(c("3 workers","4 workers", "5 workers","6 workers","7 workers"),.N,
                                                prob = c(.01,.01,.01,.1,.57),replace = TRUE)]
    hh_workers[hh_size_10=="06-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=sample(c("3 workers","4 workers", "5 workers","6 workers"),.N,
                                                prob = c(.01,.01,.01,.77),replace = TRUE)]
    hh_workers[hh_size_10=="05-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=sample(c("3 workers","4 workers", "5 workers"),.N,
                                                prob = c(.1,.1,.8),replace = TRUE)]
    hh_workers[hh_size_10=="04-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=sample(c("3 workers", "4 workers"),.N,
                                                prob = c(.1,.9),replace = TRUE)]
    hh_workers[hh_size_10=="03-person household"&number_workers_in_hh=="3 or more workers",
               ("number_workers_in_hh"):=list("3 workers")]
    hh_workers[number_workers_in_hh!="10 workers",("number_workers_in_hh"):=paste0("0",number_workers_in_hh)]
    #remember you lose households with 0 workers and (I'm guessing) adults who work from home! target is 2061700
    workers_exp <- hh_workers[,uncount(.SD,as.numeric(substr(number_workers_in_hh,1,2)),.remove = FALSE,.id = "worker_")]
    workers_exp$number_sams <- NULL #trying to fix shallow copy warning
    
    transport_age_dt[,("age_work_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                     by=.(tract)]
    workers_exp[order(hh_size_7),("age_work_id") := paste0(tract,as.character(1000000+seq.int(1:.N))),
                by=.(tract)]
    transport_age_dt[,c("hh_size_10","hh_size","number_workers_in_hh","hh_workers_id"):=
                       workers_exp[.SD, c(list(hh_size_10),list(hh_size_7),list(number_workers_in_hh),list(hh_workers_id)),
                                   on = .(age_work_id)]]
    workers_exp[,("missed"):=transport_age_dt[.SD,list(hh_size),on = .(age_work_id)]]
    
    #maybe in wrong tracts for ones that are larger? 
    transport_age_dt[is.na(hh_size),
                     ("age_work2_id") := paste0(as.character(1000000+sample(1:.N)))]
    workers_exp[is.na(missed),
                ("age_work2_id") := paste0(as.character(1000000+seq.int(1:.N)))]
    transport_age_dt[is.na(hh_size),
                     c("hh_size_10","hh_size","number_workers_in_hh","hh_workers_id"):=
                       workers_exp[.SD, c(list(hh_size_10),list(hh_size_7),list(number_workers_in_hh),list(hh_workers_id)),
                                   on = .(age_work2_id)]]
    
    #create with workers, then select back to sam_eth/race, having chosen randomly from workers exp by hh_worker_id
    #then do expansion from within sam_eth/race
    transport_sex_dt[,("sex_age_work_id") := paste0(tract,means_transport_5,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport_5)]
    transport_age_dt[,("sex_age_work_id") := paste0(tract,means_transport_5,as.character(1000000+sample(1:.N))),
                by=.(tract,means_transport_5)]
    transport_age_dt[,c("sex","means_transport_6a"):=
                       transport_sex_dt[.SD, c(list(sex),list(means_transport)),
                                   on = .(sex_age_work_id)]]
    #test hh8a
    #test<-table(
    #  transport_age_dt$tract,
    #  transport_age_dt$means_transport_5,
    #  transport_age_dt$means_transport_6a,
    #  transport_age_dt$sex
    #)==table(
    #  transport_sex_dt$tract,
    #  transport_sex_dt$means_transport_5,
    #  transport_sex_dt$means_transport,
    #  transport_sex_dt$sex
    #)
    #length(test[test==F])==0
    #industry and occupation - not using transport_occupation, because can't get a match with industry that's very good
    transport_industry_dt[,("ind_age_work_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport)]
    transport_age_dt[,("ind_age_work_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport)]
    transport_age_dt[,c("industry","occupation"):=
                       transport_industry_dt[.SD, c(list(industry),list(occupation_match)),
                                        on = .(ind_age_work_id)]]
    #test hh8b
    #test<-table(
    #  transport_age_dt$tract,
    #  transport_age_dt$means_transport,
    #  transport_age_dt$industry,
    #  transport_age_dt$occupation
    #)==table(
    #  transport_industry_dt$tract,
    #  transport_industry_dt$means_transport,
    #  transport_industry_dt$industry,
    #  transport_industry_dt$occupation_match
    #)
    #length(test[test==F])==0
    #add time and when to work #people who worked at home were given no time, which is why it's smaller nrow
    transport_time_work_dt[time_to_work=="Less than 10 minutes",("time_to_work"):=list("0 to 10 minutes")]
    time_to_work_sex_dt[time_to_work=="Less than 5 minutes",("time_to_work"):=list("0 to 5 minutes")]
    time_to_work_sex_dt[time_to_work=="5 to 9 minutes",("time_to_work"):=list("05 to 9 minutes")]
    time_to_work_sex_dt[order(time_to_work),("time_work_id") := paste0(tract,as.character(1000000+seq.int(1:.N))),
                        by=.(tract)]
    transport_time_work_dt[order(time_to_work),("time_work_id") := paste0(tract,as.character(1000000+seq.int(1:.N))),
                     by=.(tract)]
    transport_time_work_dt[,c("sex","commute_time"):=
                             time_to_work_sex_dt[.SD, c(list(sex),list(time_to_work)),
                                        on = .(time_work_id)]]
    #test hh8c
    #test<-table(
    #  transport_time_work_dt$tract,
    #  transport_time_work_dt$sex,
    #  transport_time_work_dt$commute_time
    #)==table(
    #  time_to_work_sex_dt$tract,
    #  time_to_work_sex_dt$sex,
    #  time_to_work_sex_dt$time_to_work
    #)
    #length(test[test==F])==0
    
    when_go_work_sex_dt[,("sex_work_id") := paste0(tract,sex,as.character(1000000+sample(1:.N))),
                        by=.(tract,sex)]
    transport_time_work_dt[,("sex_work_id") := paste0(tract,sex,as.character(1000000+sample(1:.N))),
                           by=.(tract,sex)]
    transport_time_work_dt[,c("when_go_to_work"):=
                             when_go_work_sex_dt[.SD, list(when_go_to_work),
                                                 on = .(sex_work_id)]]
    #test hh8d
    #test<-table(
    #  transport_time_work_dt$tract,
    #  transport_time_work_dt$sex,
    #  transport_time_work_dt$when_go_to_work
    #)==table(
    #  when_go_work_sex_dt$tract,
    #  when_go_work_sex_dt$sex,
    #  when_go_work_sex_dt$when_go_to_work
    #)
    #length(test[test==F])==0
    
    transport_time_work_dt[,("sex_time_work_id") := paste0(tract,sex,means_transport_4,as.character(1000000+sample(1:.N))),
                          by=.(tract,sex,means_transport_4)]
    transport_age_dt[means_transport_5!="Worked at home",("sex_time_work_id") := paste0(tract,sex,means_transport_5,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,means_transport_5)]
    transport_age_dt[means_transport_5!="Worked at home",c("commute_time","when_go_to_work"):=
                       transport_time_work_dt[.SD, c(list(commute_time),list(when_go_to_work)),
                                             on = .(sex_time_work_id)]]
    transport_time_work_dt[,c("missing"):=
                             transport_age_dt[.SD, list(commute_time),
                                              on = .(sex_time_work_id)]]
    #second time, letting sex from transport_time trump on subset
    transport_time_work_dt[is.na(missing),
                           ("sex_time_work2_id") := paste0(tract,means_transport_4,as.character(1000000+sample(1:.N))),
                           by=.(tract,means_transport_4)]
    transport_age_dt[is.na(commute_time)&means_transport_5!="Worked at home",
                     ("sex_time_work2_id") := paste0(tract,means_transport_5,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport_5)]
    transport_age_dt[is.na(commute_time)&means_transport_5!="Worked at home",
                     c("sex","commute_time","when_go_to_work"):=
                       transport_time_work_dt[.SD, c(list(sex),list(commute_time),list(when_go_to_work)),
                                              on = .(sex_time_work2_id)]]
    #test hh8e
    #test<-table(
    #  transport_time_work_dt$tract,
    #  transport_time_work_dt$sex,
    #  transport_time_work_dt$commute_time,
    #  transport_time_work_dt$when_go_to_work
    #)==table(
    #  transport_age_dt[means_transport_5!="Worked at home"]$tract,
    #  transport_age_dt[means_transport_5!="Worked at home"]$sex,
    #  transport_age_dt[means_transport_5!="Worked at home"]$commute_time,
    #  transport_age_dt[means_transport_5!="Worked at home"]$when_go_to_work
    #)
    #length(test[test==F])==0
    transport_age_dt[is.na(commute_time),("commute_time"):="0 minutes (works at home)"] #m before t, so still first in sort vs. "0 to 5"
    
    #add language
    transport_language_dt[,("language_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                           by=.(tract,means_transport)]
    transport_age_dt[,("language_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport)]
    transport_age_dt[,c("language","English_level"):=
                       transport_language_dt[.SD, c(list(language),list(English_level)),
                                              on = .(language_id)]]
    #test hh8f
    #test<-table(
    #  transport_language_dt$tract,
    #  transport_language_dt$language,
    #  transport_language_dt$English_level
    #)==table(
    #  transport_age_dt$tract,
    #  transport_age_dt$language,
    #  transport_age_dt$English_level
    #)
    #length(test[test==F])==0
    
    #income and tenure don't match others exactly, so match and then prefer existing on transport_age
    transport_tenure_dt[,("own_rent"):=if_else(str_detect(owner_renter,"renter"),"Renter occupied","Owner occupied")]
    transport_tenure_dt[,("tenure_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                          by=.(tract,means_transport)]
    transport_age_dt[,("tenure_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport)]
    transport_age_dt[,c("own_rent"):=
                       transport_tenure_dt[.SD, list(own_rent),
                                             on = .(tenure_id)]]
    #test that they all moved over nrow(transport_tenure_dt[is.na(missing)])==0
    #transport_tenure_dt[,c("missing"):=
    #                      transport_age_dt[.SD, list(own_rent),
    #                                       on = .(tenure_id)]]
    transport_income_dt[,("income_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                        by=.(tract,means_transport)]
    transport_age_dt[,("income_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport)]
    transport_age_dt[,c("income_range"):=
                       transport_income_dt[.SD, list(income_range),
                                           on = .(income_id)]]
    #test that they all moved over nrow(transport_income_dt[is.na(missing)])==0
    #transport_income_dt[,c("missing"):=
    #                      transport_age_dt[.SD, list(income_range),
    #                                       on = .(income_id)]]
    
    #add race and eth....
    transport_race_dt[,("race_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                        by=.(tract,means_transport)]
    transport_eth_dt[,("race_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                      by=.(tract,means_transport)]
    transport_age_dt[,("race_id") := paste0(tract,means_transport,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport)]
    transport_age_dt[,c("race"):=
                       transport_race_dt[.SD, list(race),
                                           on = .(race_id)]]
    transport_age_dt[,c("ethnicity"):=
                       transport_eth_dt[.SD, list(ethnicity),
                                         on = .(race_id)]]
    #test hh8h
    #test<-table(
    #  transport_eth_dt$tract,
    #  transport_eth_dt$ethnicity
    #)==table(
    #  transport_age_dt$tract,
    #  transport_age_dt$ethnicity
    #)
    #length(test[test==F])==0
    #test<-table(
    #  transport_race_dt$tract,
    #  transport_race_dt$means_transport,
    #  transport_race_dt$race
    #)==table(
    #  transport_age_dt$tract,
    #  transport_age_dt$means_transport,
    #  transport_age_dt$race
    #)
    #length(test[test==F])==0
    
    
    #get age_range in shape to match
    transport_age_dt[,("age_range_6"):=case_when(
      age_range_7=="16 to 19 years" | age_range_7=="20 to 24 years" ~ "15 to 24 years",
      TRUE ~ age_range_7
    )]
    sam_eth_hh[,("age_range_6"):=case_when(
      householder_age_9=="Householder 25 to 34 years" | householder_age_9=="Householder 35 to 44 years" ~ "25 to 44 years",
      householder_age_9=="Householder 65 to 74 years" | householder_age_9=="Householder 75 to 84 years" |
        householder_age_9=="Householder 85 years and over" ~ "65 years and over",
      TRUE ~ str_remove(householder_age_9,"Householder ")
    )]
    sam_race_hh[,("age_range_6"):=case_when(
      householder_age_9=="Householder 25 to 34 years" | householder_age_9=="Householder 35 to 44 years" ~ "25 to 44 years",
      householder_age_9=="Householder 65 to 74 years" | householder_age_9=="Householder 75 to 84 years" |
        householder_age_9=="Householder 85 years and over" ~ "65 years and over",
      TRUE ~ str_remove(householder_age_9,"Householder ")
    )]
    #have to redo match on income, sex, race and eth, etc. without hh later, but same totals for workers, even if juggle by hh

#separate hh from non-hh workers add_workers_hh will be used later
    #get back to only householders who are in labor force
    transport_age_dt[,("track_hh_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    # number in labor force
    #transport_hh <- transport_age_dt[,.SD[1],by = .(hh_workers_id)] # number in labor force, but about 4k short of correct total...
    transport_age_dt[order(-age_range_6),("potential_hh"):=as.character(1:.N),by=.(hh_workers_id)]
    transport_hh <- transport_age_dt[potential_hh=="1"]
#do this after rematching, below
add_workers_hh <- anti_join(transport_age_dt,transport_hh,by="track_hh_id")
    #there are <1k too many if you assign no workers to 272910 hh
    sam_race_hh[,("rejoin_race_id") := paste0(tract,sex,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                      by=.(tract,sex,own_rent,age_range_6,race,hh_size)]
    sam_eth_hh[,("rejoin_eth_id") := paste0(tract,sex,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_6,ethnicity,hh_size)]
    transport_hh[,("rejoin_race_id") := paste0(tract,sex,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_6,race,hh_size)]
    transport_hh[,("rejoin_eth_id") := paste0(tract,sex,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_6,ethnicity,hh_size)]
    sam_race_hh[,c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                   "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                          list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                          list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                         on = .(rejoin_race_id)]]
    sam_eth_hh[,c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                         list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                         list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                        on = .(rejoin_eth_id)]]
    transport_hh[,("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth_id)]]
    transport_hh[,("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race_id)]]
    #get a little more than a third matching
    #since own_rent is binary, just take it out, then don't write over, because sam has more tied to it now
    sam_race_hh[is.na(means_transport),("rejoin_race2_id") := paste0(tract,sex,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,age_range_6,race,hh_size)]
    sam_eth_hh[is.na(means_transport),("rejoin_eth2_id") := paste0(tract,sex,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,age_range_6,ethnicity,hh_size)]
    transport_hh[is.na(missing_race),("rejoin_race2_id") := paste0(tract,sex,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_6,race,hh_size)]
    transport_hh[is.na(missing_eth),("rejoin_eth2_id") := paste0(tract,sex,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_6,ethnicity,hh_size)]
    sam_race_hh[is.na(means_transport),
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                   "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race2_id)]]
    sam_eth_hh[is.na(means_transport),
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                     list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                     list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                              on = .(rejoin_eth2_id)]]
    transport_hh[is.na(missing_eth),
                 ("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth2_id)]]
    transport_hh[is.na(missing_race),
                 ("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race2_id)]]
    #about two-thirds matched - reshuffle, keeping same totals on transport side and just sampling from other members of hhs
    transport_age_eth_2 <- as.data.table(anti_join(transport_age_dt,transport_hh[!is.na(missing_eth)],by="track_hh_id"))  
    transport_age_race_2 <- as.data.table(anti_join(transport_age_dt,transport_hh[!is.na(missing_race)],by="track_hh_id"))
    transport_hh[is.na(missing_eth),("rejoin_eth2b1_id") := paste0(tract,sex,age_range_6,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_6)]
    transport_age_eth_2[,("rejoin_eth2b1_id") := paste0(tract,sex,age_range_6,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_6)]
    transport_hh[is.na(missing_eth),
                 c("ethnicity","track_hh_id"):=transport_age_eth_2[.SD,c(list(ethnicity),list(track_hh_id)),on=.(rejoin_eth2b1_id)]]
    transport_hh[is.na(missing_race),("rejoin_race2b1_id") := paste0(tract,sex,own_rent,age_range_6,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_6,hh_size)]
    transport_age_race_2[,("rejoin_race2b1_id") := paste0(tract,sex,own_rent,age_range_6,hh_size,as.character(1000000+sample(1:.N))),
                        by=.(tract,sex,own_rent,age_range_6,hh_size)]
    transport_hh[is.na(missing_race),
                 c("race","track_hh_id"):=transport_age_race_2[.SD,c(list(race),list(track_hh_id)),on=.(rejoin_race2b1_id)]]
    #then try again, with new race/eth having just been sampled (not actually moved over yet)
    sam_race_hh[is.na(means_transport),("rejoin_race2c1_id") := paste0(tract,sex,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_6,race,hh_size)]
    sam_eth_hh[is.na(means_transport),("rejoin_eth2c1_id") := paste0(tract,sex,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_6,ethnicity,hh_size)]
    transport_hh[is.na(missing_race),("rejoin_race2c1_id") := paste0(tract,sex,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_6,race,hh_size)]
    transport_hh[is.na(missing_eth),("rejoin_eth2c1_id") := paste0(tract,sex,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_6,ethnicity,hh_size)]
    sam_race_hh[is.na(means_transport),
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race2c1_id)]]
    sam_eth_hh[is.na(means_transport),
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                     list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                     list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                              on = .(rejoin_eth2c1_id)]]
    transport_hh[is.na(missing_eth),
                 ("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth2c1_id)]]
    transport_hh[is.na(missing_race),
                 ("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race2c1_id)]]
    
    #get age_range_6=="65 years and over" - just match and finish off, since retired is most important and not that many left
    sam_race_hh[is.na(means_transport)&age_range_6=="65 years and over",
                ("rejoin_race2a_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                by=.(tract)]
    sam_eth_hh[is.na(means_transport)&age_range_6=="65 years and over",
               ("rejoin_eth2a_id") := paste0(tract,as.character(1000000+sample(1:.N))),
               by=.(tract)]
    transport_hh[is.na(missing_race)&age_range_6=="65 years and over",
                 ("rejoin_race2a_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                 by=.(tract)]
    transport_hh[is.na(missing_eth)&age_range_6=="65 years and over",
                 ("rejoin_eth2a_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                 by=.(tract)]
    sam_race_hh[is.na(means_transport)&age_range_6=="65 years and over",
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race2a_id)]]
    sam_eth_hh[is.na(means_transport)&age_range_6=="65 years and over",
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                     list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                     list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                              on = .(rejoin_eth2a_id)]]
    transport_hh[is.na(missing_eth)&age_range_6=="65 years and over",
                 ("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth2a_id)]]
    transport_hh[is.na(missing_race)&age_range_6=="65 years and over",
                 ("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race2a_id)]]
    
    #without age or hh_size, but moving it over to keep straight at end of expanding
    sam_race_hh[is.na(means_transport),("rejoin_race3_id") := paste0(tract,sex,race,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,race)]
    sam_eth_hh[is.na(means_transport),("rejoin_eth3_id") := paste0(tract,sex,ethnicity,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,ethnicity)]
    transport_hh[is.na(missing_race),("rejoin_race3_id") := paste0(tract,sex,race,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,race)]
    transport_hh[is.na(missing_eth),("rejoin_eth3_id") := paste0(tract,sex,ethnicity,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,ethnicity)]
    sam_race_hh[is.na(means_transport),
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race3_id)]]
    sam_eth_hh[is.na(means_transport),
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                     list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                     list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                              on = .(rejoin_eth3_id)]]
    transport_hh[is.na(missing_eth),
                 ("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth3_id)]]
    transport_hh[is.na(missing_race),
                 ("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race3_id)]]
    #Not really sure what's going on, but don't have time to fix it all now
    #just finish moving over
    sam_race_hh[is.na(means_transport),("rejoin_race4_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                by=.(tract)]
    sam_eth_hh[is.na(means_transport),("rejoin_eth4_id") := paste0(tract,as.character(1000000+sample(1:.N))),
               by=.(tract)]
    transport_hh[is.na(missing_race),("rejoin_race4_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                 by=.(tract)]
    transport_hh[is.na(missing_eth),("rejoin_eth4_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                 by=.(tract)]
    sam_race_hh[is.na(means_transport),
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race4_id)]]
    sam_eth_hh[is.na(means_transport),
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                     list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                     list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                              on = .(rejoin_eth4_id)]]
    transport_hh[is.na(missing_eth),
                 ("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth4_id)]]
    transport_hh[is.na(missing_race),
                 ("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race4_id)]]
    sam_eth_hh[is.na(number_workers_in_hh),("number_workers_in_hh"):="00 workers"]
    sam_race_hh[is.na(number_workers_in_hh),("number_workers_in_hh"):="00 workers"]
    sam_eth_hh[number_workers_in_hh=="03 or more workers",("number_workers_in_hh"):="03 workers"]
    sam_race_hh[number_workers_in_hh=="03 or more workers",("number_workers_in_hh"):="03 workers"]
    
    #clean up things for non-householders on the additional_workers - stop matching on household_id, too; can regenerate from sam_hh
    #or just move over from wives/partners and then from other_relatives, etc.?
    
    #vehicles_hh and vehicles_workers added after back to sam_eth/race_hh
    vehicles_hh[,("veh_id") := paste0(tract,number_vehicles_in_hh,as.character(1000000+sample(1:.N))),
                by=.(tract,number_vehicles_in_hh)]
    vehicles_workers_dt[,("veh_id") := paste0(tract,number_vehicles_in_hh,as.character(1000000+sample(1:.N))),
                by=.(tract,number_vehicles_in_hh)]
    vehicles_hh[,("number_workers_in_hh"):=vehicles_workers_dt[.SD,list(number_workers_in_hh),on=.(veh_id)]]
    #test hh11h
    #test<-table(
    #  vehicles_hh$tract,
    #  vehicles_hh$number_workers_in_hh
    #)==table(
    #  vehicles_workers_dt$tract,
    #  vehicles_workers_dt$number_workers_in_hh
    #)
    #length(test[test==F])==0
    
    #put vehicles on sam_eth/race
    sam_eth_hh[,("hh_size_4"):=if_else(as.numeric(substr(hh_size,1,1))>3,"4-or-more-person household",hh_size)]
    sam_eth_hh[,("match_workers"):=case_when( #close, but not exact match
      as.numeric(substr(number_workers_in_hh,1,2))==0 ~ "No workers",
      as.numeric(substr(number_workers_in_hh,1,2))==1 ~ "1 worker",
      as.numeric(substr(number_workers_in_hh,1,2))==2 ~ "2 workers",
      TRUE ~ "3 or more workers"
    )]
    sam_race_hh[,("match_workers"):=case_when( #close, but not exact match
      as.numeric(substr(number_workers_in_hh,1,2))==0 ~ "No workers",
      as.numeric(substr(number_workers_in_hh,1,2))==1 ~ "1 worker",
      as.numeric(substr(number_workers_in_hh,1,2))==2 ~ "2 workers",
      TRUE ~ "3 or more workers"
    )]
    sam_race_hh[,("hh_size_4"):=if_else(as.numeric(substr(hh_size,1,1))>3,"4-or-more-person household",hh_size)]
    vehicles_hh[,("vehicle_id") := paste0(tract,hh_size_4,number_workers_in_hh,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4,number_workers_in_hh)]
    sam_eth_hh[,("vehicle_id") := paste0(tract,hh_size_4,match_workers,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4,match_workers)]
    sam_race_hh[,("vehicle_id") := paste0(tract,hh_size_4,match_workers,as.character(1000000+sample(1:.N))),
               by=.(tract,hh_size_4,match_workers)]
    sam_eth_hh[,("number_vehicles_hh"):=vehicles_hh[.SD,list(number_vehicles_in_hh),on=.(vehicle_id)]]
    sam_race_hh[,("number_vehicles_hh"):=vehicles_hh[.SD,list(number_vehicles_in_hh),on=.(vehicle_id)]]
    vehicles_hh[,("missing_eth"):=sam_eth_hh[.SD,list(number_vehicles_hh),on=.(vehicle_id)]]
    vehicles_hh[,("missing_race"):=sam_race_hh[.SD,list(number_vehicles_hh),on=.(vehicle_id)]]
    
    #rest just on hh_size_4
    vehicles_hh[is.na(missing_eth),("vehicle2_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4)]
    sam_eth_hh[is.na(number_vehicles_hh),("vehicle2_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
               by=.(tract,hh_size_4)]
    vehicles_hh[is.na(missing_race),("vehicle2r_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4)]
    sam_race_hh[is.na(number_vehicles_hh),("vehicle2r_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4)]
    sam_eth_hh[is.na(number_vehicles_hh),("number_vehicles_hh"):=vehicles_hh[.SD,list(number_vehicles_in_hh),on=.(vehicle2_id)]]
    sam_race_hh[is.na(number_vehicles_hh),("number_vehicles_hh"):=vehicles_hh[.SD,list(number_vehicles_in_hh),on=.(vehicle2r_id)]]
    #test hh11i
    #test<-table(
    #  vehicles_hh$tract,
    #  vehicles_hh$number_vehicles_in_hh
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$number_vehicles_hh
    #)
    #length(test[test==F])==0
    #test<-table(
    #  vehicles_hh$tract,
    #  vehicles_hh$number_vehicles_in_hh
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$number_vehicles_hh
    #)
    #length(test[test==F])==0
    
    #add related kids age_ranges (can be more than one kid in category, but does give you "no children") - will be trumped by some of individual stuff on each child
    hh_kids[order(family_role,match(kid_age,c("No children","Under 6 years only","6 to 17 years only","Under 6 years and 6 to 17 years"))),
            ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_eth_hh[family=="Family households" & order(family_role,hh_size),
           ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_eth_hh[family=="Family households",c("kids_by_age") := hh_kids[.SD, list(kid_age), on = .(hh_kids_id)]] #may have more than 1 in other categories
    sam_eth_hh$hh_kids_id <- NULL
    sam_race_hh[family=="Family households" & order(family_role,hh_size),
               ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_race_hh[family=="Family households",c("kids_by_age") := hh_kids[.SD, list(kid_age), on = .(hh_kids_id)]] #may have more than 1 in other categories
    sam_race_hh$hh_kids_id <- NULL
    #test hh12
    #test <- table(
    #  sam_race_hh$tract,
    #  sam_race_hh$kids_by_age
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$kids_by_age
    #)
    #length(test[test==F])==0

    #add employment for family - census file is misssing 4384, all married couple families...
    #because the number_of_workers came through the workers by transportation side, it has other info like age
    #implicitly coded
    #THIS ENDED UP BEING A MESS, ONLY PARTIALLY FIXED POST HOC
    family_employment_dt[,("min_workers"):=case_when(
      single_hh_employ!="Not in labor force" ~ 1,
      wife_employed!="Wife not in labor force" & husband_employed!="Husband not in labor force" ~ 2,
      wife_employed!="Wife not in labor force" & husband_employed=="Husband not in labor force" ~ 1,
      wife_employed=="Wife not in labor force" & husband_employed!="Husband not in labor force" ~ 1,
      TRUE ~ 0
    )]
    #with min_workers first
    #could do one at end to pick up non-family and the last 4384 Married-couple families
    family_employment_dt[,("employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[family=="Family households",
           ("own_kids"):=if_else(kids_by_age=="No children",
                                 "No children under 18 years","With own children under 18 years")]
    sam_eth_hh[number_workers_in_hh=="No workers",("number_workers_in_hh"):="0 workers"]
    sam_eth_hh[,("min_workers"):=as.numeric(substr(number_workers_in_hh,1,2))]
    sam_eth_hh[min_workers>2,("min_workers"):=as.numeric(2)]
    sam_eth_hh[family=="Family households",
           ("employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
           by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[family=="Family households",
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
             family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                         list(single_hh_employ),list(name)),
                                  on = .(employ_id)]]
    sam_race_hh[number_workers_in_hh=="No workers",("number_workers_in_hh"):="0 workers"]
    sam_race_hh[,("min_workers"):=as.numeric(substr(number_workers_in_hh,1,2))]
    sam_race_hh[min_workers>2,("min_workers"):=as.numeric(2)]
    sam_race_hh[family=="Family households",
               ("own_kids"):=if_else(kids_by_age=="No children",
                                     "No children under 18 years","With own children under 18 years")]
    sam_race_hh[family=="Family households",
               ("employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_race_hh[family=="Family households",
                c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)), 
                                      on = .(employ_id)]]
    family_employment_dt[,("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ_id)]]
    family_employment_dt[,("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ_id)]]
    #got about 2/3rds in first pass - seem to be evenly spread out among family_role; own_children is binary, so switch that 
    #ideally would generate through back and forth with children by race/eth
    family_employment_dt[is.na(missing_eth),
                         ("employ2_id"):=paste0(tract,min_workers,family_role,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers,family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               ("employ2_id"):=paste0(tract,min_workers,family_role,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ2_id)]]
    family_employment_dt[is.na(missing_eth),
                         ("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ2_id)]]
    family_employment_dt[is.na(missing_race),
                         ("employ3_id"):=paste0(tract,min_workers,family_role,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
               ("employ3_id"):=paste0(tract,min_workers,family_role,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ3_id)]]
    family_employment_dt[is.na(missing_race),
                         ("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ3_id)]]
    #seems not to have gotten very many more at all.
    family_employment_dt[is.na(missing_eth),
                         ("employ4_id"):=paste0(tract,min_workers,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               ("employ4_id"):=paste0(tract,min_workers,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ4_id)]]
    family_employment_dt[is.na(missing_eth),
                         ("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ4_id)]]
    family_employment_dt[is.na(missing_race),
                         ("employ5_id"):=paste0(tract,min_workers,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                ("employ5_id"):=paste0(tract,min_workers,as.character(1000000+sample(1:.N))),
                by=.(tract,min_workers)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                  family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                              list(single_hh_employ),list(name)),
                                       on = .(employ5_id)]]
    family_employment_dt[is.na(missing_race),
                         ("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ5_id)]]
    #think through how this assigns...
    family_employment_dt[is.na(missing_eth),
                         ("employ6_id"):=paste0(tract,family_role,as.character(1000000+sample(1:.N))),
                         by=.(tract,family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               ("employ6_id"):=paste0(tract,family_role,as.character(1000000+sample(1:.N))),
               by=.(tract,family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ6_id)]]
    family_employment_dt[is.na(missing_eth),
                         ("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ6_id)]]
    family_employment_dt[is.na(missing_race),
                         ("employ7_id"):=paste0(tract,family_role,as.character(1000000+sample(1:.N))),
                         by=.(tract,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                ("employ7_id"):=paste0(tract,family_role,as.character(1000000+sample(1:.N))),
                by=.(tract,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                  family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                              list(single_hh_employ),list(name)),
                                       on = .(employ7_id)]]
    family_employment_dt[is.na(missing_race),
                         ("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ7_id)]]
    #still missing ~20k
    family_employment_dt[is.na(missing_eth),
                         ("employ8_id"):=paste0(as.character(1000000+sample(1:.N)))]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               ("employ8_id"):=paste0(as.character(1000000+sample(1:.N)))]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ8_id)]]
    family_employment_dt[is.na(missing_eth),
                         ("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ8_id)]]
    family_employment_dt[is.na(missing_race),
                         ("employ9_id"):=paste0(as.character(1000000+sample(1:.N)))]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                ("employ9_id"):=paste0(as.character(1000000+sample(1:.N)))]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                  family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                              list(single_hh_employ),list(name)),
                                       on = .(employ9_id)]]
    family_employment_dt[is.na(missing_race),
                         ("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ9_id)]]
    #what are the things that determine employment? can I fix them post_hoc??? and 4384 missing from married couples in census file
    sam_eth_hh[family_role=="Married-couple family"&!is.na(single_hh_employed),c("husband_employed"):=single_hh_employed]
    sam_eth_hh[family_role=="Married-couple family"&!is.na(single_hh_employed),c("single_hh_employed"):="NA"]
    sam_race_hh[family_role=="Married-couple family"&!is.na(single_hh_employed),c("husband_employed"):=single_hh_employed]
    sam_race_hh[family_role=="Married-couple family"&!is.na(single_hh_employed),c("single_hh_employed"):="NA"]
    
    sam_eth_hh[family_role=="Female householder no husband present"&!is.na(wife_employed),c("single_hh_employed"):=wife_employed]
    sam_eth_hh[family_role=="Female householder no husband present"&!is.na(wife_employed),c("wife_employed"):="NA"]
    sam_race_hh[family_role=="Female householder no husband present"&!is.na(wife_employed),c("single_hh_employed"):=wife_employed]
    sam_race_hh[family_role=="Female householder no husband present"&!is.na(wife_employed),c("wife_employed"):="NA"]
    
    sam_eth_hh[family_role=="Female householder no husband present"&!is.na(husband_employed),c("single_hh_employed"):=husband_employed]
    sam_eth_hh[family_role=="Female householder no husband present"&!is.na(husband_employed),c("husband_employed"):="NA"]
    sam_race_hh[family_role=="Female householder no husband present"&!is.na(husband_employed),c("single_hh_employed"):=husband_employed]
    sam_race_hh[family_role=="Female householder no husband present"&!is.na(husband_employed),c("husband_employed"):="NA"]
    
    sam_eth_hh[family_role=="Male householder no wife present"&!is.na(wife_employed),c("single_hh_employed"):=wife_employed]
    sam_eth_hh[family_role=="Male householder no wife present"&!is.na(wife_employed),c("wife_employed"):="NA"]
    sam_race_hh[family_role=="Male householder no wife present"&!is.na(wife_employed),c("single_hh_employed"):=wife_employed]
    sam_race_hh[family_role=="Male householder no wife present"&!is.na(wife_employed),c("wife_employed"):="NA"]
    
    sam_eth_hh[family_role=="Male householder no wife present"&!is.na(husband_employed),c("single_hh_employed"):=husband_employed]
    sam_eth_hh[family_role=="Male householder no wife present"&!is.na(husband_employed),c("husband_employed"):="NA"]
    sam_race_hh[family_role=="Male householder no wife present"&!is.na(husband_employed),c("single_hh_employed"):=husband_employed]
    sam_race_hh[family_role=="Male householder no wife present"&!is.na(husband_employed),c("husband_employed"):="NA"]
    
    family_employment_dt[family_role=="Married-couple family",("husband_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[family_role=="Married-couple family"&is.na(husband_employed),
               ("husband_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[family_role=="Married-couple family"&is.na(husband_employed),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(husband_employ_id)]]
    sam_race_hh[family_role=="Married-couple family"&is.na(husband_employed),
               ("husband_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_race_hh[family_role=="Married-couple family"&is.na(husband_employed),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(husband_employ_id)]]
    family_employment_dt[family_role=="Married-couple family",("wife_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[family_role=="Married-couple family"&is.na(wife_employed),
               ("wife_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[family_role=="Married-couple family"&is.na(wife_employed),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(wife_employ_id)]]
    sam_race_hh[family_role=="Married-couple family"&is.na(wife_employed),
               ("wife_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_race_hh[family_role=="Married-couple family"&is.na(wife_employed),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(wife_employ_id)]]
    
    
    #gives unmarried partners, straight and same-sex concept: UNMARRIED-PARTNER HOUSEHOLDS BY SEX OF PARTNER 
    #https://www.census.gov/library/stories/2019/09/unmarried-partners-more-diverse-than-20-years-ago.html - by 2017, close to even across ages / ethnicities, etc.
    hh_partner_dt[partner_type == "Female householder and female partner",("sex_partner") := "Female"]
    hh_partner_dt[partner_type == "Male householder and male partner",("sex_partner") := "Male"]
    hh_partner_dt[partner_type == "Male householder and female partner",("sex_partner") := "Female"]
    hh_partner_dt[partner_type == "Female householder and male partner",("sex_partner") := "Male"]
    hh_partner_dt[partner_type == "Female householder and female partner",("sex") := "Female"]
    hh_partner_dt[partner_type == "Male householder and male partner",("sex") := "Male"]
    hh_partner_dt[partner_type == "Male householder and female partner",("sex") := "Male"]
    hh_partner_dt[partner_type == "Female householder and male partner",("sex") := "Female"]
    hh_partner_dt[unmarried=="Unmarried-partner households",
                  ("partner_type_id"):=paste0(tract,sex,as.character(1000000+sample(1:.N))),by=.(tract,sex)]
    sam_eth_hh[family_type=="Other family",# lose 2627 of 92975 in first pass - not sure where else to put them
               ("partner_type_id"):=paste0(tract,if_else(is.na(sex),"none",sex),as.character(1000000+sample(1:.N))),by=.(tract,sex)]
    sam_eth_hh[family_type=="Other family",
               c("partner_type","sex_partner","hh_sex") := 
                 hh_partner_dt[.SD, c(list(partner_type),list(sex_partner),list(sex)), on = .(partner_type_id)]]
    sam_race_hh[family_type=="Other family",# lose 2627 of 92975 in first pass - not sure where else to put them
                ("partner_type_id"):=paste0(tract,if_else(is.na(sex),"none",sex),as.character(1000000+sample(1:.N))),by=.(tract,sex)]
    sam_race_hh[family_type=="Other family",
                c("partner_type","sex_partner","hh_sex") := 
                  hh_partner_dt[.SD, c(list(partner_type),list(sex_partner),list(sex)), on = .(partner_type_id)]]
    hh_partner_dt[unmarried=="Unmarried-partner households",
                  c("matched_eth") := 
                    sam_eth_hh[.SD, c(list(partner_type)), on = .(partner_type_id)]]
    hh_partner_dt[unmarried=="Unmarried-partner households",
                  c("matched_race") := 
                    sam_race_hh[.SD, c(list(partner_type)), on = .(partner_type_id)]]
    hh_partner_dt[unmarried=="Unmarried-partner households" & is.na(matched_eth),
                  ("second_join_eth_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    hh_partner_dt[unmarried=="Unmarried-partner households" & is.na(matched_race),
                  ("second_join_race_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    sam_eth_hh[family_type=="Householder not living alone" & is.na(partner_type),
               ("second_join_eth_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    sam_eth_hh[family_type=="Householder not living alone" & is.na(partner_type),
               c("partner_type","sex_partner","hh_sex") := 
                 hh_partner_dt[.SD, c(list(partner_type),list(sex_partner),list(sex)), on = .(second_join_eth_id)]]
    sam_eth_hh$partner_type_id <- NULL
    sam_eth_hh$second_join_eth_id <- NULL
    sam_race_hh[family_type=="Householder not living alone" & is.na(partner_type),
                ("second_join_race_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    sam_race_hh[family_type=="Householder not living alone" & is.na(partner_type),
                c("partner_type","sex_partner","hh_sex") := 
                  hh_partner_dt[.SD, c(list(partner_type),list(sex_partner),list(sex)), on = .(second_join_race_id)]]
    sam_race_hh$partner_type_id <- NULL
    sam_race_hh$second_join_race_id <- NULL
    #clean up on the ones that don't have hh_sex and sex
    sam_race_hh[!is.na(hh_sex),("sex"):=hh_sex]
    sam_eth_hh[!is.na(hh_sex),("sex"):=hh_sex]
    #test hh13
    #test<-table(
    #  hh_partner_dt$tract,
    #  hh_partner_dt$partner_type,
    #  hh_partner_dt$sex_partner,
    #  hh_partner_dt$sex
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$partner_type,
    #  sam_race_hh$sex_partner,
    #  sam_race_hh$hh_sex
    #)
    #length(test[test==F])==0
    #test<-table(
    #  hh_partner_dt$tract,
    #  hh_partner_dt$partner_type,
    #  hh_partner_dt$sex_partner,
    #  hh_partner_dt$sex
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$partner_type,
    #  sam_eth_hh$sex_partner,
    #  sam_eth_hh$hh_sex
    #)
    #length(test[test==F])==0
    
    #put in a couple of others before expanding
    sam_eth_hh[,("housing_units_6"):=case_when(
      housing_units=="1 attached" | housing_units=="1 detached" ~ "1 detached or attached",
      housing_units=="2" | housing_units=="3 or 4" ~ "2 to 4",
      housing_units=="5 to 9" | housing_units=="10 to 19" ~ "5 to 19",
      housing_units=="Boat RV van etc." | housing_units=="Mobile home" ~ "Mobile home boat RV van etc.",
      TRUE ~ housing_units
    )]
    sam_race_hh[,("housing_units_6"):=case_when(
      housing_units=="1 attached" | housing_units=="1 detached" ~ "1 detached or attached",
      housing_units=="2" | housing_units=="3 or 4" ~ "2 to 4",
      housing_units=="5 to 9" | housing_units=="10 to 19" ~ "5 to 19",
      housing_units=="Boat RV van etc." | housing_units=="Mobile home" ~ "Mobile home boat RV van etc.",
      TRUE ~ housing_units
    )]
    sam_race_hh[,("rent_whenr_id"):=paste0(tract,own_rent,housing_units_6,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,housing_units_6)]
    sam_eth_hh[,("rent_when_id"):=paste0(tract,own_rent,housing_units_6,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,housing_units_6)]
    tenure_yr_moved_units_hh[,("rent_whenr_id"):=paste0(tract,own_rent,housing_units,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,housing_units)]
    tenure_yr_moved_units_hh[,("rent_when_id"):=paste0(tract,own_rent,housing_units,as.character(1000000+sample(1:.N))),
                             by=.(tract,own_rent,housing_units)]
    sam_race_hh[,c("when_moved_in") := 
                  tenure_yr_moved_units_hh[.SD, list(when_moved), on = .(rent_whenr_id)]]
    sam_eth_hh[,c("when_moved_in") := 
                  tenure_yr_moved_units_hh[.SD, list(when_moved), on = .(rent_when_id)]]
    tenure_yr_moved_units_hh[,c("missed") := 
                               sam_eth_hh[.SD, list(when_moved_in), on = .(rent_when_id)]]
    tenure_yr_moved_units_hh[,c("missedr") := 
                               sam_race_hh[.SD, list(when_moved_in), on = .(rent_whenr_id)]]
    #looks like own_rent is problem
    sam_race_hh[is.na(when_moved_in),
                ("rent_whenr2_id"):=paste0(tract,housing_units_6,as.character(1000000+sample(1:.N))),
                by=.(tract,housing_units_6)]
    sam_eth_hh[is.na(when_moved_in),
               ("rent_when2_id"):=paste0(tract,housing_units_6,as.character(1000000+sample(1:.N))),
               by=.(tract,housing_units_6)]
    tenure_yr_moved_units_hh[is.na(missedr),
                             ("rent_whenr2_id"):=paste0(tract,housing_units,as.character(1000000+sample(1:.N))),
                             by=.(tract,housing_units)]
    tenure_yr_moved_units_hh[is.na(missed),
                             ("rent_when2_id"):=paste0(tract,housing_units,as.character(1000000+sample(1:.N))),
                             by=.(tract,housing_units)]
    sam_race_hh[is.na(when_moved_in),
                c("when_moved_in") := 
                  tenure_yr_moved_units_hh[.SD, list(when_moved), on = .(rent_whenr2_id)]]
    sam_eth_hh[is.na(when_moved_in),
               c("when_moved_in") := 
                 tenure_yr_moved_units_hh[.SD, list(when_moved), on = .(rent_when2_id)]]
    
    #test hh14
    #test<-table(
    #  tenure_yr_moved_units_hh$tract,
    #  #tenure_yr_moved_units_hh$own_rent,
    #  tenure_yr_moved_units_hh$housing_units,
    #  tenure_yr_moved_units_hh$when_moved
    #)==table(
    #  sam_race_hh$tract,
    #  #sam_race_hh$own_rent,
    #  sam_race_hh$housing_units_6,
    #  sam_race_hh$when_moved_in
    #)
    #length(test[test==F])==0
    #test<-table(
    #  tenure_yr_moved_units_hh$tract,
    #  #tenure_yr_moved_units_hh$own_rent,
    #  tenure_yr_moved_units_hh$housing_units,
    #  tenure_yr_moved_units_hh$when_moved
    #)==table(
    #  sam_eth_hh$tract,
    #  #sam_eth_hh$own_rent,
    #  sam_eth_hh$housing_units_6,
    #  sam_eth_hh$when_moved_in
    #)
    #length(test[test==F])==0
    
    #encode hh relation
    sam_eth_hh[,("inhousehold_id"):=paste0(household_id,"_01")]
    sam_race_hh[,("inhousehold_id"):=paste0(household_id,"_01")]
    sam_eth_hh[,("role_in_family"):=list("Householder")]
    sam_race_hh[,("role_in_family"):=list("Householder")]
    
    #the last few will all be juggled together as we grow back and forth
    additional_workers_eth <- as.data.table(anti_join(transport_age_dt,sam_eth_hh,by="track_hh_id"))
    additional_workers_race <- as.data.table(anti_join(transport_age_dt,sam_race_hh,by="track_hh_id"))
    partners_eth <- sam_eth_hh[,uncount(.SD[!is.na(partner_type)],1,.remove = TRUE,.id="partner")]
    wives_eth <- sam_eth_hh[,uncount(.SD[family_role=="Married-couple family"],1,.remove = TRUE,.id="wife")]
    partners_race <- sam_race_hh[,uncount(.SD[!is.na(partner_type)],1,.remove = TRUE,.id="partner")]
    wives_race <- sam_race_hh[,uncount(.SD[family_role=="Married-couple family"],1,.remove = TRUE,.id="wife")]
    #change the files to mirror
    #list() seems to keep it from getting that shallow copy warning; not sure what's going on / didn't work second time...
    partners_eth[,("inhousehold_id"):=list(paste0(household_id,"_02"))] 
    partners_race[,("inhousehold_id"):=list(paste0(household_id,"_02"))]
    wives_eth[,("inhousehold_id"):=list(paste0(household_id,"_02"))]
    wives_race[,("inhousehold_id"):=list(paste0(household_id,"_02"))]
    partners_eth[,("role_in_family"):=list("Unmarried partner")]
    partners_race[,("role_in_family"):=list("Unmarried partner")]
    wives_eth[,("role_in_family"):=list("Spouse")]
    wives_race[,("role_in_family"):=list("Spouse")]
    partners_eth[,("sex"):=sex_partner]
    partners_race[,("sex"):=sex_partner]
    wives_eth[,("sex"):="Female"]
    wives_race[,("sex"):="Female"]
    partners_eth[,c("family_role","family_role_4"):="Partner"]
    partners_race[,c("family_role","family_role_4"):="Partner"]
    wives_eth[,c("family_role","family_role_4"):="Wife"]
    wives_race[,c("family_role","family_role_4"):="Wife"]
    #employment
    partners_eth[,("employment"):=single_hh_employed]
    partners_eth[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    partners_race[,("employment"):=single_hh_employed]
    partners_race[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    wives_eth[,("employment"):=wife_employed]
    wives_race[,("employment"):=wife_employed]
    sam_eth_hh[,("employment"):=husband_employed]
    sam_eth_hh[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    sam_race_hh[,("employment"):=husband_employed]
    sam_race_hh[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    #could do interracial marriages - 17% of new marriages in 2015; 3% in 1967... other.race gets a lot of increase... - fix later
    
    
    wives_eth[,c("industry","occupation","commute_time","when_go_to_work","English_level","income_range_workers"):=NULL]
    wives_eth[employment!="Not in labor force",
              ("pw_match_id"):=paste0(tract,sex,ethnicity,
                                      language,means_transport,age_range_6, 
                                      as.character(1000000+sample(1:.N))),
                  by = .(tract)]
    additional_workers_eth[,("pw_match_id"):=paste0(tract,sex,ethnicity,
                                                    language,means_transport,age_range_6, 
                                                    as.character(1000000+sample(1:.N))),
                           by = .(tract)]
    wives_eth[,c(#"means_transport",
                 "industry","occupation","commute_time","when_go_to_work",
                                          # "language",
                 "English_level","income_range_workers"):=
                additional_workers_eth[.SD, c(#list(means_transport),
                                              list(industry),list(occupation),
                                        list(commute_time),list(when_go_to_work),
                                        #list(language),
                                        list(English_level),list(income_range_workers)),
                                 on = .(pw_match_id)]]
    #take out of additional_workers
    additional_workers_eth[,("missing_wives"):=wives_eth[.SD,list(industry),on = .(pw_match_id)]]
    test <- nrow(wives_eth[is.na(industry)])==0
    wives_eth[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    #add language and English level for folks not in labor force....
    
    
    
    #rbindlist into one whole and then match...
    sam_pw_hh_eth <- bind_rows(sam_eth_hh,partners_eth,wives_eth)
    sam_pw_hh_race <- bind_rows(sam_race_hh,partners_race,wives_race)
    
    #match wives/partners with workers - "number_workers_in_hh",
    #sam_pw_hh_eth[!is.na(inhousehold_id),
    #              ("pw_match_id"):=paste0(tract),
    #              by = .(tract)]
    #sam_pw_hh_eth[!is.na(inhousehold_id),c("means_transport","industry","occupation","commute_time","when_go_to_work",
    #                                       "language","English_level","income_range_workers"):=
    #                transport_hh[.SD, c(list(means_transport),list(industry),list(occupation),
    #                                    list(commute_time),list(when_go_to_work),list(language),
    #                                    list(English_level),list(income_range)),
    #                             on = .(pw_match_id)]]
    
    
    #hh_income_dt
    #computer_internet_hh
    #internet_hh
    #educ_internet_hh
    #income_internet_hh
    #food_stamps_eth_dt
    #food_stamps_race_dt
    #
    #age_hh_monthly_costs_income_hh
    #age_internet_hh
    #
    #family_employment_dt
    #
    #mortgage stuff
    #percent_income_monthly_costs_hh
    #tenure_income_monthly_costs_hh
    #tenure_yr_moved_units_hh
    
    
    
    
    
    
    
    #create larger workers file, to start process with race/eth for hh info
    #move over info for hh where they are employed and wife
    #add own_rent and sex from sam to transport, by race, so that we have the subset that is HH already prepped
    sam_pw_hh_eth[employment!="Not in labor force",("transport_occup_id"):=paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),by=.(tract,ethnicity)]
    transport_eth_dt[,("transport_occup_id"):=paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),by=.(tract,ethnicity)]
    sam_pw_hh_race[employment!="Not in labor force",("transport_occup_id"):=paste0(tract,race,as.character(1000000+sample(1:.N))),by=.(tract,race)]
    transport_race_dt[,("transport_occup_id"):=paste0(tract,race,as.character(1000000+sample(1:.N))),by=.(tract,race)]
    transport_eth_dt[,c("sex","own_rent") := 
                       sam_pw_hh_eth[.SD, c(list(sex),list(own_rent)), on = .(transport_occup_id)]]
    sam_pw_hh_eth$transport_occup_id <- NULL
    transport_race_dt[,c("sex","own_rent") := 
                        sam_pw_hh_race[.SD, c(list(sex),list(own_rent)), on = .(transport_occup_id)]]
    sam_pw_hh_race$transport_occup_id <- NULL
    #about 30k didn't match; since some HHs aren't employed, it may be ok. - not really sure why this would happen if only matching race/eth...
    
    #fill in rest (and juggle, as needed) for own_rent and then everything with sex
    #mark the ones on transport_tenure that are already used on means_transport and own_rent
    transport_eth_dt[employment!="Not in labor force",("transport_occup1_id"):=
                       paste0(tract,means_transport,own_rent,as.character(1000000+sample(1:.N))),
                     by=.(tract,means_transport,own_rent)]
    transport_race_dt[employment!="Not in labor force",("transport_occup1_id"):=
                        paste0(tract,means_transport,own_rent,as.character(1000000+sample(1:.N))),
                      by=.(tract,means_transport,own_rent)]
    transport_tenure_dt[,("transport_occup1_id"):=
                          paste0(tract,means_transport,own_rent,as.character(1000000+sample(1:.N))),
                        by=.(tract,means_transport,own_rent)]
    transport_tenure_dt[,c("miss_own_rent_race") := 
                          transport_race_dt[.SD, list(own_rent), on = .(transport_occup1_id)]]
    transport_tenure_dt[,c("miss_own_rent_eth") := 
                          transport_eth_dt[.SD, list(own_rent), on = .(transport_occup1_id)]]
    #shuffle to get sex, own_rent relative to transportation 
    #have to get the means_transport - own_rent pairs from transport_tenure to eth/race
    #since own_rent is binary, just switch for leftovers and see if that matches rest
    transport_eth_dt[is.na(miss_own_rent) & !is.na(own_rent),
                     ("own_rent"):=if_else(own_rent=="Owner occupied","Renter occupied","Owner occupied")]
    
    
    #continue with transport_eth/race for rest from transport_tenure
    transport_eth_dt[is.na(miss_own_rent_eth),
                     ("transport_occup2e_id"):=
                       paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                     by=.(tract,ethnicity)]
    transport_tenure_dt[,("transport_occup2e_id"):=
                          paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                        by=.(tract,ethnicity)]
    transport_race_dt[is.na(miss_own_rent_race),
                      ("transport_occup2r_id"):=
                        paste0(tract,race,as.character(1000000+sample(1:.N))),
                      by=.(tract,race)]
    transport_tenure_dt[,("transport_occup2r_id"):=
                          paste0(tract,race,as.character(1000000+sample(1:.N))),
                        by=.(tract,race)]
    transport_eth_dt[is.na(own_rent),c("own_rent") := 
                       transport_tenure_dt[.SD, list(own_rent), on = .(transport_occup2e_id)]]
    transport_race_dt[is.na(own_rent),c("own_rent") := 
                        transport_tenure_dt[.SD, list(own_rent), on = .(transport_occup2r_id)]]
    
    #test hh14
    test <- table(
      transport_eth_dt$tract,
      transport_eth_dt$means_transport,
      transport_eth_dt$own_rent
    )==table(
      transport_tenure_dt$tract,
      transport_tenure_dt$means_transport,
      transport_tenure_dt$own_rent
    )
    length(test[test==F])
    test <- table(
      transport_race_dt$tract,
      transport_race_dt$means_transport,
      transport_race_dt$own_rent
    )==table(
      transport_tenure_dt$tract,
      transport_tenure_dt$means_transport,
      transport_tenure_dt$own_rent
    )
    length(test[test==F])
    

    
    
    #add vehicles_workers_dt - use to add hh to larger workers, with stuff on hh_size and sex
    #add vehicles - should vary with more infor from individual size
    sam_eth_hh[,("vehicle_occup_id"):=paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),by=.(tract,hh_size_4)]
    sam_race_hh[,("vehicle_occup_id"):=paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),by=.(tract,hh_size_4)]
    vehicles_hh[,("vehicle_occup_id"):=paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),by=.(tract,hh_size_4)]
    sam_eth_hh[,c("number_vehicles_in_hh") := 
             vehicles_hh[.SD, list(number_vehicles_in_hh), on = .(vehicle_occup_id)]]
    sam_eth_hh$vehicle_occup_id <- NULL
    sam_race_hh[,c("number_vehicles_in_hh") := 
                 vehicles_hh[.SD, list(number_vehicles_in_hh), on = .(vehicle_occup_id)]]
    sam_race_hh$vehicle_occup_id <- NULL
    #test hh14
    #test<-table(
    #  vehicles_hh$tract,
    #  vehicles_hh$hh_size_4,
    #  vehicles_hh$number_vehicles_in_hh
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$hh_size_4,
    #  sam_race_hh$number_vehicles_in_hh
    #)
    #length(test[test==F])==0
    #test<-table(
    #  vehicles_hh$tract,
    #  vehicles_hh$hh_size_4,
    #  vehicles_hh$number_vehicles_in_hh
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$hh_size_4,
    #  sam_eth_hh$number_vehicles_in_hh
    #)
    #length(test[test==F])==0

    #add income - not a lot determined yet, so match on own_rent but ordered by people_per_room
    sam_eth_hh[order(-people_per_room),
           ("income_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
           by=.(tract,own_rent)]
    sam_race_hh[order(-people_per_room),
               ("income_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent)]
    hh_income_dt[order(income_low),("income_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
                 by=.(tract,own_rent)]
    sam_eth_hh[,c("hh_income_level","income_low","income_high") := 
             hh_income_dt[.SD, c(list(hh_income_level),list(income_low),list(income_high)), on = .(income_occup_id)]]
    sam_race_hh[,c("hh_income_level","income_low","income_high") := 
                 hh_income_dt[.SD, c(list(hh_income_level),list(income_low),list(income_high)), on = .(income_occup_id)]]
    sam_eth_hh$income_occup_id <- NULL
    sam_race_hh$income_occup_id <- NULL
    #test hh15
    #test<-table(
    #  hh_income_dt$tract,
    #  hh_income_dt$hh_income_level,
    #  hh_income_dt$income_low,
    #  hh_income_dt$income_high
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$hh_income_level,
    #  sam_race_hh$income_low,
    #  sam_race_hh$income_high
    #)
    #length(test[test==F])==0
    #test<-table(
    #  hh_income_dt$tract,
    #  hh_income_dt$hh_income_level,
    #  hh_income_dt$income_low,
    #  hh_income_dt$income_high
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$hh_income_level,
    #  sam_eth_hh$income_low,
    #  sam_eth_hh$income_high
    #)
    #length(test[test==F])==0
    
    #add hh_education - need to think through how it matches with individual education on sex_age_race
    #make same variable categories with sam_hh
    hh_educ_dt[str_detect(own_rent,"Owner"),("own_rent"):="Owner occupied"]
    hh_educ_dt[str_detect(own_rent,"Renter"),("own_rent"):="Renter occupied"]
    sam_eth_hh[order(income_low),
           ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
           by=.(tract,own_rent)]
    sam_race_hh[order(income_low),
               ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),
               by=.(tract,own_rent)]
    hh_educ_dt[order(match(hh_education_level,c("Less than high school graduate","High school graduate (including equivalency)",
                                                "Some college or associate's degree","Bachelor's degree or higher"))),
               ("educ_occup_id"):=paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    sam_eth_hh[,c("hh_education_level") := 
             hh_educ_dt[.SD, list(hh_education_level), on = .(educ_occup_id)]]
    sam_race_hh[,c("hh_education_level") := 
                 hh_educ_dt[.SD, list(hh_education_level), on = .(educ_occup_id)]]
    sam_eth_hh$educ_occup_id <- NULL
    sam_race_hh$educ_occup_id <- NULL
    #test hh16
    #test<-table(
    #  hh_educ_dt$tract,
    #  hh_educ_dt$hh_education_level
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$hh_education_level
    #)
    #length(test[test==F])==0
    #test<-table(
    #  hh_educ_dt$tract,
    #  hh_educ_dt$hh_education_level
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$hh_education_level
    #)
    #length(test[test==F])==0
    
    #add rent by income
    gross_rent_hh[rent_cash=="With cash rent",("gross_inc_id"):=
                    paste0(tract,gross_rent_high2,as.character(1000000+seq.int(1:.N))),by=.(tract,gross_rent_high2)]
    income_gross_rent_hh[,("gross_inc_id"):=
                           paste0(tract,gross_rent_high2,as.character(1000000+seq.int(1:.N))),by=.(tract,gross_rent_high2)]
    #add income to gross_rent, then to sam_hh own_rent by income?
    gross_rent_hh[,c("hh_income_renters") := 
                    income_gross_rent_hh[.SD, list(hh_income_renters), on = .(gross_inc_id)]]
    #clean up for the ones who pay no rent
    gross_rent_hh[is.na(hh_income_renters),("hh_income_renters"):="Less than $10 000"]
    gross_rent_hh[hh_income_renters=="Less than $10 000",("hh_income_renters"):="$1   000"]
    gross_rent_hh[,("income_low"):= as.numeric(substr(hh_income_renters,2,4))*1000]
    #now to sam_hh
    gross_rent_hh[order(income_low),("inc_id"):=
                    paste0(tract,"Renter occupied",as.character(1000000+seq.int(1:.N))),by=.(tract)]
    sam_eth_hh[order(income_low),("inc_id"):=
                           paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    sam_race_hh[order(income_low),("inc_id"):=
                 paste0(tract,own_rent,as.character(1000000+seq.int(1:.N))),by=.(tract,own_rent)]
    #add income to gross_rent, then to sam_hh own_rent by income?
    #want to compare to see if it adds information, later, but may not be of use
    sam_eth_hh[,c("hh_income_renters") := 
        gross_rent_hh[.SD, list(hh_income_renters), on = .(inc_id)]]
    sam_eth_hh$inc_id <- NULL
    sam_race_hh[,c("hh_income_renters") := 
                 gross_rent_hh[.SD, list(hh_income_renters), on = .(inc_id)]]
    sam_race_hh$inc_id <- NULL
    #test hh17
    #test<-table(
    #  gross_rent_hh$tract,
    #  gross_rent_hh$hh_income_renters
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$hh_income_renters
    #)
    #length(test[test==F])==0
    #test<-table(
    #  gross_rent_hh$tract,
    #  gross_rent_hh$hh_income_renters
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$hh_income_renters
    #)
    #length(test[test==F])==0

    
  
    #SNAP stuff is problematic at a couple of levels - could try to combine with SSI, too?
    sam_eth_hh[order(people_per_room),
               ("SNAP_id"):=paste0(tract,ethnicity,as.character(1000000+seq.int(1:.N))),
               by=.(tract,ethnicity)]
    sam_race_hh[order(people_per_room),
                ("SNAP_id"):=paste0(tract,race,as.character(1000000+seq.int(1:.N))),
                by=.(tract,race)]
    food_stamps_eth_dt[order(food_stamps),
                       ("SNAP_id"):=paste0(tract,ethnicity,as.character(1000000+seq.int(1:.N))),
               by=.(tract,ethnicity)]
    food_stamps_race_dt[order(food_stamps),
                        ("SNAP_id"):=paste0(tract,race,as.character(1000000+seq.int(1:.N))),
                       by=.(tract,race)]
    sam_eth_hh[,c("SNAP") := 
                 food_stamps_eth_dt[.SD, list(food_stamps), on = .(SNAP_id)]]
    sam_race_hh[,c("SNAP") := 
                  food_stamps_race_dt[.SD, list(food_stamps), on = .(SNAP_id)]]
    sam_eth_hh$SNAP_id <- NULL
    sam_race_hh$SNAP_id <- NULL
    #test hh19
    #test<-table(
    #  food_stamps_race_dt$tract,
    #  food_stamps_race_dt$race,
    #  food_stamps_race_dt$food_stamps
    #)==table(
    #  sam_race_hh$tract,
    #  sam_race_hh$race,
    #  sam_race_hh$SNAP
    #)
    #length(test[test==F])==0
    #test<-table(
    #  food_stamps_eth_dt$tract,
    #  food_stamps_eth_dt$ethnicity,
    #  food_stamps_eth_dt$food_stamps
    #)==table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$ethnicity,
    #  sam_eth_hh$SNAP
    #)
    #length(test[test==F])==0
    
    

    saveRDS(sam_eth_hh,file = paste0(housingdir, vintage, "/sam_eth.hh",Sys.Date(),".RDS")) 
    saveRDS(sam_race_hh,file = paste0(housingdir, vintage, "/sam_race.hh",Sys.Date(),".RDS"))
#    rm(list = setdiff(ls(),"sam_hh")) # or just the ones that are in the list that is passed from expand_hh_from_census
    #when go through and set break if error points, also remove after using - just to clean up space
  #END BASE_HH_GEN here
  }   
}
    






    #should remember kids_grand_age, too
    #we have household type by kids, household type by seniors and household type by whole population, - can also get family_type from sam_hh by age, but this may be something to write back over... 
#    hh_type_kids #very close to sex_age_race <18yo, but have to add after sam_hh is added to say who has or doesn't have children!!
    #on sam_hh already kids_ages_dt #5 age range, family_type - F/M householder or married couple - only own_kids - needs related kids to find
    
    
    
    
    
    #move this into apportioning
    
    sam_hh[,c("hh_1_role") := "self"]
    sam_hh[,c("householder") := TRUE]
    sam_hh[,("age_range"):=householder_age_9]
    relations_dt[,c("relations_id"):=list(paste0(family_role,tract,as.character(3000000+sample(1:.N)))),by=.(tract)]
    
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
              c("child_8_match_id"):=list(paste0(tract,"child8",as.character(2000000+sample(1:.N)))),by=.(tract)]
    relations_dt_no_GQ[,c("child_8_match_id"):=list(paste0(tract,"child8",as.character(2000000+sample(1:.N)))),by=.(tract)]
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
           ("hh_2_role"):="Spouse"] 
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
    
    
    
    #income median by race B19013 - one value per tract per race
    #race_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19013")
    
    #gets per_capita by race per tract - one value per tract per race
    #per_capita_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19301")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     
    
    
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
    
    

    #AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS FOR FAMILIES BY FAMILY TYPE -only below poverty... too complicated to unwind and explain
    agg_deficit_income_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17011")
    
    #by number of children and poverty B17023
    number_children_poverty_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17023")
    
    #by tenure B17019 (whether renter or not)
    household_tenure_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B17019")
    
  saveRDS(sam_hh,file = paste0(censusdir, vintage,"/sam_hh.RDS"))
  #and a dated copy?
  saveRDS(sam_hh,file = paste0(censusdir, vintage, "/sam_hh_",Sys.Date(),".RDS"))








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
income_value_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25121")






