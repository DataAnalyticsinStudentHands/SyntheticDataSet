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
    #sometimes, when running, get an error because they have all matched??
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
    
    #CHECK HH_SIZE==1 IF family_type=="Householder living alone"
    
    #test hh8
    #nrow(sam_eth_hh[family_type=="Householder living alone"&hh_size=="1-person household"])==nrow(hh_size_dt[hh_size=="1-person household"])
    #nrow(sam_race_hh[family_type=="Householder living alone"&hh_size=="1-person household"])==nrow(hh_size_dt[hh_size=="1-person household"])
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
    
    #put a hh_size_vehicle to match against later - there are some other weird things I'm not sure how to deal with
    sam_eth_hh[,("hh_size_vehicle"):=hh_size]
    sam_race_hh[,("hh_size_vehicle"):=hh_size]
    
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
    
    #put number of !00 workers per tract on every one in transport_hh
    #done in expand_hh_from_census.R
    #hh_workers[,("no_workers"):=nrow(.SD[number_workers_in_hh!="00 workers"]),by=.(tract)]
    
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
    #nrow(sam_eth_hh[family_type=="Householder living alone"])==nrow(hh_workers[hh_size_4=="1-person household"])
    workers_exp <- hh_workers[,uncount(.SD,as.numeric(substr(number_workers_in_hh,1,2)),.remove = FALSE,.id = "worker_")]
    workers_exp$number_sams <- NULL #trying to fix shallow copy warning
    
    transport_age_dt[,("age_work_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                     by=.(tract)]
    workers_exp[order(hh_size_7),("age_work_id") := paste0(tract,as.character(1000000+seq.int(1:.N))),
                by=.(tract)]
    transport_age_dt[,c("hh_size_10","hh_size","number_workers_in_hh","hh_workers_id","no_workers"):=
                       workers_exp[.SD, c(list(hh_size_10),list(hh_size_7),list(number_workers_in_hh),
                                          list(hh_workers_id),list(no_workers)),
                                   on = .(age_work_id)]]
    workers_exp[,("missed"):=transport_age_dt[.SD,list(hh_size),on = .(age_work_id)]]
    
    #maybe in wrong tracts for ones that are larger? 
    transport_age_dt[is.na(hh_size),
                     ("age_work2_id") := paste0(as.character(1000000+sample(1:.N)))]
    workers_exp[is.na(missed),
                ("age_work2_id") := paste0(as.character(1000000+seq.int(1:.N)))]
    transport_age_dt[is.na(hh_size),
                     c("hh_size_10","hh_size","number_workers_in_hh","hh_workers_id","no_workers"):=
                       workers_exp[.SD, c(list(hh_size_10),list(hh_size_7),list(number_workers_in_hh),
                                          list(hh_workers_id),list(no_workers)),
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
    #separate hh from non-hh workers add_workers_hh will be used later
    #clean up age
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
    #get back to only householders who are in labor force
    transport_age_dt[,("track_hh_id"):=paste0(tract,as.character(1000000+sample(1:.N))),by=.(tract)]
    transport_age_dt[,("ethnicity"):=if_else(ethnicity!="H" | ethnicity!="I",ethnicity,"_")]
    # number in labor force
    #transport_hh <- transport_age_dt[,.SD[1],by = .(hh_workers_id)] # number in labor force, but about 4k short of correct total...
    transport_age_dt[order(-age_range_6),("potential_hh"):=as.character(1:.N),by=.(hh_workers_id)]
    transport_hh <- transport_age_dt[potential_hh=="1"]
    #add cut_off for number of workers per tract
    sam_race_hh[,("cut_off_id") := paste0(tract,as.character(1000000+sample(1:.N))),
                by=.(tract)]
    sam_eth_hh[,("cut_off_id") := paste0(tract,as.character(1000000+sample(1:.N))),
               by=.(tract)]
    hh_workers[,("cut_off_id") := paste0(tract,as.character(1000000+sample(1:.N))),
               by=.(tract)]
    sam_race_hh[,("no_workers"):=hh_workers[.SD,no_workers,on=.(cut_off_id)]]
    sam_eth_hh[,("no_workers"):=hh_workers[.SD,no_workers,on=.(cut_off_id)]]
    
    #test hh8i
    #test <- table(
    #  sam_race_hh$tract,
    #  sam_race_hh$no_workers
    #)==table(
    #  hh_workers$tract,
    #  hh_workers$no_workers
    #)
    #length(test[test==F])==0
    #test <- table(
    #  sam_eth_hh$tract,
    #  sam_eth_hh$no_workers
    #)==table(
    #  hh_workers$tract,
    #  hh_workers$no_workers
    #)
    #length(test[test==F])==0
    
    #get age_range in shape to match
    
    #get easier age for next step
    transport_hh[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    sam_race_hh[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    sam_eth_hh[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    
    #MOVE STUFF FROM FAMILIES GENERATOR BACK HERE, SO CAN MATCH ON WIVES AND PARTNERS AT SAME TIME, HERE... JUST STUFF BEFORE THE EXPAND


    #nrow(transport_hh[hh_size=="1-person household"])==nrow(hh_workers[hh_size_4=="1-person household"&number_workers_in_hh=="01 worker"])
    #1-person households on sam_eth/race don't have sex
    #put in cut off
    #sam_eth_hh$sample_cut_off <- NULL
    #sam_eth_hh$transport_cut_off <- NULL
    #sam_race_hh$sample_cut_off <- NULL
    #sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[hh_size=="1-person household",
               ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[hh_size=="1-person household",
               ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[hh_size=="1-person household",
                ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[hh_size=="1-person household",
                ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #rejoin just for 1-person hh
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                ("rejoin_race1p_id") := paste0(tract,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,age_range_6,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               ("rejoin_eth1p_id") := paste0(tract,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,age_range_6,ethnicity,hh_size)]
    transport_hh[hh_size=="1-person household",("rejoin_race1p_id") := 
                   paste0(tract,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,own_rent,age_range_6,race,hh_size)]
    transport_hh[hh_size=="1-person household",("rejoin_eth1p_id") := 
                   paste0(tract,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,own_rent,age_range_6,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                c("sex","means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(sex),list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race1p_id)]]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               c("sex","means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(sex),list(means_transport),list(number_workers_in_hh),list(industry),
                                     list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                     list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                              on = .(rejoin_eth1p_id)]]
    transport_hh[hh_size=="1-person household",
                 ("missing_eth"):=sam_eth_hh[.SD,means_transport,on=.(rejoin_eth1p_id)]]
    transport_hh[hh_size=="1-person household",
                 ("missing_race"):=sam_race_hh[.SD,means_transport,on=.(rejoin_race1p_id)]]
    
    #nrow transport_hh
    #reshuffle_start
    #reshuffle <- as.data.frame(Sys.time())
    #reshuffle$te01p0 <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se01p0 <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr01p0 <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr01p0 <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$srm01p0 <- mean(sam_race_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$sem01p0 <- mean(sam_eth_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$r10p0 <- nrow(sam_race_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #reshuffle$e10p0 <- nrow(sam_eth_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #sam_cut_off_010p <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_010p <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    
    #tests - nrow(sam_race_hh[family_type=="Householder living alone"&hh_size=="1-person household"]) == nrow(sam_race_hh[hh_size=="1-person household"])
    #no sex on 1-person households, since they are all non-family - have to figure out how to get sex on non-family, more than one-person and 
    #how to have right number of non-working 1-person hh.
    
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport)&hh_size=="1-person household",
               ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport)&hh_size=="1-person household",
               ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport)&hh_size=="1-person household",
                ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport)&hh_size=="1-person household",
                ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager <- transport_hh[is.na(missing_race)&hh_size=="1-person household"]
    transport_hh[is.na(missing_race)&hh_size=="1-person household",
                 ("rejoin_raceA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                 by=.(tract,own_rent)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent)]
    #for track_hh_id, to get it exact, would need to also write back new matches to transport_hh - too much trouble at this point
    #not moving track_hh_id now, so it's easier to get right number at end
    reshuffle_ager[,c("cnt_missing","race","age_range_6","age_range_w3","hh_size","hh_size_10","sex"):= #,"track_hh_id"):=
                     transport_hh[.SD, c(list(race),list(race),list(age_range_6),list(age_range_w3),
                                         list(hh_size),list(hh_size_10),list(sex)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)&hh_size=="1-person household"]
    transport_hh[is.na(missing_eth)&hh_size=="1-person household",
                 ("rejoin_ethA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                 by=.(tract,own_rent)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent)]
    reshuffle_agee[,c("cnt_missing","ethnicity","age_range_6","age_range_w3","hh_size","hh_size_10","sex"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(ethnicity),list(ethnicity),list(age_range_6),list(age_range_w3),
                                         list(hh_size),list(hh_size_10),list(sex)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    #put on sam
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                ("rejoin_race2_id") := paste0(tract,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               ("rejoin_eth2_id") :=paste0(tract,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,age_range_w3,ethnicity,hh_size)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,race,hh_size)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id","sex"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),list(sex)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id","sex"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),list(sex)),
                                on = .(rejoin_eth2_id)]]
    reshuffle_agee[,("missing_eth"):=sam_eth_hh[.SD,means_transport,on=.(rejoin_eth2_id)]]
    reshuffle_ager[,("missing_race"):=sam_race_hh[.SD,means_transport,on=.(rejoin_race2_id)]]
    transport_hh[is.na(missing_eth)&hh_size=="1-person household",
                 ("missing_eth"):=
                   reshuffle_agee[.SD,missing_eth,on=.(rejoin_ethA_id)]]
    transport_hh[is.na(missing_race)&hh_size=="1-person household",
                 ("missing_race"):=
                   reshuffle_ager[.SD,missing_race,on=.(rejoin_raceA_id)]]
    
    #reshuffle01
    #reshuffle$te01sp <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se01sp <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr01sp <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr01sp <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$srm01sp <- mean(sam_race_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$sem01sp <- mean(sam_eth_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$r1p0 <- nrow(sam_race_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #reshuffle$e1p0 <- nrow(sam_eth_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #reshuffle$etrack01sp <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack01sp <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_01sp <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_01sp <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffle for 1-person, on eth/race
    #put in cut off
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh$rejoin_raceA_id <- NULL
    transport_hh$rejoin_ethA_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport)&hh_size=="1-person household",
               ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport)&hh_size=="1-person household",
               ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport)&hh_size=="1-person household",
                ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport)&hh_size=="1-person household",
                ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager <- transport_hh[is.na(missing_race)&hh_size=="1-person household"]
    transport_hh[is.na(missing_race)&hh_size=="1-person household",
                 ("rejoin_raceA_id") := paste0(tract,race,as.character(1000000+sample(1:.N))),
                 by=.(tract,race)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,race,as.character(1000000+sample(1:.N))),
                   by=.(tract,race)]
    #for track_hh_id, to get it exact, would need to also write back new matches to transport_hh - too much trouble at this point
    #not moving track_hh_id now, so it's easier to get right number at end
    reshuffle_ager[,c("cnt_missing","own_rent","age_range_6","age_range_w3","hh_size","hh_size_10","sex"):= #,"track_hh_id"):=
                     transport_hh[.SD, c(list(race),list(own_rent),list(age_range_6),list(age_range_w3),
                                         list(hh_size),list(hh_size_10),list(sex)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)&hh_size=="1-person household"]
    transport_hh[is.na(missing_eth)&hh_size=="1-person household",
                 ("rejoin_ethA_id") := paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                 by=.(tract,ethnicity)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                   by=.(tract,ethnicity)]
    reshuffle_agee[,c("cnt_missing","own_rent","age_range_6","age_range_w3","hh_size","hh_size_10","sex"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(ethnicity),list(own_rent),list(age_range_6),list(age_range_w3),
                                         list(hh_size),list(hh_size_10),list(sex)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    #put on sam
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                ("rejoin_race2_id") := paste0(tract,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               ("rejoin_eth2_id") :=paste0(tract,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,age_range_w3,ethnicity,hh_size)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,race,hh_size)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id","sex"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),list(sex)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id","sex"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),list(sex)),
                                on = .(rejoin_eth2_id)]]
    reshuffle_agee[,("missing_eth"):=sam_eth_hh[.SD,means_transport,on=.(rejoin_eth2_id)]]
    reshuffle_ager[,("missing_race"):=sam_race_hh[.SD,means_transport,on=.(rejoin_race2_id)]]
    transport_hh[is.na(missing_eth)&hh_size=="1-person household",
                 ("missing_eth"):=
                   reshuffle_agee[.SD,missing_eth,on=.(rejoin_ethA_id)]]
    transport_hh[is.na(missing_race)&hh_size=="1-person household",
                 ("missing_race"):=
                   reshuffle_ager[.SD,missing_race,on=.(rejoin_raceA_id)]]
    
    #reshuffle02
    #reshuffle$te02sp <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se02sp <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr02sp <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr02sp <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$srm02sp <- mean(sam_race_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$sem02sp <- mean(sam_eth_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$r1p1 <- nrow(sam_race_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #reshuffle$e1p1 <- nrow(sam_eth_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #reshuffle$etrack02sp <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack02sp <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_02sp <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_02sp <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #do same reshuffle again; age_range_w3 and own_rent dropped
    #reshuffle for 1-person, on eth/race
    #put in cut off
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh$rejoin_raceA_id <- NULL
    transport_hh$rejoin_ethA_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport)&hh_size=="1-person household",
               ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport)&hh_size=="1-person household",
               ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport)&hh_size=="1-person household",
                ("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport)&hh_size=="1-person household",
                ("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager <- transport_hh[is.na(missing_race)&hh_size=="1-person household"]
    transport_hh[is.na(missing_race)&hh_size=="1-person household",
                 ("rejoin_raceA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                 by=.(tract,age_range_w3)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    #for track_hh_id, to get it exact, would need to also write back new matches to transport_hh - too much trouble at this point
    #not moving track_hh_id now, so it's easier to get right number at end
    reshuffle_ager[,c("cnt_missing","own_rent","race","hh_size","hh_size_10","sex"):= #,"track_hh_id"):=
                     transport_hh[.SD, c(list(race),list(own_rent),list(race),
                                         list(hh_size),list(hh_size_10),list(sex)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)&hh_size=="1-person household"]
    transport_hh[is.na(missing_eth)&hh_size=="1-person household",
                 ("rejoin_ethA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                 by=.(tract,age_range_w3)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    reshuffle_agee[,c("cnt_missing","own_rent","ethnicity","hh_size","hh_size_10","sex"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(ethnicity),list(own_rent),list(ethnicity),
                                         list(hh_size),list(hh_size_10),list(sex)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    #put on sam
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                ("rejoin_race2_id") := paste0(tract,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               ("rejoin_eth2_id") :=paste0(tract,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,age_range_w3,ethnicity,hh_size)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3,race,hh_size)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE&hh_size=="1-person household",
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id","sex"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),list(sex)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE&hh_size=="1-person household",
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id","sex"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),list(sex)),
                                on = .(rejoin_eth2_id)]]
    reshuffle_agee[,("missing_eth"):=sam_eth_hh[.SD,means_transport,on=.(rejoin_eth2_id)]]
    reshuffle_ager[,("missing_race"):=sam_race_hh[.SD,means_transport,on=.(rejoin_race2_id)]]
    transport_hh[is.na(missing_eth)&hh_size=="1-person household",
                 ("missing_eth"):=
                   reshuffle_agee[.SD,missing_eth,on=.(rejoin_ethA_id)]]
    transport_hh[is.na(missing_race)&hh_size=="1-person household",
                 ("missing_race"):=
                   reshuffle_ager[.SD,missing_race,on=.(rejoin_raceA_id)]]
    
    #reshuffle03
    #reshuffle$te03sp <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se03sp <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr03sp <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr03sp <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$srm03sp <- mean(sam_race_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$sem03sp <- mean(sam_eth_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$r1p3 <- nrow(sam_race_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #reshuffle$e1p3 <- nrow(sam_eth_hh[!is.na(means_transport)&hh_size=="1-person household"])
    #reshuffle$etrack03sp <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack03sp <- length(unique(sam_race_hh$track_hh_id)) #22k that I would have wanted matched....
    #sam_cut_off_03sp <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_03sp <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    ##reshuffle on age - keeps same totals on age this way, but attaches them to different cases - it was random above, so no lost info
    ##there's got to be a simple way to make sure it's a complete shuffle; this doesn't redo all of them
    
    #put in cut off for all, including 01-person household
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    
    sam_race_hh[transport_cut_off==TRUE,
                ("rejoin_race_id") := paste0(tract,sex,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                      by=.(tract,sex,own_rent,age_range_6,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE,
               ("rejoin_eth_id") := paste0(tract,sex,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_6,ethnicity,hh_size)]
    transport_hh[,("rejoin_race_id") := paste0(tract,sex,own_rent,age_range_6,race,hh_size,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_6,race,hh_size)]
    transport_hh[,("rejoin_eth_id") := paste0(tract,sex,own_rent,age_range_6,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_6,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE,c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                   "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                          list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                          list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                         on = .(rejoin_race_id)]]
    sam_eth_hh[transport_cut_off==TRUE,c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                         list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                         list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                        on = .(rejoin_eth_id)]]
    transport_hh[is.na(missing_eth),("missing_eth"):=sam_eth_hh[.SD,means_transport,on=.(rejoin_eth_id)]]
    transport_hh[is.na(missing_race),("missing_race"):=sam_race_hh[.SD,means_transport,on=.(rejoin_race_id)]]
    
    #nrow transport_hh
    #reshuffle0
    ##reshuffle <- as.data.frame(Sys.time())
    #reshuffle$te0 <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se0 <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr0 <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr0 <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$srm0 <- mean(sam_race_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$sem0 <- mean(sam_eth_hh[,sample_cut_off],na.rm = TRUE)
    #sam_cut_off_0 <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_0 <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #create fewer hh_sizes
    sam_race_hh[,("hh_size_4"):=if_else(as.numeric(substr(hh_size,1,1))>3,"4 or more person household",hh_size)]
    sam_eth_hh[,("hh_size_4"):=if_else(as.numeric(substr(hh_size,1,1))>3,"4 or more person household",hh_size)]
    transport_hh[,("hh_size_4"):=if_else(as.numeric(substr(hh_size,1,1))>3,"4 or more person household",hh_size)]
    transport_age_dt[,("hh_size_4"):=if_else(as.numeric(substr(hh_size,1,1))>3,"4 or more person household",hh_size)]
    #the reshuffles take too long and only help about 10% of cases - would have to be more systematic
    #put in cut off
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh$rejoin_raceA_id <- NULL
    transport_hh$rejoin_ethA_id <- NULL
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    transport_hh[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent)]
    #for track_hh_id, to get it exact, would need to also write back new matches to transport_hh - too much trouble at this point
    #not moving track_hh_id now, so it's easier to get right number at end
    reshuffle_ager[,c("cnt_missing","race","age_range_6","age_range_w3","hh_size","hh_size_10"):= #,"track_hh_id"):=
                   transport_hh[.SD, c(list(race),list(race),list(age_range_6),list(age_range_w3),
                                       list(hh_size),list(hh_size_10)),#,list(track_hh_id)),
                                       on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    transport_hh[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","ethnicity","age_range_6","age_range_w3","hh_size","hh_size_10"):=#,"track_hh_id"):=
                   transport_hh[.SD, c(list(ethnicity),list(ethnicity),list(age_range_6),list(age_range_w3),
                                       list(hh_size),list(hh_size_10)),#,list(track_hh_id)),
                                on=.(rejoin_ethA_id)]]

    #put on sam
    sam_race_hh[transport_cut_off==TRUE,
                ("rejoin_race2_id") := paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE,
               ("rejoin_eth2_id") :=paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    reshuffle_ager[,("rejoin_race2_id") := 
                   paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                   paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                   "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                     list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                     list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                              on = .(rejoin_eth2_id)]]
    reshuffle_agee[,("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth2_id)]]
    reshuffle_ager[,("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race2_id)]]
    transport_hh[is.na(missing_eth),("missing_eth"):=
                   reshuffle_agee[.SD,list(missing_eth),on=.(rejoin_eth_id)]]
    transport_hh[is.na(missing_race),("missing_race"):=
                   reshuffle_ager[.SD,list(missing_race),on=.(rejoin_race_id)]]
    
    #reshuffle1
    #reshuffle$te1 <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se1 <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr1 <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr1 <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$srm1 <- mean(sam_race_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$sem1 <- mean(sam_eth_hh[,sample_cut_off],na.rm = TRUE)
    #reshuffle$etrack1 <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack1 <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_1 <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_1 <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffle again, this time on age
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh$rejoin_raceA_id <- NULL
    transport_hh$rejoin_ethA_id <- NULL
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    transport_hh[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_w3)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,age_range_w3)]
    reshuffle_ager[,c("cnt_missing","race","own_rent","hh_size","hh_size_10"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(race),list(race),list(own_rent),list(hh_size),
                                         list(hh_size_10)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    transport_hh[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_w3)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,age_range_w3)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","ethnicity","own_rent","hh_size","hh_size_10"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(ethnicity),list(ethnicity),list(own_rent),list(hh_size),
                                         list(hh_size_10)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh[transport_cut_off==TRUE,
                ("rejoin_race2_id") := 
                  paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                on = .(rejoin_eth2_id)]]
    reshuffle_agee[,("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth2_id)]]
    reshuffle_ager[,("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race2_id)]]
    transport_hh[is.na(missing_eth),("missing_eth"):=
                   reshuffle_agee[.SD,list(missing_eth),on=.(rejoin_eth_id)]]
    transport_hh[is.na(missing_race),("missing_race"):=reshuffle_ager[.SD,list(missing_race),on=.(rejoin_race_id)]]
    
    #reshuffle2
    #reshuffle$te2 <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se2 <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr2 <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr2 <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack2 <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack2 <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_2 <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_2 <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffle on hh_size
    #reshuffle again, this time on hh_size
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh$rejoin_raceA_id <- NULL
    transport_hh$rejoin_ethA_id <- NULL
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    transport_hh[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,sex,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,hh_size)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,sex,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,hh_size)]
    reshuffle_ager[,c("cnt_missing","race","own_rent","age_range_6","age_range_w3"):= #,"track_hh_id"):=
                     transport_hh[.SD, c(list(race),list(race),list(own_rent),list(age_range_6),
                                         list(age_range_w3)),# ,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    transport_hh[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,sex,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,hh_size)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,sex,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,hh_size)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","ethnicity","own_rent","age_range_6","age_range_w3"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(ethnicity),list(ethnicity),list(own_rent),list(age_range_6),
                                         list(age_range_w3)),# ,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                on = .(rejoin_eth2_id)]]
    reshuffle_agee[,("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth2_id)]]
    reshuffle_ager[,("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race2_id)]]
    transport_hh[is.na(missing_eth),("missing_eth"):=
                   reshuffle_agee[.SD,list(missing_eth),on=.(rejoin_eth_id)]]
    transport_hh[is.na(missing_race),("missing_race"):=reshuffle_ager[.SD,list(missing_race),on=.(rejoin_race_id)]]
    
    #reshuffle3
    #reshuffle$te3 <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se3 <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr3 <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr3 <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack3 <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack3 <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_3 <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_3 <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffl on race/eth
    #reshuffle again, this time on hh_size
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh$rejoin_raceA_id <- NULL
    transport_hh$rejoin_ethA_id <- NULL
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    transport_hh[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,sex,race,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,race)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,sex,race,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,race)]
    reshuffle_ager[,c("cnt_missing","hh_size","hh_size_10","own_rent","age_range_6","age_range_w3"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(race),list(hh_size),list(hh_size_10),list(own_rent),list(age_range_6),
                                         list(age_range_w3)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    transport_hh[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,sex,ethnicity,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,ethnicity)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,sex,ethnicity,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,ethnicity)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","hh_size","hh_size_10","own_rent","age_range_6","age_range_w3"):=#,"track_hh_id"):=
                     transport_hh[.SD, c(list(ethnicity),list(hh_size),list(hh_size_10),list(own_rent),
                                         list(age_range_6),list(age_range_w3)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                                on = .(rejoin_eth2_id)]]
    reshuffle_agee[,("missing_eth"):=sam_eth_hh[.SD,list(means_transport),on=.(rejoin_eth2_id)]]
    reshuffle_ager[,("missing_race"):=sam_race_hh[.SD,list(means_transport),on=.(rejoin_race2_id)]]
    transport_hh[is.na(missing_eth),("missing_eth"):=
                   reshuffle_agee[.SD,list(missing_eth),on=.(rejoin_eth_id)]]
    transport_hh[is.na(missing_race),("missing_race"):=reshuffle_ager[.SD,list(missing_race),on=.(rejoin_race_id)]]
    
    #reshuffle4
    #reshuffle$te4 <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se4 <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr4 <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr4 <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack4 <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack4 <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_4 <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_4 <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #about two-thirds matched - reshuffle, keeping same totals on transport side and just sampling from other members of transport_age households
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    transport_age_dt[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    transport_age_eth_2 <- as.data.table(anti_join(transport_age_dt,transport_hh[!is.na(missing_eth)],by="track_hh_id"))  
    transport_age_race_2 <- as.data.table(anti_join(transport_age_dt,transport_hh[!is.na(missing_race)],by="track_hh_id"))
    transport_hh[is.na(missing_eth),("rejoin_eth2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_w3,hh_size)]
    transport_age_eth_2[,("rejoin_eth2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_w3,hh_size)]
    transport_hh[is.na(missing_eth),
                 c("ethnicity"):=transport_age_eth_2[.SD,list(ethnicity),on=.(rejoin_eth2b1_id)]]
                 #c("ethnicity","track_hh_id"):=transport_age_eth_2[.SD,c(list(ethnicity),list(track_hh_id)),on=.(rejoin_eth2b1_id)]]
    transport_hh[is.na(missing_race),("rejoin_race2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_w3,hh_size)]
    transport_age_race_2[,("rejoin_race2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size,as.character(1000000+sample(1:.N))),
                        by=.(tract,sex,own_rent,age_range_w3,hh_size)]
    transport_hh[is.na(missing_race),
                 c("race"):=transport_age_race_2[.SD,list(track_hh_id),on=.(rejoin_race2b1_id)]]
                 #c("race","track_hh_id"):=transport_age_race_2[.SD,c(list(race),list(track_hh_id)),on=.(rejoin_race2b1_id)]]
    #then try again, with new race/eth having just been sampled (not actually moved over yet)
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2c1_id") := paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2c1_id") := paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    transport_hh[is.na(missing_race),("rejoin_race2c1_id") := paste0(tract,sex,own_rent,age_range_w3,race,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_w3,race,hh_size)]
    transport_hh[is.na(missing_eth),("rejoin_eth2c1_id") := paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id"):=
                  transport_hh[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                      list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                      list(English_level),list(income_range),list(hh_size_10),list(track_hh_id)),
                               on = .(rejoin_race2c1_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
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
    
    #reshuffle5
    #reshuffle$te5 <-nrow(transport_hh[!is.na(missing_eth)])
    #reshuffle$se5 <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr5 <-nrow(transport_hh[!is.na(missing_race)])
    #reshuffle$sr5 <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack5 <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack5 <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_5 <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_5 <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    rm(transport_age_eth_2)
    rm(transport_age_race_2)
    
    #do entire suite of reshuffles again
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    rm(reshuffle_ager)
    rm(reshuffle_agee)
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    transport_hh_race <- transport_hh
    transport_hh_race$missing_eth <- NULL
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent)]
    reshuffle_ager[,c("cnt_missing","race","age_range_6","age_range_w3","hh_size","hh_size_10"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(race),list(age_range_6),list(age_range_w3),
                                              list(hh_size),list(hh_size_10)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    transport_hh_eth <- transport_hh
    transport_hh_eth$missing_race <- NULL
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,own_rent)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,sex,own_rent,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent)]
    reshuffle_agee[,c("cnt_missing","ethnicity","age_range_6","age_range_w3","hh_size","hh_size_10"):=#,"track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(ethnicity),list(age_range_6),list(age_range_w3),
                                             list(hh_size),list(hh_size_10)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,sex,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size_4)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,race,hh_size_4)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                        list(means_transport)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                       list(means_transport)),
                                on = .(rejoin_eth2_id)]]
    #have to do reshuffle_match to get matching numbers on two sides
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2_id"):=reshuffle_agee[.SD,rejoin_eth2_id,on=.(rejoin_ethA_id)]]
    transport_hh_eth[is.na(missing_eth),
                 ("missing_eth"):=
                   sam_eth_hh[.SD,reshuffle_match,on=.(rejoin_eth2_id)]]
    
    transport_hh_race[is.na(missing_race),("rejoin_race2_id"):=reshuffle_ager[.SD,rejoin_race2_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[is.na(missing_race),
                 ("missing_race"):=
                   sam_race_hh[.SD,reshuffle_match,on=.(tract,rejoin_race2_id)]]
    
    #reshuffle1a
    #reshuffle$te1a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se1a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr1a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr1a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack1a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack1a <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_sr1a <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_sr1a <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffle again, this time on age
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh_race$rejoin_raceA_id <- NULL
    transport_hh_eth$rejoin_ethA_id <- NULL
    reshuffle_ager <- transport_hh_race[is.na(missing_race)]
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_w3)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,age_range_w3)]
    reshuffle_ager[,c("cnt_missing","race","own_rent","hh_size","hh_size_4","hh_size_10"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(race),list(own_rent),list(hh_size),
                                              list(hh_size_4),list(hh_size_10)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh_eth[is.na(missing_eth)]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                 by=.(tract,sex,age_range_w3)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,sex,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,age_range_w3)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","ethnicity","own_rent","hh_size","hh_size_4","hh_size_10"):=#,"track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(ethnicity),list(own_rent),list(hh_size),
                                             list(hh_size_4),list(hh_size_10)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,sex,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size_4)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,race,hh_size_4)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                        list(means_transport)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                       list(means_transport)),
                                on = .(rejoin_eth2_id)]]
    transport_hh_eth[is.na(missing_eth),
                     ("rejoin_eth2_id"):=reshuffle_agee[.SD,rejoin_eth2_id,on=.(rejoin_ethA_id)]]
    transport_hh_eth[!is.na(rejoin_eth2_id)&is.na(missing_eth),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match,on=.(rejoin_eth2_id)]]
    
    transport_hh_race[is.na(missing_race),("rejoin_race2_id"):=reshuffle_ager[.SD,rejoin_race2_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[!is.na(rejoin_race2_id)&is.na(missing_race),
                      ("missing_race"):=
                        sam_race_hh[.SD,reshuffle_match,on=.(tract,rejoin_race2_id)]]
    
    #reshuffle2a
    #reshuffle$te2a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se2a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr2a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr2a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack2a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack2a <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_sr2a <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_sr2a <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffle on hh_size
    #reshuffle again, this time on hh_size
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    transport_hh_race$rejoin_raceA_id <- NULL
    transport_hh_eth$rejoin_ethA_id <- NULL
    reshuffle_ager <- transport_hh_race[is.na(missing_race)]
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                 by=.(tract,hh_size_4)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,hh_size_4)]
    reshuffle_ager[,c("cnt_missing","race","own_rent","age_range_6","age_range_w3"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(race),list(own_rent),list(age_range_6),
                                              list(age_range_w3)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh_eth[is.na(missing_eth)]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                 by=.(tract,hh_size_4)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,hh_size_4)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","ethnicity","own_rent","age_range_6","age_range_w3"):=#,"track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(ethnicity),list(own_rent),list(age_range_6),
                                             list(age_range_w3)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,sex,own_rent,age_range_w3,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match2"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                        list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                        list(means_transport)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match2"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                       list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                       list(means_transport)),
                                on = .(rejoin_eth2_id)]]
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2_id"):=reshuffle_agee[.SD,rejoin_eth2_id,on=.(rejoin_ethA_id)]]
    transport_hh_eth[!is.na(rejoin_eth2_id) &is.na(missing_eth),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match2,on=.(rejoin_eth2_id)]]
    
    transport_hh_race[is.na(missing_race),("rejoin_race2_id"):=reshuffle_ager[.SD,rejoin_race2_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[!is.na(rejoin_race2_id) &is.na(missing_race),
                      ("missing_race"):=
                        sam_race_hh[.SD,reshuffle_match2,on=.(tract,rejoin_race2_id)]]
    
    #reshuffle3a
    #reshuffle$te3a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se3a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr3a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr3a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack3a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack3a <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_sr3a <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_sr3a <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #pull different set of potential head of hh from transport_age to try to match
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    transport_hh_eth$rejoin_eth2b1_id <- NULL
    transport_hh_race$rejoin_race2b1_id <- NULL
    transport_hh_race$rejoin_race2c1_id <- NULL
    transport_hh_eth$rejoin_eth2c1_id <- NULL
    transport_hh_race$rejoin_raceA_id <- NULL
    transport_hh_eth$rejoin_ethA_id <- NULL
    transport_age_eth_2 <- as.data.table(anti_join(transport_age_dt,transport_hh_eth[!is.na(missing_eth)],by="track_hh_id"))  
    transport_age_race_2 <- as.data.table(anti_join(transport_age_dt,transport_hh_race[!is.na(missing_race)],by="track_hh_id"))
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size_4,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_w3,hh_size_4)]
    transport_age_eth_2[,("rejoin_eth2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size_4,as.character(1000000+sample(1:.N))),
                        by=.(tract,sex,own_rent,age_range_w3,hh_size_4)]
    transport_hh_eth[is.na(missing_eth),
                     c("ethnicity"):=#,"track_hh_id"):=
                       transport_age_eth_2[.SD,c(list(ethnicity)),#,list(track_hh_id)),
                                           on=.(rejoin_eth2b1_id)]]
    transport_hh_race[is.na(missing_race),("rejoin_race2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size_4,as.character(1000000+sample(1:.N))),
                      by=.(tract,sex,own_rent,age_range_w3,hh_size_4)]
    transport_age_race_2[,("rejoin_race2b1_id") := paste0(tract,sex,own_rent,age_range_w3,hh_size_4,as.character(1000000+sample(1:.N))),
                         by=.(tract,sex,own_rent,age_range_w3,hh_size_4)]
    transport_hh_race[is.na(missing_race),
                      c("race"):=#,"track_hh_id"):=
                        transport_age_race_2[.SD,c(list(race)),#,list(track_hh_id)),
                                             on=.(rejoin_race2b1_id)]]
    #then try again, with new race/eth having just been sampled (not actually moved over yet)
    sam_race_hh$rejoin_race2c1_id <- NULL
    sam_eth_hh$rejoin_eth2c1_id <- NULL
    
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2c1_id") := paste0(tract,sex,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,sex,own_rent,age_range_w3,race,hh_size_4)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2c1_id") := paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
               by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    transport_hh_race[is.na(missing_race),("rejoin_race2c1_id") := paste0(tract,sex,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                      by=.(tract,sex,own_rent,age_range_w3,race,hh_size_4)]
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2c1_id") := paste0(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
                     by=.(tract,sex,own_rent,age_range_w3,ethnicity,hh_size_4)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                  "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match4"):=
                  transport_hh_race[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                           list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                           list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                           list(means_transport)),
                                    on = .(rejoin_race2c1_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation","commute_time","when_go_to_work",
                 "language","English_level","income_range_workers","hh_size_10","track_hh_id","reshuffle_match4"):=
                 transport_hh_eth[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                         list(occupation),list(commute_time),list(when_go_to_work),list(language),
                                         list(English_level),list(income_range),list(hh_size_10),list(track_hh_id),
                                         list(means_transport)),
                                  on = .(rejoin_eth2c1_id)]]
    transport_hh_eth[is.na(missing_eth),
                     ("missing_eth"):=sam_eth_hh[.SD,reshuffle_match4,on=.(rejoin_eth2c1_id)]]
    transport_hh_race[is.na(missing_race),
                      ("missing_race"):=sam_race_hh[.SD,reshuffle_match4,on=.(rejoin_race2c1_id)]]
    
    #reshuffle4a
    #reshuffle$te4a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se4a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr4a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr4a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack4a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack4a <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_sr4a <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_sr4a <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    rm(transport_age_eth_2)
    rm(transport_age_race_2)
    
    #reshuffle again, this time on race/eth with others resampled and dropping sex
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,race,as.character(1000000+sample(1:.N))),
                 by=.(tract,race)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,race,as.character(1000000+sample(1:.N))),
                   by=.(tract,race)]
    reshuffle_ager[,c("cnt_missing","hh_size","hh_size_4","hh_size_10","own_rent","age_range_6","age_range_w3"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(hh_size),list(hh_size_4),list(hh_size_10),list(own_rent),
                                              list(age_range_6),list(age_range_w3)),#,list(track_hh_id)),
                                  on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                 by=.(tract,ethnicity)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                   by=.(tract,ethnicity)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","hh_size","hh_size_4","hh_size_10","own_rent","age_range_6",
                                        "age_range_w3"):=#,"track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(hh_size),list(hh_size_4),list(hh_size_10),list(own_rent),
                                             list(age_range_6),list(age_range_w3)),#,list(track_hh_id)),
                                  on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth - no longer matching on sex, since so many hh don't have it; but then pull final from sex_age_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    #throws an error saying there is no such id, but if you don't do it, there are multiple matches - maybe on transport_hh is enough
    #reshuffle_ager$rejoin_race2_id <- NULL
    #reshuffle_agee$rejoin_eth2_id <- NULL
    
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,age_range_w3,race,hh_size_4)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,age_range_w3,ethnicity,hh_size_4)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,own_rent,age_range_w3,race,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,race,hh_size_4)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,ethnicity,hh_size_4)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation",
                  "commute_time","when_go_to_work","sex_transport", #use to test and match on sex at end
                  "language","English_level","income_range_workers",
                  "hh_size_10","track_hh_id","reshuffle_match3"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),
                                        list(when_go_to_work),list(sex),list(language),
                                        list(English_level),list(income_range),
                                        list(hh_size_10),list(track_hh_id),list(means_transport)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation",
                 "commute_time","when_go_to_work","sex_transport",
                 "language","English_level","income_range_workers",
                 "hh_size_10","track_hh_id","reshuffle_match3"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),
                                       list(sex),list(language),
                                       list(English_level),list(income_range),
                                       list(hh_size_10),list(track_hh_id),list(means_transport)),
                                on = .(rejoin_eth2_id)]]
    #throws an error saying there is no such id, but if you don't do it, there are multiple matches
    transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2_id"):=reshuffle_agee[.SD,rejoin_eth2_id,on=.(rejoin_ethA_id)]]
    transport_hh_eth[!is.na(rejoin_eth2_id),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match3,on=.(rejoin_eth2_id)]]
    
    transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_race[is.na(missing_race),("rejoin_race2_id"):=reshuffle_ager[.SD,rejoin_race2_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[!is.na(rejoin_race2_id),
                      ("missing_race"):=
                        sam_race_hh[.SD,reshuffle_match3,on=.(rejoin_race2_id)]]
    #reshuffle5a
    #reshuffle$te5a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se5a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr5a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr5a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack5a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack5a <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_sr5a <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_sr5a <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffle again, this time on age with others resampled and dropping sex
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager$rejoin_race2_id <- NULL
    reshuffle_agee$rejoin_eth2_id <- NULL
    transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_eth$rejoin_ethA_id <- NULL
    transport_hh_race$rejoin_raceA_id <- NULL
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                      by=.(tract,age_range_w3)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    reshuffle_ager[,c("cnt_missing","hh_size","hh_size_4","hh_size_10","own_rent","age_range_6","race"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(hh_size),list(hh_size_4),list(hh_size_10),list(own_rent),
                                              list(age_range_6),list(race)),#,list(track_hh_id)),
                                       on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                     by=.(tract,age_range_w3)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","hh_size","hh_size_4","hh_size_10","own_rent",
                                        "age_range_6","ethnicity"):=#,"track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(hh_size),list(hh_size_4),list(hh_size_10),
                                             list(own_rent),list(age_range_6),list(ethnicity)),#,list(track_hh_id)),
                                      on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth - no longer matching on sex, since so many hh don't have it; but then pull final from sex_age_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    #reshuffle_ager$rejoin_race2_id <- NULL
    #reshuffle_agee$rejoin_eth2_id <- NULL
    
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,own_rent,age_range_w3,race,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,age_range_w3,race)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,own_rent,age_range_w3,ethnicity,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,age_range_w3,ethnicity)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,own_rent,age_range_w3,race,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,race)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,own_rent,age_range_w3,ethnicity,hh_size_4,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,ethnicity)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation",
                  "commute_time","when_go_to_work","sex_transport",
                  "language","English_level","income_range_workers",
                  "track_hh_id","reshuffle_match5a"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),
                                        list(when_go_to_work),list(sex),list(language),
                                        list(English_level),list(income_range),
                                        list(track_hh_id),list(means_transport)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation",
                 "commute_time","when_go_to_work","sex_transport",
                 "language","English_level","income_range_workers",
                 "track_hh_id","reshuffle_match5a"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),
                                       list(sex),list(language),
                                       list(English_level),list(income_range),
                                       list(track_hh_id),list(means_transport)),
                                on = .(rejoin_eth2_id)]]
    #transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2_id"):=reshuffle_agee[.SD,rejoin_eth2_id,on=.(rejoin_ethA_id)]]
    transport_hh_eth[!is.na(rejoin_eth2_id) & is.na(missing_eth),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match5a,on=.(rejoin_eth2_id)]]
    
    #transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_race[is.na(missing_race),("rejoin_race2_id"):=reshuffle_ager[.SD,rejoin_race2_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[!is.na(rejoin_race2_id) & is.na(missing_race),
                      ("missing_race"):=
                        sam_race_hh[.SD,reshuffle_match5a,on=.(rejoin_race2_id)]]
    #reshuffle6a
    #reshuffle$te6a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se6a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr6a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr6a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack6a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack6a <- length(unique(sam_race_hh$track_hh_id))
    #sam_cut_off_sr6a <- table(sam_eth_hh$transport_cut_off)
    #sam_placed_sr6a <- table(sam_eth_hh[is.na(means_transport),transport_cut_off])
    
    #reshuffle again, this time on own_rent with others resampled and dropping sex and hh_size
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager$rejoin_race2_id <- NULL
    reshuffle_agee$rejoin_eth2_id <- NULL
    transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_eth$rejoin_ethA_id <- NULL
    transport_hh_race$rejoin_raceA_id <- NULL
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    #need to make sure only get as large as the hh with workers - should institute a logic for when need to do this check, above!!
    #reshuffle_ager <- reshuffle_ager[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_race_hh[!is.na(means_transport)]))]
    
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                      by=.(tract,own_rent)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent)]
    reshuffle_ager[,c("cnt_missing","hh_size","hh_size_4","hh_size_10","age_range_w3","age_range_6","race"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(hh_size),list(hh_size_4),list(hh_size_10),list(age_range_w3),
                                              list(age_range_6),list(race)),#,list(track_hh_id)),
                                       on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    #need to make sure only get as large as the hh with workers - should institute a logic for when need to do this check, above!!
    #reshuffle_agee <- reshuffle_agee[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_eth_hh[!is.na(means_transport)]))]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                     by=.(tract,own_rent)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,own_rent,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","hh_size","hh_size_4","hh_size_10","age_range_w3",
                                        "age_range_6","ethnicity"):=#,"track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(hh_size),list(hh_size_4),list(hh_size_10),
                                             list(age_range_w3),list(age_range_6),list(ethnicity)),#,list(track_hh_id)),
                                      on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth - no longer matching on sex, since so many hh don't have it; but then pull final from sex_age_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    #reshuffle_ager$rejoin_race2_id <- NULL
    #reshuffle_agee$rejoin_eth2_id <- NULL
    
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,own_rent,age_range_w3,race,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,age_range_w3,race)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,own_rent,age_range_w3,ethnicity,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,age_range_w3,ethnicity)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,own_rent,age_range_w3,race,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,race)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,own_rent,age_range_w3,ethnicity,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,ethnicity)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation",
                  "commute_time","when_go_to_work","sex_transport",
                  "language","English_level","income_range_workers",
                  "track_hh_id","reshuffle_match6a"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),
                                        list(when_go_to_work),list(sex),list(language),
                                        list(English_level),list(income_range),
                                        list(track_hh_id),list(means_transport)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation",
                 "commute_time","when_go_to_work","sex_transport",
                 "language","English_level","income_range_workers",
                 "track_hh_id","reshuffle_match6a"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),
                                       list(sex),list(language),
                                       list(English_level),list(income_range),
                                       list(track_hh_id),list(means_transport)),
                                on = .(rejoin_eth2_id)]]
    #transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2_id"):=reshuffle_agee[.SD,rejoin_eth2_id,on=.(rejoin_ethA_id)]]
    transport_hh_eth[!is.na(rejoin_eth2_id) & is.na(missing_eth),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match6a,on=.(rejoin_eth2_id)]]
    
    #transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_race[is.na(missing_race),("rejoin_race2_id"):=reshuffle_ager[.SD,rejoin_race2_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[!is.na(rejoin_race2_id) & is.na(missing_race),
                      ("missing_race"):=
                        sam_race_hh[.SD,reshuffle_match6a,on=.(rejoin_race2_id)]]
    #reshuffle7a
    #reshuffle$te7a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se7a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr7a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr7a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack7a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack7a <- length(unique(sam_race_hh$track_hh_id))
    
    #reshuffle again, this time on race/eth with others resampled and dropping sex and hh_size
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager$rejoin_race2_id <- NULL
    reshuffle_agee$rejoin_eth2_id <- NULL
    transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_eth$rejoin_ethA_id <- NULL
    transport_hh_race$rejoin_raceA_id <- NULL
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    reshuffle_ager <- transport_hh[is.na(missing_race)]
    #need to make sure only get as large as the hh with workers - should institute a logic for when need to do this check, above!!
    #reshuffle_ager <- reshuffle_ager[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_race_hh[!is.na(means_transport)]))]
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,race,as.character(1000000+sample(1:.N))),
                      by=.(tract,race)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,race,as.character(1000000+sample(1:.N))),
                   by=.(tract,race)]
    reshuffle_ager[,c("cnt_missing","hh_size","hh_size_4","hh_size_10","age_range_w3","age_range_6","own_rent"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(hh_size),list(hh_size_4),list(hh_size_10),list(age_range_w3),
                                              list(age_range_6),list(own_rent)),#,list(track_hh_id)),
                                       on=.(rejoin_raceA_id)]]
    reshuffle_agee <- transport_hh[is.na(missing_eth)]
    #need to make sure only get as large as the hh with workers - should institute a logic for when need to do this check, above!!
    #reshuffle_agee <- reshuffle_agee[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_eth_hh[!is.na(means_transport)]))]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                     by=.(tract,ethnicity)]
    reshuffle_agee[,("rejoin_ethA_id") := paste0(tract,ethnicity,as.character(1000000+sample(1:.N))),
                   by=.(tract,ethnicity)]
    reshuffle_agee[is.na(missing_eth),c("cnt_missing","hh_size","hh_size_4","hh_size_10","age_range_w3",
                                        "age_range_6","own_rent","track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(hh_size),list(hh_size_4),list(hh_size_10),list(age_range_w3),
                                             list(age_range_6),list(own_rent),list(track_hh_id)),
                                      on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth - no longer matching on sex, since so many hh don't have it; but then pull final from sex_age_race/eth
    sam_race_hh$rejoin_race2_id <- NULL
    sam_eth_hh$rejoin_eth2_id <- NULL
    #reshuffle_ager$rejoin_race2_id <- NULL
    #reshuffle_agee$rejoin_eth2_id <- NULL
    
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race2_id") := 
                  paste0(tract,own_rent,age_range_w3,race,as.character(1000000+sample(1:.N))),
                by=.(tract,own_rent,age_range_w3,race)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth2_id") := 
                 paste0(tract,own_rent,age_range_w3,ethnicity,as.character(1000000+sample(1:.N))),
               by=.(tract,own_rent,age_range_w3,ethnicity)]
    reshuffle_ager[,("rejoin_race2_id") := 
                     paste0(tract,own_rent,age_range_w3,race,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,race)]
    reshuffle_agee[,("rejoin_eth2_id") := 
                     paste0(tract,own_rent,age_range_w3,ethnicity,as.character(1000000+sample(1:.N))),
                   by=.(tract,own_rent,age_range_w3,ethnicity)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation",
                  "commute_time","when_go_to_work","sex_transport",
                  "language","English_level","income_range_workers",
                  "track_hh_id","reshuffle_match7"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),
                                        list(when_go_to_work),list(sex),list(language),
                                        list(English_level),list(income_range),
                                        list(track_hh_id),list(means_transport)),
                                 on = .(rejoin_race2_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation",
                 "commute_time","when_go_to_work","sex_transport",
                 "language","English_level","income_range_workers",
                 "track_hh_id","reshuffle_match7"):=
                 reshuffle_agee[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),
                                       list(sex),list(language),
                                       list(English_level),list(income_range),
                                       list(track_hh_id),list(means_transport)),
                                on = .(rejoin_eth2_id)]]
    #transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_eth[is.na(missing_eth),("rejoin_eth2_id"):=reshuffle_agee[.SD,rejoin_eth2_id,on=.(rejoin_ethA_id)]]
    transport_hh_eth[!is.na(rejoin_eth2_id) & is.na(missing_eth),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match7,on=.(rejoin_eth2_id)]]
    
    #transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_race[is.na(missing_race),("rejoin_race2_id"):=reshuffle_ager[.SD,rejoin_race2_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[!is.na(rejoin_race2_id) & is.na(missing_race),
                      ("missing_race"):=
                        sam_race_hh[.SD,reshuffle_match7,on=.(rejoin_race2_id)]]
    #reshuffle8a
    #reshuffle$te8a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se8a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr8a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr8a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack8a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack8a <- length(unique(sam_race_hh$track_hh_id))
    
    #reshuffle again, this time on age with others resampled and dropping sex, hh_size, race/eth and own_rent
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    reshuffle_ager$rejoin_race2_id <- NULL
    reshuffle_agee$rejoin_eth2_id <- NULL
    transport_hh_eth$rejoin_eth2_id <- NULL
    transport_hh_race$rejoin_race2_id <- NULL
    transport_hh_eth$rejoin_ethA_id <- NULL
    transport_hh_race$rejoin_raceA_id <- NULL
    rm(reshuffle_agee)
    rm(reshuffle_ager)
    reshuffle_ager <- transport_hh_race[is.na(missing_race)]
    #need to make sure only get as large as the hh with workers - should institute a logic for when need to do this check, above!!
    #reshuffle_ager <- reshuffle_ager[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_race_hh[!is.na(means_transport)]))]
    transport_hh_race[is.na(missing_race),("rejoin_raceA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                      by=.(tract,age_range_w3)]
    reshuffle_ager[,("rejoin_raceA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    reshuffle_ager[,c("cnt_missing","hh_size","hh_size_4","hh_size_10","race","own_rent"):=#,"track_hh_id"):=
                     transport_hh_race[.SD, c(list(race),list(hh_size),list(hh_size_4),list(hh_size_10),
                                              list(race),list(own_rent)),#,list(track_hh_id)),
                                       on=.(rejoin_raceA_id)]]
    reshuffle_agee3 <- transport_hh_eth[is.na(missing_eth)]
    #need to make sure only get as large as the hh with workers - should institute a logic for when need to do this check, above!!
    #reshuffle_agee <- reshuffle_agee[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_eth_hh[!is.na(means_transport)]))]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                     by=.(tract,age_range_w3)]
    reshuffle_agee3[,("rejoin_ethA_id") := paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    reshuffle_agee3[,c("cnt_missing","hh_size","hh_size_4","hh_size_10","ethnicity","own_rent"):=#,"track_hh_id"):=
                     transport_hh_eth[.SD, c(list(ethnicity),list(hh_size),list(hh_size_4),list(hh_size_10),
                                             list(ethnicity),list(own_rent)),#,list(track_hh_id)),
                                      on=.(rejoin_ethA_id)]]
    
    #Add to sam_race/eth - no longer matching on sex, since so many hh don't have it; but then pull final from sex_age_race/eth
    #sam_race_hh$rejoin_race2_id <- NULL
    #sam_eth_hh$rejoin_eth2_id <- NULL
    #reshuffle_ager$rejoin_race2_id <- NULL
    #reshuffle_agee$rejoin_eth2_id <- NULL
    
    sam_race_hh[transport_cut_off==TRUE,("rejoin_race3_id") := 
                  paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                by=.(tract,age_range_w3)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_eth3_id") := 
                 paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
               by=.(tract,age_range_w3)]
    reshuffle_ager[,("rejoin_race3_id") := 
                     paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    reshuffle_agee3[,("rejoin_eth3_id") := 
                     paste0(tract,age_range_w3,as.character(1000000+sample(1:.N))),
                   by=.(tract,age_range_w3)]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation",
                  "commute_time","when_go_to_work","sex_transport",
                  "language","English_level","income_range_workers",
                  "track_hh_id","reshuffle_match8a"):=
                  reshuffle_ager[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),
                                        list(when_go_to_work),list(sex),list(language),
                                        list(English_level),list(income_range),
                                        list(track_hh_id),list(means_transport)),
                                 on = .(rejoin_race3_id)]]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation",
                 "commute_time","when_go_to_work","sex_transport",
                 "language","English_level","income_range_workers",
                 "track_hh_id","reshuffle_match8a"):=
                 reshuffle_agee3[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),
                                       list(sex),list(language),
                                       list(English_level),list(income_range),
                                       list(track_hh_id),list(means_transport)),
                                on = .(rejoin_eth3_id)]]
    #transport_hh_eth$rejoin_eth3_id <- NULL
    #reshuffle_agee[!is.na(rejoin_eth3_id),("missed_eth"):=sam_eth_hh[.SD,reshuffle_match8a,on=.(rejoin_eth3_id)]]
    transport_hh_eth[,c("rejoin_eth3_id"):=reshuffle_agee3[.SD,c(list(rejoin_eth3_id)),on=.(rejoin_ethA_id)]]
    transport_hh_eth[!is.na(rejoin_eth3_id) & is.na(missing_eth),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match8a,on=.(rejoin_eth3_id)]]
    
    #transport_hh_race$rejoin_race3_id <- NULL
    #reshuffle_ager[!is.na(rejoin_race2_id),("missed_race"):=sam_eth_hh[.SD,reshuffle_match8a,on=.(rejoin_eth2_id)]]
    transport_hh_race[,("rejoin_race3_id"):=reshuffle_ager[.SD,rejoin_race3_id,on=.(rejoin_raceA_id)]]
    transport_hh_race[!is.na(rejoin_race3_id) & is.na(missing_race),
                      ("missing_race"):=
                        sam_race_hh[.SD,means_transport,on=.(rejoin_race3_id)]] #worked by going back to means_transport, not reshuffle_match8a???
    #reshuffle9a
    #reshuffle$te9a <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$se9a <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$tr9a <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$sr9a <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrack9a <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrack9a <- length(unique(sam_race_hh$track_hh_id))
    
    #final
    #put in cut off
    sam_eth_hh$sample_cut_off <- NULL
    sam_eth_hh$transport_cut_off <- NULL
    sam_race_hh$sample_cut_off <- NULL
    sam_race_hh$transport_cut_off <- NULL
    sam_eth_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_eth_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    sam_race_hh[is.na(means_transport),("sample_cut_off"):=sample(1:.N),by=.(tract)]
    sam_race_hh[is.na(means_transport),("transport_cut_off"):=if_else((sample_cut_off-no_workers)>0,TRUE,FALSE),by=.(tract)]
    #reshuffle transport
    sam_race_hh[transport_cut_off==TRUE,("rejoin_raceF_id") := 
                  #paste0(tract,as.character(1000000+sample(1:.N))),
                  paste0(tract,as.character(1000000+seq.int(1:.N))),
                by=.(tract)]
    sam_eth_hh[transport_cut_off==TRUE,("rejoin_ethF_id") := 
                 #paste0(tract,as.character(1000000+sample(1:.N))),
                 paste0(tract,as.character(1000000+seq.int(1:.N))),
               by=.(tract)]
    transport_hh_race[is.na(missing_race),("rejoin_raceF_id") := 
                        #paste0(tract,as.character(1000000+sample(1:.N))),
                        paste0(tract,as.character(1000000+seq.int(1:.N))),
                   by=.(tract)]
    transport_hh_eth[is.na(missing_eth),("rejoin_ethF_id") := 
                       #paste0(tract,as.character(1000000+sample(1:.N))),
                       paste0(tract,as.character(1000000+seq.int(1:.N))),
                   by=.(tract)]
    
    ###get this reshuffle_ager1 to be only size left to get right number of workers in hh
    reshuffle_ager1 <- transport_hh_race[is.na(missing_race)]
    #reshuffle_ager1 <- reshuffle_ager1[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_race_hh[!is.na(means_transport)]))]
    sam_race_hh[transport_cut_off==TRUE,
                c("means_transport","number_workers_in_hh","industry","occupation",
                  "commute_time","when_go_to_work","sex_transport",
                  "language","English_level","income_range_workers",
                  "track_hh_id","reshuffle_match9"):=
                  reshuffle_ager1[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                        list(occupation),list(commute_time),
                                        list(when_go_to_work),list(sex),list(language),
                                        list(English_level),list(income_range),
                                        list(track_hh_id),list(means_transport)),
                                 on = .(rejoin_raceF_id)]]
    reshuffle_agee1 <- transport_hh_eth[is.na(missing_eth)]
    #reshuffle_agee1 <- reshuffle_agee1[sample(nrow(hh_workers[number_workers_in_hh!="00 workers"])-nrow(sam_eth_hh[!is.na(means_transport)]))]
    sam_eth_hh[transport_cut_off==TRUE,
               c("means_transport","number_workers_in_hh","industry","occupation",
                 "commute_time","when_go_to_work","sex_transport",
                 "language","English_level","income_range_workers",
                 "track_hh_id","reshuffle_match9"):=
                 reshuffle_agee1[.SD, c(list(means_transport),list(number_workers_in_hh),list(industry),
                                       list(occupation),list(commute_time),list(when_go_to_work),
                                       list(sex),list(language),
                                       list(English_level),list(income_range),
                                       list(track_hh_id),list(means_transport)),
                                on = .(rejoin_ethF_id)]]
    transport_hh_eth[!is.na(rejoin_ethF_id) & is.na(missing_eth),
                     ("missing_eth"):=
                       sam_eth_hh[.SD,reshuffle_match9,on=.(rejoin_ethF_id)]]
    
    transport_hh_race[!is.na(rejoin_raceF_id) & is.na(missing_race),
                      ("missing_race"):=
                        sam_race_hh[.SD,reshuffle_match9,on=.(rejoin_raceF_id)]]
    #reshuffleFinal
    #reshuffle$teFinal <-nrow(transport_hh_eth[!is.na(missing_eth)])
    #reshuffle$seFinal <- nrow(sam_eth_hh[!is.na(means_transport)])
    #reshuffle$trFinal <-nrow(transport_hh_race[!is.na(missing_race)])
    #reshuffle$srFinal <- nrow(sam_race_hh[!is.na(means_transport)])
    #reshuffle$etrackF <- length(unique(sam_eth_hh$track_hh_id))
    #reshuffle$rtrackF <- length(unique(sam_race_hh$track_hh_id))
    #both eth/race came up with 1274050 - not 1289903 : very odd; last one was just a sort on tract after all....
    #could not get track_hh_id to add up properly - need to use leftover from transport_eth/race_hh
    #it's an interesting case to think through - I think what happens is that on the shuffles, it's unclear what counts as the right
    #identity for each row, and what I ended up doing was just not trying to track it back and forth 
    
    
    #assign workers for leftovers
    sam_eth_hh[is.na(number_workers_in_hh),("number_workers_in_hh"):="00 workers"]
    sam_race_hh[is.na(number_workers_in_hh),("number_workers_in_hh"):="00 workers"]
    sam_eth_hh[number_workers_in_hh=="03 or more workers",("number_workers_in_hh"):="03 workers"]
    sam_race_hh[number_workers_in_hh=="03 or more workers",("number_workers_in_hh"):="03 workers"]
    #could get fancier with number of workers and hh_workers, but not sure it's worth it, esp given that original has something weird on 3-person
    sam_eth_hh[as.numeric(substr(number_workers_in_hh,1,2))>as.numeric(substr(hh_size_4,1,1)),
               ("number_workers_in_hh"):="01 worker"]
    sam_race_hh[as.numeric(substr(number_workers_in_hh,1,2))>as.numeric(substr(hh_size_4,1,1)),
               ("number_workers_in_hh"):="01 worker"]
    sam_eth_hh[is.na(sex),("sex"):=sex_transport]
    sam_race_hh[is.na(sex),("sex"):=sex_transport]
    
    #clean up things for non-householders on the additional_workers - stop matching on household_id, too; can regenerate from sam_hh
    #or just move over from wives/partners and then from other_relatives, etc.?
    
    #vehicles_hh and vehicles_workers 
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
    sam_eth_hh[,("hh_size_4v"):=if_else(as.numeric(substr(hh_size_vehicle,1,1))>3,"4-or-more-person household",hh_size_vehicle)]
    sam_race_hh[,("hh_size_4v"):=if_else(as.numeric(substr(hh_size_vehicle,1,1))>3,"4-or-more-person household",hh_size_vehicle)]
    
    vehicles_hh[,("vehicle_id") := paste0(tract,hh_size_4,number_workers_in_hh,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4,number_workers_in_hh)]
    sam_eth_hh[,("vehicle_id") := paste0(tract,hh_size_4v,match_workers,as.character(1000000+sample(1:.N))),
               by=.(tract,hh_size_4v,match_workers)]
    sam_race_hh[,("vehicle_id") := paste0(tract,hh_size_4v,match_workers,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4v,match_workers)]
    sam_eth_hh[,("number_vehicles_hh"):=vehicles_hh[.SD,list(number_vehicles_in_hh),on=.(vehicle_id)]]
    sam_race_hh[,("number_vehicles_hh"):=vehicles_hh[.SD,list(number_vehicles_in_hh),on=.(vehicle_id)]]
    vehicles_hh[,("missing_eth"):=sam_eth_hh[.SD,list(number_vehicles_hh),on=.(vehicle_id)]]
    vehicles_hh[,("missing_race"):=sam_race_hh[.SD,list(number_vehicles_hh),on=.(vehicle_id)]]
    #nrow(vehicles_hh[is.na(missing_race)])/nrow(vehicles_hh) #0.1105
    
    #rest just on hh_size_4
    vehicles_hh[is.na(missing_eth),("vehicle2_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4)]
    sam_eth_hh[is.na(number_vehicles_hh),("vehicle2_id") := paste0(tract,hh_size_4v,as.character(1000000+sample(1:.N))),
               by=.(tract,hh_size_4v)]
    vehicles_hh[is.na(missing_race),("vehicle2r_id") := paste0(tract,hh_size_4,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4)]
    sam_race_hh[is.na(number_vehicles_hh),("vehicle2r_id") := paste0(tract,hh_size_4v,as.character(1000000+sample(1:.N))),
                by=.(tract,hh_size_4v)]
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
    hh_kids[order(match(kid_age,c("No children","Under 6 years only","6 to 17 years only","Under 6 years and 6 to 17 years"))),
            ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_eth_hh[order(-age_range_6),
           ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_eth_hh[family=="Family households",c("kids_by_age") := hh_kids[.SD, list(kid_age), on = .(hh_kids_id)]] #may have more than 1 in other categories
    sam_eth_hh$hh_kids_id <- NULL
    sam_race_hh[order(-age_range_6),
               ("hh_kids_id"):=paste0(tract,family_role,as.character(1000000+seq.int(1:.N))),by=.(tract,family_role)]
    sam_race_hh[family=="Family households",c("kids_by_age") := hh_kids[.SD, list(kid_age), on = .(hh_kids_id)]] #may have more than 1 in other categories
    sam_race_hh$hh_kids_id <- NULL
    #comparing just two because kids_by_age has different number of tracts
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
    #sam_eth_hh[number_workers_in_hh=="No workers",("number_workers_in_hh"):="0 workers"]
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
    #sam_race_hh[number_workers_in_hh=="No workers",("number_workers_in_hh"):="0 workers"]
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
    #seems not to have gotten very many more at all - additional 100k; 250k to go.
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
    
    #think through how this assigns. Looks like something odd about tracts, so allowing across tracts?
    family_employment_dt[is.na(missing_eth),
                         ("employ6_id"):=paste0(min_workers,family_role,as.character(1000000+sample(1:.N))),
                         by=.(min_workers,family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               ("employ6_id"):=paste0(min_workers,family_role,as.character(1000000+sample(1:.N))),
               by=.(min_workers,family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ6_id)]]
    family_employment_dt[is.na(missing_eth),
                         ("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ6_id)]]
    family_employment_dt[is.na(missing_race),
                         ("employ7_id"):=paste0(min_workers,family_role,as.character(1000000+sample(1:.N))),
                         by=.(min_workers,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                ("employ7_id"):=paste0(min_workers,family_role,as.character(1000000+sample(1:.N))),
                by=.(min_workers,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                  family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                              list(single_hh_employ),list(name)),
                                       on = .(employ7_id)]]
    family_employment_dt[is.na(missing_race),
                         ("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ7_id)]]
    #still missing ~20k!!!!! Something is weird here!! looks like min_workers don't match, but it's the sam side that has more
    #family_roles are distributed, with only married couples being a problem
    family_employment_dt[is.na(missing_eth),
                         ("employ8_id"):=paste0(family_role,as.character(1000000+sample(1:.N))),
                         by=.(family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               ("employ8_id"):=paste0(family_role,as.character(1000000+sample(1:.N))),
               by=.(family_role)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ8_id)]]
    family_employment_dt[is.na(missing_eth),
                         ("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ8_id)]]
    family_employment_dt[is.na(missing_race),
                         ("employ9_id"):=paste0(family_role,as.character(1000000+sample(1:.N))),
                         by=.(family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                ("employ9_id"):=paste0(family_role,as.character(1000000+sample(1:.N))),
                by=.(family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                  family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                              list(single_hh_employ),list(name)),
                                       on = .(employ9_id)]]
    family_employment_dt[is.na(missing_race),
                         ("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ9_id)]]
    
    #sample last few from whole, to fill out
    family_employment_dt[,
                         ("employ10_id"):=paste0(tract,family_role,min_workers,as.character(1000000+sample(1:.N))),
                         by=.(tract,family_role,min_workers)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               ("employ10_id"):=paste0(tract,family_role,min_workers,as.character(1000000+sample(1:.N))),
               by=.(tract,family_role,min_workers)]
    sam_eth_hh[family=="Family households"&is.na(miss_tracker),
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(employ10_id)]]
    #family_employment_dt[is.na(missing_eth),
    #                     ("missing_eth"):=sam_eth_hh[.SD, list(miss_tracker), on = .(employ8_id)]]
    family_employment_dt[,
                         ("employ11_id"):=paste0(tract,family_role,as.character(1000000+sample(1:.N))),
                         by=.(tract,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                ("employ11_id"):=paste0(tract,family_role,as.character(1000000+sample(1:.N))),
                by=.(tract,family_role)]
    sam_race_hh[family=="Family households"&is.na(miss_tracker),
                c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                  family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                              list(single_hh_employ),list(name)),
                                       on = .(employ11_id)]]
    #family_employment_dt[is.na(missing_race),
    #                     ("missing_race"):=sam_race_hh[.SD, list(miss_tracker), on = .(employ11_id)]]
    #what are the things that determine employment? can I fix them post_hoc??? and 4384 missing from married couples in census file
    #there's a bunch of odd small number cases, fixed here
    #change to real na in families_generator - should clean up later
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
    
    #some of below throw errors that don't hurt anything because there aren't any that match the NA
    family_employment_dt[family_role=="Married-couple family",("husband_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[husband_employed=="NA",
               ("husband_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[husband_employed=="NA",
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(husband_employ_id)]]
    sam_race_hh[husband_employed=="NA",
               ("husband_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_race_hh[husband_employed=="NA",
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(husband_employ_id)]]
    family_employment_dt[wife_employed=="NA",("wife_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
                         by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[wife_employed=="NA",
               ("wife_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_eth_hh[wife_employed=="NA",
               c("husband_employed","wife_employed","single_hh_employed","miss_tracker") := 
                 family_employment_dt[.SD, c(list(husband_employed),list(wife_employed),
                                             list(single_hh_employ),list(name)),
                                      on = .(wife_employ_id)]]
    sam_race_hh[wife_employed=="NA",
               ("wife_employ_id"):=paste0(tract,min_workers,family_role,own_kids,as.character(1000000+sample(1:.N))),
               by=.(tract,min_workers,family_role,own_kids)]
    sam_race_hh[wife_employed=="NA",
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
               ("partner_type_id"):=paste0(tract,sex,as.character(1000000+sample(1:.N))),by=.(tract,sex)]
    sam_eth_hh[family_type=="Other family",
               c("partner_type","sex_partner","hh_sex") := 
                 hh_partner_dt[.SD, c(list(partner_type),list(sex_partner),list(sex)), on = .(partner_type_id)]]
    sam_race_hh[family_type=="Other family",# lose 2627 of 92975 in first pass - not sure where else to put them
                ("partner_type_id"):=paste0(tract,sex,as.character(1000000+sample(1:.N))),by=.(tract,sex)]
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
#check
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
    
    #put in a couple of others before expanding - have to do some checks on when units built, etc.
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
    
    #clean up sams
    
    #create households from within info on relations for each hh, and then rematch with what is known about workers
    
    #encode hh relation
    sam_eth_hh[,("inhousehold_id"):=paste0(household_id,"_01")]
    sam_race_hh[,("inhousehold_id"):=paste0(household_id,"_01")]
    sam_eth_hh[,("role_in_family"):=list("Householder")]
    sam_race_hh[,("role_in_family"):=list("Householder")]
    
    #workers lose about 4k; if hh_id had moved perfectly, it would be anti_join with sam_eth/race
    additional_workers_eth <- as.data.table(anti_join(transport_age_dt,transport_hh_eth,by="track_hh_id"))
    additional_workers_race <- as.data.table(anti_join(transport_age_dt,transport_hh_race,by="track_hh_id"))
    
    #add language and English level for folks not in labor force....
    #not sure best way
    #sex still not assigned for 156997 non-family households
    
    sam_eth_hh <- sam_eth_hh[,c("household_id","inhousehold_id","tract","sex","ethnicity","age_range_6","own_rent","family","family_type",
                                "family_role","family_role_4","single_hh_sex","householder_age","householder_age_9",
                                "housing_units","housing_units_6","num_structures","householder_age_3","people_per_room",
                                "hh_size","hh_size_4","hh_size_10","num_rooms","num_bedrooms","when_moved_in",
                                "means_transport","number_workers_in_hh","industry","occupation",
                                "commute_time","when_go_to_work","language","English_level","income_range_workers",
                                "number_vehicles_hh","kids_by_age","own_kids",
                                "husband_employed","wife_employed","single_hh_employed",
                                "partner_type","sex_partner","role_in_family")]
    sam_race_hh <- sam_race_hh[,c("household_id","inhousehold_id","tract","sex","race","age_range_6","own_rent","family","family_type",
                                  "family_role","family_role_4","single_hh_sex","householder_age","householder_age_9",
                                  "housing_units","housing_units_6","num_structures","householder_age_3","people_per_room",
                                  "hh_size","hh_size_4","hh_size_10","num_rooms","num_bedrooms","when_moved_in",
                                  "means_transport","number_workers_in_hh","industry","occupation",
                                  "commute_time","when_go_to_work","language","English_level","income_range_workers",
                                  "number_vehicles_hh","kids_by_age","own_kids",
                                  "husband_employed","wife_employed","single_hh_employed",
                                  "partner_type","sex_partner","role_in_family")]
    
    #save working files
    saveRDS(sam_eth_hh,file = paste0(housingdir, vintage, "/sam_eth_hh_",Sys.Date(),".RDS"))
    saveRDS(sam_race_hh,file = paste0(housingdir, vintage, "/sam_race_hh_",Sys.Date(),".RDS"))
    saveRDS(additional_workers_eth,file = paste0(housingdir, vintage, "/additional_workers_eth_",Sys.Date(),".RDS"))
    saveRDS(additional_workers_race,file = paste0(housingdir, vintage, "/additional_workers_race_",Sys.Date(),".RDS"))
    
  }   
}
    






