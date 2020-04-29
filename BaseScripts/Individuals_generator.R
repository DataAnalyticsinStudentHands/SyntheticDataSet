#https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf

##SEPARATE OUT INTO A FUNCTION CALL???

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
#pregnant should count as having one child - maybe give them birthdates??
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

#could put in published stuff correlating preg and educ, but I think the tract level stuff is carrying more info
#add educ to sex_age_race, in order to match better with hh
sex_age_race[age>17 & order(age),
             ("educ_id"):=paste0(tract,sex,
                                 as.character(1000000+seq.int(1:.N))),
             by=.(tract,sex)]
sex_age_educ_dt[order(age),
                ("educ_id"):=paste0(tract,sex,
                                    as.character(1000000+seq.int(1:.N))),
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
#add ids for matching
#do the sam_hh where the sex is known - there's some very strange things with the matching; this gets a majority.
sex_age_race[,("hh_join_id"):=paste0(tract,sex,race,ethnicity,householder_age_9,education_4,
                                    as.character(1000000+sample(.N))),
             by=.(tract,sex,race,ethnicity,householder_age_9,education_4)]
sam_hh[,("hh_join_id"):=paste0(tract,if_else(is.na(sex),"none",sex),race,ethnicity,householder_age_9,hh_education_level,
                                     as.character(1000000+sample(.N))),
             by=.(tract,is.na(sex),race,ethnicity,householder_age_9,hh_education_level)]
#move to each side - householder_id, individual_id, some info for later matching...
sam_hh[,c("individual_id","sex","marital_status","spouse_present","separated") := 
         sex_age_race[.SD, c(list(individual_id),list(sex),list(marital_status),list(spouse_present),list(separated)), on = .(hh_join_id)]]

sex_age_race[age>14,
             c("household_id","hh_income_level","kids_by_age","number_workers_in_hh","number_vehicles_in_hh") := 
               sam_hh[.SD, c(list(household_id),list(hh_income_level),list(kids_by_age),list(number_workers_in_hh),list(number_vehicles_in_hh)), 
                      on = .(hh_join_id)]]
sam_hh$hh_join_id <- NULL
sex_age_race$hh_join_id <- NULL
#then pick up others, with bias toward putting males as householders (following census; but not absolute)
sex_age_race_1_hh <- sex_age_race[is.na(household_id)] #since I can't get the i to order and to subset
sex_age_race_1_hh[order(-sex),
#sex_age_race[is.na(household_id),
             ("second_join_id"):=paste0(tract,householder_age_9,education_4,
                                     as.character(1000000+seq.int(1:.N))),
             by=.(tract,householder_age_9,education_4)]
sam_hh[is.na(individual_id),("second_join_id"):=paste0(tract,householder_age_9,hh_education_level,
                               as.character(1000000+seq.int(1:.N))),
       by=.(tract,householder_age_9,hh_education_level)]
sam_hh[is.na(individual_id),c("individual_id","sex","race","ethnicity","marital_status","spouse_present","separated") := 
         sex_age_race_1_hh[.SD, c(list(individual_id),list(sex),list(race),list(ethnicity),
                                  list(marital_status),list(spouse_present),list(separated)), on = .(second_join_id)]]

sex_age_race[age>14 & is.na(household_id),
             c("household_id","hh_income_level","kids_by_age","number_workers_in_hh","number_vehicles_in_hh") := 
               sam_hh[.SD, c(list(household_id),list(hh_income_level),list(kids_by_age),list(number_workers_in_hh),list(number_vehicles_in_hh)), 
                             on = .(individual_id)]]
rm(sex_age_race_1_hh)
sam_hh$second_join_id <- NULL
#sex_age_race$second_join_id <- NULL
#pick up last 100k on education_level
sex_age_race[is.na(household_id),
             ("third_join_id"):=paste0(tract,education_4,
                                        as.character(1000000+sample(.N))),
             by=.(tract,education_4)]
sam_hh[is.na(individual_id),("third_join_id"):=paste0(tract,hh_education_level,
                                                       as.character(1000000+sample(.N))),
       by=.(tract,hh_education_level)]
sam_hh[is.na(individual_id),c("individual_id","sex","race","ethnicity","marital_status","spouse_present","separated") := 
         sex_age_race[.SD, c(list(individual_id),list(sex),list(race),list(ethnicity),
                             list(marital_status),list(spouse_present),list(separated)), on = .(third_join_id)]]

sex_age_race[age>14 & is.na(household_id),
             c("household_id","hh_income_level","kids_by_age","number_workers_in_hh","number_vehicles_in_hh") := 
               sam_hh[.SD, c(list(household_id),list(hh_income_level),list(kids_by_age),list(number_workers_in_hh),list(number_vehicles_in_hh)), 
                      on = .(individual_id)]]
sam_hh$third_join_id <- NULL
sex_age_race$third_join_id <- NULL
#should either move 
#test nrow(sam_hh[is.na(individual_id)])==0 ; nrow(sex_age_race[!is.na(household_id)])==nrow(sam_hh)

#get relations info back and forth with sam_hh
sex_age_race[!is.na(household_id),("relation_householder"):="self"]




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

