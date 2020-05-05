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
    
    #merge the ones with race first then the ones with ethnicity, then do a merge on all those factors for ethnicity back to race...
    sex_age_race[,"individual_id":=paste0(tract,as.character(2000000+sample(.N))),by=.(tract)]
    
    
    #put marital_status temp on sex_age_race by sex and race; only there for comparison
    #put age on marital_status_race; then use that to match with marital_status_age, then put final marital_status, etc. on
    #add more age categories and race to marital_status_age and preg_age, for better matching
    #move preg data to sex_age_race - sort by age, since it's not even on age_ranges, and sampling for age isn't exact for sex_age_race
    sex_age_race[age>14, ("married_id"):=paste0(tract,sex,race,
                                                as.character(1000000+seq.int(1:.N))),
                 by=.(tract,sex,race)]
    marital_status_race_dt[,("married_id"):=paste0(tract,sex,race,
                                                   as.character(1000000+sample(.N))),
                           by=.(tract,sex,race)]
    sex_age_race[,c("marital_status_tmp"):=
                   marital_status_race_dt[.SD, list(marital_status), on = .(married_id)]]
    marital_status_race_dt[,c("age"):=
                             sex_age_race[.SD, list(age), on = .(married_id)]]
    sex_by_age_eth[age>14,("married_id"):=paste0(tract,sex,ethnicity,
                                                 as.character(1000000+seq.int(1:.N))),
                   by=.(tract,sex,ethnicity)]
    marital_status_eth_dt[,("married_id"):=paste0(tract,sex,ethnicity,
                                                  as.character(1000000+sample(.N))),
                          by=.(tract,sex,ethnicity)]
    sex_by_age_eth[,c("marital_status_tmp"):=
                     marital_status_eth_dt[.SD, list(marital_status), on = .(married_id)]]
    marital_status_eth_dt[,c("age"):=
                            sex_by_age_eth[.SD, list(age), on = .(married_id)]] 
    sex_age_race$married_id <- NULL
    sex_by_age_eth$married_id <- NULL
    #test <- nrow(sex_age_race[!is.na(marital_status_tmp)]) == nrow(marital_status_age_dt)
    
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
    sex_by_age_eth[order(age) & !is.na(marital_status_tmp),
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
    marital_status_race_dt[,("married2_id"):=NULL]
    marital_status_eth_dt[,("married3_id"):=NULL]
    #test <- table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$marital_status,marital_status_age_dt$spouse_present,marital_status_age_dt$separated,marital_status_age_dt$age_range_marital)==
    # table(marital_status_race_dt$tract,marital_status_race_dt$sex,marital_status_race_dt$marital_status,marital_status_race_dt$spouse_present,marital_status_race_dt$separated,marital_status_race_dt$age_range_marital)
    #length(test[test==FALSE])==0
    
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
    #test <- nrow(marital_status_eth_dt[!is.na(pregnant)]) == nrow(preg_eth_dt)
    #test <- nrow(marital_status_race_dt[!is.na(pregnant)]) == nrow(preg_race_dt)
    
    #adding relation data before matching back on eth and race
    #add to sex_age_race, before getting householders out??
    
    
    #add marital_status, based on race
    #catch the subset who match
    #marital_status_race_dt[,("family_role_3"):=if_else(marital_status=="Now married","In married-couple family","not determined")]
    #marital_status_eth_dt[,("family_role_3"):=if_else(marital_status=="Now married","In married-couple family","not determined")]
    
    #family_or_non matches on hh_relations_dt[group_or_hh=="In households"] - only family
    ###need it to be only adults to match with marital_status!!!
    
    
    #to match sr_relations$relative - 1472 folks who are >= 65 are Child of Householder
    hh_relations_dt[,("relative_sr"):=case_when(
      relative=="Householder" ~ "Householder",
      relative=="Spouse" ~ "Spouse",
      relative=="Nonrelatives" ~ "Nonrelatives",
      relative=="Parent" ~ "Parent",
      relative=="Parent-in-law" ~ "Parent-in-law",
      relative=="Child" ~ "Child of householder",
      relative=="Other relatives" | relative=="Brother or sister" | 
        relative=="Grandchild" ~ "Other relatives",
      TRUE ~ "not determined"
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
    hh_relations_dt[,c("sex_sr_hh","living_alone","age_range_3"):=
                      sr_relations[.SD, c(list(sex_sr_relations),list(living_alone),list(age_range_3)), 
                                   on = .(rel_sr_id)]]
    #4 tracts from sr_relations not in hh_relations
    #test<-table(hh_relations_dt[tract%in%unique(sr_relations$tract)]$tract,hh_relations_dt[tract%in%unique(sr_relations$tract)]$sex_sr_hh,hh_relations_dt[tract%in%unique(sr_relations$tract)]$living_alone)==
    #  table(sr_relations$tract,sr_relations$sex_sr_relations,sr_relations$living_alone)
    #length(test[test==F])==0

    #mostly matches adults_relations$relation_hh, for age>17 
    #looks like it matches on sex_age_race, although group_quarters isn't shown distributing over age (except for seniors)
    hh_relations_dt[group_or_hh=="In households",("relation_hh"):=case_when(
      relative=="Child" | relative=="Grandchild" ~ "Child of householder", #but only for adults; Foster child is non-relative
      relative=="Spouse" ~ "Householder living with spouse or spouse of householder",
      role_in_family=="Unmarried partner" ~ "Householder living with unmarried partner or unmarried partner of householder",
      relative=="Nonrelatives" | relative=="Parent-in-law" ~ "Other nonrelatives",
      relative=="Other relatives" | relative =="Parent" | relative=="Brother or sister" |
        relative=="Son-in-law or daughter-in-law"~ "Other relatives",
      TRUE ~ "Householder" 
    )]
    #test: nrow(hh_relations_dt[relation_hh=="Householder"])==nrow(hh_relations_dt[relative=="Householder"])
    
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
                    c("age_range_3"):=
                      adults_relations[.SD, list(age_range_3), 
                                       on = .(rel_adults_id)]]
    
    adults_relations[relation_hh=="Lives alone",("relation_hh"):="Householder Lives alone"]
    hh_relations_dt[is.na(age_range_3) & str_detect(relation_hh,"Householder"), 
                    ("rel_hh_id"):=
                      paste0(tract,as.character(1000000+sample(.N))),
                    by=.(tract)]
    adults_relations[age_range_3!="65 years and over" & str_detect(relation_hh,"Householder"), 
                     ("rel_hh_id"):=
                       paste0(tract,as.character(1000000+sample(.N))),
                     by=.(tract)]
    hh_relations_dt[is.na(age_range_3) & str_detect(relation_hh,"Householder"),
                    c("age_range_3"):= 
                      adults_relations[.SD, list(age_range_3), 
                                       on = .(rel_hh_id)]]
    
#give the remaining ones from hh_relations the Householder again, to fill out - not sure why the originals didn't add up
    
    #ends up with ~29k too many assigned -  the rest fit into >18 well; could be weird overlaps for married 16yo, etc.
    #clean up a 1592 edge cases?
    hh_relations_dt[is.na(age_range_3) & str_detect(relative,"Parent"),("role_in_family") :="Son-in-law or daughter-in-law"]
    hh_relations_dt[is.na(age_range_3) & str_detect(relative,"Parent"),("relative") :="Son-in-law or daughter-in-law"]

    #then hh_type_kids [[add kids_ages after have "no children" to exclude from joined with sam_hh; family_role_3 as match]]
    #hh_type_kids includes grandkids, but excludes hh, spouses, partners, and is everyone under 18 otherwise - marital should give that extra
    hh_type_kids[,("rel_kids_id"):= 
                   paste0(tract,family_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,family_or_non)]
    hh_relations_dt[group_or_hh=="In households",
                    ("rel_kids_id"):=
                      paste0(tract,family_or_non,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,family_or_non)]
    hh_relations_dt[!is.na(age_range_3),("age_range_4"):=age_range_3]
    hh_relations_dt[is.na(age_range_3),("age_range_4"):=list("0  to 17 years")]
    hh_relations_dt[group_or_hh=="In households",c("in_family_role","family_role_3","age_range_4"):=
                             hh_type_kids[.SD, c(list(in_family_role),list(family_role_3),list(age_range_4)), 
                                                         on = .(rel_kids_id)]]
    #test <- table(hh_relations_dt$in_family_role)==table(hh_type_kids$in_family_role) - missing tracts, but still matches totals!!
    
    #missing_tracts <- unique(place_born_age_full_dt$tract)[unique(place_born_age_full_dt$tract)%in%unique(origin_data_dt$tract)==F]
    #View(hh_relations_dt[tract==missing_tracts])  #only 9 folks in 980000
    #put eth/race builds on place_born by age
    place_born_age_full_dt[,("age_range_4"):=case_when(
      age_range=="Under 5 years" ~ "0  to 17 years",
      age_range=="5 to 17 years" ~ "0  to 17 years",
      age_range=="18 to 24 years" ~ "18 to 34 years",
      age_range=="25 to 34 years" ~ "18 to 34 years",
      age_range=="35 to 44 years" ~ "35 to 64 years",
      age_range=="45 to 54 years" ~ "35 to 64 years",
      age_range=="55 to 59 years" ~ "35 to 64 years",
      age_range=="60 and 61 years" ~ "35 to 64 years",
      age_range=="62 to 64 years" ~ "35 to 64 years",
      TRUE ~ "65 years and over"
    )]
    hh_relations_dt[,("hh_pb_age_id"):= 
                      paste0(tract,age_range_4,
                             as.character(1000000+sample(.N))),
                    by=.(tract,age_range_4)]
    place_born_age_full_dt[,("hh_pb_age_id"):= 
                      paste0(tract,age_range_4,
                             as.character(1000000+sample(.N))),
                    by=.(tract,age_range_4)]
    hh_relations_dt[,c("place_born","age_range_11"):=
                      place_born_age_full_dt[.SD, c(list(place_born),list(age_range)), 
                                   on = .(hh_pb_age_id)]]
    #ages didn't match, so pick up others...
    place_born_age_full_dt[,("missing"):=
                             hh_relations_dt[.SD,list(place_born),
                                             on = .(hh_pb_age_id)]]
    hh_rel_tmp <- hh_relations_dt[is.na(place_born)]
    pb_age_tmp <- place_born_age_full_dt[is.na(missing)]
    hh_rel_tmp[order(age_range_4),("hh_pb_age_id2"):= 
                      paste0(tract,
                             as.character(1000000+seq.int(1:.N))),
                    by=.(tract)]
    pb_age_tmp[order(age_range_4),("hh_pb_age_id2"):= 
                             paste0(tract,
                                    as.character(1000000+seq.int(1:.N))),
                           by=.(tract)]
    hh_rel_tmp[,c("place_born","age_range_11"):=
                 pb_age_tmp[.SD, c(list(place_born),list(age_range)), 
                                 on = .(hh_pb_age_id2)]]
    hh_relations_dt[is.na(place_born),c("place_born","age_range_11"):=
                      hh_rel_tmp[.SD, c(list(place_born),list(age_range_11)), 
                                             on = .(hh_pb_age_id)]]
    rm(pb_age_tmp)
    rm(hh_rel_tmp)
    
    #add fb_origin (foreign_born_origin) onto place_born_age - origin and sex_place_when_dt are both on foreign_born, only
    #should come up with a marker for when one is losing information - this will not get at how long folks have been here, by itself.
    hh_relations_dt[place_born=="Foreign born",("fb_origin_id"):= 
                             paste0(tract,
                                    as.character(1000000+sample(.N))),
                           by=.(tract)]
    origin_data_dt[,("fb_origin_id"):= 
                     paste0(tract,
                            as.character(1000000+sample(.N))),
                   by=.(tract)]
    hh_relations_dt[place_born=="Foreign born",c("fb_origin_continent","fb_origin_area","fb_origin_region","fb_origin_country"):=
                             origin_data_dt[.SD, c(list(origin_continent),list(origin_area),list(origin_region),list(origin_country)), 
                                            on = .(fb_origin_id)]]
    
    #add sex and age for foreign born
    sex_nativity_age_dt[,("age_range"):=case_when(
      age_range=="Under 5 years" ~ "0  to 5 years",
      age_range=="5 to 9 years" ~ "05  to 9 years",
      TRUE ~ age_range
    )]
    #get sex from seniors - sex_sr_hh on to sn_sex
    hh_relations_dt[age_range_3=="65 years and over",("sn_id"):= 
                      paste0(tract,place_born,sex_sr_hh,
                             as.character(1000000+seq.int(1:.N))),
                    by=.(tract,place_born,sex_sr_hh)]
    sex_nativity_age_dt[as.numeric(substr(age_range,1,2))>64,("sn_id"):= 
                     paste0(tract,"Foreign born",sex,
                            as.character(1000000+seq.int(1:.N))),
                   by=.(tract,sex)]
    hh_relations_dt[place_born=="Foreign born",c("sn_age_range","sn_sex"):=
                      sex_nativity_age_dt[.SD, c(list(age_range),list(sex)), 
                                          on = .(sn_id)]]
    sex_nativity_age_dt[as.numeric(substr(age_range,1,2))>64,c("missing"):=
                          hh_relations_dt[.SD, c(list(sn_sex)), 
                                          on = .(sn_id)]]
    #then rest from sex_nativity
    
#NEED TO KEEP MATCHING ON SEX!!
    hh_relations_dt[is.na(sn_age_range) & place_born=="Foreign born",("sn2_id"):= 
                      paste0(as.character(100000000+sample(.N)))]
    sex_nativity_age_dt[is.na(missing),("sn2_id"):= 
                          paste0(as.character(100000000+sample(.N)))]
    hh_relations_dt[is.na(sn_age_range) & place_born=="Foreign born",
                    c("sn_age_range","sn_sex"):=
                      sex_nativity_age_dt[.SD, c(list(age_range),list(sex)), 
                                     on = .(sn2_id)]]
    test <- table(hh_relations_dt[place_born=="Foreign born",sn_sex])==table(sex_nativity_age_dt$sex)
    test <- nrow(hh_relations_dt[place_born=="Foreign born" & age_range_3=="65 years and over"])
    
    #put citizen data on sex_place_when and keep_date_entered from place_period_citizen
    place_period_citizen_dt[,("date_entered_match"):=
                              if_else(date_entered=="Entered 1990 to 1999" | date_entered=="Entered before 1990",
                                      "Entered before 2000",date_entered)]
    sex_place_when_dt[,("pb_citizen_id"):= 
                      paste0(tract,date_entered,
                             as.character(1000000+sample(.N))),
                    by=.(tract,date_entered)]
    place_period_citizen_dt[,("pb_citizen_id"):= 
                        paste0(tract,date_entered_match,
                               as.character(1000000+seq.int(1:.N))),
                      by=.(tract,date_entered_match)]
    sex_place_when_dt[,c("date_entered","fb_citizen"):= #raw numbers don't match in census files!!! need to check and choose!!
                        place_period_citizen_dt[.SD, c(list(date_entered),list(citizen)), 
                                        on = .(pb_citizen_id)]]
    
    #add when_came - lose some infor
    hh_relations_dt[place_born=="Foreign born",("fb_origin_region"):=case_when(
      str_detect(fb_origin_area,"Asia") ~ "Asia",
      str_detect(fb_origin_area,"Europe") ~ "Europe",
      TRUE ~ "Other areas"
    )]
    sex_place_when_dt[,("fb_origin_region"):=case_when(
      origin_region=="Asia"  ~ "Asia",
      origin_region=="Europe" ~ "Europe",
      TRUE ~ "Other areas"
    )]
    #test <- table(hh_relations_dt$fb_origin_region)==table(sex_place_when_dt$fb_origin_region) #since tract has that extra one
    hh_relations_dt[place_born=="Foreign born",("when_id"):= 
                      paste0(tract,sn_sex,fb_origin_region,
                             as.character(1000000+sample(.N))),
                    by=.(tract,sn_sex,fb_origin_region)]
    sex_place_when_dt[,("when_id"):= 
                          paste0(tract,sex,fb_origin_region,
                                 as.character(1000000+seq.int(1:.N))),
                        by=.(tract,sex,fb_origin_region)]
    hh_relations_dt[place_born=="Foreign born",
                    c("fb_date_entered","when_origin_country","fb_citizen"):= #raw numbers don't match in census files!!! need to check and choose!!
                      sex_place_when_dt[.SD, c(list(date_entered),list(origin_country),list(fb_citizen)), 
                                          on = .(when_id)]]
    #and pick up stragglers without matching on tract or sex - means that the totals won't match, but that everyone has a plausible one
    hh_relations_dt[place_born=="Foreign born" & is.na(fb_date_entered),("when2_id"):= 
                      paste0(fb_origin_region,
                             as.character(1000000+seq.int(1:.N))),
                    by=.(fb_origin_region)]
    sex_place_when_dt[,("when2_id"):= 
                        paste0(fb_origin_region,
                               as.character(1000000+seq.int(1:.N))),
                      by=.(fb_origin_region)]
    hh_relations_dt[place_born=="Foreign born" & is.na(fb_date_entered),
                    c("sn_sex","fb_date_entered","when_origin_country","fb_citizen"):= 
                      sex_place_when_dt[.SD, c(list(sex),list(date_entered),list(origin_country),list(fb_citizen)), 
                                        on = .(when2_id)]]
    
    #have to think about how to add pb_education
    
    hh_relations_dt[order(match(fb_date_entered,c("Entered before 2000","Entered 2000 to 2009","Entered 2010 or later"))),
                    ("pbl_id"):= 
                      paste0(tract,place_born,
                             as.character(1000000+seq.int(1:.N))),
                    by=.(tract,place_born)]
    place_born_language_dt[order(English_proficiency),("pbl_id"):= 
                            paste0(tract,place_born,
                                   as.character(1000000+seq.int(1:.N))),
                          by=.(tract,place_born)]
    hh_relations_dt[,c("fb_language_at_home","English_proficiency"):=
                      place_born_language_dt[.SD, c(list(language_at_home),list(English_proficiency)), 
                                            on = .(pbl_id)]]
    #test <- table(place_born_language_dt$fb_language_at_home)==table(hh_relations_dt$fb_language_at_home)
    #there are some surprising things in the original data, like 140051 born here who speak English less than very well
    #I copied it over - perhaps should tie to ethnicity and language? have to see how much the tract differences help!!!
    
#add pb_eth/race to hh_relations, etc., then still break up as below - add latin_x to the ethnicity thread.... 
    #transportation? allows a match on language, and gives you industry to go along with employment, but could do after this
    hh_relations_dt[,("pb_race_id"):=paste0(tract,place_born,
                                                                     as.character(1000000+sample(.N))),
                    by=.(tract,place_born)]
    place_born_race_dt[,("pb_race_id"):=paste0(tract,place_born,
                                                            as.character(1000000+sample(.N))),
                                by=.(tract,place_born)]
    place_born_eth_dt[,("pb_race_id"):=paste0(tract,place_born,
                                               as.character(1000000+sample(.N))),
                       by=.(tract,place_born)]
    hh_relations_dt[,c("pb_race"):=
                      place_born_race_dt[.SD,list(race),
                                                  on = .(pb_race_id)]]
    hh_relations_dt[,c("pb_ethnicity"):=   #adding both to hh_relations, then separating to rejoin later
                      place_born_eth_dt[.SD,list(ethnicity),
                                         on = .(pb_race_id)]]
    
    #because relatives is screwy, this only gives us some family_role info for the kids and a bit for seniors; basically had to abandon the relatives vs. non-relatives 
    #join relatives_race/eth to hh_relations - match on family_role_3 for the kids and on age_range_4 and race for others
    hh_relations_dt[!is.na(family_role_3),("hh_race1_id"):=paste0(tract,family_role_3,pb_race,
                                                                     as.character(1000000+sample(.N))),
                    by=.(tract,family_role_3,pb_race)]
    household_relatives_race_dt[,("hh_race1_id"):=paste0(tract,family_role_3,race,
                                                                     as.character(1000000+sample(.N))),
                    by=.(tract,family_role_3,race)]
    hh_relations_dt[!is.na(family_role_3),c("race"):=
                      household_relatives_race_dt[.SD,list(race),
                                                  on = .(hh_race1_id)]]
    household_relatives_race_dt[,c("used"):=
                                  hh_relations_dt[.SD,list(race),
                                                  on = .(hh_race1_id)]]
#caught 96% of available matches on family_role_3 and race- do without family_role, so have race by tract by pb - just finishing; will match on it again, second time 
    hh_relations_dt[is.na(race),("hh_race2_id"):=paste0(tract,family_or_non,pb_race,
                                                                     as.character(1000000+sample(.N))),
                    by=.(tract,family_or_non,pb_race)]
    household_relatives_race_dt[is.na(used),("hh_race2_id"):=paste0(tract,family_or_non,race,
                                                            as.character(1000000+sample(.N))),
                                by=.(tract,family_or_non,race)]
    hh_relations_dt[is.na(race),c("race"):=
                      household_relatives_race_dt[.SD,list(race),
                                                  on = .(hh_race2_id)]]
    #about 250k didn't match for some reason
    hh_relations_dt[is.na(race),("race"):=pb_race]
    #now same for ethnicity
    hh_relations_dt[!is.na(family_role_3),("hh_eth1_id"):=paste0(tract,family_role_3,pb_ethnicity,
                                                                  as.character(1000000+sample(.N))),
                    by=.(tract,family_role_3,pb_ethnicity)]
    household_relatives_eth_dt[,("hh_eth1_id"):=paste0(tract,family_role_3,ethnicity,
                                                         as.character(1000000+sample(.N))),
                                by=.(tract,family_role_3,ethnicity)]
    hh_relations_dt[!is.na(family_role_3),c("ethnicity"):=
                      household_relatives_eth_dt[.SD,list(ethnicity),
                                                  on = .(hh_eth1_id)]]
    household_relatives_eth_dt[,c("used_eth"):=
                                  hh_relations_dt[.SD,list(race),
                                                  on = .(hh_eth1_id)]]
    hh_relations_dt[is.na(ethnicity),("hh_eth2_id"):=paste0(tract,family_or_non,pb_ethnicity,
                                                        as.character(1000000+sample(.N))),
                    by=.(tract,family_or_non,pb_ethnicity)]
    household_relatives_eth_dt[is.na(used_eth),("hh_eth2_id"):=paste0(tract,family_or_non,ethnicity,
                                                                    as.character(1000000+sample(.N))),
                                by=.(tract,family_or_non,ethnicity)]
    hh_relations_dt[is.na(ethnicity),c("ethnicity"):=
                      household_relatives_eth_dt[.SD,list(ethnicity),
                                                  on = .(hh_eth2_id)]]
    hh_relations_dt[is.na(ethnicity),("ethnicity"):=pb_ethnicity]
    
    #then pb_marital  
#    place_born_marital_dt[,("marital_status"):=if_else(str_detect(marital_status,"eparated"),"Now married",marital_status)]
#    hh_relations_dt[,("pbm_id"):= 
#                      paste0(tract,place_born,
#                             as.character(1000000+sample(.N))),
#                    by=.(tract,place_born)]
#    place_born_marital_dt[,("pbm_id"):= 
#                            paste0(tract,place_born,
#                                   as.character(1000000+sample(.N))),
#                          by=.(tract,place_born)]
#    hh_relations_dt[,c("marital_status_pb"):=
#                      place_born_marital_dt[.SD, c(list(marital_status)), 
#                                            on = .(pbm_id)]]
    #test <- table(place_born_marital_dt$marital_status)==table(hh_relations_dt$marital_status_pb)
    
    #add hh_relations eth and race to marital
    #first time matching with sex, even though hh_relations_dt is missing more than half
    #second time without sex, but still marital status
    #third time, race (4th?);order by more internally, and then just count it out once???
    hh_relations_dt[order(-age_range_11),("rel_marital_race_id"):= 
                                  paste0(tract,race,
                                         as.character(1000000+seq.int(1:.N))),
                                by=.(tract,race)]
    marital_status_race_dt[order(-age),("rel_marital_race_id"):=
                             paste0(tract,race,
                                    as.character(1000000+seq.int(1:.N))),
                           by=.(tract,race)]
    #go to marital_status and sort by -age to avoid getting folks under 15.
    #then go back to get it on hh_relations...
#    marital_status_race_dt[,c("group_or_hh","family_or_non","relative","role_in_family","relation_hh",
#                              "sr_nonfamily_hh_living_alone","in_family_type","family_type_3", #changing family_role to type for clarity
#                              "place_born","fb_origin_continent","fb_origin_area","fb_origin_region",
#                              "fb_origin_country","fb_date_entered","fb_when_region","fb_citizen",
#                              "language_at_home","English_proficiency"):=
#                             hh_relations_dt[.SD,c(list(group_or_hh),list(family_or_non),list(relative),
#                                                   list(role_in_family),list(relation_hh),list(living_alone),
#                                                   list(in_family_role),list(family_role_3),list(place_born),
#                                                   list(fb_origin_continent),list(fb_origin_area),
#                                                   list(fb_origin_region),list(fb_origin_country),
#                                                   list(fb_date_entered),list(when_origin_country),
#                                                   list(fb_citizen),list(fb_language_at_home),
#                                                   list(English_proficiency)), 
#                                                    on = .(rel_marital_race_id)]]
#    
    #so - add all of the relevant parts of hh_relations to each of the maritals, then join to s.a.r, then take sam_hh out, then build
    
    
    
    
    
    
    
    #adults_relations starts at 17 (3260555) and marital (3494885) starts at 15
    
    marital_status_race_dt[,c("family_or_non","family_role_3","relative_or_non"):=
                             household_relatives_race_dt[.SD, c(list(family_or_non),list(family_role_3),list(relative_or_non)), 
                                                         on = .(rel_marital_id)]]
    household_relatives_eth_dt[,("rel_marital_id"):= 
                                 paste0(tract,ethnicity,if_else(family_role_3=="In married-couple family","Now married","not determined"),
                                        as.character(1000000+sample(.N))),
                               by=.(tract,ethnicity,family_role_3=="In married-couple family")]
    marital_status_eth_dt[,("rel_marital_id"):=
                            paste0(tract,ethnicity,if_else(marital_status=="Now married","In married-couple family","not determined"),
                                   as.character(1000000+sample(.N))),
                          by=.(tract,ethnicity,marital_status=="Now married")]
    marital_status_eth_dt[,c("family_or_non","family_role_3","relative_or_non"):=
                            household_relatives_eth_dt[.SD, c(list(family_or_non),list(family_role_3),list(relative_or_non)), 
                                                       on = .(rel_marital_id)]]
    
    #to match household_relatives_race/eth_dt - relative_or_non - only Nonrelatives in family households!!!
    hh_relations_dt[,("relative_or_non"):=case_when(
      relative=="Nonrelatives" | relative=="Parent-in-law" | relative=="Son-in-law or daughter-in-law" ~ "Nonrelatives",
      relative=="Other relatives" | relative=="Brother or sister" | relative=="Parent" | relative=="Spouse" |
        relative=="Householder" | relative=="Grandchild" | relative=="Child" ~ "Relatives"
    )]
    hh_relations_dt[,("rel_race_id"):=
                      paste0(tract,family_or_non,family_role_3,relative_or_non,
                             as.character(1000000+sample(.N))),
                    by=.(tract,family_or_non,family_role_3,relative_or_non)]
    household_relatives_eth_dt[,("rel_race_id"):=
                                 paste0(tract,family_or_non,family_role_3,relative_or_non,
                                        as.character(1000000+sample(.N))),
                               by=.(tract,family_or_non,family_role_3,relative_or_non)]
    household_relatives_race_dt[,("rel_race_id"):=
                                  paste0(tract,family_or_non,family_role_3,relative_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,family_or_non,family_role_3,relative_or_non)]
    household_relatives_eth_dt[,c("group_or_hh","relative","role_in_family","relation_hh",
                                  "sex_sr_hh","sr_living_alone","age_range_3"):=
                                 hh_relations_dt[.SD,c(list(group_or_hh),list(relative),
                                                       list(role_in_family),list(relation_hh),
                                                       list(sex_sr_hh),list(living_alone),
                                                       list(age_range_3)),
                                                 on = .(rel_race_id)]]
    household_relatives_race_dt[,c("group_or_hh","relative","role_in_family","relation_hh",
                                   "sex_sr_hh","sr_living_alone","age_range_3"):=
                                  hh_relations_dt[.SD,c(list(group_or_hh),list(relative),
                                                        list(role_in_family),list(relation_hh),
                                                        list(sex_sr_hh),list(living_alone),
                                                        list(age_range_3)),
                                                  on = .(rel_race_id)]]
    
    #match hh_relations with marital on eth/race
    #catch sex_sr_hh, sr_living_alone and marital on sex and status first, then the rest
    household_relatives_race_dt[,
                                ("rel_marital_id"):=
                                  paste0(tract,race,family_or_non,family_role_3,relative_or_non,
                                         as.character(1000000+sample(.N))),
                                by=.(tract,race,family_or_non,family_role_3,relative_or_non)]
    marital_status_race_dt[,
                           ("rel_marital_id"):=
                             paste0(tract,race,family_or_non,family_role_3,relative_or_non,
                                    as.character(1000000+sample(.N))),
                           by=.(tract,race,family_or_non,family_role_3,relative_or_non)]
    marital_status_race_dt[,c(""):=
                             hh_relations_dt[.SD, list(family_role_3), #all the things that will fit!!
                                             on = .(rel_marital_id)]]
    household_relatives_eth_dt[,
                               ("rel_marital_id"):=
                                 paste0(tract,race,family_or_non,family_role_3,relative_or_non,
                                        as.character(1000000+sample(.N))),
                               by=.(tract,race,family_or_non,family_role_3,relative_or_non)]
    marital_status_eth_dt[,
                          ("rel_marital_id"):=
                            paste0(tract,ethnicity,family_or_non,family_role_3,relative_or_non,
                                   as.character(1000000+sample(.N))),
                          by=.(tract,ethnicity,family_or_non,family_role_3,relative_or_non)]
    marital_status_eth_dt[,c(""):=
                            hh_relations_dt[.SD, list(family_role_3), #all the things that will fit!!
                                            on = .(rel_marital_id)]]
    
    #add kid and grandkid data - relative_or_non==relative and family_role_3 and place_born?????
    
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
    sex_age_race$marital_status_tmp <- NULL
    sex_by_age_eth$marital_status_tmp <- NULL
    
    
    
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

