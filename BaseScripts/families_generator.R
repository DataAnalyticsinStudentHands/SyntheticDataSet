#https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf

#' createIndividuals
#'
#' This function simulates individual people based on census data (age, sex, race, as of April 1, vintage year).
#' If simulated data already exists, it will read it from an RDS file.
#'Input should include exp_census from expand_from_census.R, which is a list of data.tables
#' @return sam_residents A dataframe of simulated people.
createFamilies <- function() {
  #have to fix
  sam_families_data_file <- paste0(censusdir, vintage,"/sam_hh.RDS") 
  #Create or read in individual sam residents
  if(file.exists(sam_residents_data_file)) {
    # import saved sam residents from RDS file
    sam_residents <- readRDS(sam_residents_data_file)
    print(sprintf("Done reading sam residents RDS from %s", sam_residents_data_file ))
  } else {
    additional_workers_race <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/additional_workers_race_2020-07-12.RDS")
    additional_workers_eth <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/additional_workers_eth_2020-07-12.RDS")
    sam_race_hh <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/sam_race_hh_2020-07-12.RDS")
    sam_eth_hh <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/sam_eth_hh_2020-07-12.RDS")
    
    #get sam_eth/race and additional_workers_eth/race from RDS
    #clean up NAs from households
    sam_eth_hh[single_hh_employed=="NA",c("single_hh_employed"):=NA]
    sam_race_hh[single_hh_employed=="NA",c("single_hh_employed"):=NA]
    
    partners_eth <- sam_eth_hh[,uncount(.SD[!is.na(partner_type)],1,.remove = TRUE,.id="partner")]
    wives_eth <- sam_eth_hh[,uncount(.SD[family_role=="Married-couple family"],1,.remove = TRUE,.id="wife")]
    partners_race <- sam_race_hh[,uncount(.SD[!is.na(partner_type)],1,.remove = TRUE,.id="partner")]
    wives_race <- sam_race_hh[,uncount(.SD[family_role=="Married-couple family"],1,.remove = TRUE,.id="wife")]
    #see if avoid shallow copy warning by an operation
    partners_eth$partner <- NULL
    partners_race$partner <- NULL
    wives_eth$wife <- NULL
    wives_race$wife <- NULL
    #change the files to mirror hh
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
    partners_eth[,("employment"):=single_hh_employed] #just to give it something - not a great solution
    partners_eth[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    partners_race[,("employment"):=single_hh_employed]
    partners_race[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    wives_eth[,("employment"):=wife_employed]
    wives_eth[employment=="Wife not in labor force",("employment"):="Not in labor force"]
    wives_race[,("employment"):=wife_employed]
    wives_race[employment=="Wife not in labor force",("employment"):="Not in labor force"]
    sam_eth_hh[,("employment"):=husband_employed]
    sam_eth_hh[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    sam_eth_hh[is.na(employment),("employment"):=single_hh_employed]
    sam_race_hh[,("employment"):=husband_employed]
    sam_race_hh[employment=="Husband not in labor force",("employment"):="Not in labor force"]
    sam_race_hh[is.na(employment),("employment"):=single_hh_employed]
    #could do interracial marriages - 17% of new marriages in 2015; 3% in 1967... other.race gets a lot of increase... - fix later
    
    
    #do each, then put them together and finish
    wives_eth[,c("industry","occupation","commute_time","when_go_to_work","income_range_workers"):=NULL]
    wives_eth[employment!="Not in labor force",
              ("pw_match_id"):=paste0(tract,sex,ethnicity,
                                      language,English_level,
                                      age_range_6, 
                                      as.character(1000000+sample(1:.N))),
              by = .(tract,sex,ethnicity,language,English_level,age_range_6)]
    additional_workers_eth[,("pw_match_id"):=paste0(tract,sex,ethnicity,
                                                    language,English_level,
                                                    age_range_6,
                                                    as.character(1000000+sample(1:.N))),
                           by = .(tract,sex,ethnicity,language,English_level,age_range_6)]
    wives_eth[employment!="Not in labor force",
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range)),
                  on = .(pw_match_id)]]
    #take out of additional_workers
    additional_workers_eth[,("missing_workers"):=wives_eth[.SD,industry,on = .(pw_match_id)]]
    #reshuffle test
    reshuffle <- as.data.frame(Sys.time())
    reshuffle$eth_w1 <- nrow(wives_eth[!is.na(industry)])
    reshuffle$eth_w1T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
    #reshuffle
    #change age_range for better matches and fix odd cases post_hoc?
    wives_eth[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    reshuff_add_we <- additional_workers_eth[is.na(missing_workers)]
    additional_workers_eth[is.na(missing_workers),("reshuff_id"):=
                             paste0(tract,as.character(1000000+sample(1:.N))),
                           by=.(tract)]
    reshuff_add_we[is.na(missing_workers),("reshuff_id"):=
                     paste0(tract,as.character(1000000+sample(1:.N))),
                   by=.(tract)]
    additional_workers_eth[is.na(missing_workers),
                           c("ethnicity","language","English_level"):=
                             reshuff_add_we[.SD, c(list(ethnicity),list(language),list(English_level)),
                                            on=.(reshuff_id)]]
    #recast
    wives_eth[employment!="Not in labor force"&is.na(industry),
              ("pw1_match_id"):=paste0(tract,ethnicity,
                                      language,English_level,
                                      age_range_w3, 
                                      as.character(1000000+sample(1:.N))),
              by = .(tract,ethnicity,language,English_level,age_range_w3)]
    additional_workers_eth[is.na(missing_workers),("pw1_match_id"):=paste0(tract,ethnicity,
                                                    language,English_level,
                                                    age_range_w3,
                                                    as.character(1000000+sample(1:.N))),
                           by = .(tract,ethnicity,language,English_level,age_range_w3)]
    wives_eth[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range)),
                  on = .(pw1_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=wives_eth[.SD,industry,on = .(pw1_match_id)]]
    reshuffle$eth_w2 <- nrow(wives_eth[!is.na(industry)])
    reshuffle$eth_w2T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
    #pick up stragglers - info will be lost - age_range_w3 is the one not matching more, so let it get a few mixed up on ethnicity
    wives_eth[employment!="Not in labor force"&is.na(industry),
              ("pw2_match_id"):=paste0(tract,#ethnicity,
                                       age_range_w3, 
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,age_range_w3)]
    additional_workers_eth[is.na(missing_workers),("pw2_match_id"):=paste0(tract,#ethnicity,
                                                                           age_range_w3,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,age_range_w3)]
    wives_eth[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers","ethnicity_worker"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range),list(ethnicity)),
                  on = .(pw2_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=wives_eth[.SD,industry,on = .(pw2_match_id)]]
    reshuffle$eth_w3 <- nrow(wives_eth[!is.na(industry)])
    reshuffle$eth_w3T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
    #dropping age - will need to fix later!! householder_age can be matched...
    wives_eth[employment!="Not in labor force"&is.na(industry),
              ("pw3_match_id"):=paste0(tract,ethnicity,
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,ethnicity)]
    additional_workers_eth[is.na(missing_workers),("pw3_match_id"):=paste0(tract,ethnicity,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,ethnicity)]
    wives_eth[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers","age_range_w3_worker","age_range_6_worker"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range),
                  list(age_range_6),list(age_range_w3)),
                  on = .(pw3_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=wives_eth[.SD,industry,on = .(pw3_match_id)]]
    reshuffle$eth_w4 <- nrow(wives_eth[!is.na(industry)])
    reshuffle$eth_w4T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
    reshuffle$eth_w_left <- nrow(wives_eth[employment!="Not in labor force"])-nrow(wives_eth[!is.na(industry)])
    #redo others
    #partners_eth
    partners_eth[,c("industry","occupation","commute_time","when_go_to_work","income_range_workers"):=NULL]
    partners_eth[employment!="Not in labor force",
              ("p_match_id"):=paste0(tract,sex,ethnicity,
                                      language,English_level,
                                      age_range_6, 
                                      as.character(1000000+sample(1:.N))),
              by = .(tract,sex,ethnicity,language,English_level,age_range_6)]
    additional_workers_eth[is.na(missing_workers),("p_match_id"):=paste0(tract,sex,ethnicity,
                                                    language,English_level,
                                                    age_range_6,
                                                    as.character(1000000+sample(1:.N))),
                           by = .(tract,sex,ethnicity,language,English_level,age_range_6)]
    partners_eth[employment!="Not in labor force",
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range)),
                  on = .(p_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=partners_eth[.SD,industry,on = .(p_match_id)]]
    #reshuffle test
    reshuffle$eth_p1 <- nrow(partners_eth[!is.na(industry)])
    reshuffle$eth_p1T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-
                                                                      nrow(wives_eth[!is.na(industry)])   
    #reshuffle
    #change age_range for better matches and fix odd cases post_hoc?
    partners_eth[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    reshuff_add_pe <- additional_workers_eth[is.na(missing_workers)]
    additional_workers_eth[is.na(missing_workers),("reshuff_id"):=
                             paste0(tract,as.character(1000000+sample(1:.N))),
                           by=.(tract)]
    reshuff_add_pe[is.na(missing_workers),("reshuff_id"):=
                     paste0(tract,as.character(1000000+sample(1:.N))),
                   by=.(tract)]
    additional_workers_eth[is.na(missing_workers),
                           c("ethnicity","language","English_level"):=
                             reshuff_add_pe[.SD, c(list(ethnicity),list(language),list(English_level)),
                                            on=.(reshuff_id)]]
    #recast
    partners_eth[employment!="Not in labor force"&is.na(industry),
              ("p1_match_id"):=paste0(tract,ethnicity,
                                       language,English_level,
                                       age_range_w3, 
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,ethnicity,language,English_level,age_range_w3)]
    additional_workers_eth[is.na(missing_workers),("p1_match_id"):=paste0(tract,ethnicity,
                                                                           language,English_level,
                                                                           age_range_w3,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,ethnicity,language,English_level,age_range_w3)]
    partners_eth[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range)),
                  on = .(p1_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=partners_eth[.SD,industry,on = .(p1_match_id)]]
    reshuffle$eth_p2 <- nrow(partners_eth[!is.na(industry)])
    reshuffle$eth_p2T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-nrow(wives_eth[!is.na(industry)])
    #pick up stragglers - info will be lost - age_range_w3 is the one not matching more, so let it get a few mixed up on ethnicity
    partners_eth[employment!="Not in labor force"&is.na(industry),
              ("p2_match_id"):=paste0(tract,#ethnicity,
                                       age_range_w3, 
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,age_range_w3)]
    additional_workers_eth[is.na(missing_workers),("p2_match_id"):=paste0(tract,#ethnicity,
                                                                           age_range_w3,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,age_range_w3)]
    partners_eth[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers","ethnicity_worker"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range),list(ethnicity)),
                  on = .(p2_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=partners_eth[.SD,industry,on = .(p2_match_id)]]
    reshuffle$eth_p3 <- nrow(partners_eth[!is.na(industry)])
    reshuffle$eth_p3T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-nrow(wives_eth[!is.na(industry)])
    #dropping age - will need to fix later!! householder_age can be matched...
    partners_eth[employment!="Not in labor force"&is.na(industry),
              ("p3_match_id"):=paste0(tract,ethnicity,
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,ethnicity)]
    additional_workers_eth[is.na(missing_workers),("p3_match_id"):=paste0(tract,ethnicity,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,ethnicity)]
    partners_eth[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers","age_range_w3_worker","age_range_6_worker"):=
                additional_workers_eth[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range),
                  list(age_range_6),list(age_range_w3)),
                  on = .(p3_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=partners_eth[.SD,industry,on = .(p3_match_id)]]
    reshuffle$eth_p4 <- nrow(partners_eth[!is.na(industry)])
    reshuffle$eth_p4T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-nrow(wives_eth[!is.na(industry)])
    reshuffle$eth_p_left <- nrow(partners_eth[employment!="Not in labor force"])-nrow(partners_eth[!is.na(industry)])
    
    #for race
    wives_race[,c("industry","occupation","commute_time","when_go_to_work","income_range_workers"):=NULL]
    wives_race[employment!="Not in labor force",
              ("pw_match_id"):=paste0(tract,sex,race,
                                      language,English_level,
                                      age_range_6, 
                                      as.character(1000000+sample(1:.N))),
              by = .(tract,sex,race,language,English_level,age_range_6)]
    additional_workers_race[,("pw_match_id"):=paste0(tract,sex,race,
                                                    language,English_level,
                                                    age_range_6,
                                                    as.character(1000000+sample(1:.N))),
                           by = .(tract,sex,race,language,English_level,age_range_6)]
    wives_race[employment!="Not in labor force",
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers"):=
                additional_workers_race[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range)),
                  on = .(pw_match_id)]]
    #take out of additional_workers
    additional_workers_race[,("missing_workers"):=wives_race[.SD,industry,on = .(pw_match_id)]]
    #reshuffle test
    reshuffle$race_w1 <- nrow(wives_race[!is.na(industry)])
    reshuffle$race_w1T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])
    #reshuffle
    #change age_range for better matches and fix odd cases post_hoc?
    wives_race[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    reshuff_add_wr <- additional_workers_race[is.na(missing_workers)]
    additional_workers_race[is.na(missing_workers),("reshuff_id"):=
                             paste0(tract,as.character(1000000+sample(1:.N))),
                           by=.(tract)]
    reshuff_add_wr[is.na(missing_workers),("reshuff_id"):=
                     paste0(tract,as.character(1000000+sample(1:.N))),
                   by=.(tract)]
    additional_workers_race[is.na(missing_workers),
                           c("race","language","English_level"):=
                             reshuff_add_wr[.SD, c(list(race),list(language),list(English_level)),
                                            on=.(reshuff_id)]]
    #recast
    wives_race[employment!="Not in labor force"&is.na(industry),
              ("pw1_match_id"):=paste0(tract,race,
                                       language,English_level,
                                       age_range_w3, 
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,race,language,English_level,age_range_w3)]
    additional_workers_race[is.na(missing_workers),("pw1_match_id"):=paste0(tract,race,
                                                                           language,English_level,
                                                                           age_range_w3,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,race,language,English_level,age_range_w3)]
    wives_race[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers"):=
                additional_workers_race[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range)),
                  on = .(pw1_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=wives_race[.SD,industry,on = .(pw1_match_id)]]
    reshuffle$race_w2 <- nrow(wives_race[!is.na(industry)])
    reshuffle$race_w2T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])
    #pick up stragglers - info will be lost - age_range_w3 is the one not matching more, so let it get a few mixed up on ethnicity
    wives_race[employment!="Not in labor force"&is.na(industry),
              ("pw2_match_id"):=paste0(tract,#ethnicity,
                                       age_range_w3, 
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,age_range_w3)]
    additional_workers_race[is.na(missing_workers),("pw2_match_id"):=paste0(tract,#ethnicity,
                                                                           age_range_w3,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,age_range_w3)]
    wives_race[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers","race_worker"):=
                additional_workers_race[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range),list(race)),
                  on = .(pw2_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=wives_race[.SD,industry,on = .(pw2_match_id)]]
    reshuffle$race_w3 <- nrow(wives_race[!is.na(industry)])
    reshuffle$race_w3T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])
    #dropping age - will need to fix later!! householder_age can be matched...
    wives_race[employment!="Not in labor force"&is.na(industry),
              ("pw3_match_id"):=paste0(tract,race,
                                       as.character(1000000+sample(1:.N))),
              by = .(tract,race)]
    additional_workers_race[is.na(missing_workers),("pw3_match_id"):=paste0(tract,race,
                                                                           as.character(1000000+sample(1:.N))),
                           by = .(tract,race)]
    wives_race[employment!="Not in labor force"&is.na(industry),
              c("industry","occupation","commute_time","when_go_to_work",
                "means_transport","income_range_workers","age_range_w3_worker","age_range_6_worker"):=
                additional_workers_race[.SD, c(
                  list(industry),list(occupation),
                  list(commute_time),list(when_go_to_work),
                  list(means_transport),list(income_range),
                  list(age_range_6),list(age_range_w3)),
                  on = .(pw3_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=wives_race[.SD,industry,on = .(pw3_match_id)]]
    reshuffle$race_w4 <- nrow(wives_race[!is.na(industry)])
    reshuffle$race_w4T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
    reshuffle$race_w_left <- nrow(wives_race[employment!="Not in labor force"])-nrow(wives_race[!is.na(industry)])
    #redo others
    #partners_eth
    partners_race[,c("industry","occupation","commute_time","when_go_to_work","income_range_workers"):=NULL]
    partners_race[employment!="Not in labor force",
                 ("p_match_id"):=paste0(tract,sex,race,
                                        language,English_level,
                                        age_range_6, 
                                        as.character(1000000+sample(1:.N))),
                 by = .(tract,sex,race,language,English_level,age_range_6)]
    additional_workers_race[is.na(missing_workers),("p_match_id"):=paste0(tract,sex,race,
                                                                         language,English_level,
                                                                         age_range_6,
                                                                         as.character(1000000+sample(1:.N))),
                           by = .(tract,sex,race,language,English_level,age_range_6)]
    partners_race[employment!="Not in labor force",
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers"):=
                   additional_workers_race[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range)),
                     on = .(p_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=partners_race[.SD,industry,on = .(p_match_id)]]
    #reshuffle test
    reshuffle$race_p1 <- nrow(partners_race[!is.na(industry)])
    reshuffle$race_p1T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-
      nrow(wives_race[!is.na(industry)])   
    #reshuffle
    #change age_range for better matches and fix odd cases post_hoc?
    partners_race[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    reshuff_add_pr <- additional_workers_eth[is.na(missing_workers)]
    additional_workers_race[is.na(missing_workers),("reshuff_id"):=
                             paste0(tract,as.character(1000000+sample(1:.N))),
                           by=.(tract)]
    reshuff_add_pr[is.na(missing_workers),("reshuff_id"):=
                     paste0(tract,as.character(1000000+sample(1:.N))),
                   by=.(tract)]
    additional_workers_race[is.na(missing_workers),
                           c("race","language","English_level"):=
                             reshuff_add_pr[.SD, c(list(race),list(language),list(English_level)),
                                            on=.(reshuff_id)]]
    #recast
    partners_race[employment!="Not in labor force"&is.na(industry),
                 ("p1_match_id"):=paste0(tract,race,
                                         language,English_level,
                                         age_range_w3, 
                                         as.character(1000000+sample(1:.N))),
                 by = .(tract,race,language,English_level,age_range_w3)]
    additional_workers_race[is.na(missing_workers),("p1_match_id"):=paste0(tract,race,
                                                                          language,English_level,
                                                                          age_range_w3,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,race,language,English_level,age_range_w3)]
    partners_race[employment!="Not in labor force"&is.na(industry),
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers"):=
                   additional_workers_race[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range)),
                     on = .(p1_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=partners_race[.SD,industry,on = .(p1_match_id)]]
    reshuffle$race_p2 <- nrow(partners_race[!is.na(industry)])
    reshuffle$race_p2T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-nrow(wives_race[!is.na(industry)])
    #pick up stragglers - info will be lost - age_range_w3 is the one not matching more, so let it get a few mixed up on ethnicity
    partners_race[employment!="Not in labor force"&is.na(industry),
                 ("p2_match_id"):=paste0(tract,
                                         age_range_w3, 
                                         as.character(1000000+sample(1:.N))),
                 by = .(tract,age_range_w3)]
    additional_workers_race[is.na(missing_workers),("p2_match_id"):=paste0(tract,
                                                                          age_range_w3,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,age_range_w3)]
    partners_race[employment!="Not in labor force"&is.na(industry),
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers","race_worker"):=
                   additional_workers_race[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range),list(race)),
                     on = .(p2_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=partners_race[.SD,industry,on = .(p2_match_id)]]
    reshuffle$race_p3 <- nrow(partners_race[!is.na(industry)])
    reshuffle$race_p3T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-nrow(wives_race[!is.na(industry)])
    #dropping age - will need to fix later!! householder_age can be matched...
    partners_race[employment!="Not in labor force"&is.na(industry),
                 ("p3_match_id"):=paste0(tract,race,
                                         as.character(1000000+sample(1:.N))),
                 by = .(tract,race)]
    additional_workers_race[is.na(missing_workers),("p3_match_id"):=paste0(tract,race,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,race)]
    partners_race[employment!="Not in labor force"&is.na(industry),
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers","age_range_w3_worker","age_range_6_worker"):=
                   additional_workers_race[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range),
                     list(age_range_6),list(age_range_w3)),
                     on = .(p3_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=partners_race[.SD,industry,on = .(p3_match_id)]]
    reshuffle$race_p4 <- nrow(partners_race[!is.na(industry)])
    reshuffle$race_p4T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-nrow(wives_race[!is.na(industry)])
    reshuffle$race_p_left <- nrow(partners_race[employment!="Not in labor force"])-nrow(partners_race[!is.na(industry)])
    
    #bindrows into one whole household_ids will match
    sam_pw_hh_eth <- bind_rows(sam_eth_hh,partners_eth,wives_eth)
    sam_pw_hh_race <- bind_rows(sam_race_hh,partners_race,wives_race)
    #saveRDS(sam_pw_hh_eth,file = paste0(housingdir, vintage, "/sam_pw_hh_eth_",Sys.Date(),".RDS"))
    #saveRDS(sam_pw_hh_race,file = paste0(housingdir, vintage, "/sam_pw_hh_race_",Sys.Date(),".RDS"))
    
    #get relations and start matching... preferring numbers from individual totals when necessary
    
    
    #should remember kids_grand_age, too
    #we have household type by kids, household type by seniors and household type by whole population, - can also get family_type from sam_hh by age, but this may be something to write back over... 
    #    hh_type_kids #very close to sex_age_race <18yo, but have to add after sam_hh is added to say who has or doesn't have children!!
    #on sam_hh already kids_ages_dt #5 age range, family_type - F/M householder or married couple - only own_kids - needs related kids to find
    
    
    
    
    
    hh_relations_race <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/hh_relations_race_2020-05-30.RDS")
    hh_relations_eth <- readRDS("/Users/dan/Downloads/UH_OneDrive/OneDrive - University Of Houston/Social Network Hypergraphs/HCAD/2017/hh_relations_eth_2020-05-30.RDS")
    
    
    
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
    
  
    
    saveRDS(hh_relations_eth,file = paste0(housingdir, vintage, "/hh_relations_eth_",Sys.Date(),".RDS"))
    saveRDS(hh_relations_race,file = paste0(housingdir, vintage, "/hh_relations_race_",Sys.Date(),".RDS"))
  }
}

  

