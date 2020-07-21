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
    #SOMETHING WEIRD WITH NUMBER OF WORKERS!!!!
    sam_eth_hh[as.numeric(substr(hh_size,1,1))>4 & number_workers_in_hh=="00 workers",("number_workers_in_hh"):="03 workers"]
    sam_eth_hh[as.numeric(substr(hh_size,1,1))>4 & number_workers_in_hh=="01 worker",("number_workers_in_hh"):="04 workers"]
    sam_eth_hh[number_workers_in_hh=="00 workers",("employment"):="Not in labor force"] #about 4500 strays
    sam_eth_hh[number_workers_in_hh=="00 workers",("wife_employed"):="Wife not in labor force"]
    sam_eth_hh[number_workers_in_hh=="01 worker" & employment!="Not in labor force",("wife_employed"):="Wife not in labor force"]
    sam_eth_hh[,("numeric_workers"):=as.numeric(substr(number_workers_in_hh,1,2))]
    sam_eth_hh[,("numeric_workers"):=if_else(employment=="Not in labor force",numeric_workers,numeric_workers-1)]
    sam_eth_hh[,("numeric_workers"):=if_else(wife_employed=="Wife not in labor force",numeric_workers,numeric_workers-1)]
    sam_eth_hh[number_workers_in_hh=="04 workers" & as.numeric(substr(hh_size,1,1))>4,
               ("numeric_workers"):=as.numeric(substr(hh_size,1,1))] #a little extreme, but not sure what else to do
    sam_eth_hh[numeric_workers==7,
               ("numeric_workers"):=sample(7:10,size=.N,replace = TRUE)] #a little extreme, but not sure what else to do
    sam_race_hh[as.numeric(substr(hh_size,1,1))>4 & number_workers_in_hh=="00 workers",("number_workers_in_hh"):="03 workers"]
    sam_race_hh[as.numeric(substr(hh_size,1,1))>4 & number_workers_in_hh=="01 worker",("number_workers_in_hh"):="04 workers"]
    sam_race_hh[number_workers_in_hh=="00 workers",("employment"):="Not in labor force"] #about 4500 strays
    sam_race_hh[number_workers_in_hh=="00 workers",("wife_employed"):="Wife not in labor force"]
    sam_race_hh[number_workers_in_hh=="01 worker" & employment!="Not in labor force",("wife_employed"):="Wife not in labor force"]
    sam_race_hh[,("numeric_workers"):=as.numeric(substr(number_workers_in_hh,1,2))]
    sam_race_hh[,("numeric_workers"):=if_else(employment=="Not in labor force",numeric_workers,numeric_workers-1)]
    sam_race_hh[,("numeric_workers"):=if_else(wife_employed=="Wife not in labor force",numeric_workers,numeric_workers-1)]
    sam_race_hh[number_workers_in_hh=="04 workers" & as.numeric(substr(hh_size,1,1))>4,
               ("numeric_workers"):=as.numeric(substr(hh_size,1,1))] #a little extreme, but not sure what else to do
    sam_race_hh[numeric_workers==7,
               ("numeric_workers"):=sample(7:10,size=.N,replace = TRUE)] #a little extreme, but not sure what else to do
    hh_workers_eth <- sam_eth_hh[,uncount(.SD[numeric_workers>0],numeric_workers,.remove = TRUE,.id="worker_id")]
    hh_workers_race <- sam_race_hh[,uncount(.SD[numeric_workers>0],numeric_workers,.remove = TRUE,.id="worker_id")]
    
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
    ##reshuffle <- as.data.frame(Sys.time())
    ##reshuffle$eth_w1 <- nrow(wives_eth[!is.na(industry)])
    ##reshuffle$eth_w1T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
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
    ##reshuffle$eth_w2 <- nrow(wives_eth[!is.na(industry)])
    ##reshuffle$eth_w2T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
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
    ##reshuffle$eth_w3 <- nrow(wives_eth[!is.na(industry)])
    ##reshuffle$eth_w3T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
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
                  list(age_range_w3),list(age_range_6)),
                  on = .(pw3_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=wives_eth[.SD,industry,on = .(pw3_match_id)]]
    ##reshuffle$eth_w4 <- nrow(wives_eth[!is.na(industry)])
    ##reshuffle$eth_w4T <- nrow(wives_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])
    ##reshuffle$eth_w_left <- nrow(wives_eth[employment!="Not in labor force"])-nrow(wives_eth[!is.na(industry)])
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
    ##reshuffle$eth_p1 <- nrow(partners_eth[!is.na(industry)])
    ##reshuffle$eth_p1T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-
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
    ##reshuffle$eth_p2 <- nrow(partners_eth[!is.na(industry)])
    ##reshuffle$eth_p2T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-nrow(wives_eth[!is.na(industry)])
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
    ##reshuffle$eth_p3 <- nrow(partners_eth[!is.na(industry)])
    ##reshuffle$eth_p3T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-nrow(wives_eth[!is.na(industry)])
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
                  list(age_range_w3),list(age_range_6)),
                  on = .(p3_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=partners_eth[.SD,industry,on = .(p3_match_id)]]
    ##reshuffle$eth_p4 <- nrow(partners_eth[!is.na(industry)])
    ##reshuffle$eth_p4T <- nrow(partners_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-nrow(wives_eth[!is.na(industry)])
    ##reshuffle$eth_p_left <- nrow(partners_eth[employment!="Not in labor force"])-nrow(partners_eth[!is.na(industry)])
    
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
    ##reshuffle$race_w1 <- nrow(wives_race[!is.na(industry)])
    ##reshuffle$race_w1T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])
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
    ##reshuffle$race_w2 <- nrow(wives_race[!is.na(industry)])
    ##reshuffle$race_w2T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])
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
    ##reshuffle$race_w3 <- nrow(wives_race[!is.na(industry)])
    ##reshuffle$race_w3T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])
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
                  list(age_range_w3),list(age_range_6)),
                  on = .(pw3_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=wives_race[.SD,industry,on = .(pw3_match_id)]]
    ##reshuffle$race_w4 <- nrow(wives_race[!is.na(industry)])
    ##reshuffle$race_w4T <- nrow(wives_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])
    ##reshuffle$race_w_left <- nrow(wives_race[employment!="Not in labor force"])-nrow(wives_race[!is.na(industry)])
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
    ##reshuffle$race_p1 <- nrow(partners_race[!is.na(industry)])
    ##reshuffle$race_p1T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-
    ##  nrow(wives_race[!is.na(industry)])   
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
    ##reshuffle$race_p2 <- nrow(partners_race[!is.na(industry)])
    ##reshuffle$race_p2T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-nrow(wives_race[!is.na(industry)])
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
    ##reshuffle$race_p3 <- nrow(partners_race[!is.na(industry)])
    ##reshuffle$race_p3T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-nrow(wives_race[!is.na(industry)])
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
                     list(age_range_w3),list(age_range_6)),
                     on = .(p3_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=partners_race[.SD,industry,on = .(p3_match_id)]]
    ##reshuffle$race_p4 <- nrow(partners_race[!is.na(industry)])
    ##reshuffle$race_p4T <- nrow(partners_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-nrow(wives_race[!is.na(industry)])
    ##reshuffle$race_p_left <- nrow(partners_race[employment!="Not in labor force"])-nrow(partners_race[!is.na(industry)])
    
    #hh_workers_eth
    hh_workers_eth[,c("industry","occupation","commute_time","when_go_to_work","income_range_workers"):=NULL]
    hh_workers_eth[employment!="Not in labor force",
                 ("w_match_id"):=paste0(tract,sex,ethnicity,
                                        language,English_level,
                                        age_range_6, 
                                        as.character(1000000+sample(1:.N))),
                 by = .(tract,sex,ethnicity,language,English_level,age_range_6)]
    additional_workers_eth[is.na(missing_workers),("w_match_id"):=paste0(tract,sex,ethnicity,
                                                                         language,English_level,
                                                                         age_range_6,
                                                                         as.character(1000000+sample(1:.N))),
                           by = .(tract,sex,ethnicity,language,English_level,age_range_6)]
    hh_workers_eth[employment!="Not in labor force",
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers"):=
                   additional_workers_eth[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range)),
                     on = .(w_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=hh_workers_eth[.SD,industry,on = .(w_match_id)]]
    #reshuffle test
    ##reshuffle$eth_we1 <- nrow(hh_workers_eth[!is.na(industry)])
    ##reshuffle$eth_we1T <- nrow(hh_workers_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-
    ##  nrow(wives_eth[!is.na(industry)])-nrow(partners_eth[!is.na(industry)])   
    #reshuffle
    #change age_range for better matches and fix odd cases post_hoc?
    hh_workers_eth[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    reshuff_add_w <- additional_workers_eth[is.na(missing_workers)]
    additional_workers_eth[is.na(missing_workers),("reshuffw_id"):=
                             paste0(tract,as.character(1000000+sample(1:.N))),
                           by=.(tract)]
    reshuff_add_w[is.na(missing_workers),("reshuffw_id"):=
                     paste0(tract,as.character(1000000+sample(1:.N))),
                   by=.(tract)]
    additional_workers_eth[is.na(missing_workers),
                           c("ethnicity","language","English_level"):=
                             reshuff_add_w[.SD, c(list(ethnicity),list(language),list(English_level)),
                                            on=.(reshuffw_id)]]
    #recast
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                 ("w1_match_id"):=paste0(tract,ethnicity,
                                         language,English_level,
                                         age_range_w3, 
                                         as.character(1000000+sample(1:.N))),
                 by = .(tract,ethnicity,language,English_level,age_range_w3)]
    additional_workers_eth[is.na(missing_workers),("w1_match_id"):=paste0(tract,ethnicity,
                                                                          language,English_level,
                                                                          age_range_w3,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,ethnicity,language,English_level,age_range_w3)]
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers"):=
                   additional_workers_eth[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range)),
                     on = .(w1_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=hh_workers_eth[.SD,industry,on = .(w1_match_id)]]
    ##reshuffle$eth_we2 <- nrow(hh_workers_eth[!is.na(industry)])
    ##reshuffle$eth_we2T <- nrow(hh_workers_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-
    ##  nrow(wives_eth[!is.na(industry)])-nrow(partners_eth[!is.na(industry)])
    #pick up stragglers - info will be lost - age_range_w3 is the one not matching more, so let it get a few mixed up on ethnicity
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                 ("w2_match_id"):=paste0(tract,#ethnicity,
                                         age_range_w3, 
                                         as.character(1000000+sample(1:.N))),
                 by = .(tract,age_range_w3)]
    additional_workers_eth[is.na(missing_workers),("w2_match_id"):=paste0(tract,#ethnicity,
                                                                          age_range_w3,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,age_range_w3)]
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers","ethnicity_worker"):=
                   additional_workers_eth[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range),list(ethnicity)),
                     on = .(w2_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=hh_workers_eth[.SD,industry,on = .(w2_match_id)]]
    ##reshuffle$eth_we3 <- nrow(hh_workers_eth[!is.na(industry)])
    ##reshuffle$eth_we3T <- nrow(hh_workers_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-
    ##  nrow(wives_eth[!is.na(industry)])-nrow(partners_eth[!is.na(industry)])
    #dropping age - will need to fix later!! householder_age can be matched...
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                 ("w3_match_id"):=paste0(tract,ethnicity,
                                         as.character(1000000+sample(1:.N))),
                 by = .(tract,ethnicity)]
    additional_workers_eth[is.na(missing_workers),("w3_match_id"):=paste0(tract,ethnicity,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,ethnicity)]
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                 c("industry","occupation","commute_time","when_go_to_work",
                   "means_transport","income_range_workers",
                   "age_range_w3_worker","age_range_6_worker"):=
                   additional_workers_eth[.SD, c(
                     list(industry),list(occupation),
                     list(commute_time),list(when_go_to_work),
                     list(means_transport),list(income_range),
                     list(age_range_w3),list(age_range_6)),
                     on = .(w3_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=hh_workers_eth[.SD,industry,on = .(w3_match_id)]]
    ##reshuffle$eth_we4 <- nrow(hh_workers_eth[!is.na(industry)])
    ##reshuffle$eth_we4T <- nrow(hh_workers_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-
    ##  nrow(wives_eth[!is.na(industry)])-nrow(partners_eth[!is.na(industry)])
    ##reshuffle$addw_left <- nrow(additional_workers_eth[is.na(missing_workers)])
    #only tract - will need to fix later!! householder_age can be matched...
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                   ("w4_match_id"):=paste0(tract,
                                           as.character(1000000+sample(1:.N))),
                   by = .(tract)]
    additional_workers_eth[is.na(missing_workers),("w4_match_id"):=paste0(tract,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract)]
    hh_workers_eth[employment!="Not in labor force"&is.na(industry),
                   c("industry","occupation","commute_time","when_go_to_work",
                     "means_transport","income_range_workers","age_range_w3_worker",
                     "age_range_6_worker","ethnicity_worker"):=
                     additional_workers_eth[.SD, c(
                       list(industry),list(occupation),
                       list(commute_time),list(when_go_to_work),
                       list(means_transport),list(income_range),
                       list(age_range_w3),list(age_range_6),list(ethnicity)),
                       on = .(w4_match_id)]]
    #take out of additional_workers
    additional_workers_eth[is.na(missing_workers),("missing_workers"):=hh_workers_eth[.SD,industry,on = .(w4_match_id)]]
    ##reshuffle$eth_we5 <- nrow(hh_workers_eth[!is.na(industry)])
    ##reshuffle$eth_we5T <- nrow(hh_workers_eth[!is.na(industry)])==nrow(additional_workers_eth[!is.na(missing_workers)])-
    ##  nrow(wives_eth[!is.na(industry)])-nrow(partners_eth[!is.na(industry)])
    ##reshuffle$eth_we_left <- nrow(hh_workers_eth[employment!="Not in labor force"])-nrow(hh_workers_eth[!is.na(industry)])
    ##reshuffle$addw_leftF <- nrow(additional_workers_eth[is.na(missing_workers)])
    
    #hh_workers_race
    hh_workers_race[,c("industry","occupation","commute_time","when_go_to_work","income_range_workers"):=NULL]
    hh_workers_race[employment!="Not in labor force",
                   ("w_match_id"):=paste0(tract,sex,race,
                                          language,English_level,
                                          age_range_6, 
                                          as.character(1000000+sample(1:.N))),
                   by = .(tract,sex,race,language,English_level,age_range_6)]
    additional_workers_race[is.na(missing_workers),("w_match_id"):=paste0(tract,sex,race,
                                                                         language,English_level,
                                                                         age_range_6,
                                                                         as.character(1000000+sample(1:.N))),
                           by = .(tract,sex,race,language,English_level,age_range_6)]
    hh_workers_race[employment!="Not in labor force",
                   c("industry","occupation","commute_time","when_go_to_work",
                     "means_transport","income_range_workers"):=
                     additional_workers_race[.SD, c(
                       list(industry),list(occupation),
                       list(commute_time),list(when_go_to_work),
                       list(means_transport),list(income_range)),
                       on = .(w_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=hh_workers_race[.SD,industry,on = .(w_match_id)]]
    #reshuffle test
    ##reshuffle$race_wr1 <- nrow(hh_workers_race[!is.na(industry)])
    ##reshuffle$race_wr1T <- nrow(hh_workers_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-
    ##  nrow(wives_race[!is.na(industry)])-nrow(partners_race[!is.na(industry)])   
    #reshuffle
    #change age_range for better matches and fix odd cases post_hoc?
    hh_workers_race[,("age_range_w3"):=case_when(
      age_range_6=="15 to 24 years" | age_range_6=="25 to 44 years" ~ "15 to 44 years",
      age_range_6=="45 to 54 years" | age_range_6=="55 to 59 years" ~ "45 to 59 years",
      TRUE ~ "60 years and over"
    )]
    reshuff_add_wr <- additional_workers_eth[is.na(missing_workers)]
    additional_workers_race[is.na(missing_workers),("reshuffwr_id"):=
                             paste0(tract,as.character(1000000+sample(1:.N))),
                           by=.(tract)]
    reshuff_add_wr[,("reshuffwr_id"):=
                    paste0(tract,as.character(1000000+sample(1:.N))),
                  by=.(tract)]
    additional_workers_race[is.na(missing_workers),
                           c("race","language","English_level"):=
                             reshuff_add_wr[.SD, c(list(race),list(language),list(English_level)),
                                           on=.(reshuffwr_id)]]
    #recast
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                   ("w1_match_id"):=paste0(tract,race,
                                           language,English_level,
                                           age_range_w3, 
                                           as.character(1000000+sample(1:.N))),
                   by = .(tract,race,language,English_level,age_range_w3)]
    additional_workers_race[is.na(missing_workers),("w1_match_id"):=paste0(tract,race,
                                                                          language,English_level,
                                                                          age_range_w3,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,race,language,English_level,age_range_w3)]
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                   c("industry","occupation","commute_time","when_go_to_work",
                     "means_transport","income_range_workers"):=
                     additional_workers_race[.SD, c(
                       list(industry),list(occupation),
                       list(commute_time),list(when_go_to_work),
                       list(means_transport),list(income_range)),
                       on = .(w1_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=hh_workers_race[.SD,industry,on = .(w1_match_id)]]
    ##reshuffle$race_wr2 <- nrow(hh_workers_race[!is.na(industry)])
    ##reshuffle$race_wr2T <- nrow(hh_workers_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-
    ##  nrow(wives_race[!is.na(industry)])-nrow(partners_race[!is.na(industry)])
    #pick up stragglers - info will be lost - age_range_w3 is the one not matching more, so let it get a few mixed up on ethnicity
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                   ("w2_match_id"):=paste0(tract,
                                           age_range_w3, 
                                           as.character(1000000+sample(1:.N))),
                   by = .(tract,age_range_w3)]
    additional_workers_race[is.na(missing_workers),("w2_match_id"):=paste0(tract,
                                                                          age_range_w3,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,age_range_w3)]
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                   c("industry","occupation","commute_time","when_go_to_work",
                     "means_transport","income_range_workers","race_worker"):=
                     additional_workers_race[.SD, c(
                       list(industry),list(occupation),
                       list(commute_time),list(when_go_to_work),
                       list(means_transport),list(income_range),list(race)),
                       on = .(w2_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=hh_workers_race[.SD,industry,on = .(w2_match_id)]]
    ##reshuffle$race_wr3 <- nrow(hh_workers_race[!is.na(industry)])
    ##reshuffle$race_wr3T <- nrow(hh_workers_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-
    ##  nrow(wives_race[!is.na(industry)])-nrow(partners_race[!is.na(industry)])
    #dropping age - will need to fix later!! householder_age can be matched...
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                   ("w3_match_id"):=paste0(tract,race,
                                           as.character(1000000+sample(1:.N))),
                   by = .(tract,race)]
    additional_workers_race[is.na(missing_workers),("w3_match_id"):=paste0(tract,race,
                                                                          as.character(1000000+sample(1:.N))),
                           by = .(tract,race)]
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                   c("industry","occupation","commute_time","when_go_to_work",
                     "means_transport","income_range_workers","age_range_w3_worker","age_range_6_worker"):=
                     additional_workers_race[.SD, c(
                       list(industry),list(occupation),
                       list(commute_time),list(when_go_to_work),
                       list(means_transport),list(income_range),
                       list(age_range_w3),list(age_range_6)),
                       on = .(w3_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=hh_workers_race[.SD,industry,on = .(w3_match_id)]]
    ##reshuffle$race_wr4 <- nrow(hh_workers_race[!is.na(industry)])
    ##reshuffle$race_wr4T <- nrow(hh_workers_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-
    ##  nrow(wives_race[!is.na(industry)])-nrow(partners_race[!is.na(industry)])
    ##reshuffle$race_wr_left <- nrow(hh_workers_race[employment!="Not in labor force"])-nrow(hh_workers_race[!is.na(industry)])
    ##reshuffle$addwr_left <- nrow(additional_workers_race[is.na(missing_workers)])
    #dropping age - will need to fix later!! householder_age can be matched...
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                    ("w4_match_id"):=paste0(tract,
                                            as.character(1000000+sample(1:.N))),
                    by = .(tract)]
    additional_workers_race[is.na(missing_workers),("w4_match_id"):=paste0(tract,
                                                                           as.character(1000000+sample(1:.N))),
                            by = .(tract)]
    hh_workers_race[employment!="Not in labor force"&is.na(industry),
                    c("industry","occupation","commute_time","when_go_to_work",
                      "means_transport","income_range_workers","age_range_w3_worker","age_range_6_worker"):=
                      additional_workers_race[.SD, c(
                        list(industry),list(occupation),
                        list(commute_time),list(when_go_to_work),
                        list(means_transport),list(income_range),
                        list(age_range_w3),list(age_range_6)),
                        on = .(w4_match_id)]]
    #take out of additional_workers
    additional_workers_race[is.na(missing_workers),("missing_workers"):=hh_workers_race[.SD,industry,on = .(w4_match_id)]]
    ##reshuffle$race_wr5 <- nrow(hh_workers_race[!is.na(industry)])
    ##reshuffle$race_wr5T <- nrow(hh_workers_race[!is.na(industry)])==nrow(additional_workers_race[!is.na(missing_workers)])-
    ##  nrow(wives_race[!is.na(industry)])-nrow(partners_race[!is.na(industry)])
    ##reshuffle$race_wr_left <- nrow(hh_workers_race[employment!="Not in labor force"])-nrow(hh_workers_race[!is.na(industry)])
    ##reshuffle$addwr_leftF <- nrow(additional_workers_race[is.na(missing_workers)])
    #44k workers not assigned a household!!! saving whole, but only is.na(missing_workers weren't assigned) - may be able to reshuffle
    saveRDS(additional_workers_eth,file = paste0(housingdir, vintage, "/additional_workers_eth_",Sys.Date(),".RDS"))
    saveRDS(additional_workers_race,file = paste0(housingdir, vintage, "/additional_workers_race_",Sys.Date(),".RDS"))
    #save and find places for workers_leftF??
#subvert shallow copy by making a NULL call in right places
        
    #bindrows into one whole household_ids will match
    sam_pw_hh_eth <- bind_rows(sam_eth_hh,partners_eth,wives_eth,hh_workers_eth)
    sam_pw_hh_race <- bind_rows(sam_race_hh,partners_race,wives_race,hh_workers_race)
    saveRDS(sam_pw_hh_eth,file = paste0(housingdir, vintage, "/sam_pw_hh_eth_",Sys.Date(),".RDS"))
    saveRDS(sam_pw_hh_race,file = paste0(housingdir, vintage, "/sam_pw_hh_race_",Sys.Date(),".RDS"))
    
    
  }
}

  

