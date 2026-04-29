setwd("~/Documents/SyntheticDataSet")
source('Sam/get_tools.R')
library(stringr)
library(data.table)
maindir = "~/University\ Of\ Houston/Engaged\ Data\ Science\ -\ Data/" #Dan Studio
#maindir = "~/Documents/Sam_data/" #if need local
censusdir = paste0(maindir,"Census/") 
vintage = "2020"
state = "48" #48 Texas; 22 Louisiana
county = "*" 
tract = "*"
#you don't need a censuskey if you're not pulling new files down; you can only use this one if you have correct access to mine on the OneDrive
censuskey <- readLines(paste0(censusdir, "2017", "/key"))

#get saved files
file_path <- valid_file_path(censusdir,vintage,state,county="*",api_type="dec",geo_type="block_group",
                             groupname="bg_SARE",path_suff="wrk")
#"~/University Of Houston/Engaged Data Science - Data/Census/2020/state_48/2020_48_dec_block_group_bg_hhSARETT_wrk.RDS"
bg_SARE <- readRDS(file_path)

#and get bg_hhSARETT
file_path <- valid_file_path(censusdir,vintage,state,county="*",api_type="dec",geo_type="block_group",
                             groupname="bg_hhSARETT",path_suff="wrk")

bg_hhSARETT <- readRDS(file_path)

#remember anyone_60, etc., has more, presumably when householder is younger
#create unique identifiers
bg_SARE[,("ind_ID"):=paste0(GEOID,as.character(1000000+(1:.N)))]
bg_GQ[,("gq_ID"):=paste0(GEOID,as.character(1000000+(1:.N)))]
bg_hhSARETT[,("hh_ID"):=paste0(GEOID,as.character(1000000+(1:.N)))]
#fix age_range
bg_SARE[,("age_range_9hh"):=fcase(age_num>14&age_num<25,"15 to 24 years",
                                  age_num>24&age_num<35,"25 to 34 years",
                                  age_num>34&age_num<45,"35 to 44 years",
                                  age_num>44&age_num<55,"45 to 54 years",
                                  age_num>54&age_num<60,"55 to 59 years",
                                  age_num>59&age_num<65,"60 to 64 years",
                                  age_num>64&age_num<75,"65 to 74 years",
                                  age_num>74&age_num<85,"75 to 84 years",
                                  age_num>84,"85 years and over",
                                  default = age_range)]
bg_hhSARETT[,("age_range_9hh"):=str_remove_all(age_range_9,"Householder ")]
bg_hhSARETT[,("hh_match1_id"):=
             paste0(tract,re_code_14,age_range_9hh,sex,alone,as.character(100000+sample(1:.N))),
           by=.(tract,re_code_14,age_range_9hh,sex,alone)]
bg_SARE[role=="Householder",("hh_match1_id"):=
               paste0(tract,re_code,age_range_9hh,sex,alone,as.character(100000+sample(1:.N))),
             by=.(tract,re_code,age_range_9hh,sex,alone)]
bg_hhSARETT[,c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match1_id)]]
bg_SARE[role=="Householder",c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3"):=
               bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                                 list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                                 list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                                 list(household_60),list(household_65),list(household_75),list(rent_own),
                                 list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3)),on=.(hh_match1_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #5929250 56% not matching, mostly because of incomplete data on alone and sex
#try without alone or sex
bg_hhSARETT[is.na(ind_ID),("hh_match2_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[role=="Householder"&is.na(hh_ID),("hh_match2_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match2_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                              "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                              "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3)),on=.(hh_match2_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #1161418 - 11% not matching
#with age_range_3hh
bg_hhSARETT[,("age_range_3hh"):=fcase(as.numeric(str_sub(age_range_9hh,1,2))<45,"15 to 44 years",
                                      as.numeric(str_sub(age_range_9hh,1,2))>44&as.numeric(str_sub(age_range_9hh,1,2))<65,"45 to 64 years",
                                      as.numeric(str_sub(age_range_9hh,1,2))>64,"65 years and over",
                                      default = age_range_9hh)]
bg_SARE[,("age_range_3hh"):=fcase(as.numeric(str_sub(age_range_9hh,1,2))>15&as.numeric(str_sub(age_range_9hh,1,2))<45,"15 to 44 years",
                                      as.numeric(str_sub(age_range_9hh,1,2))>44&as.numeric(str_sub(age_range_9hh,1,2))<65,"45 to 64 years",
                                      as.numeric(str_sub(age_range_9hh,1,2))>64,"65 years and over",
                                      default = age_range_9hh)] #warnings because of "Under"
bg_hhSARETT[is.na(ind_ID),("hh_match2a_id"):=
              paste0(tract,re_code_14,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_3hh)]
bg_SARE[role=="Householder"&is.na(hh_ID),("hh_match2a_id"):=
          paste0(tract,re_code,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_3hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match2a_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                                           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3)),on=.(hh_match2a_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #783226 - 7% not matching
#with only age_range_3hh
bg_hhSARETT[is.na(ind_ID),("hh_match3_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[role=="Householder"&is.na(hh_ID),("hh_match3_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match3_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                                           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3)),on=.(hh_match3_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #454386 - 4%
#get the 4% from folks not matching on role=="Householder"
bg_hhSARETT[is.na(ind_ID),("hh_match4_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[is.na(hh_ID),("hh_match4_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match4_id)]]
bg_SARE[is.na(hh_ID),c("role_new","hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                                           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3"):=
          bg_hhSARETT[.SD,c("Householder",list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3)),on=.(hh_match4_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #42922 - is it just tracts being funky? GQ stuff haunting us? 

#one more try
bg_hhSARETT[is.na(ind_ID),("hh_match5_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[is.na(hh_ID),("hh_match5_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match5_id)]]
bg_SARE[is.na(hh_ID),c("role_new","hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                       "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                       "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3"):=
          bg_hhSARETT[.SD,c("Householder",list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3)),on=.(hh_match5_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #463 - remarkably evenly distributed. Don't try to capture
#because we're using bg_hhSARETT as having better household info, projecting to it
#looks like you're only counted as an unmarried partner if there's a child! hh_size_7 clearly works that way, but not sure how it relates to underlying count
#with own children under 18 also seems to require at least three people total. No single parent with single kids??- have to rethink!!


#assign significant others 


#assign children

#assign other relatives

#assign others

#assign GQ?

#and get GQ, b/c never satisfied with how it broke out
file_path <- valid_file_path(censusdir,vintage,state,county="*",api_type="dec",geo_type="block_group",
                             groupname="bg_GQ",path_suff="wrk")
#"~/University Of Houston/Engaged Data Science - Data/Census/2020/state_48/2020_48_dec_block_group_bg_hhSARETT_wrk.RDS"
bg_GQ <- readRDS(file_path)



#use bg_hhSARETT as ground for householders, but write over to bg_SARE? Keep two files

#nrow(bg_SARE[role=="Householder"])-nrow(bg_hhSARETT) #-81530 (0.7%) (a good bit is artifact of Sam_3, but quite different at tract level even back to orig; in Sam_3, off by single digits from beginning, off by 100s per tract now)
#test_hh <- table(bg_SARE[role=="Householder",GEOID])-table(bg_hhSARETT[,GEOID]) #doesn't work at tract, either
#max(test_hh)#272
#mean(test_hh[test_hh>0]) #31.24
#
#length(unique(bg_SARE[,GEOID])) #18561
#length(unique(bg_hhSARETT[,GEOID])) #18524
#length(unique(bg_SARE[,tract])) #6868
#length(unique(bg_hhSARETT[,tract])) #6855

#classifying topos is the target / destination for the structures of a model for the functors; working backwards from the tract as classifying topos is what the adjoint gets us
