setwd("~/Documents/SyntheticDataSet")
source('Sam/get_tools.R')
library(stringr)
library(data.table)
library(readxl) 
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

file_path <- valid_file_path(censusdir,vintage,state,county="*",api_type="dec",geo_type="block_group",
                             groupname="bg_GQ",path_suff="wrk")
bg_GQ <- readRDS(file_path)

#remember anyone_60, etc., has more, presumably when householder is younger
#create unique identifiers
bg_SARE[,("ind_ID"):=paste0(GEOID,as.character(1000000+(1:.N)))]
bg_GQ[,("gq_ID"):=paste0(GEOID,as.character(1000000+(1:.N)))]
bg_hhSARETT[,("hh_ID"):=paste0(GEOID,as.character(1000000+(1:.N)))]
bg_hhSARETT[,("role"):="Householder"]
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
           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
               bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                                 list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                                 list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                                 list(household_60),list(household_65),list(household_75),list(rent_own),
                                 list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list("Householder")),on=.(hh_match1_id)]]
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
                              "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list("Householder")),on=.(hh_match2_id)]]
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
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list("Householder")),on=.(hh_match2a_id)]]
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
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list("Householder")),on=.(hh_match3_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #454386 - 4%
#get the 4% from folks not matching on role=="Householder"
bg_hhSARETT[is.na(ind_ID),("hh_match4_id"):=
              paste0(tract,re_code_7,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_7,age_range_9hh)]
bg_SARE[is.na(hh_ID)&!str_detect(role,"stitutional"),("hh_match4_id"):=
          paste0(tract,re_code_7,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code_7,age_range_9hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match4_id)]]
bg_SARE[is.na(hh_ID)&!str_detect(role,"stitutional"),c("role_new","hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                                           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(role),list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list("Householder")),on=.(hh_match4_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #42922; 411k new householders - is it just tracts being funky? GQ stuff haunting us? 

#one more try
bg_hhSARETT[is.na(ind_ID),("hh_match5_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[is.na(hh_ID)&!str_detect(role,"stitutional"),("hh_match5_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match5_id)]]
bg_SARE[is.na(hh_ID)&!str_detect(role,"stitutional"),c("role_new","hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                       "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                       "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(role),list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list("Householder")),on=.(hh_match5_id)]]
nrow(bg_hhSARETT[is.na(ind_ID)]) #463 - remarkably evenly distributed. Don't try to capture
#because we're using bg_hhSARETT as having better household info, projecting to it
#looks like you're only counted as an unmarried partner if there's a child! hh_size_7 clearly works that way, but not sure how it relates to underlying count
#with own children under 18 also seems to require at least three people total. No single parent with kids?? all I did was match on family and rent_own, but somehow that fell out?? single parent with kids is not family? own_kids?

#do tables on role_new and role to see if there's anything too funky about just writing over?
#table(bg_SARE[,role],bg_SARE[,role_new],useNA = "ifany") #biggest groups are opposite-sex spouse and biological child
#table(bg_SARE[role_new=="Householder"&role=="Opposite-sex spouse",sex])
#Female   Male 
#108211  84127

#match for spouses and partners, with folks close to same age
#they were supposed to have made it so that Householder could be female in a married couple in 2020, but I couldn't see any evidence for how that was implemented; I let them distribute differently at end
bg_hhSARETT[,("sex"):=fcase(couple_gender=="Female-female married couple households"|couple_gender=="Female-female unmarried partner household","Female",default = "Male")]#end up making a lot more female, later

bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households",("role_match_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&role=="Same-sex spouse"&is.na(hh_ID),("role_match_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households",c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match_id)]]
bg_SARE[sex=="Male"&role=="Same-sex spouse"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex spouse")),on=.(role_match_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&!is.na(spouse_partner_ID)]) #only matched about 4546 of 25k; lots of problems with bg_SARE age_range on couple_gender
#nrow(bg_SARE[sex=="Male"&role=="Same-sex spouse"&is.na(hh_ID)]) #19416
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),("role_match1_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),("role_match1_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match1_id)]]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex spouse")),on=.(role_match1_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&!is.na(spouse_partner_ID)]) #22990
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"])-nrow(bg_hhSARETT[!is.na(spouse_partner_ID)]) #1719
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),("role_match2_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),("role_match2_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match2_id)]]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex spouse")),on=.(role_match2_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"]) #24709
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&!is.na(spouse_partner_ID)]) #24587

#female-female married
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),("role_match3_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&role=="Same-sex spouse"&is.na(hh_ID),("role_match3_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match3_id)]]
bg_SARE[sex=="Female"&role=="Same-sex spouse"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex spouse")),on=.(role_match3_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&!is.na(spouse_partner_ID)]) #only matched about 5571 
#nrow(bg_SARE[sex=="Female"&role=="Same-sex spouse"&!is.na(hh_ID)])
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),("role_match4_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),("role_match4_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match4_id)]]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex spouse")),on=.(role_match4_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"]) #29795
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&!is.na(spouse_partner_ID)]) #28149
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),("role_match5_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),("role_match5_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match5_id)]]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex spouse")),on=.(role_match5_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"]) #29795
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&!is.na(spouse_partner_ID)]) #29699

#for unmarried partner couples
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household",("role_match6_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&role=="Same-sex unmarried partner"&is.na(hh_ID),("role_match6_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household",c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match6_id)]]
bg_SARE[sex=="Male"&role=="Same-sex unmarried partner"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex unmarried partner")),on=.(role_match6_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"]) #19801
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&!is.na(spouse_partner_ID)]) #only matched 3498 
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),("role_match7_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),("role_match7_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match7_id)]]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex unmarried partner")),on=.(role_match7_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&!is.na(spouse_partner_ID)]) #18443
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),("role_match8_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),("role_match8_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match8_id)]]
bg_SARE[sex=="Male"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex unmarried partner")),on=.(role_match8_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"]) #19801
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&!is.na(spouse_partner_ID)]) #19699

#female-female unmarried partner
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),("role_match9_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&role=="Same-sex unmarried partner"&is.na(hh_ID),("role_match9_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match9_id)]]
bg_SARE[sex=="Female"&role=="Same-sex unmarried partner"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex unmarried partner")),on=.(role_match9_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&!is.na(spouse_partner_ID)]) #only matched about 3777
#nrow(bg_SARE[sex=="Female"&role=="Same-sex unmarried partner"&is.na(hh_ID)])
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),("role_match10_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),("role_match10_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match10_id)]]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex unmarried partner")),on=.(role_match10_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&!is.na(spouse_partner_ID)]) #20942
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),("role_match11_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),("role_match11_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match11_id)]]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Same sex unmarried partner")),on=.(role_match11_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"]) #22276
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&!is.na(spouse_partner_ID)]) #22246
#opposite-sex married couples - use same_sex
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match12_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&role=="Opposite-sex spouse",("role_match12_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match12_id)]]
bg_SARE[sex=="Female"&role=="Opposite-sex spouse",c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match12_id)]]
nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"]) #5025134
nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #2492590
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match13_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),("role_match13_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match13_id)]]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match13_id)]]
nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #2737785
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match14_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),("role_match14_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14_id)]]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match14_id)]]
nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #2900862
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match14a_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),("role_match14a_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14a_id)]]
bg_SARE[sex=="Female"&str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match14a_id)]]
nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #2924022 #that is, not much additional
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match14c_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[str_detect(role,"-sex")&is.na(hh_ID),("role_match14c_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14c_id)]]
bg_SARE[str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match14c_id)]]
nrow(bg_hhSARETT[same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #4464907
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),("role_match14b_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[str_detect(role,"-sex")&is.na(hh_ID),("role_match14b_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),
            c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14b_id)]]
bg_SARE[str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match14b_id)]]
nrow(bg_hhSARETT[same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #4557724 
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),("role_match14c_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[str_detect(role,"-sex")&is.na(hh_ID),("role_match14c_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),
            c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14c_id)]]
bg_SARE[str_detect(role,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match14c_id)]]
nrow(bg_hhSARETT[same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #4516384 #i.e., 500k short, all "Married couple family" in hh, 
#pick up leftover householders from bg_SARE
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),("role_match14d_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[role=="Householder"&is.na(hh_ID),("role_match14d_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),
            c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14d_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match14d_id)]]
nrow(bg_hhSARETT[!is.na(spouse_partner_ID)]) #5027818

#get from others - but have to think about what that does for using bg_SARE role
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),("role_match15_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[!str_detect(role,"stitutional")&is.na(hh_ID),("role_match15_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),
            c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match15_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match15_id)]]
#nrow(bg_hhSARETT[!is.na(same_sex)])#5719560
nrow(bg_hhSARETT[!is.na(same_sex)&!is.na(spouse_partner_ID)]) #5585600 (2% not matched)
#without re_code
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),("role_match15a_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[!str_detect(role,"stitutional")&is.na(hh_ID),("role_match15a_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),
            c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match15a_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match15a_id)]]
#nrow(bg_hhSARETT[!is.na(same_sex)])#5719560
nrow(bg_hhSARETT[!is.na(same_sex)&!is.na(spouse_partner_ID)]) #5689673 (0.5% not matched)
#with age_range_3hh
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),("role_match15b_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[!str_detect(role,"stitutional")&is.na(hh_ID),("role_match15b_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),
            c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match15b_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list("Opposite sex spouse or partner")),on=.(role_match15b_id)]]
#nrow(bg_hhSARETT[!is.na(same_sex)])#5719560
nrow(bg_hhSARETT[!is.na(same_sex)&!is.na(spouse_partner_ID)]) #5705567 (0.25% not matched)
#table(bg_hhSARETT[is.na(spouse_partner_ID),same_sex])

#fix on hh and bg
bg_hhSARETT[,("sex"):=fcase(spouse_partner_sex=="Male"&same_sex=="Opposite-sex married couple household","Female",
                            spouse_partner_sex=="Male"&same_sex=="Opposite-sex married couple household","Female",
                            family_type_4=="Male householder","Male", #only a couple wrong, most of problem is with Female hh
                            family_type_4=="Female householder","Female",default = sex)]

#assign children
table(bg_hhSARETT[,family_type_7])

#assign other relatives

#assign others

#assign GQ?
#expand for post-enumeration survey [after matching for families to get the rent/own]
#want column n from the xslx file - for 2020, just copied tables from https://www2.census.gov/programs-surveys/decennial/coverage-measurement/pes/net-coverage-error-and-components-of-coverage-by-race-hispanic-origin.pdf
#using percentage for net coverage error (column N) in /Users/dan/Library/CloudStorage/OneDrive-SharedLibraries-UniversityOfHouston/Engaged Data Science - Data/Census/2020/post_enumeration_errors.xlsx
net_cov_err_file <- paste0(censusdir,"2020/post_enumeration_errors.xlsx")
net_coverage_err <- read_excel(net_cov_err_file,col_names = TRUE)
net_coverage_err <- as.data.table(net_coverage_err)
net_coverage_err[,("age_num_err"):=as.integer(substr(Label,1,2))]
net_coverage_err <- net_coverage_err[!is.na(age_num_err)&!is.na(race_err)]

bg_SARE[,("age_num_err"):=fcase(age_num<5,0,
                                age_num>4&age_num<10,5,
                                age_num>9&age_num<18,10,
                                age_num>17&age_num<30,18,
                                age_num>29&age_num<50,30,
                                age_num>49,50,
                                default = "not known")]
bg_SARE[,("race_err"):=fcase(race=="WHITE ALONE, NOT HISPANIC OR LATINO","Non-Hispanic White",
                             race=="BLACK OR AFRICAN AMERICAN ALONE, NOT HISPANIC OR LATINO","Black",
                             race=="ASIAN ALONE, NOT HISPANIC OR LATINO","Asian",
                             race=="AMERICAN INDIAN AND ALASKA NATIVE ALONE, NOT HISPANIC OR LATINO","American Indian or Alaska Native",
                             race=="NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE, NOT HISPANIC OR LATINO","Native Hawaiian or Other Pacific Islander",
                             race=="SOME OTHER RACE ALONE, NOT HISPANIC OR LATINO" | race=="TWO OR MORE RACES, NOT HISPANIC OR LATINO","Some Other Race",
                             default = "Hispanic or Latino")]
#bg_SARE[,("rent_own"):=,by=("hh_ID")] #or make rent_own one of the things moved down when doing the matches
bg_SARE[,("rent_own_err"):=fcase(rent_own=="Owner occupied","Owner",
                                 rent_own=="Renter occupied","Renter",
                                default = "not known")]
net_coverage_err[,("sex_err"):=fcase(str_detect(Label," males"),"male",
                                     str_detect(Label," females"),"female",
                                     default = "not given")]
#duplicate all the ones with not_given, first not_given is male, second is female, same missing_pct
net_coverage_err <- as.data.table(rbind(lapply(net_coverage_err[,.SD[sex_err=="not given"]],rep,2),lapply(net_coverage_err[,.SD[sex_err!="not given"]],rep,1)))
net_coverage_err[,("cnt"):=.N,by=c("sex_err","race_err","age_num_err","rent_own_err")]
net_coverage_err[,("sex_err"):=fcase(sex_err=="not given"&cnt==1,"male",
                                     sex_err=="not given"&cnt==2,"female",
                                     default = sex_err)]
test <- net_coverage_err[bg_SARE,on=c("age_num_err","race_err","rent_own_err","sex_err")]
bg_SARE[pct_missing<0,("to_remove"):=sample(c(TRUE,FALSE),1,replace = TRUE,c(-pct_missing,1+pct_missing))] #if it's negative, we add; if positive, we subtract 
bg_SARE <- bg_SARE[!to_remove]
bg_SARE[pct_missing>0,("to_duplicate"):=sample(c(TRUE,FALSE),1,replace = TRUE,c(pct_missing,1-pct_missing))] #if it's negative, we add; if positive, we subtract 
#When duplicating some rows, have to deal with householders differently - two steps? or don't have any householders or spouses in the dups???



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
