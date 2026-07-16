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
bg_hhSARETT[,("household"):="In households"]
#fix age_range
bg_SARE[,("age_range_9hh"):=fcase(age_num>14&age_num<25,"15 to 24 years",
                                  age_num>24&age_num<35,"25 to 34 years",
                                  age_num>34&age_num<45,"35 to 44 years",
                                  age_num>44&age_num<55,"45 to 54 years",
                                  age_num>54&age_num<50,"55 to 59 years",
                                  age_num>59&age_num<55,"60 to 64 years",
                                  age_num>64&age_num<75,"65 to 74 years",
                                  age_num>74&age_num<85,"75 to 84 years",
                                  age_num>84,"85 years and over",
                                  default = age_range)]
bg_hhSARETT[,("age_range_9hh"):=str_remove_all(age_range_9,"Householder ")]
#create extra householders for a slightly broader match; need to pull back down from role_orig for total numbers at tract level (distributions not perfect for age, etc.)
bg_SARE[,("role_orig"):=role]
bg_SARE[,("role"):=fcase(role%in%c("Grandchild","Adopted child","Foster child","Stepchild","Biological child")&age_num>29,"Householder",
                         str_detect(role,"sex")&age_num<20,"Biological child",
                         role=="Son-in-law or daughter-in-law"&age_num<20,"Biological child",
                         str_detect(role,"Parent")&age_num<64,"Householder",
                         role=="Householder"&age_num<18,"Biological child",
                         default = role)]
bg_hhSARETT[,("hh_match1_id"):=
             paste0(tract,re_code_14,age_range_9hh,sex,alone,as.character(100000+sample(1:.N))),
           by=.(tract,re_code_14,age_range_9hh,sex,alone)]
bg_SARE[role=="Householder",("hh_match1_id"):=
               paste0(tract,re_code,age_range_9hh,sex,alone,as.character(100000+sample(1:.N))),
             by=.(tract,re_code,age_range_9hh,sex,alone)]
bg_hhSARETT[,c("ind_ID","age_range","HvL","race_1","race_2"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2)),
                      on=.(hh_match1_id)]]
bg_SARE[role=="Householder",c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
               bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                                 list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                                 list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                                 list(household_60),list(household_65),list(household_75),list(rent_own),
                                 list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list(role)),on=.(hh_match1_id)]]
#nrow(bg_hhSARETT[is.na(ind_ID)]) #6265996 58% not matching, mostly because of incomplete data on alone and sex
#try without alone or sex
bg_hhSARETT[is.na(ind_ID),("hh_match2_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[role=="Householder"&is.na(hh_ID),("hh_match2_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2)),on=.(hh_match2_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                              "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                              "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list(role)),on=.(hh_match2_id)]]
#nrow(bg_hhSARETT[is.na(ind_ID)]) #1529706 - 15% not matching
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
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2)),on=.(hh_match2a_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                                           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list(role)),on=.(hh_match2a_id)]]
#nrow(bg_hhSARETT[is.na(ind_ID)]) #356195 - 3.4% not matching
#table(bg_hhSARETT[is.na(ind_ID),re_code_14]) #mostly well-distributed on re_code and age_range

#with only age_range_3hh
bg_hhSARETT[is.na(ind_ID),("hh_match3_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[role=="Householder"&is.na(hh_ID),("hh_match3_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2)),on=.(hh_match3_id)]]
bg_SARE[role=="Householder"&is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                                           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3",
                                           "hh_role","re_code_hh"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),
                            list(role),list(re_code)),on=.(hh_match3_id)]]
#nrow(bg_hhSARETT[is.na(ind_ID)]) #94793 - 1%
#get the 1% from folks not matching on role=="Householder"; remembering that bg_SARE was expanded on role
bg_hhSARETT[is.na(ind_ID),("hh_match4a_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
#bg_SARE[is.na(hh_ID)&!str_detect(role,"stitutional"),("hh_match4_id"):=
bg_SARE[is.na(hh_ID),("hh_match4a_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2)),on=.(hh_match4a_id)]]
#bg_SARE[is.na(hh_ID)&!str_detect(role,"stitutional"),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
bg_SARE[is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",                                                       
                                           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                                           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),list(role)),on=.(hh_match4a_id)]]
#nrow(bg_hhSARETT[is.na(ind_ID)]) #13403 when opening to gq; #24164 when no gq matches allowed

#one more try, with gq allowed
bg_hhSARETT[is.na(ind_ID),("hh_match5_id"):=
              paste0(tract,re_code_7,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_7,age_range_3hh)]
bg_SARE[is.na(hh_ID),("hh_match5_id"):=
          paste0(tract,re_code_7,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code_7,age_range_3hh)]
bg_hhSARETT[is.na(ind_ID),c("ind_ID","age_range","HvL","race_1","race_2"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2)),on=.(hh_match5_id)]]
bg_SARE[is.na(hh_ID),c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
                       "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
                       "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3",
                       "hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                            list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                            list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                            list(household_60),list(household_65),list(household_75),list(rent_own),
                            list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3),
                            list(role)),on=.(hh_match5_id)]]
#nrow(bg_hhSARETT[is.na(ind_ID)])#236, if GQ allowed and no re_code; #3880 with re_code_7; without gq and no re_code, #5441 - remarkably evenly distributed. Don't try to capture last .05%? 
#for 13k last matches, did not match on re_code

#without gq, length(unique(bg_hhSARETT[is.na(ind_ID),GEOID]))#198 (which is 1% of total number of block groups - group quarters folks might be issue) 
#without gq, max(table(bg_hhSARETT[is.na(ind_ID),GEOID])) #766 (3796 are in block groups with more than 50 not matched)
bg_hhSARETT[,("races"):=asplit(.SD,1),.SDcols=c("race_1","race_2")]
bg_hhSARETT[,c("race_1","race_2"):=NULL]

#make the non-matched from expanded role=="Householder" on bg_SARE work for matches - will iterate for each spouse type
bg_SARE[,("role_hh_spouse"):=fcase(role=="Householder"&is.na(hh_ID),"Same-sex spouse",default = role)]


#match for spouses and partners, with folks close to same age
#they were supposed to have made it so that Householder could be female in a married couple in 2020, but I couldn't see any evidence for how that was implemented; I let them distribute differently at end
bg_hhSARETT[,("sex"):=fcase(couple_gender=="Female-female married couple households"|couple_gender=="Female-female unmarried partner household","Female",default = "Male")]#end up making a lot more female, later

bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households",("role_match_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&role_hh_spouse=="Same-sex spouse"&is.na(hh_ID),("role_match_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households",c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match_id)]]
bg_SARE[sex=="Male"&role_hh_spouse=="Same-sex spouse"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&!is.na(spouse_partner_ID)]) #13138 of 24709
#nrow(bg_SARE[sex=="Male"&role=="Same-sex spouse"&is.na(hh_ID)]) #20804
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),("role_match1_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match1_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match1_id)]]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match1_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&!is.na(spouse_partner_ID)]) #21128 of 24709
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),("role_match2_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match2_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match2_id)]]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match2_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"]) #24709
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male married couple households"&!is.na(spouse_partner_ID)]) #22236

#female-female married
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),("role_match3_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&role_hh_spouse=="Same-sex spouse"&is.na(hh_ID),("role_match3_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match3_id)]]
bg_SARE[sex=="Female"&role_hh_spouse=="Same-sex spouse"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match3_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&!is.na(spouse_partner_ID)]) #15887 of 29795
#nrow(bg_SARE[sex=="Female"&role=="Same-sex spouse"&!is.na(hh_ID)]) #3181
#nrow(bg_SARE[sex=="Female"&role_hh_spouse=="Same-sex spouse"&!is.na(hh_ID)]) #16179
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),("role_match4_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match4_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match4_id)]]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match4_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"]) #29795
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&!is.na(spouse_partner_ID)]) #25641 of 29795
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),("role_match5_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match5_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match5_id)]]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match5_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"]) #29795
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female married couple households"&!is.na(spouse_partner_ID)]) #26789

bg_SARE[,("role_hh_spouse"):=fcase(role=="Householder"&is.na(hh_ID),"Same-sex unmarried partner",default = role)]
#for unmarried partner couples
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household",("role_match6_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&role_hh_spouse=="Same-sex unmarried partner"&is.na(hh_ID),("role_match6_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household",c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match6_id)]]
bg_SARE[sex=="Male"&role_hh_spouse=="Same-sex unmarried partner"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match6_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"]) #19801
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&!is.na(spouse_partner_ID)]) #10133 
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),("role_match7_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match7_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match7_id)]]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match7_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&!is.na(spouse_partner_ID)]) #17539
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),("role_match8_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match8_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match8_id)]]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match8_id)]]
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"]) #19801
#nrow(bg_hhSARETT[sex=="Male"&couple_gender=="Male-male unmarried partner household"&!is.na(spouse_partner_ID)]) #18565

#female-female unmarried partner
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),("role_match9_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&role_hh_spouse=="Same-sex unmarried partner"&is.na(hh_ID),("role_match9_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match9_id)]]
bg_SARE[sex=="Female"&role_hh_spouse=="Same-sex unmarried partner"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match9_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&!is.na(spouse_partner_ID)]) #11734 of 19798
#nrow(bg_SARE[sex=="Female"&role=="Same-sex unmarried partner"&is.na(hh_ID)]) #18298
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),("role_match10_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match10_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match10_id)]]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match10_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&!is.na(spouse_partner_ID)]) #19798
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),("role_match11_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match11_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match11_id)]]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match11_id)]]
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"]) #22276
#nrow(bg_hhSARETT[sex=="Female"&couple_gender=="Female-female unmarried partner household"&!is.na(spouse_partner_ID)]) #20927

bg_SARE[,("role_hh_spouse"):=fcase(role=="Householder"&is.na(hh_ID),"Opposite-sex spouse",default = role)]
#opposite-sex married couples - use same_sex
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match12_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&role_hh_spouse=="Opposite-sex spouse",("role_match12_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match12_id)]]
bg_SARE[sex=="Female"&role_hh_spouse=="Opposite-sex spouse",c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match12_id)]]
#nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"]) #5025134
#nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #2600953 with role_hh_spouse; 2291670 on just role
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match13_id"):=
              paste0(tract,re_code_14,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match13_id"):=
          paste0(tract,re_code,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_9hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match13_id)]]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match13_id)]]
#nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #2812395
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match14_id"):=
              paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_9hh)]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match14_id"):=
          paste0(tract,age_range_9hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_9hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14_id)]]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match14_id)]]
#nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #3055124 with role_hh_spouse; 2664952 with role
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match14a_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match14a_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14a_id)]]
bg_SARE[sex=="Female"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match14a_id)]]
#nrow(bg_hhSARETT[sex=="Male"&same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #3537282 with role_hh_spouse (30% missing); 2923077 with role only (of 5025134)

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
nrow(bg_hhSARETT[same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #4446747

#pick up female head of household in married couples (should be different for 2020 than for earlier decennials)
bg_hhSARETT[same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),("role_match14a1_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),("role_match14a1_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[same_sex=="Opposite-sex married couple household"&is.na(spouse_partner_ID),c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14a1_id)]]
bg_SARE[sex=="Male"&str_detect(role_hh_spouse,"-sex")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match14a1_id)]]
nrow(bg_hhSARETT[same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #4839249 (3.5% missing)

#pull in last 3.5% from gq
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),("role_match14c1_id"):=
              paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
            by=.(tract,age_range_3hh)]
bg_SARE[str_detect(role,"stitutional")&is.na(hh_ID),("role_match14c1_id"):=
          paste0(tract,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3hh)]
bg_hhSARETT[!is.na(same_sex)&is.na(spouse_partner_ID)&!str_detect(family_type_7,"solitary"),
            c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(role_match14c1_id)]]
bg_SARE[str_detect(role,"stitutional")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(couple_gender)),on=.(role_match14c1_id)]]
#nrow(bg_hhSARETT[same_sex=="Opposite-sex married couple household"&!is.na(spouse_partner_ID)]) #4892410 of 5025134 (201948 is 4%)
#table(bg_hhSARETT[is.na(spouse_partner_ID),family_type]) #missing about 2.6% of married couples...
#have to think through why still missing 2.6% of married couples...

bg_hhSARRETT[,("sex"):=fcase(same_sex=="Opposite-sex married couple household"&spouse_partner_sex=="Male","Female",default = sex)]


#fix on hh and bg
bg_hhSARETT[,("sex"):=fcase(spouse_partner_sex=="Male"&same_sex=="Opposite-sex married couple household","Female",
                            spouse_partner_sex=="Male"&same_sex=="Opposite-sex married couple household","Female",
                            family_type_4=="Male householder","Male", #only a couple wrong, most of problem is with Female hh
                            family_type_4=="Female householder","Female",default = sex)]

#how many people in bg_SARE not in bg_hhSARETT
#sum(bg_hhSARETT[,as.integer(substr(hh_size_7,1,1))],na.rm = TRUE) #28341574 , so nrow(bg_SARE)-28341574-nrow(bg_GQ) #197886 folks missing - but can all come from 7-or more

#In Texas, approximately 15% to 17% of family households with minor children have three or more children. Out of the roughly 3 million families with children statewide, this translates to an estimated 450,000 to 500,000 families
#get hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=as.integer(substr(hh_size_7,1,1))]
#table(bg_hhSARETT[,hh_size_cnt],bg_hhSARETT[,hh_size_7],useNA = "ifany") 
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(spouse_partner_ID),hh_size_cnt-as.integer(1),default = hh_size_cnt)]
#2558 with zero, which is all folks in 1-person households where alone=="Not living alone" - so goes back to an earlier mismatch; have to fix in post...

#table for percentages of re_code matching for spouse and hh
#table(bg_hhSARETT[re_code_14==spouse_partner_re_code,kid_age_range_3],bg_hhSARETT[re_code_14==spouse_partner_re_code,spouse_partner_re_code])/table(bg_hhSARETT[,kid_age_range_3],bg_hhSARETT[,spouse_partner_re_code])
#percentages that match are a little low, but we don't do much with it - should explore how it works over re_code_7 at some point...
#remember age_num increments from beginning of range; have to move 6 year olds over to Under 5
bg_SARE[age_num==5,("age_num_move_5y"):=sample(c(0,6,7,8,9),.N,replace = TRUE),by=.(tract,age_num)]
bg_SARE[,("age_num_5y"):=fcase(age_num_move_5y==0,as.integer(0),default = age_num)]
#assign children - own_kids under 18 first 
#under 6 same race
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code,("kid_match_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),("kid_match_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code,
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match_id)]] #have to change to household - match overshoots when I put text in
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code]) #311400
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #268286 
##under 6 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&is.na(child_own_1_ID),
            ("kid_match1_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match1_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match1_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match1_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"]) #624318
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #293019
##under 6, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&is.na(child_own_1_ID),
            ("kid_match2_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match2_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match2_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match2_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"]) #624318
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #328159
##for under 6 and 6-17, match with all under 18
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&is.na(child_own_1_ID),
            ("kid_match3_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),("kid_match3_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match3_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match3_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code]) #361989
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #634638
##under 6 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&is.na(child_own_1_ID),
            ("kid_match4_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),("kid_match4_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match4_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match4_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"]) #721066 (have to have at least twice as many)
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)&kid_age_range_3=="Under 6 years and 6 to 17 years"]) #340956 of 670k total child_own_1
##under 6 or under 18, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&is.na(child_own_1_ID),
            ("kid_match5_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,("kid_match5_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match5_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match5_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"]) #721066
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #872415
#
##for only 6-17, same race parents
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&is.na(child_own_1_ID),
            ("kid_match6_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),("kid_match6_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match6_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match6_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code]) #910446
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #1650432
#6 to 17 different race parents, biracial child - avoiding complexity of actually matching by race2, but still on re_code for householder, etc.; will miss a lot, but no time to implement now
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&is.na(child_own_1_ID),
            ("kid_match7_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match7_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match7_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match7_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"]) #1915650
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)&kid_age_range_3=="6 to 17 years only"]) #859101 (of 1731322 total child_own_1)
#6 to 17, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&is.na(child_own_1_ID),
            ("kid_match8_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match8_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&is.na(child_own_1_ID),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"]) #1915650
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #1841421
#
##re_code_14 for all_kids (not just "own") and biological child
bg_hhSARETT[is.na(child_own_1_ID)&str_detect(all_kid_18,"own"),("kid_match8a_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,("kid_match8a_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[is.na(child_own_1_ID)&str_detect(all_kid_18,"own"),
            c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8a_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8a_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_1_ID)]) #3625462 with re_code_14 
#table(bg_hhSARETT[,child_own_1_role],bg_hhSARETT[,kid_age_range_3],useNA = "ifany") 
#table(bg_hhSARETT[,all_kid_18],bg_hhSARETT[,kid_age_range_3],useNA = "ifany") #the 480k biological kids who are not own is why moved to all_kids
#
bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Own child",default = hh_role)]


bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_own_1_ID),hh_size_cnt-as.integer(1),default = hh_size_cnt)] #want it to be >1, since 1 is the householder; getting some 0s, etc., but keep using as general guide
#got 790k at 0, which should be ok for single family kids
#stepchild under 18 and hh over 34
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>34,("kid_match8b_id"):=
              paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_7)]
bg_SARE[role=="Stepchild"&is.na(hh_ID)&age_num<18,("kid_match8b_id"):=
          paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
        by=.(tract,re_code_7)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>34,
            c("child_step_ID","child_step_sex","child_step_age","child_step_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8b_id)]]
bg_SARE[role=="Stepchild"&is.na(hh_ID)&age_num<18,c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8b_id)]]
#nrow(bg_hhSARETT[!is.na(child_step_ID)]) #261069

#stepchild and hh over 64
bg_hhSARETT[is.na(child_step_ID)&hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>64,
            ("kid_match8c_id"):=
              paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_7)]
bg_SARE[role=="Stepchild"&is.na(hh_ID),("kid_match8c_id"):=
          paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
        by=.(tract,re_code_7)]
bg_hhSARETT[is.na(child_step_ID)&hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>64,
            c("child_step_ID","child_step_sex","child_step_age","child_step_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8c_id)]]
bg_SARE[role=="Stepchild"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8c_id)]]
#nrow(bg_hhSARETT[!is.na(child_step_ID)]) #387004
bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Stepchild",default = hh_role)]

#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_step_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#grandkids for householders over 54
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>54,
            ("kid_match8d_id"):=
              paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_7)]
bg_SARE[role=="Grandchild"&is.na(hh_ID),("kid_match8d_id"):=
          paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
        by=.(tract,re_code_7)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>54,
            c("child_grand_ID","child_grand_sex","child_grand_age","child_grand_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8d_id)]]
bg_SARE[role=="Grandchild"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8d_id)]]
#nrow(bg_hhSARETT[!is.na(child_grand_ID)]) #839056 (879134 total grandkids)

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Grandchild",default = hh_role)]

bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_grand_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#adopted
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>24,
            ("kid_match8e_id"):=
              paste0(tract,as.character(100000+sample(1:.N))),
            by=.(tract)]
bg_SARE[role=="Adopted child"&is.na(hh_ID),("kid_match8e_id"):=
          paste0(tract,as.character(100000+sample(1:.N))),
        by=.(tract)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>24,
            c("child_adopted_ID","child_adopted_sex","child_adopted_age","child_adopted_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8e_id)]]
bg_SARE[role=="Adopted child"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8e_id)]]
#nrow(bg_hhSARETT[!is.na(child_adopted_ID)]) #160600 (165881 total adopted kids)

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Adopted child",default = hh_role)]

bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_adopted_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#foster
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>24,
            ("kid_match8f_id"):=
              paste0(tract,as.character(100000+sample(1:.N))),
            by=.(tract)]
bg_SARE[role=="Foster child"&is.na(hh_ID),("kid_match8f_id"):=
          paste0(tract,as.character(100000+sample(1:.N))),
        by=.(tract)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>24,
            c("child_foster_ID","child_foster_sex","child_foster_age","child_foster_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8f_id)]]
bg_SARE[role=="Foster child"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8f_id)]]
#nrow(bg_hhSARETT[!is.na(child_foster_ID)]) #23077 (23098 total foster kids)

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Foster child",default = hh_role)]

bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_foster_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#parent or parent-in-law
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))<64,
            ("kid_match8g_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[str_detect(role,"Parent")&is.na(hh_ID),("kid_match8g_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))<64,
            c("parent_ID","parent_sex","parent_age","parent_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match8g_id)]]
bg_SARE[str_detect(role,"Parent")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match8g_id)]]
#nrow(bg_hhSARETT[!is.na(parent_ID)]) #465694 (410321 total parents and 104767 parents in law)

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Parent",default = hh_role)]

bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(parent_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

##for second child 
##for both under 6 and over, all same re_code_14
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID),("kid_match9_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),("kid_match9_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&!is.na(child_own_1_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match9_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match9_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code]) #362286
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #168489
#Under 6 years and 6 to 17 years different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            ("kid_match10_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),("kid_match10_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match10_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match10_id)]]
#Under 6 years and 6 to 17 years, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            ("kid_match11_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),("kid_match11_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match11_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match11_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"]) #721066
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #141893
#
#for only under 6
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),("kid_match12_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),("kid_match12_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match12_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match12_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code]) #311097
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #283507
#under 6 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            ("kid_match13_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match13_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match13_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match13_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"]) #1915650
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #295155
#under 6, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            ("kid_match14_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match14_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match14_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match14_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"]) #624318
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #317163
##for only 6-17
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),("kid_match15_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),("kid_match15_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match15_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match15_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code]) #910863
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #1037743
#6 to 17 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            ("kid_match16_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match16_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match16_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match16_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"]) #1915650 (have to have at least twice as many)
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #1099803
#6 to 17, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            ("kid_match17_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match17_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_1_ID)&is.na(child_own_2_ID),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match17_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match17_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #598285
#
##get by re_code_7 with all-kids added - this would include spouses' children who are not householders'
bg_hhSARETT[!is.na(child_own_1_ID)&is.na(child_own_2_ID)&str_detect(all_kid_18,"own"),("kid_match17a_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,("kid_match17a_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[!is.na(child_own_1_ID)&is.na(child_own_2_ID)&str_detect(all_kid_18,"own"),
            c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match17a_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match17a_id)]]
nrow(bg_hhSARETT[!is.na(child_own_2_ID)]) #1977867
#table(bg_SARE[!is.na(hh_ID)&age_num<18,role],useNA = "ifany")
#nrow(bg_SARE[is.na(hh_ID)&age_num<18&role=="Biological child"])/ nrow(bg_SARE[age_num<18&role=="Biological child"]) #2% (127965 bio children matched)

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Own child",default = hh_role)]
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_own_2_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]


#son or daughter-in-law
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>54,
            ("kid_match28a_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[str_detect(role,"daughter")&is.na(hh_ID),("kid_match28a_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households"&as.integer(substr(age_range_9hh,1,2))>54,
            c("son_daughter_in_law_ID","son_daughter_in_law_sex","son_daughter_in_law_age","son_daughter_in_law_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match28a_id)]]
bg_SARE[str_detect(role,"daughter")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match28a_id)]]
#nrow(bg_hhSARETT[!is.na(son_daughter_in_law_ID)]) #142695 (170778 total parents and 104767 parents in law)

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Son or daughter-in-law",default = hh_role)]
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(son_daughter_in_law_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#Brother or sister
bg_hhSARETT[hh_size_cnt>1&family=="Family households",
            ("kid_match28b_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[str_detect(role,"sister")&is.na(hh_ID),("kid_match28b_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households",
            c("sibling_ID","sibling_sex","sibling_age","sibling_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match28b_id)]]
bg_SARE[str_detect(role,"sister")&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match28b_id)]]
#nrow(bg_hhSARETT[!is.na(sibling_ID)]) #373236 

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Sibling",default = hh_role)]
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(sibling_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#Other relatives
bg_hhSARETT[hh_size_cnt>1&family=="Family households",
            ("kid_match28c_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Other relatives"&is.na(hh_ID),("kid_match28c_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[hh_size_cnt>1&family=="Family households",
            c("relatives_ID","relatives_sex","relatives_age","relatives_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match28c_id)]]
bg_SARE[role=="Other relatives"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match28c_id)]]
#nrow(bg_hhSARETT[!is.na(relatives_ID)]) #433572 

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Other relatives",default = hh_role)]
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(relatives_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

bg_hhSARETT[hh_size_cnt>1,
            ("kid_match28d_id"):=
              paste0(tract,as.character(100000+sample(1:.N))),
            by=.(tract)]
bg_SARE[role=="Other nonrelatives"&is.na(hh_ID),("kid_match28d_id"):=
          paste0(tract,as.character(100000+sample(1:.N))),
        by=.(tract)]
bg_hhSARETT[hh_size_cnt>1,
            c("nonrelatives_ID","nonrelatives_sex","nonrelatives_age","nonrelatives_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match28d_id)]]
bg_SARE[role=="Other nonrelatives"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match28d_id)]]
#nrow(bg_hhSARETT[!is.na(nonrelatives_ID)]) #736109

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Other nonrelatives",default = hh_role)]
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(nonrelatives_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

##for third own child 
##for both under 6 and over
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID),("kid_match18_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),("kid_match18_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&!is.na(child_own_2_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match18_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match18_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code]) #361579
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #12079
##Under 6 years and 6 to 17 years different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            ("kid_match19_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),("kid_match19_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match19_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match19_id)]]
#Under 6 years and 6 to 17 years, but no spouse_partner #zero found!
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #7393

bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            ("kid_match20_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),("kid_match20_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match20_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match20_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"]) #720345
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #24829
#
#for only under 6
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),("kid_match21_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),("kid_match21_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match21_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match21_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code]) #311400
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #30347 
#under 6 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            ("kid_match22_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match22_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match22_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match22_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"]) #1915993
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #33552
#under 6, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            ("kid_match23_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match23_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match23_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match23_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"]) #624695
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #38112
##for only 6-17
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),("kid_match24_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),("kid_match24_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match24_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match24_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code]) #911344
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #56169
#6 to 17 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            ("kid_match25_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match25_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match25_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match25_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #67334
##6 to 17, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            ("kid_match26_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match26_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_2_ID)&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match26_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match26_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #52035
#re_code_7 and tract only
bg_hhSARETT[!is.na(child_own_2_ID)&all_kid_18!="No children under 18 years"&is.na(child_own_3_ID),("kid_match26a_id"):=
              paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_7)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,("kid_match26a_id"):=
          paste0(tract,re_code_7,as.character(100000+sample(1:.N))),
        by=.(tract,re_code_7)]
bg_hhSARETT[!is.na(child_own_2_ID)&all_kid_18!="No children under 18 years"&is.na(child_own_3_ID),
            c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match26a_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18,c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match26a_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_3_ID)]) #417092 - in line with online estimates for state of Texas 

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Own child",default = hh_role)]
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_own_3_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#not own child living with hh
bg_hhSARETT[all_kid_18!="No children under 18 years"&kid_age_range_3=="No own children under 18 years"&hh_size_cnt>1,
            ("kid_match27a_id"):=
              paste0(tract,as.character(100000+sample(1:.N))),
            by=.(tract)]
bg_SARE[!str_detect(role,"stitutional")&is.na(hh_ID)&age_num<18,("kid_match27a_id"):=
          paste0(tract,as.character(100000+sample(1:.N))),
        by=.(tract)]
bg_hhSARETT[all_kid_18!="No children under 18 years"&kid_age_range_3=="No own children under 18 years"&hh_size_cnt>1,
            c("child_not_own_1_ID","child_not_own_1_sex","child_not_own_1_age","child_not_own_1_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match27a_id)]]
bg_SARE[!str_detect(role,"stitutional")&is.na(hh_ID)&age_num<18,c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match27a_id)]]
#nrow(bg_hhSARETT[!is.na(child_not_own_1_ID)]) #71575
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_not_own_1_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#second not own child living with hh
bg_hhSARETT[!is.na(child_not_own_1_ID)&hh_size_cnt>1,
            ("kid_match28_id"):=
              paste0(tract,as.character(100000+sample(1:.N))),
            by=.(tract)]
bg_SARE[!str_detect(role,"stitutional")&is.na(hh_ID)&age_num<18,("kid_match28_id"):=
          paste0(tract,as.character(100000+sample(1:.N))),
        by=.(tract)]
bg_hhSARETT[!is.na(child_not_own_1_ID)&hh_size_cnt>1,
            c("child_not_own_2_ID","child_not_own_2_sex","child_not_own_2_age","child_not_own_2_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match28_id)]]
bg_SARE[!str_detect(role,"stitutional")&is.na(hh_ID)&age_num<18,c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match28_id)]]
#nrow(bg_hhSARETT[!is.na(child_not_own_2_ID)]) #14710. #these numbers should be higher!

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Child not own",default = hh_role)]
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_not_own_2_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

##for fourth child 
#for both under 6 and over
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID),("kid_match30_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),("kid_match30_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&!is.na(child_own_3_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match30_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match30_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14==spouse_partner_re_code]) #361292
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #371
#Under 6 years and 6 to 17 years different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            ("kid_match31_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),("kid_match31_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match31_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match31_id)]]
#Under 6 years and 6 to 17 years, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            ("kid_match32_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),("kid_match32_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match32_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match32_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years and 6 to 17 years"]) #720345
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #59051
#
##for only under 6
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),("kid_match33_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),("kid_match33_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match33_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match33_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14==spouse_partner_re_code]) #311400
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #1539
#under 6 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            ("kid_match34_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match34_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match34_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match34_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"]) #1915993
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #1763
##under 6, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            ("kid_match35_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),("kid_match35_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="Under 6 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match35_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y==0&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match35_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="Under 6 years only"]) #624695
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #89686
##for only 6-17
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),("kid_match36_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),("kid_match36_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match36_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match36_id)]]
#nrow(bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14==spouse_partner_re_code]) #911344
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #162048
#6 to 17 different race - avoiding complexity of actually matching by race2, etc.
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            ("kid_match37_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match37_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&re_code_14!=spouse_partner_re_code&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match37_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match37_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #1088
#6 to 17, but no spouse_partner
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            ("kid_match38_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),("kid_match38_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[kid_age_range_3=="6 to 17 years only"&is.na(spouse_partner_re_code)&hh_size_cnt>1&
              !is.na(child_own_3_ID)&is.na(child_own_4_ID),
            c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match38_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID)&age_num_5y>0&age_num<18&!is.na(race_2),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match38_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_4_ID)]) #1634
bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Own child",default = hh_role)]

#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_own_4_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#add older biological children, child_1
bg_hhSARETT[hh_size_cnt>1&as.integer(substr(age_range_9hh,1,2))>54&family=="Family households",
            ("kid_match39a_id"):=
              paste0(tract,re_code_14,as.character(100000+sample(1:.N))),
            by=.(tract,re_code_14)]
bg_SARE[role=="Biological child"&is.na(hh_ID),("kid_match39a_id"):=
          paste0(tract,re_code,as.character(100000+sample(1:.N))),
        by=.(tract,re_code)]
bg_hhSARETT[hh_size_cnt>1&as.integer(substr(age_range_9hh,1,2))>54&family=="Family households",
            c("child_own_add_ID","child_own_add_sex","child_own_add_age","child_own_add_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(kid_match39a_id)]]
bg_SARE[role=="Biological child"&is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_hhSARETT[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(kid_match39a_id)]]
#nrow(bg_hhSARETT[!is.na(child_own_add_ID)]) #582246

bg_SARE[,("hh_role"):=fcase(hh_role=="In households","Own child",default = hh_role)]
#table(bg_SARE[,hh_role],useNA = "ifany")
#get new hh_size_cnt
bg_hhSARETT[,("hh_size_cnt"):=fcase(!is.na(child_own_add_ID),as.integer(hh_size_cnt)-as.integer(1),default = as.integer(hh_size_cnt))]

#nrow(bg_SARE[is.na(hh_ID)&age_num<18]) #80000 or 1% 
#nrow(bg_SARE[is.na(hh_ID)])/nrow(bg_SARE) #12%

#add GQ to bg_hhSARETT
bg_hhSARETT[,("age_num"):=fcase(is.na(age_range),as.integer(str_sub(age_range_9hh,start=1,end=2)),
                                default = as.integer(str_sub(age_range,start=1,end=2)))]
bg_GQ[,("age_num"):=fcase(age_range_3a=="15 to 17 years",as.integer(15),
                          age_range_3a=="17 years and under",as.integer(16),
                          age_range_3a=="18 to 21 years",as.integer(18),
                          age_range_3a=="18 to 64 years",as.integer(19),
                          age_range_3a=="50 to 64 years",as.integer(50),
                          age_range_3a=="65 years and over",as.integer(65),default = as.integer(19))]
bg_GQ[,("age_range_3hh"):=age_range]
bg_GQ[,("age_range"):=NULL]
bg_GQ[,("household"):="In group quarters"]

match_cols <- grep("_match",names(bg_hhSARETT),value = TRUE)
bg_hhSARETT[,(match_cols):=NULL]
bg_SARE[,(match_cols):=NULL]

bg_GQHH <- merge(bg_GQ,bg_hhSARETT,by=c("GEOID","tract","household","sex","age_range_3hh","age_num"),all = TRUE) #keeps columns from having .x, etc
bg_GQHH[,("hh_ID"):=fcase(is.na(hh_ID),paste0("gq_",gq_ID),default = hh_ID)]

#match first on folks that should be same age
bg_GQHH[hh_size_cnt>1,
            ("all_match_id"):=
              paste0(GEOID,age_range_9hh,re_code_14,as.character(100000+sample(1:.N))),
            by=.(GEOID,age_range_9hh,re_code_14)]
bg_SARE[is.na(hh_ID),("all_match_id"):=
          paste0(GEOID,age_range_9hh,re_code,as.character(100000+sample(1:.N))),
        by=.(GEOID,age_range_9hh,re_code)]
bg_GQHH[hh_size_cnt>1,
            c("add_ID","add_sex","add_age","add_re_code"):=
              bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(all_match_id)]]
bg_SARE[is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_GQHH[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(all_match_id)]]
#nrow(bg_GQHH[!is.na(add_ID)]) #889451

#match on broad age_range to get gq population
bg_GQHH[as.integer(substr(hh_size_7,1,1))>2 | is.na(hh_size_7),
        ("all_match1_id"):=
          paste0(GEOID,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(GEOID,age_range_3hh)]
bg_SARE[is.na(hh_ID),("all_match1_id"):=
          paste0(GEOID,age_range_3hh,as.character(100000+sample(1:.N))),
        by=.(GEOID,age_range_3hh)]
bg_GQHH[as.integer(substr(hh_size_7,1,1))>2 | is.na(hh_size_7),
        c("add_1_ID","add_1_sex","add_1_age","add_1_re_code"):=
          bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(all_match1_id)]]
bg_SARE[is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_GQHH[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(all_match1_id)]]
#nrow(bg_GQHH[!is.na(add_1_ID)]) # 797878
#nrow(bg_SARE[is.na(hh_ID)]) #1745331
#table(bg_SARE[is.na(hh_ID),role],useNA = "ifany")
#finish getting group quarters matched...

#match without anything but GEOID
#move over rest
bg_GQHH[as.integer(substr(hh_size_7,1,1))>2 | is.na(hh_size_7),
        ("all_match2b_id"):=
          paste0(GEOID,as.character(100000+sample(1:.N))),
        by=.(GEOID)]
bg_SARE[is.na(hh_ID),("all_match2b_id"):=
          paste0(GEOID,as.character(100000+sample(1:.N))),
        by=.(GEOID)]
bg_GQHH[as.integer(substr(hh_size_7,1,1))>2 | is.na(hh_size_7),
        c("add_2_ID","add_2_sex","add_2_age","add_2_re_code"):=
          bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(all_match2b_id)]]
bg_SARE[is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
          bg_GQHH[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(all_match2b_id)]]
nrow(bg_GQHH[!is.na(add_2_ID)]) #1589890
nrow(bg_SARE[is.na(hh_ID)]) #1825
#let the rest get picked up after PES expansion

##and at tract
#bg_GQHH[as.integer(substr(hh_size_7,1,1))>2 | is.na(hh_size_7),
#        ("all_match3a_id"):=
#          paste0(tract,as.character(100000+sample(1:.N))),
#        by=.(tract)]
#bg_SARE[is.na(hh_ID),("all_match3a_id"):=
#          paste0(tract,as.character(100000+sample(1:.N))),
#        by=.(tract)]
#bg_GQHH[as.integer(substr(hh_size_7,1,1))>2 | is.na(hh_size_7),
#        c("add_3_ID","add_3_sex","add_3_age","add_3_re_code"):=
#          bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(all_match3a_id)]]
#bg_SARE[is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
#          bg_GQHH[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(all_match3a_id)]]
#nrow(bg_GQHH[!is.na(add_3_ID)]) #4025
#nrow(bg_SARE[is.na(hh_ID)]) #97
#
#bg_GQHH[as.integer(substr(hh_size_7,1,1))>2 | is.na(hh_size_7),
#        ("all_match4b_id"):=
#          paste0(tract,as.character(100000+sample(1:.N))),
#        by=.(tract)]
#bg_SARE[is.na(hh_ID),("all_match4b_id"):=
#          paste0(tract,as.character(100000+sample(1:.N))),
#        by=.(tract)]
#bg_GQHH[as.integer(substr(hh_size_7,1,1))>2  | is.na(hh_size_7),
#        c("add_4_ID","add_4_sex","add_4_age","add_4_re_code"):=
#          bg_SARE[.SD,c(list(ind_ID),list(sex),list(age_num),list(re_code)),on=.(all_match4b_id)]]
#bg_SARE[is.na(hh_ID),c("hh_ID","rent_own","hh_role"):=
#          bg_GQHH[.SD,c(list(hh_ID),list(rent_own),list(household)),on=.(all_match4b_id)]]
#nrow(bg_GQHH[!is.na(add_4_ID)]) #12
#nrow(bg_SARE[is.na(hh_ID)]) #85
#length(unique(bg_SARE[is.na(hh_ID),tract])) #11
#table(bg_SARE[is.na(hh_ID),role],useNA = "ifany") #all about non-matching GQ....
#maybe not in family and hh_size > 1

###setting PES aside to do later, if time to work through - the current approach is getting far too few
###EXPAND BY PES THEN DISTRIBUTE AGAIN, THEN END WITH THIS COMPRESSION AND OTHER CLEAN UP ON ROLES, ETC.
###expand for post-enumeration survey [after matching for families to get the rent/own]
###in general, 1999 Supreme Court ruling forbid (5-4) ever using statistical analysis to redo census for apportionment and official counts have never been changed b/c of statistical considerations
###post-enumeration surveys are done differently every decennial. 2010 was supposed to be particularly good; maybe don't redo?
###1990, Census recommended using PES to change official numbers, but Dept. of Commerce declined. https://items.ssrc.org/from-our-archives/who-counts-the-politics-of-census-taking-in-contemporary-america/ and https://pubmed.ncbi.nlm.nih.gov/12155394/
###2000 was a mess and they tried to fix the coverage error estimates 3 times, never using them to adjust the official numbers: https://www.census.gov/programs-surveys/decennial-census/about/coverage-measurement/pes.2000.html#list-tab-400924250
###2010 was better: https://www.census.gov/programs-surveys/decennial-census/about/coverage-measurement/pes.2010.html
###2020 showed 1.92% undercount for Texas as a whole: https://www2.census.gov/programs-surveys/decennial/coverage-measurement/pes/census-coverage-estimates-for-people-in-the-united-states-by-state-and-census-operations.pdf
###want column n from the xslx file - for 2020, just copied tables from https://www2.census.gov/programs-surveys/decennial/coverage-measurement/pes/net-coverage-error-and-components-of-coverage-by-race-hispanic-origin.pdf
###using percentage for net coverage error (column N) in /Users/dan/Library/CloudStorage/OneDrive-SharedLibraries-UniversityOfHouston/Engaged Data Science - Data/Census/2020/post_enumeration_errors.xlsx
##net_cov_err_file <- paste0(censusdir,"2020/post_enumeration_errors.xlsx")
##net_coverage_err <- read_excel(net_cov_err_file,col_names = TRUE)
##net_coverage_err <- as.data.table(net_coverage_err)
##net_coverage_err[,("age_num_err"):=as.integer(substr(Label,1,2))]
##net_coverage_err <- net_coverage_err[!is.na(age_num_err)&!is.na(race_err)]
##
###add to each individual on bg_SARE, then move to bg_GQHH
##bg_SARE[,("age_num_err"):=fcase(age_num<5,0,
##                                age_num>4&age_num<10,5,
##                                age_num>9&age_num<18,10,
##                                age_num>17&age_num<30,18,
##                                age_num>29&age_num<50,30,
##                                age_num>49,50,
##                                default = "not known")]
###get a warning about NA, but no NAs created? table(net_coverage_err[,Label],net_coverage_err[,sex_err],useNA = "ifany")
##bg_SARE[,("race_err"):=fcase(race=="WHITE ALONE, NOT HISPANIC OR LATINO","Non-Hispanic White",
##                             race=="BLACK OR AFRICAN AMERICAN ALONE, NOT HISPANIC OR LATINO","Black",
##                             race=="ASIAN ALONE, NOT HISPANIC OR LATINO","Asian",
##                             race=="AMERICAN INDIAN AND ALASKA NATIVE ALONE, NOT HISPANIC OR LATINO","American Indian or Alaska Native",
##                             race=="NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE, NOT HISPANIC OR LATINO","Native Hawaiian or Other Pacific Islander",
##                             race=="SOME OTHER RACE ALONE, NOT HISPANIC OR LATINO" | race=="TWO OR MORE RACES, NOT HISPANIC OR LATINO","Some Other Race",
##                             default = "Hispanic or Latino")]
##bg_SARE[,("rent_own_err"):=fcase(rent_own=="Owner occupied","Owner",
##                                 rent_own=="Renter occupied","Renter",
##                                 default = "not known")]
##bg_SARE[,("sex_err"):=fcase(age_num<18,"not known",default = sex)]
##net_coverage_err[,("sex_err"):=fcase(str_detect(Label," males"),"Male",
##                                     str_detect(Label," females"),"Female",
##                                     default = "not given")]
##net_coverage_err[,("Pct_missing"):=as.numeric(str_trim(Pct_missing,side = "right"))/100] #non-breaking spaces from excel sheet 
##bg_SARE <- net_coverage_err[bg_SARE,on=c("age_num_err","race_err","rent_own_err","sex_err")]
##
##bg_SARE[,("remove_pct"):=as.integer((Pct_missing*100)+100)]
##bg_SARE[!is.na(remove_pct),("to_remove") :=fcase(Pct_missing>0,as.numeric(sample(1:remove_pct,size=.N,replace=TRUE)),default = as.numeric(0)),by=.(remove_pct)] #remove all over 100
##remove_bg <- bg_SARE[to_remove>100]
###find ind_IDs on bg_GQHH and remove there, too; have to search through all the hh members...
##
##
##bg_SARE[,("add_pct"):=as.integer((-Pct_missing*100)+100)]
##bg_SARE[!is.na(add_pct),("to_add") :=fcase(Pct_missing<0,as.numeric(sample(1:add_pct,size=.N,replace=TRUE)),default = as.numeric(0)),by=.(add_pct)] #double all over 100
##add_bg <- bg_SARE[to_add>100]
###how to deal with householders?
##table(add_bg[,hh_role])
##add_bg[,("ind_ID"):=paste0("pes_",ind_ID)]
##
###small 5 digit differences - not enough, but not sure what's wrong with how I set it up... re_codes J and L only for add and I, J and L for remove
##
##bg_SARE <- rbindlist(list(bg_SARE,add_bg))
###just add as more rows to bg_SARE, then distribute up to number of each role for everyone on bg_GQHH 
##


#assign roles for additionals / remember to give gq_id 

#look at role totals from bg_SARE and move over to bg_GQHH for final assignments
role_summary <- bg_SARE[,.(role_totals = .N), by = .(GEOID,role_orig)]
bg_role_summary <- dcast(role_summary,GEOID~role_orig,value.var="role_totals",fun.aggregate = sum)
bg_GQHH <- bg_role_summary[bg_GQHH,on="GEOID"]

#get previous totals
#bg_GQHH[!is.na(spouse_partner_ID),are they same sex, etc., as different totals, calculate total left and assign

bg_GQHH[!is.na(child_own_1_ID),("child_own_1_cnt"):=.N,by=.(GEOID)]
bg_GQHH[!is.na(child_own_2_ID),("child_own_2_cnt"):=.N,by=.(GEOID)] #only if own_1
bg_GQHH[!is.na(child_own_3_ID),("child_own_3_cnt"):=.N,by=.(GEOID)] #only if own_2
bg_GQHH[!is.na(child_own_4_ID),("child_own_4_cnt"):=.N,by=.(GEOID)] #only if own_3
bg_GQHH[!is.na(child_own_add_ID),("child_own_add_cnt"):=.N,by=.(GEOID)] #can be added without an own_4
bg_GQHH[order(-child_own_4_cnt,-child_own_3_cnt,-child_own_2_cnt,-child_own_1_cnt),("child_own_cnt"):=.SD[1,
                                sum(c(child_own_1_cnt,child_own_2_cnt,child_own_3_cnt,child_own_4_cnt),na.rm=TRUE)],
        by=.(GEOID)]
bg_GQHH[order(-child_own_add_cnt),("child_own_cnt"):=.SD[1,
               sum(c(child_own_cnt,child_own_add_cnt),na.rm=TRUE)],
        by=.(GEOID)]
bg_GQHH[!is.na(child_not_own_1_ID),("child_not_own_1_cnt"):=.N,by=.(GEOID)]
bg_GQHH[!is.na(child_not_own_2_ID),("child_not_own_2_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-child_not_own_2_cnt,-child_not_own_1_cnt),("child_not_own_cnt"):=.SD[1,
                              sum(c(child_not_own_1_cnt,child_not_own_2_cnt),na.rm=TRUE)],
        by=.(GEOID)]
bg_GQHH[!is.na(child_step_ID),("child_step_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-child_step_cnt),("child_step_cnt"):=.SD[1,child_step_cnt],by=.(GEOID)]
bg_GQHH[!is.na(child_grand_ID),("child_grand_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-child_grand_cnt),("child_grand_cnt"):=.SD[1,child_grand_cnt],by=.(GEOID)]
bg_GQHH[!is.na(child_adopted_ID),("child_adopted_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-child_adopted_cnt),("child_adopted_cnt"):=.SD[1,child_adopted_cnt],by=.(GEOID)]
bg_GQHH[!is.na(child_foster_ID),("child_foster_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-child_foster_cnt),("child_foster_cnt"):=.SD[1,child_foster_cnt],by=.(GEOID)]
bg_GQHH[!is.na(son_daughter_in_law_ID),("son_daughter_in_law_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-son_daughter_in_law_cnt),("son_daughter_in_law_cnt"):=.SD[1,son_daughter_in_law_cnt],by=.(GEOID)]
bg_GQHH[!is.na(parent_ID),("parent_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-parent_cnt),("parent_cnt"):=.SD[1,parent_cnt],by=.(GEOID)]
bg_GQHH[!is.na(sibling_ID),("sibling_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-sibling_cnt),("sibling_cnt"):=.SD[1,sibling_cnt],by=.(GEOID)]
bg_GQHH[!is.na(relatives_ID),("relatives_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-relatives_cnt),("relatives_cnt"):=.SD[1,relatives_cnt],by=.(GEOID)]
bg_GQHH[!is.na(nonrelatives_ID),("nonrelatives_cnt"):=.N,by=.(GEOID)]
bg_GQHH[order(-nonrelatives_cnt),("nonrelatives_cnt"):=.SD[1,nonrelatives_cnt],by=.(GEOID)]

nrow(bg_GQHH[!is.na(child_step_ID)]) #277054
bg_GQHH[!is.na(add_1_ID),("child_step_ID"):=
  fcase(child_step_cnt<`Stepchild`-(1:.N)&add_1_age<20,add_1_ID,
        #default = paste0(`Stepchild`-(1:.N)," no match")),
        default = child_step_ID),
  by = .(GEOID)]
nrow(bg_GQHH[!is.na(child_step_ID)]) #; target is 405405

#do all the roles and then do all the matches back to bg_SARE, so it's also fully informed.
nrow(bg_GQHH[!is.na(child_grand_ID)]) #target is 879134
bg_GQHH[!is.na(add_1_ID),("child_grand_ID"):=
          fcase(child_grand_cnt<`Grandchild`-(1:.N)&add_1_age<20,add_1_ID,
        #        default = paste0(`Grandchild`-(1:.N)," no match")),
        default = child_grand_ID),
        by = .(GEOID)]
nrow(bg_GQHH[!is.na(child_grand_ID)]) #target is 879134
#redo the matches to bg_SARE, so you get it recorded both directions. bg_GQHH[,c("child_own_add_ID","child_own_add_sex","child_own_add_age","child_own_add_re_code"):=.SD[]]


#group members for better access by javascript later
bg_GQHH[!is.na(spouse_partner_ID),("spouse_partner"):=asplit(.SD,1),.SDcols=c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code")]
bg_GQHH[,c("spouse_partner_ID","spouse_partner_sex","spouse_partner_age","spouse_partner_re_code"):=NULL]

bg_GQHH[!is.na(child_own_1_ID),("child_own_1"):=asplit(.SD,1),.SDcols=c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code")]
bg_GQHH[,c("child_own_1_ID","child_own_1_sex","child_own_1_age","child_own_1_re_code"):=NULL]

bg_GQHH[!is.na(child_own_2_ID),("child_own_2"):=asplit(.SD,1),.SDcols=c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code")]
bg_GQHH[,c("child_own_2_ID","child_own_2_sex","child_own_2_age","child_own_2_re_code"):=NULL]

bg_GQHH[!is.na(child_own_3_ID),("child_own_3"):=asplit(.SD,1),.SDcols=c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code")]
bg_GQHH[,c("child_own_3_ID","child_own_3_sex","child_own_3_age","child_own_3_re_code"):=NULL]

bg_GQHH[!is.na(child_own_4_ID),("child_own_4"):=asplit(.SD,1),.SDcols=c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code")]
bg_GQHH[,c("child_own_4_ID","child_own_4_sex","child_own_4_age","child_own_4_re_code"):=NULL]

bg_GQHH[!is.na(child_own_add_ID),("child_own_add"):=asplit(.SD,1),.SDcols=c("child_own_add_ID","child_own_add_sex","child_own_add_age","child_own_add_re_code")]
bg_GQHH[,c("child_own_add_ID","child_own_add_sex","child_own_add_age","child_own_add_re_code"):=NULL]

bg_GQHH[!is.na(child_not_own_1_ID),("child_not_own_1"):=asplit(.SD,1),.SDcols=c("child_not_own_1_ID","child_not_own_1_sex","child_not_own_1_age","child_not_own_1_re_code")]
bg_GQHH[,c("child_not_own_1_ID","child_not_own_1_sex","child_not_own_1_age","child_not_own_1_re_code"):=NULL]

bg_GQHH[!is.na(child_not_own_2_ID),("child_not_own_2"):=asplit(.SD,1),.SDcols=c("child_not_own_2_ID","child_not_own_2_sex","child_not_own_2_age","child_not_own_2_re_code")]
bg_GQHH[,c("child_not_own_2_ID","child_not_own_2_sex","child_not_own_2_age","child_not_own_2_re_code"):=NULL]

bg_GQHH[!is.na(child_step_ID),("child_step"):=asplit(.SD,1),.SDcols=c("child_step_ID","child_step_sex","child_step_age","child_step_re_code")]
bg_GQHH[,c("child_step_ID","child_step_sex","child_step_age","child_step_re_code"):=NULL]

bg_GQHH[!is.na(child_grand_ID),("child_grand"):=asplit(.SD,1),.SDcols=c("child_grand_ID","child_grand_sex","child_grand_age","child_grand_re_code")]
bg_GQHH[,c("child_grand_ID","child_grand_sex","child_grand_age","child_grand_re_code"):=NULL]

bg_GQHH[!is.na(child_adopted_ID),("child_adopted"):=asplit(.SD,1),.SDcols=c("child_adopted_ID","child_adopted_sex","child_adopted_age","child_adopted_re_code")]
bg_GQHH[,c("child_adopted_ID","child_adopted_sex","child_adopted_age","child_adopted_re_code"):=NULL]

bg_GQHH[!is.na(child_foster_ID),("child_foster"):=asplit(.SD,1),.SDcols=c("child_foster_ID","child_foster_sex","child_foster_age","child_foster_re_code")]
bg_GQHH[,c("child_foster_ID","child_foster_sex","child_foster_age","child_foster_re_code"):=NULL]

bg_GQHH[!is.na(son_daughter_in_law_ID),("son_daughter_in_law"):=asplit(.SD,1),.SDcols=c("son_daughter_in_law_ID","son_daughter_in_law_sex","son_daughter_in_law_age","son_daughter_in_law_re_code")]
bg_GQHH[,c("son_daughter_in_law_ID","son_daughter_in_law_sex","son_daughter_in_law_age","son_daughter_in_law_re_code"):=NULL]

bg_GQHH[!is.na(parent_ID),("parent"):=asplit(.SD,1),.SDcols=c("parent_ID","parent_sex","parent_age","parent_re_code")]
bg_GQHH[,c("parent_ID","parent_sex","parent_age","parent_re_code"):=NULL]

bg_GQHH[!is.na(sibling_ID),("sibling"):=asplit(.SD,1),.SDcols=c("sibling_ID","sibling_sex","sibling_age","sibling_re_code")]
bg_GQHH[,c("sibling_ID","sibling_sex","sibling_age","sibling_re_code"):=NULL]

bg_GQHH[!is.na(relatives_ID),("relatives"):=asplit(.SD,1),.SDcols=c("relatives_ID","relatives_sex","relatives_age","relatives_re_code")]
bg_GQHH[,c("relatives_ID","relatives_sex","relatives_age","relatives_re_code"):=NULL]

bg_GQHH[!is.na(nonrelatives_ID),("nonrelatives"):=asplit(.SD,1),.SDcols=c("nonrelatives_ID","nonrelatives_sex","nonrelatives_age","nonrelatives_re_code")]
bg_GQHH[,c("nonrelatives_ID","nonrelatives_sex","nonrelatives_age","nonrelatives_re_code"):=NULL]

bg_GQHH[,("members"):=asplit(.SD,1),.SDcols=c("spouse_partner","parent","sibling","relatives","nonrelatives","son_daughter_in_law","child_foster","child_adopted",
                                                  "child_grand","child_step","child_own_add","child_own_1","child_own_2","child_own_3","child_own_4")]
#clean out the nulls
bg_GQHH[, members := lapply(members, function(x) x[!sapply(x, is.null)])]
#can get length from members to know hh_size actually given.
bg_GQHH[,("hh_size"):=lengths(members)+1] #need more testing for fixes
#table(bg_GQHH[,hh_size],bg_GQHH[,hh_size_7],useNA = "ifany")


#then do them for the others, then do an overall list of lists, with household included, and you can do the length of each list

#clean up excess columns
match_cols <- grep("_match|_cnt",names(bg_GQHH),value = TRUE)
bg_GQHH[,(match_cols):=NULL]
match_cols <- grep("_match",names(bg_SARE),value = TRUE)
bg_SARE[,(match_cols):=NULL]


#classifying topos is the target / destination for the structures of a model for the functors; working backwards from the tract as classifying topos is what the adjoint gets us
