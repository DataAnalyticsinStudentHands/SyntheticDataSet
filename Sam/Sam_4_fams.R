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

#and get GQ, b/c never satisfied with how it broke out
file_path <- valid_file_path(censusdir,vintage,state,county="*",api_type="dec",geo_type="block_group",
                             groupname="bg_GQ",path_suff="wrk")
#"~/University Of Houston/Engaged Data Science - Data/Census/2020/state_48/2020_48_dec_block_group_bg_hhSARETT_wrk.RDS"
bg_GQ <- readRDS(file_path)
 
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
bg_SARE[,("age_range_9hh"):="NEED TO MAKE MATCH"]
bg_hhSARETT[,("age_range_9hh"):=str_remove_all(age_range_9,"Householder ")]
bg_hhSARETT[,("hh_match1_id"):=
             paste0(GEOID,re_code_14,age_range_9hh,sex,alone,as.character(100000+sample(1:.N))),
           by=.(GEOID,re_code_14,age_range_9hh,sex,alone)]
bg_SARE[,("hh_match1_id"):=
               paste0(GEOID,re_code,age_range_9hh,sex,alone,as.character(100000+sample(1:.N))),
             by=.(GEOID,re_code,age_range_9hh,sex,alone)]
bg_hhSARETT[,c("ind_ID","age_range","HvL","race_1","race_2","race_3","race_4","race_5","race_6"):=
              bg_SARE[.SD,c(list(ind_ID),list(age_range),list(HvL),list(race_1),list(race_2),
                            list(race_3),list(race_4),list(race_5),list(race_6)),on=.(hh_match1_id)]]
bg_SARE[,c("hh_ID","family","family_type","family_type_4","family_type_7","no_spouse_sex","same_sex",
           "couple_gender","match_type_5","hh_size_7","multi_gen_hh","rel_in_house","anyone_60","anyone_65","anyone_75",
           "household_60","household_65","household_75","rent_own","tenure","all_kid_18","own_kids","kid_age_range_3"):=
               bg_hhSARETT[.SD,c(list(hh_ID),list(family),list(family_type),list(family_type_4),list(family_type_7),
                                 list(no_spouse_sex),list(same_sex),list(couple_gender),list(match_type_5),list(hh_size_7),
                                 list(multi_gen_hh),list(rel_in_house),list(anyone_60),list(anyone_65),list(anyone_75),
                                 list(household_60),list(household_65),list(household_75),list(rent_own),
                                 list(tenure),list(all_kid_18),list(own_kids),list(kid_age_range_3)),on=.(hh_match1_id)]]

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
