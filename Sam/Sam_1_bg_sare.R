##Finish SAE for Tract and Block_group!!

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

#neighborhood functor gives a sense for structure that is combined with individual structure - 
#how that embeds in either the idea of number (with count or average, or regression, etc., all being embedded within) is point of the exercise
#that it shows how topology of destination changes is key, although it's constrained by the census schema at individual level

#https://www.census.gov/programs-surveys/acs/data/data-tables/table-ids-explained.html #no longer has full list...
#block_group for sex by age by race
groupname <- "P12" #SEX BY AGE FOR SELECTED AGE CATEGORIES (race/ethnicity)
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bgSARE_dec_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county,
                   api_type,path_suff)
if(names(bgSARE_dec_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("sex","age_range")
  #row_c1 determined by hand; table names for matching
  row_c1 <- c(unique(bgSARE_dec_data_from_census[!is.na(label_2)&
                                                   str_detect(concept,"NOT HISPANIC OR LATINO")&
                                                   !str_detect(concept,"IN COMBINATION") | 
                                                   !is.na(label_2)&
                                                   !str_detect(concept,"IN COMBINATION")&
                                                   str_detect(concept,", HISPANIC OR LATINO"),name]))
  #in combination counts people twice (or more) - may not want to use.
  #for TX, 34464309 in total with duplicate counts; 5133738 counted as two or more races; 185066 seem to be counted triple or more
  row_c2 <- c(unique(bgSARE_dec_data_from_census[!is.na(label_2)&
                                                   !str_detect(concept,"NOT HISPANIC OR LATINO")&
                                                   str_detect(concept,"IN COMBINATION") | 
                                                   !is.na(label_2)&
                                                   str_detect(concept,"IN COMBINATION")&
                                                   !str_detect(concept,", HISPANIC OR LATINO"),name]))
  test_total_pop <- tests_download_data(bgSARE_dec_data_from_census,label_c1,row_c1,state=state)
  bgSARE_data <- relabel(bgSARE_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  bgSARE2_data <- relabel(bgSARE_dec_data_from_census[!is.na(label)],label_c1,row_c2,groupname)
  write_relabel(bgSARE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
  write_relabel(bgSARE2_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff="combo_est")
}else{
  print("Using already given labels; no rewrite.")
  bgSARE_data <- bgSARE_dec_data_from_census
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff="combo_est_relabeled")
  if(file.exists(file_path)){
      bgSARE2_data <- read_rds(file_path)
    }else{
      print(paste0("bgSARE2 file does not exist at: ",file_path))
    }
}

bgSARE_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"SEX BY AGE FOR SELECTED AGE CATEGORIES \\(","")][
    ,("race") := str_replace(race,"\\)","")][
      ,("age_range") := str_replace(age_range, "Under 1 year", "0")]

#reshape a bit and make list of individuals
Geoids <- colnames(bgSARE_data[,.SD,.SDcols = startsWith(names(bgSARE_data),state)])
bgSARE_melted <- melt(bgSARE_data, id.vars = c("re_code","race","sex","age_range"), measure.vars = Geoids,
                      value.name = "codom_SARE", variable.name = "GEOID")
bgSARE <- as.data.table(lapply(bgSARE_melted[,.SD],rep,bgSARE_melted[,codom_SARE]))

#get two or more races as duplicated relations
bgSARE2_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"SEX BY AGE FOR SELECTED AGE CATEGORIES \\(","")][
    ,("race") := str_replace(race,"\\)","")][
      ,("age_range") := str_replace(age_range, "Under 1 year", "0")]

#reshape a bit and make list of individuals
bgSARE2_melted <- melt(bgSARE2_data, id.vars = c("re_code","race","sex","age_range"), measure.vars = Geoids,
                       value.name = "codom_SARE2", variable.name = "GEOID")
bgSARE2 <- as.data.table(lapply(bgSARE2_melted[,.SD],rep,bgSARE2_melted[,codom_SARE2]))

#assign order to individuals (maybe worth a comment on why different than doing subtraction first and then casting to individual)
#row_nums to both, merge toward combined; redo row_nums on combined, only if they don't have 

#clean up
rm(bgSARE_dec_data_from_census)
rm(bgSARE_data)
rm(bgSARE_melted)
rm(bgSARE2_data)
rm(bgSARE2_melted)

groupname <- "P8" #RACE (but with up to 6 combinations for "two or more races")
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bgR_dec_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county,
                   api_type,path_suff)
if(names(bgR_dec_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("number_races","race_description","multiple_races")
  #row_c1 determined by hand; table names for matching
  row_c1 <- c(unique(bgR_dec_data_from_census[!is.na(label_3) |
                                                   str_detect(label_2,"one"),name]))
  test_total_pop <- tests_download_data(bgR_dec_data_from_census,label_c1,row_c1,state=state)
  bgR_data <- relabel(bgR_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bgR_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bgR_data <- bgR_dec_data_from_census
}

#should be a more elegant way of assigning the series of race_1, etc...
bgR_data[,("race_descript"):=str_replace(race_description," alone","")]
bgR_data[
  ,("race_1"):=fifelse(number_races=="Population of one race",race_descript,
                       unlist(str_split(multiple_races,"; "))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,"; "))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,"; "))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,"; "))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,"; "))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,"; "))[6],by=.I]

bgR_melted <- melt(bgR_data, id.vars = c("race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids,
                   value.name = "codom_R", variable.name = "GEOID")
bgR <- as.data.table(lapply(bgR_melted[,.SD],rep,bgR_melted[,codom_R]))
rm(bgR_dec_data_from_census)
rm(bgR_data)
rm(bgR_melted)

groupname <- "P9" #HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE (combine with P8 to find HvL by race)
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bgE_dec_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county,
                   api_type,path_suff)
if(names(bgE_dec_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("HvL","number_races","race_description","multiple_races")
  #row_c1 determined by hand; table names for matching
  row_c1 <- c(unique(bgE_dec_data_from_census[!is.na(label_4) |
                                                str_detect(label_3,"one") |
                                                label_1=="Hispanic or Latino",name]))
  test_total_pop <- tests_download_data(bgE_dec_data_from_census,label_c1,row_c1,state=state)
  bgE_data <- relabel(bgE_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bgE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bgE_data <- bgE_dec_data_from_census
}
#should be a more elegant way of assigning the series of race_1, etc...
bgE_data[,("race_description"):=str_replace(race_description," alone","")] #for some reason, pipe wasn't working when called altogether, but does when separate...
bgE_data[
  ,("race_1"):=fifelse(number_races=="Population of one race",race_description,unlist(str_split(multiple_races,"; "))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,"; "))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,"; "))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,"; "))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,"; "))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,"; "))[6],by=.I]
bgE_melted <- melt(bgE_data, id.vars = c("HvL","race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids,
                   value.name = "codom_E", variable.name = "GEOID")
bgE <- as.data.table(lapply(bgE_melted[,.SD],rep,bgE_melted[,codom_E]))
rm(bgE_dec_data_from_census)
rm(bgE_data)
rm(bgE_melted)

groupname <- "P10" #RACE FOR THE POPULATION 18 YEARS AND OVER = includes breakdown by multiple races - could use it to check above?
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bgR18_dec_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county,
                   api_type,path_suff)
if(names(bgR18_dec_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("number_races","race_description","multiple_races")
  #row_c1 determined by hand; table names for matching
  row_c1 <- c(unique(bgR18_dec_data_from_census[!is.na(label_3) |
                                                str_detect(label_2,"one"),name]))
  test_total_pop <- tests_download_data(bgR18_dec_data_from_census,label_c1,row_c1,state=state)
  bgR18_data <- relabel(bgR18_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bgR18_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bgR18_data <- bgR18_dec_data_from_census
}
#should be a more elegant way of assigning the series of race_1, etc...
bgR18_data[,("race_description"):=str_replace(race_description," alone","")]
bgR18_data[
  ,("race_1"):=fifelse(number_races=="Population of one race",race_description,unlist(str_split(multiple_races,"; "))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,"; "))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,"; "))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,"; "))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,"; "))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,"; "))[6],by=.I]
bgR18_melted <- melt(bgR18_data, id.vars = c("race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids,
                     value.name = "codom_R18", variable.name = "GEOID")
bgR18 <- as.data.table(lapply(bgR18_melted[,.SD],rep,bgR18_melted[,codom_R18]))
rm(bgR18_dec_data_from_census)
rm(bgR18_data)
rm(bgR18_melted)

groupname <- "P11" #HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE FOR THE POPULATION 18 YEARS AND OVER
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bgE18_dec_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county,
                   api_type,path_suff)
if(names(bgE18_dec_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("HvL","number_races","race_description","multiple_races")
  #row_c1 determined by hand; table names for matching
  row_c1 <- c(unique(bgE18_dec_data_from_census[!is.na(label_4) |
                                                str_detect(label_3,"one") |
                                                label_1=="Hispanic or Latino",name]))
  test_total_pop <- tests_download_data(bgE18_dec_data_from_census,label_c1,row_c1,state=state)
  bgE18_data <- relabel(bgE18_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bgE18_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bgE18_data <- bgE18_dec_data_from_census
}
#should be a more elegant way of assigning the series of race_1, etc...
bgE18_data[,("race_description"):=str_replace(race_description," alone","")]
bgE18_data[
  ,("race_1"):=fifelse(number_races=="Population of one race",race_description,unlist(str_split(multiple_races,"; "))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,"; "))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,"; "))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,"; "))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,"; "))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,"; "))[6],by=.I]
bgE18_melted <- melt(bgE18_data, id.vars = c("HvL","race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids,
                     value.name = "codom_E18", variable.name = "GEOID")
bgE18 <- as.data.table(lapply(bgE18_melted[,.SD],rep,bgE18_melted[,codom_E18]))
rm(bgE18_dec_data_from_census)
rm(bgE18_data)
rm(bgE18_melted)

#collapse bgR, bgE, bgR18, and bgE18 together 
#doing my own variable construction for matching to better control multi-step process
bgR[,("races_match_id"):=
      paste0(GEOID,race_1,race_2,race_3,race_4,race_5,race_6,as.character(100000+sample(1:.N))),
    by=.(GEOID,race_1,race_2,race_3,race_4,race_5,race_6)]
bgE[,("races_match_id"):=
      paste0(GEOID,race_1,race_2,race_3,race_4,race_5,race_6,as.character(100000+sample(1:.N))),
    by=.(GEOID,race_1,race_2,race_3,race_4,race_5,race_6)]
#move just bgE with race info, b/c: table(bgE[,HvL],bgE[,race_1])
bgR[,("HvL"):=
      bgE[.SD,list(HvL),on=.(races_match_id)]]
#table(bgR[!is.na(HvL),race_1])==table(bgE[,race_1])
bgR[is.na(HvL),("HvL"):="Hispanic or Latino"]
#under 18
bgR18[,("races_match_id"):=
      paste0(GEOID,race_1,race_2,race_3,race_4,race_5,race_6,as.character(100000+sample(1:.N))),
    by=.(GEOID,race_1,race_2,race_3,race_4,race_5,race_6)]
bgE18[,("races_match_id"):=
      paste0(GEOID,race_1,race_2,race_3,race_4,race_5,race_6,as.character(100000+sample(1:.N))),
    by=.(GEOID,race_1,race_2,race_3,race_4,race_5,race_6)]
bgR18[,("HvL"):=
      bgE18[.SD,list(HvL),on=.(races_match_id)]]
#table(bgR18[!is.na(HvL),race_1])==table(bgE18[,race_1])
bgR18[is.na(HvL),("HvL"):="Hispanic or Latino"]
bgR18[,("under_18"):="18 years or older"]
bgR18[,("races_eth_match_id"):=
        paste0(GEOID,HvL,race_1,race_2,race_3,race_4,race_5,race_6,as.character(100000+sample(1:.N))),
      by=.(GEOID,HvL,race_1,race_2,race_3,race_4,race_5,race_6)]
bgR[,("races_eth_match_id"):=
        paste0(GEOID,HvL,race_1,race_2,race_3,race_4,race_5,race_6,as.character(100000+sample(1:.N))),
      by=.(GEOID,HvL,race_1,race_2,race_3,race_4,race_5,race_6)]
bgR[,("under_18"):=
      bgR18[.SD,list(under_18),on=.(races_eth_match_id)]]
#table(bgR[!is.na(under_18),race_1])==table(bgR18[,race_1])
bgR[is.na(under_18),("under_18"):="Under 18 years old"]
bgR[,("codom_races"):=.N,by=.(GEOID,HvL,under_18,race_1)] #should think through how this compares with codom_R
bgR[,("copath_races"):=.N,by=.(GEOID,HvL,under_18,race_1,race_2,race_3,race_4,race_5,race_6)]
bgR[,("weight_races"):=copath_races/codom_races] #remember all will be inside "Two or more races" 
#then, as we build, we'd know how to nudge the weights by re_code_(1:6)? 

bgR[,(paste0("codom_race_",as.character(2:6))):=nrow(.SD[!is.na(race_1)]),by=.(GEOID,HvL,race_1)]
bgR[!is.na(race_2),("copath_race_2"):=.N,by=.(GEOID,HvL,under_18,race_1,race_2)]
bgR[!is.na(race_3),("copath_race_3"):=.N,by=.(GEOID,HvL,under_18,race_2,race_3)]
bgR[!is.na(race_4),("copath_race_4"):=.N,by=.(GEOID,HvL,under_18,race_3,race_4)]
bgR[!is.na(race_5),("copath_race_5"):=.N,by=.(GEOID,HvL,under_18,race_4,race_5)]
bgR[!is.na(race_6),("copath_race_6"):=.N,by=.(GEOID,HvL,under_18,race_5,race_6)]
bgR[codom_race_2>0,("weight_race_2"):=copath_race_2/codom_race_2]
bgR[codom_race_3>0,("weight_race_3"):=copath_race_3/codom_race_3]
bgR[codom_race_4>0,("weight_race_4"):=copath_race_4/codom_race_4]
bgR[codom_race_5>0,("weight_race_5"):=copath_race_5/codom_race_5]
bgR[codom_race_6>0,("weight_race_6"):=copath_race_6/codom_race_6]
#then order by co_race_6, etc., and assign from 1 - 6 (from co_path_2 through 6) should avoid things like "Some Other Race" twice, without making an explicit rule
#need to figure out P17 relationships before that and in such a way that it adds something to the weights.
#before doing P17, will do bgSARE2 info, to help with weighting

#clean up the trail
rm(bgE)
rm(bgR18)
rm(bgE18)

#join with bgSARE2
#start with race_6, assigning probabilities, in terms of the codom vars, then, do the determination of all the others with probs, from 1-6;
#getting the age and sex from bgSARE2, as weighted by the others.
#Texas Two or more races = 5,133,738; we're trying to get just these matches right for age and sex; 
#note fewer options as multiple races are always listed in a certain order, with white first
bgR[,("re_code_1"):=fcase(HvL=="Hispanic or Latino"&race_1=="White","P",
                        HvL=="Not Hispanic or Latino"&race_1=="White","I",
                        HvL=="Hispanic or Latino"&race_1=="Black or African American","Q",
                        HvL=="Not Hispanic or Latino"&race_1=="Black or African American","J",
                        HvL=="Hispanic or Latino"&race_1=="Asian","S",
                        HvL=="Not Hispanic or Latino"&race_1=="Asian","L",
                        HvL=="Hispanic or Latino"&race_1=="American Indian and Alaska Native","R",
                        HvL=="Not Hispanic or Latino"&race_1=="American Indian and Alaska Native","K",
                        HvL=="Hispanic or Latino"&race_1=="Native Hawaiian and Other Pacific Islander","T",
                        HvL=="Not Hispanic or Latino"&race_1=="Native Hawaiian and Other Pacific Islander","M",
                        HvL=="Hispanic or Latino"&race_1=="Some Other Race","U",
                        HvL=="Not Hispanic or Latino"&race_1=="Some Other Race","N",default = NA)]
bgR[,("re_code_2"):=fcase(HvL=="Hispanic or Latino"&race_2=="Black or African American","Q",
                          HvL=="Not Hispanic or Latino"&race_2=="Black or African American","J",
                          HvL=="Hispanic or Latino"&race_2=="Asian","S",
                          HvL=="Not Hispanic or Latino"&race_2=="Asian","L",
                          HvL=="Hispanic or Latino"&race_2=="American Indian and Alaska Native","R",
                          HvL=="Not Hispanic or Latino"&race_2=="American Indian and Alaska Native","K",
                          HvL=="Hispanic or Latino"&race_2=="Native Hawaiian and Other Pacific Islander","T",
                          HvL=="Not Hispanic or Latino"&race_2=="Native Hawaiian and Other Pacific Islander","M",
                          HvL=="Hispanic or Latino"&race_2=="Some Other Race","U",
                          HvL=="Not Hispanic or Latino"&race_2=="Some Other Race","N",default = NA)]
bgR[,("re_code_3"):=fcase(HvL=="Hispanic or Latino"&race_3=="Asian","S",
                          HvL=="Not Hispanic or Latino"&race_3=="Asian","L",
                          HvL=="Hispanic or Latino"&race_3=="American Indian and Alaska Native","R",
                          HvL=="Not Hispanic or Latino"&race_3=="American Indian and Alaska Native","K",
                          HvL=="Hispanic or Latino"&race_3=="Native Hawaiian and Other Pacific Islander","T",
                          HvL=="Not Hispanic or Latino"&race_3=="Native Hawaiian and Other Pacific Islander","M",
                          HvL=="Hispanic or Latino"&race_3=="Some Other Race","U",
                          HvL=="Not Hispanic or Latino"&race_3=="Some Other Race","N",default = NA)]
bgR[,("re_code_4"):=fcase(HvL=="Hispanic or Latino"&race_4=="Asian","S",
                          HvL=="Not Hispanic or Latino"&race_4=="Asian","L",
                          HvL=="Hispanic or Latino"&race_4=="Native Hawaiian and Other Pacific Islander","T",
                          HvL=="Not Hispanic or Latino"&race_4=="Native Hawaiian and Other Pacific Islander","M",
                          HvL=="Hispanic or Latino"&race_4=="Some Other Race","U",
                          HvL=="Not Hispanic or Latino"&race_4=="Some Other Race","N",default = NA)]
bgR[,("re_code_5"):=fcase(HvL=="Hispanic or Latino"&race_5=="Native Hawaiian and Other Pacific Islander","T",
                          HvL=="Not Hispanic or Latino"&race_5=="Native Hawaiian and Other Pacific Islander","M",
                          HvL=="Hispanic or Latino"&race_5=="Some Other Race","U",
                          HvL=="Not Hispanic or Latino"&race_5=="Some Other Race","N",default = NA)]
bgR[,("re_code_6"):=fcase(HvL=="Hispanic or Latino"&race_6=="Some Other Race","U",
                          HvL=="Not Hispanic or Latino"&race_6=="Some Other Race","N",default = NA)]
bgSARE2[,("re_codeB"):=fcase(race=="AMERICAN INDIAN AND ALASKA NATIVE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, HISPANIC OR LATINO","R",
                             race=="AMERICAN INDIAN AND ALASKA NATIVE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, NOT HISPANIC OR LATINO","K",
                             race=="ASIAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, HISPANIC OR LATINO","S",
                             race=="ASIAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, NOT HISPANIC OR LATINO","L",
                             race=="WHITE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, HISPANIC OR LATINO","P",
                             race=="WHITE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, NOT HISPANIC OR LATINO","I",
                             race=="BLACK OR AFRICAN AMERICAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, HISPANIC OR LATINO","Q",
                             race=="BLACK OR AFRICAN AMERICAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, NOT HISPANIC OR LATINO","J",
                             race=="NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, HISPANIC OR LATINO","T",
                             race=="NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, NOT HISPANIC OR LATINO","M",
                             race=="SOME OTHER RACE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, HISPANIC OR LATINO","U",
                             race=="SOME OTHER RACE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES, NOT HISPANIC OR LATINO","N",default = NA)]
bgSARE[,("age_num"):=fcase(age_range=="Under 5 years",as.integer(0),
                            age_range=="5 to 9 years",as.integer(5),default = as.integer(str_sub(age_range,start=1,end=2)))]
bgSARE[,("HvL"):=fifelse(str_detect(race,"NOT"),"Not Hispanic or Latino","Hispanic or Latino")]
bgSARE2[,("age_num"):=fcase(age_range=="Under 5 years",as.integer(0),
                            age_range=="5 to 9 years",as.integer(5),default = as.integer(str_sub(age_range,start=1,end=2)))]
bgSARE2[,("under_18"):=fifelse(age_num<18,"Under 18 years old","18 years or older")]
bgSARE2[,("HvL"):=fifelse(str_detect(race,"NOT"),"Not Hispanic or Latino","Hispanic or Latino")]


#keeping track of weights, but may show that they are not needed b/c the mapping re-asserts the ground truth...
#do a match between bgSARE and bgSARE2, so that we have all the ones that aren't "two or more races" in both.
bgSARE[,("races_age_match_id"):=
      list(paste0(GEOID,re_code,sex,age_range,as.character(100000+sample(1:.N)))),
    by=.(GEOID,re_code,sex,age_range)] #re_codes account for HvL
bgSARE2[,c("codom_re_code_1","races_age_match_id"):=
          c(list(.N),list(paste0(GEOID,re_codeB,sex,age_range,as.character(100000+sample(1:.N))))),
        by=.(GEOID,re_codeB,sex,age_range)]
bgSARE2[,("matched1"):=
      bgSARE[.SD,list(re_code),on=.(races_age_match_id)]]
#nrow(bgSARE[str_detect(race,"TWO")])+nrow(bgSARE2[!is.na(matched1)])==nrow(bgSARE)
#with calculation for probability by codomain...

#this is what it means to do a copath determination and not a codomain
#determine along the path as if it were a set of codomains = weights
#determine / shrink the region and then map so that the right grounded frame is called = co-paths
#everything but the two or more races 
bgR[is.na(race_2),("races_age_match_1_id"):=
        paste0(GEOID,re_code_1,under_18,as.character(100000+sample(1:.N))),
      by=.(GEOID,re_code_1,under_18)] #re_codes account for HvL
bgSARE2[!is.na(matched1),c("codom_re_code_1","races_age_match_1_id"):=
          c(list(.N),list(paste0(GEOID,re_codeB,under_18,as.character(100000+sample(1:.N))))),
      by=.(GEOID,re_codeB,under_18)]
bgR[is.na(race_2),c("sex","age_range","age_num","codom_re_code_1"):=
      bgSARE2[!is.na(matched1)][.SD,c(list(sex),list(age_range),list(age_num),list(codom_re_code_1)),
              on=.(races_age_match_1_id)]]
nrow(bgR[is.na(race_2)])
#nrow(bgR[is.na(sex)])==nrow(bgSARE[str_detect(race,"TWO")])
#table(bgR[,sex],bgR[,age_range])==table(bgSARE[!str_detect(race,"TWO"),sex],bgSARE[!str_detect(race,"TWO"),age_range])
#table(bgR[,sex],bgR[,age_range])==table(bgSARE2[!is.na(matched1),sex],bgSARE2[!is.na(matched1),age_range])
#because I'm not changing values on bgR, path from 1- 6 will stay; all I get from SARE2 is that there is a match

#finish path along races for two or more races group
bgR[!is.na(race_2),("races_age_match_2_id"):=
      list(paste0(GEOID,re_code_2,under_18,as.character(100000+sample(1:.N)))),
    by=.(GEOID,re_code_2,under_18)] #re_codes account for HvL
bgSARE2[is.na(matched1),c("codom_re_code_2","races_age_match_2_id"):=
          c(list(.N),list(paste0(GEOID,re_codeB,under_18,as.character(100000+sample(1:.N))))),
        by=.(GEOID,re_codeB,under_18)]
bgR[!is.na(race_2),c("sex","age_range","age_num","codom_re_code_2"):=
      bgSARE2[.SD,c(list(sex),list(age_range),list(age_num),list(codom_re_code_2)),
              on=.(races_age_match_2_id)]]
bgSARE2[is.na(matched1),("matched1"):= 
          bgR[!is.na(race_2)][.SD,list(re_codeB),
        on=.(races_age_match_2_id)]]
nrow(bgR[!is.na(codom_re_code_2)])
nrow(bgR[!is.na(race_2)])
#nrow(bgR[is.na(sex)])==0
#nrow(bgSARE2[is.na(matched1)])==0 #that is, all two or more have matched already... 
#the idea that the whole match along the 6 races adds structure

bgR[!is.na(race_3),("races_age_match_3_id"):=
      paste0(GEOID,re_code_3,sex,age_range,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code_3,sex,age_range)] #re_codes account for HvL
bgSARE2[!is.na(codom_re_code_2),c("codom_re_code_3","races_age_match_3_id"):=
          c(list(.N),list(paste0(GEOID,re_codeB,sex,age_range,as.character(100000+sample(1:.N))))),
        by=.(GEOID,re_codeB,sex,age_range)]
bgR[!is.na(race_3),("codom_re_code_3"):=
      bgSARE2[.SD,list(codom_re_code_3),
              on=.(races_age_match_3_id)]]
bgSARE2[!is.na(codom_re_code_2),("matched2"):=
          bgR[!is.na(race_3)][.SD,list(re_codeB),
              on=.(races_age_match_3_id)]]
nrow(bgR[!is.na(codom_re_code_3)])
nrow(bgR[!is.na(codom_re_code_3)])/nrow(bgR[!is.na(race_3)])#percent that matched for race_3

bgR[!is.na(race_4),("races_age_match_4_id"):=
      paste0(GEOID,re_code_4,sex,age_range,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code_4,sex,age_range)] #re_codes account for HvL
bgSARE2[!is.na(matched2),c("codom_re_code_4","races_age_match_4_id"):=
          c(list(.N),list(paste0(GEOID,re_codeB,sex,age_range,as.character(100000+sample(1:.N))))),
        by=.(GEOID,re_codeB,sex,age_range)]
bgR[!is.na(race_4),("codom_re_code_4"):=
      bgSARE2[.SD,list(codom_re_code_4),
              on=.(races_age_match_4_id)]]
bgSARE2[!is.na(matched2),("matched2"):=
          bgR[!is.na(race_4)][.SD,list(re_codeB),
              on=.(races_age_match_4_id)]]
nrow(bgR[!is.na(codom_re_code_4)])
nrow(bgR[!is.na(race_4)])

bgR[!is.na(race_5),("races_age_match_5_id"):=
      paste0(GEOID,re_code_5,sex,age_range,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code_5,sex,age_range)] #re_codes account for HvL
bgSARE2[!is.na(matched2),c("codom_re_code_5","races_age_match_5_id"):=
          c(list(.N),list(paste0(GEOID,re_codeB,sex,age_range,as.character(100000+sample(1:.N))))),
        by=.(GEOID,re_codeB,sex,age_range)]
bgR[!is.na(race_5),("codom_re_code_5"):=
      bgSARE2[.SD,list(codom_re_code_5),
              on=.(races_age_match_5_id)]]
bgSARE2[!is.na(matched2),("matched2"):=
          bgR[!is.na(race_5)][.SD,list(re_codeB),
              on=.(races_age_match_5_id)]]
nrow(bgR[!is.na(codom_re_code_5)])
nrow(bgR[!is.na(race_5)])

bgR[!is.na(race_6),("races_age_match_6_id"):=
      paste0(GEOID,re_code_6,sex,age_range,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code_6,sex,age_range)] #re_codes account for HvL
bgSARE2[!is.na(matched2),c("codom_re_code_6","races_age_match_6_id"):=
          c(list(.N),list(paste0(GEOID,re_codeB,sex,age_range,as.character(100000+sample(1:.N))))),
        by=.(GEOID,re_codeB,sex,age_range)]
bgR[!is.na(race_6),("codom_re_code_6"):=
      bgSARE2[.SD,list(codom_re_code_6),
              on=.(races_age_match_6_id)]]
bgSARE2[!is.na(matched2),("matched2"):=
          bgR[!is.na(race_6)][.SD,list(re_codeB),
              on=.(races_age_match_6_id)]]
nrow(bgR[!is.na(codom_re_code_6)])
nrow(bgR[!is.na(race_6)])

#that it matched so well at race_1 of two or more races is remarkable, but we're only getting 70-80% of matches after that.


#compare the matching we just did with one that includes the weights as codomain markers, and then talk about final matching back...



#idea is that you're putting the weights (codom_race_6/codom_races) with the race - need to make sure they vary...

#TO DO NEXT:::each layer of race_ needs to be constructed - idea is to have the paths built up to indicate embedding on the race_01 through 6 as an example
#
#does determining backwards ensure no hallucinations? Is it the same problem as hallucination??
#test - ??
#do some tables and think about what to add...
#have to think harder about race_1, too...
#then move race_1 through 6 to bgSARE
bgR[,("races_age2_match_id"):=
      paste0(GEOID,HvL,re_code,under_18,as.character(100000+sample(1:.N))),
    by=.(GEOID,HvL,re_code,under_18)]
bgSARE[,c("codom_races","races_age2_match_id"):=
         c(list(.N),list(paste0(GEOID,HvL,re_code,under_18,as.character(100000+sample(1:.N))))),
       by=.(GEOID,HvL,re_code,under_18)]
bgSARE[,c("codom_re_code_18","codom_race_6","codom_race_5","codom_race_4","codom_race_3","codom_race_2","matched1"):=
          bgR[.SD,c(list(codom_re_code_18),list(codom_race_6),list(codom_race_5),list(codom_race_4),list(codom_race_3)
                    ,list(codom_race_2),list(race_6)),on=.(races_age_match_id)]]


#need to not make order in bgARE determinative (White is not always race_1; Black is not always race_2, etc)
#should we wait, in some sense, so that siblings have matching multiples?
#should we do th ¬¬ logic for some determinations?
#should we go back to the cartesian expansion idea for finding within a space? I don't think so - not very elegant, after all...

#taking bgSARE as the base, assign ¬¬weights for every second race to indicate a potential direction, not yet filled. 
#final goal is having a distance to calculate against as we get to point of determination.
#so, for race_6, only value is on ¬¬Some Other Race, but applies to everyone. 
#For each block group, what does the total number in that category indicate? Do we multiply by that magnitude in that direction?
#magnitude should be #to assign per bg / # that could've matched (I believe it's either value.i or value!)


#schematic_sam_dec
groupname <- "P16" #HOUSEHOLDER AGE/RACE/ETH
groupname <- "H13" #HOUSEHOLDER AGE / TENURE
groupname <- "H13" #HOUSEHOLD TYPE / TENURE

groupname <- "P17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP
groupname <- "PCT17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP WITH RACE/ETH - more categories at tract
#and PCT8 RELATIONSHIP BY AGE FOR THE POPULATION UNDER 18 YEARS
groupname <- "P20" #OWN CHILDREN
#PCT15 is coupled households, including same sex


#moved from schematic_sam.Rmd - moved to data.table only
groupname <- "P18" #GROUP QUARTERS POPULATION BY SEX BY AGE BY MAJOR GROUP QUARTERS TYPE
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_gq_age_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_gq_age_data_from_census)[6]=="label_1"){
  #labels determined by hand
  label_c1 <- c("sex","age_range","gq_institution","gq_type")
  #row_c1 determined by hand
  row_c1 <- c(unique(bg_gq_age_data_from_census[!is.na(label_4),name]))
  test_total_pop <- tests_download_data(bg_gq_age_data_from_census,label_c1,row_c1,as.character(state))
  bgGQ_data <- relabel(bg_gq_age_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bgGQ_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bgGQ_data <- bg_gq_age_data_from_census
}
#if(!test_total_pop){test_total_pop<-sum(bgGQ_data[,total],na.rm = TRUE)}
rm(bg_gq_age_data_from_census)

bgGQ_data[,("beg_age_gq"):=fcase(age_range=="Under 18 years", as.numeric(0),
                            age_range=="18 to 64 years", as.numeric(18),
                            age_range=="65 years and over", as.numeric(65))]
bgGQ_data[,("gq_type_6"):=fcase(str_detect(gq_type,"College"), #catching some idiosyncratic capitalization
                             "College/University student housing",
                           str_detect(gq_type,"Nursing"),
                             "Nursing facilities",
                           gq_type=="Correctional facilities for adults (101-106)",
                             "Correctional facilities for adults",
                           gq_type=="Juvenile facilities (201-203)",
                             "Juvenile facilities",
                           gq_type=="Other institutional facilities (401-405)",
                             "Other institutional facilities",
                           gq_type=="Other noninstitutional facilities (701-702, 704, 706, 801-802, 900-901, 903-904)",
                             "Other noninstitutional facilities")]
#reshape a bit and make list of individuals 
Geoids <- colnames(bgGQ_data[,10:(ncol(bgGQ_data)-3)])
bgGQ_melted <- melt(bgGQ_data, id.vars = c("sex","age_range","beg_age_gq","gq_institution","gq_type","gq_type_6"), measure.vars = Geoids,
                    value.name = "codom_GQSAT", variable.name = "GEOID")
bgGQ <- as.data.table(lapply(bgGQ_melted[,.SD],rep,bgGQ_melted[,codom_GQSAT]))
rm(bgGQ_data)
rm(bgGQ_melted)
