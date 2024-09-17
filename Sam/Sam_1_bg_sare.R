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
bgSARE_melted <- melt(bgSARE_data, id.vars = c("re_code","race","sex","age_range"), measure.vars = Geoids)
bgSARE <- as.data.table(lapply(bgSARE_melted[,.SD],rep,bgSARE_melted[,value]))

#get two or more races as duplicated relations
bgSARE2_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"SEX BY AGE FOR SELECTED AGE CATEGORIES \\(","")][
    ,("race") := str_replace(race,"\\)","")][
      ,("age_range") := str_replace(age_range, "Under 1 year", "0")]

#reshape a bit and make list of individuals
bgSARE2_melted <- melt(bgSARE2_data, id.vars = c("re_code","race","sex","age_range"), measure.vars = Geoids)
bgSARE2 <- as.data.table(lapply(bgSARE_melted[,.SD],rep,bgSARE_melted[,value]))

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
  ,("race_1"):=fifelse(number_races=="Population of one race",race_descript,unlist(str_split(multiple_races,";"))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,";"))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,";"))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,";"))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,";"))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,";"))[6],by=.I]

bgR_melted <- melt(bgR_data, id.vars = c("race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids)
bgR <- as.data.table(lapply(bgR_melted[,.SD],rep,bgR_melted[,value]))
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
  ,("race_1"):=fifelse(number_races=="Population of one race",race_description,unlist(str_split(multiple_races,";"))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,";"))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,";"))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,";"))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,";"))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,";"))[6],by=.I]
bgE_melted <- melt(bgE_data, id.vars = c("HvL","race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids)
bgE <- as.data.table(lapply(bgE_melted[,.SD],rep,bgE_melted[,value]))
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
  ,("race_1"):=fifelse(number_races=="Population of one race",race_description,unlist(str_split(multiple_races,";"))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,";"))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,";"))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,";"))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,";"))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,";"))[6],by=.I]
bgR18_melted <- melt(bgR18_data, id.vars = c("race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids)
bgR18 <- as.data.table(lapply(bgR18_melted[,.SD],rep,bgR18_melted[,value]))
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
  ,("race_1"):=fifelse(number_races=="Population of one race",race_description,unlist(str_split(multiple_races,";"))[1]), by=.I][
    ,("race_2"):=unlist(str_split(multiple_races,";"))[2],by=.I][
      ,("race_3"):=unlist(str_split(multiple_races,";"))[3],by=.I][
        ,("race_4"):=unlist(str_split(multiple_races,";"))[4],by=.I][
          ,("race_5"):=unlist(str_split(multiple_races,";"))[5],by=.I][
            ,("race_6"):=unlist(str_split(multiple_races,";"))[6],by=.I]
bgE18_melted <- melt(bgE18_data, id.vars = c("HvL","race_1","race_2","race_3","race_4","race_5","race_6"), measure.vars = Geoids)
bgE18 <- as.data.table(lapply(bgE18_melted[,.SD],rep,bgE18_melted[,value]))
rm(bgE18_dec_data_from_census)
rm(bgE18_data)
rm(bgE18_melted)

#collapse bgR, bgE, bgR18, and bgE18 together - variable is GEOID
bgR[,("row_num"):=1:.N,by=.(variable,race_1,race_2,race_3,race_4,race_5,race_6)]
setnames(bgR,"value","race_value")
bgE[,("row_num"):=1:.N,by=.(variable,race_1,race_2,race_3,race_4,race_5,race_6)]
setnames(bgE,"value","eth_value")
bgRE <- bgE[bgR,on=.(variable,race_1,race_2,race_3,race_4,race_5,race_6,row_num)] 
bgRE[,("HvL"):=fifelse(is.na(HvL),"Hispanic or Latino",HvL)]
#setorderv(bgR18,c("variable","race_1","race_2","race_3","race_4","race_5","race_6"),na.last=TRUE)
#setorderv(bgE18,c("variable","race_1","race_2","race_3","race_4","race_5","race_6"),na.last=TRUE)
bgR18[,("row_num"):=1:.N,by=.(variable,race_1,race_2,race_3,race_4,race_5,race_6)]
setnames(bgR18,"value","race18_value")
bgE18[,("row_num"):=1:.N,by=.(variable,race_1,race_2,race_3,race_4,race_5,race_6)]
setnames(bgE18,"value","eth18_value")
bgRE18 <- bgE18[bgR18,on=.(variable,race_1,race_2,race_3,race_4,race_5,race_6,row_num)]
bgRE18[,("HvL"):=fifelse(is.na(HvL),"Hispanic or Latino",HvL)]
#think about renaming value and i.value and row_num...
bgARE <- bgRE[bgRE18,on=.(variable,HvL,race_1,race_2,race_3,race_4,race_5,race_6,row_num)]
bgARE[,("under_18"):=fifelse(is.na(race18_value),T,F)]
#clean up the trail
rm(bgR)
rm(bgE)
rm(bgR18)
rm(bgE18)
rm(bgRE)
rm(bgRE18)

#join with bgSARE
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
bgGQ_melted <- melt(bgGQ_data, id.vars = c("sex","age_range","beg_age_gq","gq_institution","gq_type","gq_type_6"), measure.vars = Geoids)
bgGQ <- as.data.table(lapply(bgGQ_melted[,.SD],rep,bgGQ_melted[,value]))
rm(bgGQ_data)
rm(bgGQ_melted)
