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

#moved from schematic_sam.Rmd - moved to data.table only - LOOKS LIKE BG HAS SAME AS TR!!
#groupname <- "P12" #SEX BY AGE FOR SELECTED AGE CATEGORIES (race/ethnicity)
#geo_type <- "tract"
#api_type <- "dec/dhc"
#path_suff <- "est"
#trSARE_dec_data_from_census <- 
#  census_tract_get(censusdir, vintage, state, censuskey, 
#                   groupname,county,
#                   api_type,path_suff)
#if(names(trSARE_dec_data_from_census)[6]=="label_1"){
#  #labels determined by hand
#  label_c1 <- c("sex","age_range")
#  #row_c1 determined by hand; table names for matching
#  row_c1 <- c(unique(trSARE_dec_data_from_census[!is.na(label_2)&
#                                                  str_detect(concept,"NOT HISPANIC OR LATINO")&
#                                                  !str_detect(concept,"IN COMBINATION") | 
#                                                  !is.na(label_2)&
#                                                  !str_detect(concept,"IN COMBINATION")&
#                                                  str_detect(concept,", HISPANIC OR LATINO"),name]))
#  #in combination counts people twice (or more) - may not want to use.
#  #for TX, 34464309 in total with duplicate counts; 5133738 counted as two or more races; 185066 seem to be counted triple or more
#  row_c2 <- c(unique(trSARE_dec_data_from_census[!is.na(label_2)&
#                                                  !str_detect(concept,"NOT HISPANIC OR LATINO")&
#                                                  str_detect(concept,"IN COMBINATION") | 
#                                                  !is.na(label_2)&
#                                                  str_detect(concept,"IN COMBINATION")&
#                                                  !str_detect(concept,", HISPANIC OR LATINO"),name]))
#  test_total_pop <- tests_download_data(trSARE_dec_data_from_census,"_001",label_c1,row_c1,state)
#  trSARE_data <- relabel(trSARE_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
#  write_relabel(trSARE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
#}else{
#  print("Using already given labels; no rewrite.")
#  trSARE_data <- trSARE_dec_data_from_census
#}
#
#trSARE_data[,("re_code") := substr(name,4,4)]
#trSARE_data[,("race") := str_replace(concept,"SEX BY AGE FOR SELECTED AGE CATEGORIES \\(","")]
#trSARE_data[,("race") := str_replace(race,"\\)","")]
#trSARE_data[,("age_range") := str_replace(age_range, "Under 1 year", "0")]
#trSARE_data[,("age_range") := str_replace(age_range,"year"," year")]
#suppressWarnings(
#  trSARE_data[,("age") := as.integer(substr(age_range,1,3))])
#
##reshape a bit and make list of individuals
#Geoids <- colnames(trSARE_data[,8:(ncol(trSARE_data)-4)])
#trSARE_melted <- melt(trSARE_data, id.vars = c("re_code","race","sex","age_range","age"), measure.vars = Geoids)
#trSARE <- as.data.table(lapply(trSARE_melted[,.SD],rep,trSARE_melted[,value]))
#
##get trSAE for Hispanic or Latino
#rm(trSARE_dec_data_from_census)
#rm(trSARE_data)
#rm(trSARE_melted)

#block_group level of same data
groupname <- "P12" #SEX BY AGE FOR SELECTED AGE CATEGORIES (race/ethnicity)
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bgSARE_dec_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county,
                   api_type,path_suff)
if(names(bgSARE_dec_data_from_census)[6]=="label_1"){
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
  test_total_pop <- tests_download_data(bgSARE_dec_data_from_census,label_c1,row_c1,as.character(state))
  bgSARE_data <- relabel(bgSARE_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  bgSARE2_data <- relabel(bgSARE_dec_data_from_census[!is.na(label)],label_c1,row_c2,groupname)
  write_relabel(bgSARE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
  write_relabel(bgSARE2_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff="combo_est")
}else{
  print("Using already given labels; no rewrite.")
  bgSARE_data <- bgSARE_dec_data_from_census
  file_path <- valid_file_path(censusdir,vintage,state,county,api_type,geo_type,groupname,path_suff="combo_est")
  if(file.exists(file_path)){
      bgSARE2_data <- read_rds(file_path)
    }else{
      print(paste0("bgSARE2 file does not exist at: ",file_path))
    }
}

bgSARE_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"SEX BY AGE FOR SELECTED AGE CATEGORIES \\(","")][
    ,("race") := str_replace(race,"\\)","")][
      ,("age_range") := str_replace(age_range, "Under 1 year", "0")][
        ,("age_range") := str_replace(age_range,"year"," year")][
          ,("age") := as.integer(substr(age_range,1,3))]

#reshape a bit and make list of individuals
Geoids <- colnames(bgSARE_data[,8:(ncol(bgSARE_data)-4)])
bgSARE_melted <- melt(bgSARE_data, id.vars = c("re_code","race","sex","age_range","age"), measure.vars = Geoids)
bgSARE <- as.data.table(lapply(bgSARE_melted[,.SD],rep,bgSARE_melted[,value]))

#get two or more races as duplicated relations
bgSARE2_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"SEX BY AGE FOR SELECTED AGE CATEGORIES \\(","")][
    ,("race") := str_replace(race,"\\)","")][
      ,("age_range") := str_replace(age_range, "Under 1 year", "0")][
        ,("age_range") := str_replace(age_range,"year"," year")][
          ,("age") := as.integer(substr(age_range,1,3))]

#reshape a bit and make list of individuals
bgSARE2_melted <- melt(bgSARE2_data, id.vars = c("re_code","race","sex","age_range","age"), measure.vars = Geoids)
bgSARE2 <- as.data.table(lapply(bgSARE_melted[,.SD],rep,bgSARE_melted[,value]))



rm(bgSARE_dec_data_from_census)
rm(bgSARE_data)
rm(bgSARE_melted)

groupname <- "P17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP
groupname <- "PCT17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP WITH RACE/ETH


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
