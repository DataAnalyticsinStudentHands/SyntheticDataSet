source('Sam/get_tools.R')
library(stringr)
library(data.table)
maindir = "~/University\ Of\ Houston/Engaged\ Data\ Science\ -\ Data/" #Dan Studio
#maindir = "~/Documents/Sam_data/" #if need local
censusdir = paste0(maindir,"Census/") 
vintage = "2020"
state = 48 #48 Texas; 22 Louisiana
county = "*" 
tract = "*"
#you don't need a censuskey if you're not pulling new files down; you can only use this one if you have correct access to mine on the OneDrive
censuskey <- readLines(paste0(censusdir, "2017", "/key"))

#moved from schematic_sam.Rmd - moved to data.table only 
groupname <- "P12" #SEX BY AGE FOR SELECTED AGE CATEGORIES (race/ethnicity)
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
trSAR_dec_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county,
                   api_type,path_suff)
if(names(trSAR_dec_data_from_census)[6]=="label_1"){
  #labels determined by hand
  label_c1 <- c("sex","age_range")
  #row_c1 determined by hand
  row_c1 <- c(unique(trSAR_dec_data_from_census[!is.na(label_2)&
                                                  str_detect(concept,"NOT HISPANIC OR LATINO")&
                                                  !str_detect(concept,"IN COMBINATION") | !is.na(label_2)&
                                                  str_detect(concept,"\\(HISPANIC OR LATINO"),name]))
  test_total_pop <- tests_download_data(trSAR_dec_data_from_census,label_c1,row_c1)
  trSAR_data <- relabel(trSAR_dec_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bgGQ,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  trSAR_data <- trSAR_dec_data_from_census
}
rm(trSAR_dec_data_from_census)

trSAR_data[,("beg_age_gq"):=fcase(age_range=="Under 18 years", as.numeric(0),
                                 age_range=="18 to 64 years", as.numeric(18),
                                 age_range=="65 years and over", as.numeric(65))]
trSAR_data[,("gq_type_6"):=fcase(str_detect(gq_type,"College"), #catching some idiosyncratic capitalization
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
Geoids <- colnames(trSAR_data[,10:(ncol(trSAR_data)-4)])
trSAR_melted <- melt(trSAR_data, id.vars = c("sex","age_range","gq_institution","gq_type","gq_type_6"), measure.vars = Geoids)
trSAR <- as.data.table(lapply(trSAR_melted[,.SD],rep,trSAR_melted[,value]))
rm(trSAR_data)
rm(trSAR_melted)


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
  test_result <- tests_download_data(bg_gq_age_data_from_census,label_c1,row_c1)
  bgGQ_data <- relabel(bg_gq_age_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bgGQ,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bgGQ_data <- bg_gq_age_data_from_census
}
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
Geoids <- colnames(bgGQ_data[,10:(ncol(bgGQ_data)-4)])
bgGQ_melted <- melt(bgGQ_data, id.vars = c("sex","age_range","gq_institution","gq_type","gq_type_6"), measure.vars = Geoids)
bgGQ <- as.data.table(lapply(bgGQ_melted[,.SD],rep,bgGQ_melted[,value]))
rm(bgGQ_data)
rm(bgGQ_melted)
