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

#https://api.census.gov/data/2020/dec/dhc/variables.html

#schematic_sam_dec
groupname <- "P16" #HOUSEHOLDER TYPE/RACE/ETH
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhType_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhType_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("family","family_type","no_spouse_sex")
  #row_c1 determined by hand 
  bg_hhType_data_from_census_a <- bg_hhType_data_from_census[!str_detect(label_2,"Other") | !is.na(label_3)]
  row_c1 <- c(unique(bg_hhType_data_from_census_a[str_detect(concept,", HISP") & !is.na(label_2) | 
                                                    str_detect(concept,"NOT HISP") & !is.na(label_2),name]))
  test_total_pop <- tests_download_data(bg_hhType_data_from_census,label_c1,row_c1,state=state)
  bg_hhType_data <- relabel(bg_hhType_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhType_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhType_data <- bg_hhType_data_from_census
}
#this has a different total than the ACS 1-year, which is what is reported as official? 10491147 vs. 11260645 #CANNOT FIND WHY THEY PREFERRED THE ACS!!!
#cf., https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_general_handbook_2020_ch09.pdf and numerous user complaints online.
#but no reason that population for 2020 is reported from decennial and households are from ACS (but are higher, even though residency is stricter and not point of time)
#if(!test_total_pop){test_total_pop<-sum(bg_hhFam_data[,total],na.rm = TRUE)}

bg_hhType_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"HOUSEHOLD TYPE \\(","")][
    ,("race") := str_replace(race,"\\)","")]

#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhType_data[,.SD,.SDcols = startsWith(names(bg_hhType_data),state)])
bg_hhType_melted <- melt(bg_hhType_data, id.vars = c("re_code","race","family","family_type","no_spouse_sex"), measure.vars = Geoids,
                         value.name = "codom_hhType", variable.name = "GEOID")
bg_hhType <- as.data.table(lapply(bg_hhType_melted[,.SD],rep,bg_hhType_melted[,codom_hhType]))
#This has race and ethnicity mixed together, so get the right ones
#bg_hhType <- bg_hhType[str_detect(race,", NOT") | str_detect(race,", HISP")]

rm(bg_hhType_data_from_census)
rm(bg_hhType_data)
rm(bg_hhType_melted)

groupname <- "H13" #HOUSEHOLDER AGE / TENURE / RACE / ETHx2
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhAge_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
#tract level with same groupname does not have more categories
if(names(bg_hhAge_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("rent_own","age_range_9")
  #row_c1 determined by hand 
  row_c1 <- c(unique(bg_hhAge_data_from_census[!is.na(label_2) & concept!="TENURE BY AGE OF HOUSEHOLDER",name])) #test with:  & !str_detect(concept,"HISPANIC")
  #test_total_pop <- tests_download_data(bg_hhAge_data_from_census,label_c1,row_c1,state=state)
  #do this with HvL, to divide later, since don't have whole population by both
  bg_hhAge_data <- relabel(bg_hhAge_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhAge_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhAge_data <- bg_hhAge_data_from_census
}
bg_hhAge_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"TENURE BY AGE OF HOUSEHOLDER \\(","")][
    ,("race") := str_replace(race,"\\)","")]

#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhAge_data[,.SD,.SDcols = startsWith(names(bg_hhAge_data),state)])
bg_hhAge_melted <- melt(bg_hhAge_data, id.vars = c("re_code","race","rent_own","age_range_9"), measure.vars = Geoids,
                        value.name = "codom_hhAge", variable.name = "GEOID")
bg_hhAge <- as.data.table(lapply(bg_hhAge_melted[,.SD],rep,bg_hhAge_melted[,codom_hhAge]))
bg_hhAgeR <- bg_hhAge[!re_code %in% c("H","I")]
bg_hhAgeE <- bg_hhAge[re_code %in% c("H","I")]
#put ethnicity on all

#tract level for PCT13 - more ages
groupname <- "PCT13" # SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS (Race/eth x 2) #total pop - group quarters
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhAge_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhAge_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("sex","age_range_23")
  #row_c1 determined by hand 
  row_c1 <- c(unique(tr_hhAge_data_from_census[!is.na(label_2) & concept!="SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS",name]))
  #test_total_pop <- tests_download_data(bg_hhAge_data_from_census,label_c1,row_c1,state=state)
  #do this with HvL, to divide later, since don't have whole population by both
  tr_hhAge_data <- relabel(tr_hhAge_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhAge_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhAge_data <- tr_hhAge_data_from_census
}
tr_hhAge_data[,("re_code") := substr(name,6,6)][
  ,("race") := str_replace(concept,"SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS \\(","")][
    ,("race") := str_replace(race,"\\)","")]

#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhAge_data[,.SD,.SDcols = startsWith(names(tr_hhAge_data),state)])
tr_hhAge_melted <- melt(tr_hhAge_data, id.vars = c("re_code","race","sex","age_range_23"), measure.vars = Geoids,
                        value.name = "codom_hhAge", variable.name = "GEOID")
tr_hhAge <- as.data.table(lapply(tr_hhAge_melted[,.SD],rep,tr_hhAge_melted[,codom_hhAge]))
tr_hhAgeR <- tr_hhAge[!re_code %in% c("H","I")]
tr_hhAgeE <- tr_hhAge[re_code %in% c("H","I")]


groupname <- "H14" #HOUSEHOLDER TYPE / TENURE
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhTenure_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhTenure_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("rent_own","family","family_type","no_spouse_sex","age_range_3") #follow above, but will have to divide
  #row_c1 determined by hand 
  row_c1 <- c(unique(bg_hhTenure_data_from_census[str_detect(label_5,"years") | str_detect(label_4,"years"),name]))
  test_total_pop <- tests_download_data(bg_hhTenure_data_from_census,label_c1,row_c1,state=state)
  bg_hhTenure_data <- relabel(bg_hhTenure_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhTenure_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhTenure_data <- bg_hhTenure_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhTenure_data[,.SD,.SDcols = startsWith(names(bg_hhTenure_data),state)])
bg_hhTenure_melted <- melt(bg_hhTenure_data, id.vars = c("rent_own","family","family_type","no_spouse_sex","age_range_3"), measure.vars = Geoids,
                        value.name = "codom_hhTenure", variable.name = "GEOID")
bg_hhTenure <- as.data.table(lapply(bg_hhTenure_melted[,.SD],rep,bg_hhTenure_melted[,codom_hhTenure]))

groupname <- "P17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhRel_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhRel_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household","role","sex","alone","age_range_2") 
  #arrange things to get the totals right
  bg_hhRel_data_from_census[str_detect(label_2,"child")&is.na(label_3),("label_5"):="over_17"]
  bg_hhRel_data_from_census[label_3=="Under 18 years",("label_5"):="under_18"] #foster child doesn't have under_18
  #row_c1 by hand
  row_c1 <- c(unique(bg_hhRel_data_from_census[!is.na(label_2) & is.na(label_3) & name!="P17_003N" | !is.na(label_4) | !is.na(label_5),name]))
  test_total_pop <- tests_download_data(bg_hhRel_data_from_census,label_c1,row_c1,state=state)
  bg_hhRel_data <- relabel(bg_hhRel_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhRel_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhRel_data <- bg_hhRel_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhRel_data[,.SD,.SDcols = startsWith(names(bg_hhRel_data),state)])
bg_hhRel_melted <- melt(bg_hhRel_data, id.vars = c("household","role","sex","alone","age_range_2"), measure.vars = Geoids,
                           value.name = "codom_hhRel", variable.name = "GEOID")
#clean up to get right number
bg_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Stepchild" & age_range_2=="over_17",
                  .SD[role=="Stepchild" & age_range_2=="over_17",codom_hhRel]-
                    .SD[role=="Stepchild" & age_range_2=="under_18",codom_hhRel],codom_hhRel),by=GEOID]
bg_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Biological child" & age_range_2=="over_17",
                                         .SD[role=="Biological child" & age_range_2=="over_17",codom_hhRel]-
                                           .SD[role=="Biological child" & age_range_2=="under_18",codom_hhRel],codom_hhRel),by=GEOID]
bg_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Adopted child" & age_range_2=="over_17",
                                         .SD[role=="Adopted child" & age_range_2=="over_17",codom_hhRel]-
                                           .SD[role=="Adopted child" & age_range_2=="under_18",codom_hhRel],codom_hhRel),by=GEOID]
bg_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Grandchild" & age_range_2=="over_17",
                                         .SD[role=="Grandchild" & age_range_2=="over_17",codom_hhRel]-
                                           .SD[role=="Grandchild" & age_range_2=="under_18",codom_hhRel],codom_hhRel),by=GEOID]
bg_hhRel_melted[,("age_range_2"):=ifelse(role=="Foster child","under_18",age_range_2)] #since it doesn't say...

bg_hhRel <- as.data.table(lapply(bg_hhRel_melted[,.SD],rep,bg_hhRel_melted[,codom_hhRel]))
sum(test_total_pop[,.SD,.SDcols = Geoids])==nrow(bg_hhRel)
rm(bg_hhRel)
rm(bg_hhRel_data)
rm(bg_hhRel_data_from_census)
rm(bg_hhRel_melted)

groupname <- "PCT17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP WITH RACE/ETHx2 
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhRel_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhRel_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household","role","sex","alone","age_range_2","re_code","race") 
  #arrange things to get the totals right
  tr_hhRel_data_from_census[str_detect(label_2,"child")&is.na(label_3),("label_5"):="over_17"]
  tr_hhRel_data_from_census[label_3=="Under 18 years",("label_5"):="under_18"] #foster child doesn't have under_18
  tr_hhRel_data_from_census[,("re_code") := substr(name,6,6)][
    ,("race") := str_replace(concept,"HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  #row_c1 by hand
  row_c1 <- c(unique(tr_hhRel_data_from_census[!is.na(label_2) & is.na(label_3) & !str_detect(name,"003N") | !is.na(label_4) | !is.na(label_5),name]))
  test_total_pop <- tests_download_data(tr_hhRel_data_from_census,label_c1,row_c1,state=state)
  tr_hhRel_data <- relabel(tr_hhRel_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhRel_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhRel_data <- tr_hhRel_data_from_census
}

#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhRel_data[,.SD,.SDcols = startsWith(names(tr_hhRel_data),state)])
tr_hhRel_melted <- melt(tr_hhRel_data, id.vars = c("household","role","sex","alone","age_range_2","re_code","race"), measure.vars = Geoids,
                        value.name = "codom_hhRel", variable.name = "GEOID")
#clean up to get right number #by GEOID and RACE??? WHY NOT WORKING????
tr_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Stepchild" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Stepchild" & age_range_2=="over_17",codom_hhRel])-
                                           as.numeric(.SD[role=="Stepchild" & age_range_2=="under_18",codom_hhRel]),codom_hhRel),by=.(GEOID,re_code)]
tr_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Biological child" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Biological child" & age_range_2=="over_17",codom_hhRel])-
                                           as.numeric(.SD[role=="Biological child" & age_range_2=="under_18",codom_hhRel]),codom_hhRel),by=.(GEOID,re_code)]
tr_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Adopted child" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Adopted child" & age_range_2=="over_17",codom_hhRel])-
                                           as.numeric(.SD[role=="Adopted child" & age_range_2=="under_18",codom_hhRel]),codom_hhRel),by=.(GEOID,re_code)]
tr_hhRel_melted[,("codom_hhRel"):=ifelse(role=="Grandchild" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Grandchild" & age_range_2=="over_17",codom_hhRel])-
                                           as.numeric(.SD[role=="Grandchild" & age_range_2=="under_18",codom_hhRel]),codom_hhRel),by=.(GEOID,re_code)]
tr_hhRel_melted[,("age_range_2"):=ifelse(role=="Foster child","under_18",age_range_2)] #since it doesn't say...

tr_hhRel <- as.data.table(lapply(tr_hhRel_melted[,.SD],rep,tr_hhRel_melted[,codom_hhRel]))
tr_hhRelR <- tr_hhRel[!re_code %in% c("H","I")]
tr_hhRelE <- tr_hhRel[re_code %in% c("H","I")]
sum(as.numeric(test_total_pop[,.SD,.SDcols = Geoids]),na.rm = TRUE)#==nrow(tr_hhRelR) - seems like it's all individuals.
rm(tr_hhRel)
rm(tr_hhRel_data)
rm(tr_hhRel_data_from_census)
rm(tr_hhRel_melted)

groupname <- "PCT8" # RELATIONSHIP BY AGE FOR THE POPULATION UNDER 18 YEARS
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhRel18_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "P20" #OWN CHILDREN
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhOwnKids_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)


groupname <- "P19" #HOUSEHOLDS BY PRESENCE OF PEOPLE 65 YEARS AND OVER, HOUSEHOLD SIZE, AND HOUSEHOLD TYPE
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hh65SizeType_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)

groupname <- "PCT4" #HOUSEHOLDS BY PRESENCE OF PEOPLE 60 YEARS AND OVER BY HOUSEHOLD TYPE
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh60Type_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT5" #HOUSEHOLDS BY PRESENCE OF PEOPLE 60 YEARS AND OVER, HOUSEHOLD SIZE, AND HOUSEHOLD TYPE
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh60SizeType_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT6" #HOUSEHOLDS BY PRESENCE OF PEOPLE 75 YEARS AND OVER, HOUSEHOLD SIZE, AND HOUSEHOLD TYPE
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh75SizeType_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT9" #HOUSEHOLD TYPE BY RELATIONSHIP FOR THE POPULATION 65 YEARS AND OVER, by race/eth, includes GQ
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh65RelRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT10" #FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN, race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTypeOwnKids_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT14" #PRESENCE OF MULTIGENERATIONAL HOUSEHOLDS, race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhMultiGenRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT15" #COUPLED HOUSEHOLDS, BY TYPE, including same sex
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhCouple_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT16" #NONFAMILY HOUSEHOLDS BY SEX OF HOUSEHOLDER BY LIVING ALONE BY AGE OF HOUSEHOLDER
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_nfCouple_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "H4" #Tenure - by race and mortgage
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_TenureRE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)

groupname <- "H12" #TENURE BY HOUSEHOLD SIZE and race/eth
api_type <- "dec/dhc"
geo_type <- "block_group"
path_suff <- "est"
bg_hhSizeTenureRE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)

groupname <- "PCT7" #HOUSEHOLD TYPE BY HOUSEHOLD SIZE, race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTypeSizeRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT2" #HOUSEHOLD SIZE BY HOUSEHOLD TYPE BY PRESENCE OF OWN CHILDREN
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhSizeTypeOwnKids_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "PCT8" #RELATIONSHIP BY AGE FOR THE POPULATION UNDER 18 YEARS
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhRelKids_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "H13" #TENURE BY AGE OF HOUSEHOLDER and race/eth
api_type <- "dec/dhc"
geo_type <- "block_group"
path_suff <- "est"
bg_hhAgeTenureRE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)

groupname <- "H14" #TENURE BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER
api_type <- "dec/dhc"
geo_type <- "block_group"
path_suff <- "est"
bg_hhTypeTenureAge_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)

groupname <- "H15" #TENURE BY PRESENCE OF PEOPLE UNDER 18 YEARS (EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS)
api_type <- "dec/dhc"
geo_type <- "block_group"
path_suff <- "est"
bg_hh18Tenure_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)

groupname <- "HCT12" #TENURE BY PRESENCE AND AGE OF OWN CHILDREN (more categories)
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTenureOwnKids_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

groupname <- "HCT1" #TENURE BY HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER BY RACE OF HOUSEHOLDER - gives everything at tract keeps from guessing about race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTenureRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)

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
  test_total_pop <- tests_download_data(bg_gq_age_data_from_census,label_c1,row_c1,state=state)
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
