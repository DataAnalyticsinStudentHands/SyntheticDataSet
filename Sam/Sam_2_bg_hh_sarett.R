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
bg_hhTypeRE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhTypeRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("family","family_type","no_spouse_sex","re_code","race")
  #row_c1 determined by hand 
  bg_hhTypeRE_data_from_census[,("re_code") := substr(name,4,4)][
    ,("race") := str_replace(concept,"HOUSEHOLD TYPE \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  bg_hhTypeRE_data_from_census_a <- bg_hhTypeRE_data_from_census[!str_detect(label_2,"Other") | !is.na(label_3)] #b/c other family we want has label_3
  row_c1 <- c(unique(bg_hhTypeRE_data_from_census_a[str_detect(concept,", HISP") & !is.na(label_2) | 
                                                    str_detect(concept,"NOT HISP") & !is.na(label_2),name]))
  rm(bg_hhTypeRE_data_from_census_a) #since it was just a workaround for the too complicated row_c1 calculation
  test_total_pop <- tests_download_data(bg_hhTypeRE_data_from_census,label_c1,row_c1,state=state)
  bg_hhTypeRE_data <- relabel(bg_hhTypeRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhTypeRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhTypeRE_data <- bg_hhTypeRE_data_from_census
}
#this has a different total than the ACS 1-year, which is what is reported as official? 10491147 vs. 11260645 #CANNOT FIND WHY THEY PREFERRED THE ACS!!! Just give a warning about "modeled data"
#cf., https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_general_handbook_2020_ch09.pdf and numerous user complaints online.
#but no reason that population for 2020 is reported from decennial and households are from ACS (but are higher, even though residency is stricter and not point of time)
#if(!test_total_pop){test_total_pop<-sum(bg_hhFam_data[,total],na.rm = TRUE)}

#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhTypeRE_data[,.SD,.SDcols = startsWith(names(bg_hhTypeRE_data),state)])
bg_hhTypeRE_melted <- melt(bg_hhTypeRE_data, id.vars = c("re_code","race","family","family_type","no_spouse_sex"), measure.vars = Geoids,
                         value.name = "codom_hhTypeRE", variable.name = "GEOID")
bg_hhTypeRE <- as.data.table(lapply(bg_hhTypeRE_melted[,.SD],rep,bg_hhTypeRE_melted[,codom_hhTypeRE]))
rm(bg_hhTypeRE_data_from_census)
rm(bg_hhTypeRE_data)
rm(bg_hhTypeRE_melted)

#merging PCT9 and PCT17 then joining with P16

groupname <- "PCT9" #HOUSEHOLD TYPE BY RELATIONSHIP FOR THE POPULATION 65 YEARS AND OVER, by race/eth, includes GQ
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh65RelRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hh65RelRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household","role","alone","sex","re_code","race") 
  tr_hh65RelRE_data_from_census[,("re_code") := substr(name,5,5)][
    ,("race") := str_replace(concept,"HOUSEHOLD TYPE BY RELATIONSHIP FOR THE POPULATION 65 YEARS AND OVER \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  #row_c1 by hand
  row_c1 <- c(unique(tr_hh65RelRE_data_from_census[!is.na(label_4) & str_detect(concept,"\\)") |
                                                     label_2!="Householder" & str_detect(concept,"\\)"),name]))
  test_total_pop <- tests_download_data(tr_hh65RelRE_data_from_census,label_c1,row_c1,state=state)
  tr_hh65RelRE_data <- relabel(tr_hh65RelRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hh65RelRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hh65RelRE_data <- tr_hh65RelRE_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hh65RelRE_data[,.SD,.SDcols = startsWith(names(tr_hh65RelRE_data),state)])
tr_hh65RelRE_melted <- melt(tr_hh65RelRE_data, id.vars = c("household","role","alone","sex","re_code","race"), measure.vars = Geoids,
                            value.name = "codom_tr_hh65RelRE", variable.name = "GEOID")
tr_hh65RelRE <- as.data.table(lapply(tr_hh65RelRE_melted[,.SD],rep,tr_hh65RelRE_melted[,codom_tr_hh65RelRE]))
#tr_hh65RelR <- tr_hh65RelRE[!re_code %in% c("H","I")]
#tr_hh65RelE <- tr_hh65RelRE[re_code %in% c("H","I")]
#seems to be 44k off on the totals (state-wide) from test_total_pop???? Not at all obvious what is missing...
rm(tr_hh65RelRE_data_from_census)
rm(tr_hh65RelRE_data)
rm(tr_hh65RelRE_melted)
#rm(tr_hh65RelRE)

groupname <- "PCT17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP WITH RACE/ETHx2 
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
#should be tr_hhRelRE
tr_hhRelRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhRelRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household","role","sex","alone","age_range_2","re_code","race") 
  #arrange things to get the totals right
  tr_hhRelRE_data_from_census[str_detect(label_2,"child")&is.na(label_3),("label_5"):="over_17"]
  tr_hhRelRE_data_from_census[label_3=="Under 18 years",("label_5"):="under_18"] #foster child doesn't have under_18
  tr_hhRelRE_data_from_census[,("re_code") := substr(name,6,6)][
    ,("race") := str_replace(concept,"HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  #row_c1 by hand
  row_c1 <- c(unique(tr_hhRelRE_data_from_census[!is.na(label_2) & is.na(label_3) & !str_detect(name,"003N") | !is.na(label_4) | !is.na(label_5),name]))
  test_total_pop <- tests_download_data(tr_hhRelRE_data_from_census,label_c1,row_c1,state=state)
  tr_hhRelRE_data <- relabel(tr_hhRelRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhRelRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhRelRE_data <- tr_hhRelRE_data_from_census
}

#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhRelRE_data[,.SD,.SDcols = startsWith(names(tr_hhRelRE_data),state)])
tr_hhRelRE_melted <- melt(tr_hhRelRE_data, id.vars = c("household","role","sex","alone","age_range_2","re_code","race"), measure.vars = Geoids,
                        value.name = "codom_hhRelRE", variable.name = "GEOID")
#clean up to get right number #by GEOID and RACE
tr_hhRelRE_melted[,("codom_hhRelRE"):=ifelse(role=="Stepchild" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Stepchild" & age_range_2=="over_17",codom_hhRelRE])-
                                           as.numeric(.SD[role=="Stepchild" & age_range_2=="under_18",codom_hhRelRE]),codom_hhRelRE),by=.(GEOID,re_code)]
tr_hhRelRE_melted[,("codom_hhRelRE"):=ifelse(role=="Biological child" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Biological child" & age_range_2=="over_17",codom_hhRelRE])-
                                           as.numeric(.SD[role=="Biological child" & age_range_2=="under_18",codom_hhRelRE]),codom_hhRelRE),by=.(GEOID,re_code)]
tr_hhRelRE_melted[,("codom_hhRelRE"):=ifelse(role=="Adopted child" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Adopted child" & age_range_2=="over_17",codom_hhRelRE])-
                                           as.numeric(.SD[role=="Adopted child" & age_range_2=="under_18",codom_hhRelRE]),codom_hhRelRE),by=.(GEOID,re_code)]
tr_hhRelRE_melted[,("codom_hhRelRE"):=ifelse(role=="Grandchild" & age_range_2=="over_17",
                                         as.numeric(.SD[role=="Grandchild" & age_range_2=="over_17",codom_hhRelRE])-
                                           as.numeric(.SD[role=="Grandchild" & age_range_2=="under_18",codom_hhRelRE]),codom_hhRelRE),by=.(GEOID,re_code)]
#some should be made impossible to match for 65 and over, although one can imagine some edge cases; same-sex is not weighted by age since we don't know
tr_hhRelRE_melted[,("role_7"):=fcase(str_detect(role,"Par"),"Parent or parent-in-law",
                                   str_detect(role,"-sex"),"Spouse or partner",
                                   str_detect(role,"Other rel") | str_detect(role,"or sister"),"Other relatives",
                                   default = role)]


tr_hhRelRE <- as.data.table(lapply(tr_hhRelRE_melted[,.SD],rep,tr_hhRelRE_melted[,codom_hhRelRE]))
#tr_hhRelR <- tr_hhRel[!re_code %in% c("H","I")]
#tr_hhRelE <- tr_hhRel[re_code %in% c("H","I")]
#sum(as.numeric(test_total_pop[,.SD,.SDcols = Geoids]),na.rm = TRUE)#==nrow(tr_hhRelR) - seems like it's all individuals.
#rm(tr_hhRel)
rm(tr_hhRelRE_data)
rm(tr_hhRelRE_data_from_census)
rm(tr_hhRelRE_melted)

#match then move up to P16 - need to think through codomain and copath logic for matching
tr_hhRelRE[,("re65_match_id"):=
      paste0(GEOID,re_code,household,role_7,sex,alone,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code,household,role_7,sex,alone)]
tr_hh65RelRE[,("re65_match_id"):=
      paste0(GEOID,re_code,household,role,sex,alone,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code,household,role,sex,alone)]
tr_hhRelRE[,c("over_64","codom_tr_hh65RelRE"):=
            tr_hh65RelRE[.SD,c(list(re_code),list(codom_tr_hh65RelRE)),on=.(re65_match_id)]]
#table(tr_hhRelRE[,over_64])==table(tr_hh65RelRE[,re_code])
rm(tr_hh65RelRE)


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
#sum(test_total_pop[,.SD,.SDcols = Geoids])==nrow(bg_hhRel)
rm(bg_hhRel_data)
rm(bg_hhRel_data_from_census)
rm(bg_hhRel_melted)
#test<-table(tr_hhRelRE[re_code%in%c(LETTERS[1:7]),household],tr_hhRelRE[re_code%in%c(LETTERS[1:7]),role],
#      tr_hhRelRE[re_code%in%c(LETTERS[1:7]),alone],tr_hhRelRE[re_code%in%c(LETTERS[1:7]),sex],tr_hhRelRE[re_code%in%c(LETTERS[1:7]),age_range_2])==
#  table(bg_hhRel[,household],bg_hhRel[,role],bg_hhRel[,alone],bg_hhRel[,sex],bg_hhRel[,age_range_2])
#length(test[test==FALSE])

#move the ethnicity over with the codom info for sorting
tr_hhRelR <- tr_hhRelRE[re_code%in%c(LETTERS[1:7])]
tr_hhRelI <- tr_hhRelRE[re_code=="I"]
tr_hhRelR[,("bg_rel_match_id"):=
            paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,sex,alone,age_range_2)]
tr_hhRelI[,("bg_rel_match_id"):=
           paste0(substr(GEOID,1,13),household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(substr(GEOID,1,13),household,role,sex,alone,age_range_2)]
###UGH!!! - how will that add at end????
tr_hhRelR[,("codom_hhRelI"):=
           tr_hhRelI[.SD,list(codom_hhRelRE),on=.(bg_relI_match_id)]]

#match then move up to P16 - need to think through codomain and copath logic for matching
tr_hhRelR[,("bg_rel_match_id"):=
             paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
           by=.(GEOID,household,role,sex,alone,age_range_2)]
bg_hhRel[,("bg_rel_match_id"):=
               paste0(substr(GEOID,1,13),household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
             by=.(substr(GEOID,1,13),household,role,sex,alone,age_range_2)]
bg_hhRel[,c("tract","codom_hhRelRE"):=
           tr_hhRelR[.SD,c(list(GEOID),list(codom_hhRelRE)),on=.(bg_rel_match_id)]]
#test <- table(bg_hhRel[,tract])==table(tr_hhRelRE[,GEOID])
#length(test[test==FALSE])
rm(tr_hhRelRE)

#nrow(bg_hhTypeRE)-nrow(tr_hhRelRE[re_code%in%c(LETTERS[1:7])&role_7=="Householder"]) 117 different??? 


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







groupname <- "PCT8" # RELATIONSHIP BY AGE FOR THE POPULATION UNDER 18 YEARS; really only own-child and group quarter information
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhRel18_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhRel18_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household","child_role","age_range_adolescent") 
  #row_c1 by hand
  row_c1 <- c(unique(tr_hhRel18_data_from_census[!is.na(label_3),name]))
  test_total_pop <- tests_download_data(tr_hhRel18_data_from_census,label_c1,row_c1,state=state)
  #this is 6k off for entire state - need to ensure we understand why different
  tr_hhRel18_data <- relabel(tr_hhRel18_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhRel18_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhRel18_data <- tr_hhRel18_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhRel18_data[,.SD,.SDcols = startsWith(names(tr_hhRel18_data),state)])
tr_hhRel18_melted <- melt(tr_hhRel18_data, id.vars = c("household","child_role","age_range_adolescent"), measure.vars = Geoids,
                           value.name = "codom_tr_hhRel18", variable.name = "GEOID")
tr_hhRel18 <- as.data.table(lapply(tr_hhRel18_melted[,.SD],rep,tr_hhRel18_melted[,codom_tr_hhRel18]))
rm(tr_hhRel18_data_from_census)
rm(tr_hhRel18_melted)
rm(tr_hhRel18_data)

groupname <- "P20" #OWN CHILDREN
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhOwnKids_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhOwnKids_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household_type_6","in_house","age_range_2_sr") 
  #row_c1 by hand
  row_c1 <- c(unique(bg_hhOwnKids_data_from_census[!is.na(label_2),name]))
  test_total_pop <- tests_download_data(bg_hhOwnKids_data_from_census,label_c1,row_c1,state=state)
  #this is 6k off for entire state - need to ensure we understand why different
  bg_hhOwnKids_data <- relabel(bg_hhOwnKids_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhOwnKids_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhOwnKids_data <- bg_hhOwnKids_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhOwnKids_data[,.SD,.SDcols = startsWith(names(bg_hhOwnKids_data),state)])
bg_hhOwnKids_melted <- melt(bg_hhOwnKids_data, id.vars = c("household_type_6","in_house","age_range_2_sr"), measure.vars = Geoids,
                          value.name = "codom_bg_hhOwnKids", variable.name = "GEOID")
bg_hhOwnKids_melted[,("codom_bg_hhOwnKids"):=ifelse(household_type_6=="Male householder, no spouse or partner present" &
                                                      in_house=="Living alone" & is.na(age_range_2_sr),
                                         as.numeric(.SD[household_type_6=="Male householder, no spouse or partner present" &
                                                          in_house=="Living alone" & is.na(age_range_2_sr),codom_bg_hhOwnKids])-
                                           as.numeric(.SD[household_type_6=="Male householder, no spouse or partner present" & 
                                                            in_house=="Living alone" & 
                                                            age_range_2_sr=="65 years and over",codom_bg_hhOwnKids]),codom_bg_hhOwnKids),by=.(GEOID)]
bg_hhOwnKids_melted[,("codom_bg_hhOwnKids"):=ifelse(household_type_6=="Female householder, no spouse or partner present" &
                                                      in_house=="Living alone" & is.na(age_range_2_sr),
                                                    as.numeric(.SD[household_type_6=="Female householder, no spouse or partner present" &
                                                                     in_house=="Living alone" & is.na(age_range_2_sr),codom_bg_hhOwnKids])-
                                                      as.numeric(.SD[household_type_6=="Female householder, no spouse or partner present" & 
                                                                       in_house=="Living alone" & 
                                                                       age_range_2_sr=="65 years and over",codom_bg_hhOwnKids]),codom_bg_hhOwnKids),by=.(GEOID)]
bg_hhOwnKids <- as.data.table(lapply(bg_hhOwnKids_melted[,.SD],rep,bg_hhOwnKids_melted[,codom_bg_hhOwnKids]))
rm(bg_hhOwnKids_data_from_census)
rm(bg_hhOwnKids_melted)
rm(bg_hhOwnKids_data)


groupname <- "P19" #HOUSEHOLDS BY PRESENCE OF PEOPLE 65 YEARS AND OVER, HOUSEHOLD SIZE, AND HOUSEHOLD TYPE
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hh65SizeType_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hh65SizeType_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household_65","hh_size_2","household") 
  #row_c1 by hand
  row_c1 <- c(unique(bg_hh65SizeType_data_from_census[!is.na(label_3) | label_2=="1-person household",name]))
  test_total_pop <- tests_download_data(bg_hh65SizeType_data_from_census,label_c1,row_c1,state=state)
  #this is 6k off for entire state - need to ensure we understand why different
  bg_hh65SizeType_data <- relabel(bg_hh65SizeType_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hh65SizeType_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hh65SizeType_data <- bg_hh65SizeType_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_hh65SizeType_data[,.SD,.SDcols = startsWith(names(bg_hh65SizeType_data),state)])
bg_hh65SizeType_melted <- melt(bg_hh65SizeType_data, id.vars = c("household_65","hh_size_2","household"), measure.vars = Geoids,
                          value.name = "codom_bg_hh65SizeType", variable.name = "GEOID")
bg_hh65SizeType <- as.data.table(lapply(bg_hh65SizeType_melted[,.SD],rep,bg_hh65SizeType_melted[,codom_bg_hh65SizeType]))
rm(bg_hh65SizeType_data_from_census)
rm(bg_hh65SizeType_melted)
rm(bg_hh65SizeType_data)

groupname <- "PCT4" #HOUSEHOLDS BY PRESENCE OF PEOPLE 60 YEARS AND OVER BY HOUSEHOLD TYPE
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh60Type_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hh60Type_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household_60","family_type","household_type_5","spouse") 
  #row_c1 by hand
  row_c1 <- c(unique(tr_hh60Type_data_from_census[label_2=="Nonfamily households" | 
                                                    label_3=="Married couple family" |
                                                    !is.na(label_4),name]))
  test_total_pop <- tests_download_data(tr_hh60Type_data_from_census,label_c1,row_c1,state=state)
  #this is 2.5m off for entire state - need to ensure we understand why different; seems to be that there is no non-family without 60yo present, which is a close number
  tr_hh60Type_data <- relabel(tr_hh60Type_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hh60Type_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hh60Type_data <- tr_hh60Type_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hh60Type_data[,.SD,.SDcols = startsWith(names(tr_hh60Type_data),state)])
tr_hh60Type_melted <- melt(tr_hh60Type_data, id.vars = c("household_60","family_type","household_type_5","spouse"), measure.vars = Geoids,
                               value.name = "codom_tr_hh60Type", variable.name = "GEOID")
tr_hh60Type <- as.data.table(lapply(tr_hh60Type_melted[,.SD],rep,tr_hh60Type_melted[,codom_tr_hh60Type]))
rm(tr_hh60Type_data_from_census)
rm(tr_hh60Type_melted)
rm(tr_hh60Type_data)

groupname <- "PCT5" #HOUSEHOLDS BY PRESENCE OF PEOPLE 60 YEARS AND OVER, HOUSEHOLD SIZE, AND HOUSEHOLD TYPE
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh60SizeType_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hh60SizeType_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household_60","hh_size_2","household") 
  #row_c1 by hand
  row_c1 <- c(unique(tr_hh60SizeType_data_from_census[!is.na(label_3) | label_2=="1-person household",name]))
  test_total_pop <- tests_download_data(tr_hh60SizeType_data_from_census,label_c1,row_c1,state=state)
  tr_hh60SizeType_data <- relabel(tr_hh60SizeType_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hh60SizeType_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hh60SizeType_data <- tr_hh60SizeType_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hh60SizeType_data[,.SD,.SDcols = startsWith(names(tr_hh60SizeType_data),state)])
tr_hh60SizeType_melted <- melt(tr_hh60SizeType_data, id.vars = c("household_60","hh_size_2","household"), measure.vars = Geoids,
                               value.name = "codom_tr_hh60SizeType", variable.name = "GEOID")
tr_hh60SizeType <- as.data.table(lapply(tr_hh60SizeType_melted[,.SD],rep,tr_hh60SizeType_melted[,codom_tr_hh60SizeType]))
rm(tr_hh60SizeType_data_from_census)
rm(tr_hh60SizeType_melted)
rm(tr_hh60SizeType_data)

groupname <- "PCT6" #HOUSEHOLDS BY PRESENCE OF PEOPLE 75 YEARS AND OVER, HOUSEHOLD SIZE, AND HOUSEHOLD TYPE
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hh75SizeType_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hh75SizeType_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("household_75","hh_size_2","household") 
  #row_c1 by hand
  row_c1 <- c(unique(tr_hh75SizeType_data_from_census[!is.na(label_3) | label_2=="1-person household",name]))
  test_total_pop <- tests_download_data(tr_hh75SizeType_data_from_census,label_c1,row_c1,state=state)
  tr_hh75SizeType_data <- relabel(tr_hh75SizeType_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hh75SizeType_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hh75SizeType_data <- tr_hh75SizeType_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hh75SizeType_data[,.SD,.SDcols = startsWith(names(tr_hh75SizeType_data),state)])
tr_hh75SizeType_melted <- melt(tr_hh75SizeType_data, id.vars = c("household_75","hh_size_2","household"), measure.vars = Geoids,
                               value.name = "codom_tr_hh75SizeType", variable.name = "GEOID")
tr_hh75SizeType <- as.data.table(lapply(tr_hh75SizeType_melted[,.SD],rep,tr_hh75SizeType_melted[,codom_tr_hh75SizeType]))
rm(tr_hh75SizeType_data_from_census)
rm(tr_hh75SizeType_melted)
rm(tr_hh75SizeType_data)

groupname <- "PCT10" #FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN, race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTypeOwnKidsRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhTypeOwnKidsRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("family_2","family_type","own_kids","kid_age_2","re_code","race") 
  tr_hhTypeOwnKidsRE_data_from_census[,("re_code") := substr(name,6,6)][
    ,("race") := str_replace(concept,"FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  tr_hhTypeOwnKidsRE_data_from_census[,("label_4"):=ifelse(str_detect(label_1,"Married couple family"),label_3,label_4)]
  tr_hhTypeOwnKidsRE_data_from_census[,("label_3"):=ifelse(str_detect(label_1,"Married couple family"),label_2,label_3)]
  #row_c1 by hand
  row_c1 <- c(unique(tr_hhTypeOwnKidsRE_data_from_census[!is.na(label_4) & str_detect(concept,"\\)") | 
                                                           str_detect(label_3,"No own") & str_detect(concept,"\\)"),name]))
  test_total_pop <- tests_download_data(tr_hhTypeOwnKidsRE_data_from_census,label_c1,row_c1,state=state)
  tr_hhTypeOwnKidsRE_data <- relabel(tr_hhTypeOwnKidsRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhTypeOwnKidsRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhTypeOwnKidsRE_data <- tr_hhTypeOwnKidsRE_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhTypeOwnKidsRE_data[,.SD,.SDcols = startsWith(names(tr_hhTypeOwnKidsRE_data),state)])
tr_hhTypeOwnKidsRE_melted <- melt(tr_hhTypeOwnKidsRE_data, id.vars = c("family_2","family_type","own_kids","kid_age_2","re_code","race"), measure.vars = Geoids,
                            value.name = "codom_tr_hhTypeOwnKidsRE", variable.name = "GEOID")
tr_hhTypeOwnKidsRE <- as.data.table(lapply(tr_hhTypeOwnKidsRE_melted[,.SD],rep,tr_hhTypeOwnKidsRE_melted[,codom_tr_hhTypeOwnKidsRE]))
tr_hhTypeOwnKidsR <- tr_hhTypeOwnKidsRE[!re_code %in% c("H","I")] #is right number...
tr_hhTypeOwnKidsE <- tr_hhTypeOwnKidsRE[re_code %in% c("H","I")]
rm(tr_hhTypeOwnKidsRE_data_from_census)
rm(tr_hhTypeOwnKidsRE_data)
rm(tr_hhTypeOwnKidsRE_melted)
rm(tr_hhTypeOwnKidsRE)

groupname <- "PCT14" #PRESENCE OF MULTIGENERATIONAL HOUSEHOLDS, race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhMultiGenRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhMultiGenRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("multi_gen_hh","multi_gen_3","re_code","race") 
  tr_hhMultiGenRE_data_from_census[,("re_code") := substr(name,6,6)][
    ,("race") := str_replace(concept,"PRESENCE OF MULTIGENERATIONAL HOUSEHOLDS \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  #row_c1 by hand
  row_c1 <- c(unique(tr_hhMultiGenRE_data_from_census[str_detect(label_1,"generations") & str_detect(concept,"\\)"),name]))
  test_total_pop <- tests_download_data(tr_hhMultiGenRE_data_from_census,label_c1,row_c1,state=state) #seems to not get right total row!!
  tr_hhMultiGenRE_data <- relabel(tr_hhMultiGenRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhMultiGenRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhMultiGenRE_data <- tr_hhMultiGenRE_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhMultiGenRE_data[,.SD,.SDcols = startsWith(names(tr_hhMultiGenRE_data),state)])
tr_hhMultiGenRE_melted <- melt(tr_hhMultiGenRE_data, id.vars = c("multi_gen_hh","multi_gen_3","re_code","race"), measure.vars = Geoids,
                                  value.name = "codom_tr_hhMultiGenRE", variable.name = "GEOID")
tr_hhMultiGenRE <- as.data.table(lapply(tr_hhMultiGenRE_melted[,.SD],rep,tr_hhMultiGenRE_melted[,codom_tr_hhMultiGenRE]))
tr_hhMultiGenR <- tr_hhMultiGenRE[!re_code %in% c("H","I")] #is right number...
tr_hhMultiGenE <- tr_hhMultiGenRE[re_code %in% c("H","I")]
rm(tr_hhMultiGenRE_data_from_census)
rm(tr_hhMultiGenRE_data)
rm(tr_hhMultiGenRE_melted)
rm(tr_hhMultiGenRE)

groupname <- "PCT15" #COUPLED HOUSEHOLDS, BY TYPE, including same sex
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhCouple_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhCouple_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("hh_type_3","same_sex","couple_gender") 
  tr_hhCouple_data_from_census[,("label_3"):=ifelse(label_2=="Same-sex unmarried partner households" & is.na(label_3),
                                                    "Male-male unmarried partner household",label_3)]
  
  #row_c1 by hand
  row_c1 <- c(unique(tr_hhCouple_data_from_census[label_1=="All other households" | 
                                                    str_detect(label_2,"Opposite") | 
                                                    !is.na(label_3),name]))
  test_total_pop <- tests_download_data(tr_hhCouple_data_from_census,label_c1,row_c1,state=state) #not right because male-male unmarried is off
  tr_hhCouple_data <- relabel(tr_hhCouple_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhCouple_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhCouple_data <- tr_hhCouple_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhCouple_data[,.SD,.SDcols = startsWith(names(tr_hhCouple_data),state)])
tr_hhCouple_melted <- melt(tr_hhCouple_data, id.vars = c("hh_type_3","same_sex","couple_gender"), measure.vars = Geoids,
                               value.name = "codom_tr_hhCouple", variable.name = "GEOID")
tr_hhCouple_melted[,("codom_tr_hhCouple"):=fcase(couple_gender=="Male-male unmarried partner household",
                                         as.numeric(.SD[couple_gender=="Male-male unmarried partner household",codom_tr_hhCouple])-
                                           as.numeric(.SD[couple_gender=="Female-female unmarried partner household",codom_tr_hhCouple]),
                                         default=as.numeric(codom_tr_hhCouple)),by=.(GEOID)]
tr_hhCouple <- as.data.table(lapply(tr_hhCouple_melted[,.SD],rep,tr_hhCouple_melted[,codom_tr_hhCouple])) #right number
rm(tr_hhCouple_data_from_census)
rm(tr_hhCouple_data)
rm(tr_hhCouple_melted)

groupname <- "PCT16" #NONFAMILY HOUSEHOLDS BY SEX OF HOUSEHOLDER BY LIVING ALONE BY AGE OF HOUSEHOLDER (add as subset to PCT15)
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_nfCouple_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_nfCouple_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("sex","alone","age_range_2") 
  #row_c1 by hand
  row_c1 <- c(unique(tr_nfCouple_data_from_census[!is.na(label_3),name]))
  test_total_pop <- tests_download_data(tr_nfCouple_data_from_census,label_c1,row_c1,state=state) #not right because male-male unmarried is off
  tr_nfCouple_data <- relabel(tr_nfCouple_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_nfCouple_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_nfCouple_data <- tr_nfCouple_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_nfCouple_data[,.SD,.SDcols = startsWith(names(tr_nfCouple_data),state)])
tr_nfCouple_melted <- melt(tr_nfCouple_data, id.vars = c("sex","alone","age_range_2"), measure.vars = Geoids,
                           value.name = "codom_tr_nfCouple", variable.name = "GEOID")
tr_nfCouple <- as.data.table(lapply(tr_nfCouple_melted[,.SD],rep,tr_nfCouple_melted[,codom_tr_nfCouple])) #right number
rm(tr_nfCouple_data_from_census)
rm(tr_nfCouple_data)
rm(tr_nfCouple_melted)

groupname <- "H4" #Tenure - by race and mortgage
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_TenureRE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_TenureRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("tenure_1","tenure","re_code","race") 
  bg_TenureRE_data_from_census[,("re_code") := substr(name,3,3)][
    ,("race") := str_replace(concept,"TENURE \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  #row_c1 by hand
  row_c1 <- c(unique(bg_TenureRE_data_from_census[label!="!!" & label!="Geography" & 
                                                    concept!="TENURE (HISPANIC OR LATINO HOUSEHOLDER)" & 
                                                    concept!="TENURE" & str_detect(concept,"HISP"),name]))
  test_total_pop <- tests_download_data(bg_TenureRE_data_from_census,label_c1,row_c1,state=state) #seems to not get right total row!!
  bg_TenureRE_data <- relabel(bg_TenureRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_TenureRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_TenureRE_data <- bg_TenureRE_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_TenureRE_data[,.SD,.SDcols = startsWith(names(bg_TenureRE_data),state)])
bg_TenureRE_melted <- melt(bg_TenureRE_data, id.vars = c("tenure_1","tenure","re_code","race"), measure.vars = Geoids,
                               value.name = "codom_bg_TenureRE", variable.name = "GEOID")
bg_TenureRE <- as.data.table(lapply(bg_TenureRE_melted[,.SD],rep,bg_TenureRE_melted[,codom_bg_TenureRE])) #all re_codes
rm(bg_TenureRE_data_from_census)
rm(bg_TenureRE_data)
rm(bg_TenureRE_melted)

groupname <- "H12" #TENURE BY HOUSEHOLD SIZE and race/eth
api_type <- "dec/dhc"
geo_type <- "block_group"
path_suff <- "est"
bg_hhSizeTenureRE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhSizeTenureRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("rent_own","size")
  #row_c1 determined by hand 
  row_c1 <- c(unique(bg_hhSizeTenureRE_data_from_census[!is.na(label_2) & concept!="TENURE BY HOUSEHOLD SIZE",name])) 
  bg_hhSizeTenureRE_data <- relabel(bg_hhSizeTenureRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhSizeTenureRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhSizeTenureRE_data <- bg_hhSizeTenureRE_data_from_census
}
bg_hhSizeTenureRE_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"TENURE BY HOUSEHOLD SIZE \\(","")][
    ,("race") := str_replace(race,"\\)","")]

#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhSizeTenureRE_data[,.SD,.SDcols = startsWith(names(bg_hhSizeTenureRE_data),state)])
bg_hhSizeTenureRE_melted <- melt(bg_hhSizeTenureRE_data, id.vars = c("re_code","race","rent_own","size"), measure.vars = Geoids,
                        value.name = "codom_bg_hhSizeTenureRE", variable.name = "GEOID")
bg_hhSizeTenureRE <- as.data.table(lapply(bg_hhSizeTenureRE_melted[,.SD],rep,bg_hhSizeTenureRE_melted[,codom_bg_hhSizeTenureRE]))
bg_hhSizeTenureR <- bg_hhSizeTenureRE[!re_code %in% c("H","I")]
bg_hhSizeTenureE <- bg_hhSizeTenureRE[re_code %in% c("H","I")]
rm(bg_hhSizeTenureRE_data_from_census)
rm(bg_hhSizeTenureRE_data)
rm(bg_hhSizeTenureRE_melted)
rm(bg_hhSizeTenureRE)

groupname <- "PCT7" #HOUSEHOLD TYPE BY HOUSEHOLD SIZE, race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTypeSizeRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhTypeSizeRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("family","hh_size")
  #row_c1 determined by hand 
  row_c1 <- c(unique(tr_hhTypeSizeRE_data_from_census[!is.na(label_2) & concept!="HOUSEHOLD TYPE BY HOUSEHOLD SIZE",name])) 
  tr_hhTypeSizeRE_data <- relabel(tr_hhTypeSizeRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhTypeSizeRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhTypeSizeRE_data <- tr_hhTypeSizeRE_data_from_census
}
tr_hhTypeSizeRE_data[,("re_code") := substr(name,5,5)][
  ,("race") := str_replace(concept,"HOUSEHOLD TYPE BY HOUSEHOLD SIZE \\(","")][
    ,("race") := str_replace(race,"\\)","")]

#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhTypeSizeRE_data[,.SD,.SDcols = startsWith(names(tr_hhTypeSizeRE_data),state)])
tr_hhTypeSizeRE_melted <- melt(tr_hhTypeSizeRE_data, id.vars = c("re_code","race","family","hh_size"), measure.vars = Geoids,
                                 value.name = "codom_tr_hhTypeSizeRE", variable.name = "GEOID")
tr_hhTypeSizeRE <- as.data.table(lapply(tr_hhTypeSizeRE_melted[,.SD],rep,tr_hhTypeSizeRE_melted[,codom_tr_hhTypeSizeRE]))
tr_hhTypeSizeR <- tr_hhTypeSizeRE[!re_code %in% c("H","I")]
tr_hhTypeSizeE <- tr_hhTypeSizeRE[re_code %in% c("H","I")]
rm(tr_hhTypeSizeRE_data_from_census)
rm(tr_hhTypeSizeRE_data)
rm(tr_hhTypeSizeRE_melted)
rm(tr_hhTypeSizeRE)

groupname <- "PCT2" #HOUSEHOLD SIZE BY HOUSEHOLD TYPE BY PRESENCE OF OWN CHILDREN
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhSizeTypeOwnKids_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhSizeTypeOwnKids_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("hh_size_2","family","family_type","sex","own_kids")
  tr_hhSizeTypeOwnKids_data_from_census[,("label_5"):=fcase(str_detect(label_2,"householder"),
                                                            "No own children under 18 years",
                                                            str_detect(label_3,"householder"),
                                                            "Not a family",
                                                            str_detect(label_4,"under"),
                                                            label_4,
                                                            default=label_5)]
  #row_c1 determined by hand 
  row_c1 <- c(unique(tr_hhSizeTypeOwnKids_data_from_census[!is.na(label_5),name])) 
  test_total_pop <- tests_download_data(tr_hhSizeTypeOwnKids_data_from_census,label_c1,row_c1,state=state) 
  tr_hhSizeTypeOwnKids_data <- relabel(tr_hhSizeTypeOwnKids_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhSizeTypeOwnKids_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  ttr_hhSizeTypeOwnKids_data <- tr_hhSizeTypeOwnKids_data_from_census
}

#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhSizeTypeOwnKids_data[,.SD,.SDcols = startsWith(names(tr_hhSizeTypeOwnKids_data),state)])
tr_hhSizeTypeOwnKids_melted <- melt(tr_hhSizeTypeOwnKids_data, id.vars = c("hh_size_2","family","family_type","sex","own_kids"), measure.vars = Geoids,
                               value.name = "codom_tr_hhSizeTypeOwnKids", variable.name = "GEOID")
tr_hhSizeTypeOwnKids <- as.data.table(lapply(tr_hhSizeTypeOwnKids_melted[,.SD],rep,tr_hhSizeTypeOwnKids_melted[,codom_tr_hhSizeTypeOwnKids]))
rm(tr_hhSizeTypeOwnKids_data_from_census)
rm(tr_hhSizeTypeOwnKids_data)
rm(tr_hhSizeTypeOwnKids_melted)


groupname <- "H15" #TENURE BY PRESENCE OF PEOPLE UNDER 18 YEARS (EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS)
api_type <- "dec/dhc"
geo_type <- "block_group"
path_suff <- "est"
bg_hh18Tenure_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hh18Tenure_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("tenure","kid_18") 
  row_c1 <- c(unique(bg_hh18Tenure_data_from_census[!is.na(label_2),name]))
  test_total_pop <- tests_download_data(bg_hh18Tenure_data_from_census,label_c1,row_c1,state=state) #seems to not get right total row!!
  bg_hh18Tenure_data <- relabel(bg_hh18Tenure_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hh18Tenure_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hh18Tenure_data <- bg_hh18Tenure_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_hh18Tenure_data[,.SD,.SDcols = startsWith(names(bg_hh18Tenure_data),state)])
bg_hh18Tenure_melted <- melt(bg_hh18Tenure_data, id.vars = c("tenure","kid_18"), measure.vars = Geoids,
                           value.name = "codom_bg_hh18Tenure", variable.name = "GEOID")
bg_hh18Tenure <- as.data.table(lapply(bg_hh18Tenure_melted[,.SD],rep,bg_hh18Tenure_melted[,codom_bg_hh18Tenure]))
rm(bg_hh18Tenure_data_from_census)
rm(bg_hh18Tenure_data)
rm(bg_hh18Tenure_melted)

groupname <- "HCT2" #TENURE BY PRESENCE AND AGE OF OWN CHILDREN (more categories)
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTenureOwnKids_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
#for some reason, doesn't include 6-17 years only for renters, which is largest category.
if(names(tr_hhTenureOwnKids_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("tenure","kid_18","kid_age_range_3") 
  tr_hhTenureOwnKids_data_from_census[,("label_3"):=fcase(str_detect(label_2,"No"),label_2,
                                                          name=="HCT2_009N","6 to 17 years only",
                                                          default=label_3)]
  row_c1 <- c(unique(tr_hhTenureOwnKids_data_from_census[!is.na(label_3),name]))
  test_total_pop <- tests_download_data(tr_hhTenureOwnKids_data_from_census,label_c1,row_c1,state=state) #not right total b/c 6-17 for renters, fixed below
  tr_hhTenureOwnKids_data <- relabel(tr_hhTenureOwnKids_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhTenureOwnKids_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhTenureOwnKids_data <- tr_hhTenureOwnKids_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhTenureOwnKids_data[,.SD,.SDcols = startsWith(names(tr_hhTenureOwnKids_data),state)])
tr_hhTenureOwnKids_melted <- melt(tr_hhTenureOwnKids_data, id.vars = c("tenure","kid_18","kid_age_range_3"), measure.vars = Geoids,
                             value.name = "codom_tr_hhTenureOwnKids", variable.name = "GEOID")
tr_hhTenureOwnKids_melted[,("codom_tr_hhTenureOwnKids"):=fcase(tenure=="Renter occupied"&kid_age_range_3=="6 to 17 years only",
                                                 as.numeric(.SD[tenure=="Renter occupied"&kid_age_range_3=="6 to 17 years only",codom_tr_hhTenureOwnKids])-
                                                   as.numeric(.SD[tenure=="Renter occupied"&kid_age_range_3=="Under 6 years and 6 to 17 years",codom_tr_hhTenureOwnKids])-
                                                   as.numeric(.SD[tenure=="Renter occupied"&kid_age_range_3=="Under 6 years only",codom_tr_hhTenureOwnKids]),
                                                 default=as.numeric(codom_tr_hhTenureOwnKids)),by=.(GEOID)]
tr_hhTenureOwnKids <- as.data.table(lapply(tr_hhTenureOwnKids_melted[,.SD],rep,tr_hhTenureOwnKids_melted[,codom_tr_hhTenureOwnKids]))
rm(tr_hhTenureOwnKids_data_from_census)
rm(tr_hhTenureOwnKids_data)
rm(tr_hhTenureOwnKids_melted)


groupname <- "HCT1" #TENURE BY HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER BY RACE OF HOUSEHOLDER - gives everything at tract keeps from guessing about race/eth
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhTenureRE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhTenureRE_data_from_census)[11]=="label_1"){
  #labels determined by hand; didn't do re_code in name for some reason; owner_occupied, Latino, AIAN missing (about 33k in TX)
  label_c1 <- c("tenure","HvL","race") 
  #row_c1 by hand
  tr_hhTenureRE_data_from_census[,"label_3":=fcase(label_1=="Owner occupied"&
                                                     label_2=="Hispanic or Latino householder"&
                                                     is.na(label_3),"Householder who is American Indian and Alaska Native alone",default = label_3)]
  row_c1 <- c(unique(tr_hhTenureRE_data_from_census[!is.na(label_3),name]))
  test_total_pop <- tests_download_data(tr_hhTenureRE_data_from_census,label_c1,row_c1,state=state) #seems to not get right total row!!
  tr_hhTenureRE_data <- relabel(tr_hhTenureRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhTenureRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhTenureRE_data <- tr_hhTenureRE_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhTenureRE_data[,.SD,.SDcols = startsWith(names(tr_hhTenureRE_data),state)])
tr_hhTenureRE_melted <- melt(tr_hhTenureRE_data, id.vars = c("tenure","HvL","race"), measure.vars = Geoids,
                           value.name = "codom_tr_hhTenureRE", variable.name = "GEOID")
tr_hhTenureRE_melted[,("codom_tr_hhTenureRE"):=fcase(tenure=="Owner occupied"&HvL=="Hispanic or Latino householder"&
                                                       race=="Householder who is American Indian and Alaska Native alone",
                                                     as.numeric(.SD[tenure=="Owner occupied"&HvL=="Hispanic or Latino householder"&
                                                                      race=="Householder who is American Indian and Alaska Native alone",codom_tr_hhTenureRE])-
                                                       sum(as.numeric(.SD[tenure=="Owner occupied"&HvL=="Hispanic or Latino householder"&
                                                                        race!="Householder who is American Indian and Alaska Native alone",codom_tr_hhTenureRE]),na.rm = TRUE),
                                                     default=as.numeric(codom_tr_hhTenureRE)),by=.(GEOID)]
tr_hhTenureRE <- as.data.table(lapply(tr_hhTenureRE_melted[,.SD],rep,tr_hhTenureRE_melted[,codom_tr_hhTenureRE])) #right number
rm(tr_hhTenureRE_data_from_census)
rm(tr_hhTenureRE_data)
rm(tr_hhTenureRE_melted)

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
