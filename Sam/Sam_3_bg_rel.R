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

groupname <- "PCT9" #HOUSEHOLD TYPE BY RELATIONSHIP FOR THE POPULATION 65 YEARS AND OVER, by race/eth, includes GQ and individual roles
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

groupname <- "PCT17" #HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP WITH RACE/ETHx2 (includes group quarters)
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
#clean up to get right number #by GEOID and RACE / the whole thing has to be rearranged and factor names have to be changed as merged into other tables
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
tr_hhRelRE_melted[,("age_range_2"):=ifelse(role=="Foster child","under_18",age_range_2)] #since it doesn't say, we assume only until 18.
#no group quarters, only households; only used for merge with tr_hh65, so extras with children shouldn't match
tr_hhRelRE_melted[,("role_7"):=fcase(str_detect(role,"Par"),"Parent or parent-in-law",
                                   str_detect(role,"-sex"),"Spouse or partner",
                                   str_detect(role,"Other rel") | str_detect(role,"or sister"),"Other relatives",
                                   default = role)]
tr_hhRelRE <- as.data.table(lapply(tr_hhRelRE_melted[,.SD],rep,tr_hhRelRE_melted[,codom_hhRelRE]))
#tr_hhRelR <- tr_hhRelRE[!re_code %in% c("H","I")]
#tr_hhRelE <- tr_hhRelRE[re_code %in% c("H","I")]
#sum(as.numeric(test_total_pop[,.SD,.SDcols = Geoids]),na.rm = TRUE)#==nrow(tr_hhRelR) - seems like it's all individuals.
#rm(tr_hhRel)
rm(tr_hhRelRE_data)
rm(tr_hhRelRE_data_from_census)
rm(tr_hhRelRE_melted)

#match for over_64_IH (some potential info loss because of role_7 instead of role_18, but everything has a match and no other tables overlap on role and age)
tr_hhRelRE[,("age_range_2"):=fcase(is.na(age_range_2),"over_17",default = age_range_2)]
tr_hh65RelRE[,("age_range_2"):="over_17"]
tr_hhRelRE[,("re65_match_id"):=
      paste0(GEOID,re_code,household,age_range_2,role_7,sex,alone,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code,household,age_range_2,role_7,sex,alone)]
tr_hh65RelRE[,("re65_match_id"):=
      paste0(GEOID,re_code,household,age_range_2,role,sex,alone,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code,household,age_range_2,role,sex,alone)]
tr_hhRelRE[,c("over_64","codom_tr_hh65RelRE"):=
            tr_hh65RelRE[.SD,c(list(re_code),list(codom_tr_hh65RelRE)),on=.(re65_match_id)]]
tr_hh65RelRE[,("over_64_match"):=
    tr_hhRelRE[.SD,list(re_code),on=.(re65_match_id)]]
#table(tr_hhRelRE[,over_64_IH])==table(tr_hh65RelRE[,re_code]) #using re_code to test all moved over
#table(tr_hhRelRE[,household],tr_hhRelRE[,over_64])
#nrow(tr_hhRelRE[!is.na(over_64)])==nrow(tr_hh65RelRE)
tr_hhRelRE[,("over_64"):=fcase(!is.na(over_64),"over_64",default = "under_65")]
#age in sex is better than in age_range_2 (roles like householders may have under 18 that are not listed)
tr_hhRelRE[,("age_range_3"):=fcase(age_range_2=="under_18","Under 18 years",
                                over_64=="over_64","65 years and over",
                                default = "18 to 64 years")]

#a little artificial, but don't have much to go on
tr_hhRelRE[,("age_range_6"):=fcase(age_range_2=="under_18","Under 18 years",
                                   age_range_2=="over_17" & str_detect(role,"child"),"18 to 24 years",
                                   over_64=="over_64","65 years and over",
                                   over_64=="under_65" & role=="Householder","18 to 64 years",
                                   over_64=="under_65" & str_detect(role,"sex"),"18 to 64 years", #all 4 spouse/partner designations
                                   #need to match on institutionalized separately because some are under_18 but not given in age_range_2
                                   over_64=="under_65" & str_detect(role,"population"),"Institutionalized under 65 years",
                                   over_64=="under_65" & str_detect(role,"relatives"),"18 to 64 years",
                                   over_64=="under_65" & str_detect(role,"Son-in"),"18 to 34 years",
                                   over_64=="under_65" & str_detect(role,"Brother or "),"18 to 64 years",
                                   over_64=="under_65" & str_detect(role,"Parent"),"55 to 64 years",
                                   default = "default 35 to 64 years")]
table(tr_hhRelRE[,sex],tr_hhRelRE[,role],useNA = "ifany")
#following old rules on assigning male to household head, if male present in household
tr_hh65RelRE[,("sex"):=fcase(str_detect(role,"Opposite"),"Female",default = sex)]
#table(tr_hhRelRE[,role],tr_hhRelRE[,age_range_6],useNA = "ifany")
rm(tr_hh65RelRE)

#add in specific age information for RelRE, then move to P17 with age - PCT13 for 2020 had a missing row in original table - Hispanic Women who are 61 or 62.
groupname <- "PCT13" # SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS (Race/eth x 2) #total pop - group quarters
geo_type <- "tract"
api_type <- "dec/dhc"
path_suff <- "est"
tr_hhSARE_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_hhSARE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("sex","age_range_23")
  #row_c1 determined by hand 
  row_c1 <- c(unique(tr_hhSARE_data_from_census[!is.na(label_2) & concept!="SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS",name]))
  #test_total_pop <- tests_download_data(tr_hhSARE_data_from_census,label_c1,row_c1,state=state)
  #do this with HvL, to divide later, since don't have whole population by both
  tr_hhSARE_data <- relabel(tr_hhSARE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhSARE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhSARE_data <- tr_hhSARE_data_from_census
}
tr_hhSARE_data[,("re_code") := substr(name,6,6)][
  ,("race") := str_replace(concept,"SEX BY AGE FOR THE POPULATION IN HOUSEHOLDS \\(","")][
    ,("race") := str_replace(race,"\\)","")]

#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhSARE_data[,.SD,.SDcols = startsWith(names(tr_hhSARE_data),state)])
tr_hhSARE_melted <- melt(tr_hhSARE_data, id.vars = c("re_code","race","sex","age_range_23"), measure.vars = Geoids,
                         value.name = "codom_tr_hhAge", variable.name = "GEOID")
tr_hhSARE <- as.data.table(lapply(tr_hhSARE_melted[,.SD],rep,tr_hhSARE_melted[,codom_tr_hhAge]))
rm(tr_hhSARE_data_from_census)
rm(tr_hhSARE_data)
rm(tr_hhSARE_melted)
#nrow(tr_hhSARE)==nrow(tr_hhRelRE[household=="In households"]) #FALSE!!!
#problem is missing Hispanic Females 60 and 61 with re_code of H (99921 in Texas 2020)
#table(tr_hhRelRE[household=="In households",re_code])-table(tr_hhSARE[,re_code])
#table(tr_hhSARE[re_code=="H",age_range_23],tr_hhSARE[re_code=="H",sex])
#since it's only one group missing and known, except for GEOID, we can just match missing and add rows

rep_row_fix <- as.data.table(table(tr_hhRelRE[household=="In households"&re_code=="H",GEOID])-table(tr_hhSARE[re_code=="H",GEOID]))
tr_hhRelRErowfix <- tr_hhRelRE[,.(cntR = .N), by=.(GEOID,household,re_code)]
tr_hhRelRErowfix <- tr_hhRelRErowfix[re_code=="H"&household=="In households"]
tr_hhSARErowfix <- tr_hhSARE[,.(cntS = .N), by=.(GEOID,re_code)]
tr_hhSARErowfix <- tr_hhSARErowfix[re_code=="H"]
rep_row_fix <- tr_hhRelRErowfix[tr_hhSARErowfix, on=.(re_code,GEOID), .(repcnt = cntR-cntS,GEOID=GEOID,re_code=re_code)]
row_fix <- as.data.table(c(list(GEOID=rep_row_fix[,GEOID]),list(codom_tr_hhAge=rep_row_fix[,repcnt]),list(re_code="H"),
                           list(race="HISPANIC OR LATINO"),list(sex="Female"),list(age_range_23="60 and 61 years")))
#row_fix <-row_fix[rep_rf>0]
tr_hhSARErf <- as.data.table(lapply(row_fix[,.SD],rep,row_fix[,codom_tr_hhAge]))
tr_hhSARE <- rbindlist(list(tr_hhSARE,tr_hhSARErf),use.names = TRUE)
rm(rep_row_fix)
rm(row_fix)
rm(tr_hhSARErowfix)
rm(tr_hhSARErf)
tr_hhSARE[,("age_num"):=fcase(age_range_23=="Under 5 years",as.integer(0),
                           age_range_23=="5 to 9 years",as.integer(5),default = as.integer(str_sub(age_range_23,start=1,end=2)))]
#introduces NAs by coercion warning, but only Un from Under 5 years causes it; doesn't seem to hurt anything...
#age_range_6 also has: "Institutionalized under 65 years", but these are only households and have no data to match on
tr_hhSARE[,("age_range_6"):=fcase(age_num<18,"Under 18 years",
                                  age_num>17 & age_num<25,sample(c("18 to 24 years","18 to 64 years"),.N,c(.6,.4),replace=TRUE),
                                  age_num>24 & age_num<35,sample(c("18 to 34 years","18 to 64 years"),.N,c(.2,.8),replace=TRUE),
                                  age_num>34 & age_num<55,sample(c("55 to 64 years","18 to 64 years"),.N,c(.05,.95),replace=TRUE),#start with more, .1,.9 gets you close to matching total numbers
                                  age_num>64,"65 years and over",default = "18 to 64 years"
                                  )]
tr_hhSARE[,("age_range_3"):=fcase(age_num<18,"Under 18 years",
                                  age_num>64,"65 years and over",default = "18 to 64 years"
                                  )]

#age_range_6 don't match perfectly, but get it started
#for tr_hhRel, don't know sex for under_18, so will have to do a second time
#doing both age_range_3 and 6 because 6 floats over line on tr_hhRelRE
tr_hhSARE[,("tr_SARERel_match_id"):=
           paste0(GEOID,re_code,sex,age_range_3,age_range_6,as.character(100000+sample(1:.N))),
         by=.(GEOID,re_code,sex,age_range_3,age_range_6)]
tr_hhRelRE[household=="In households",("tr_SARERel_match_id"):=
           paste0(GEOID,re_code,sex,age_range_3,age_range_6,as.character(100000+sample(1:.N))),
         by=.(GEOID,re_code,sex,age_range_3,age_range_6)]
tr_hhSARE[,("match_relre"):= 
            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERel_match_id)]]
tr_hhRelRE[household=="In households",("age_range_23"):=
           tr_hhSARE[.SD,list(age_range_23),on=.(tr_SARERel_match_id)]]
#table(tr_hhRelRE[household=="In households"&!is.na(age_range_23),sex])-table(tr_hhRelRE[sex!="Under 18 years",sex]) #surprisingly good match
#without sex (the 12378124 under_18 have no sex in tr_hhRelRE)
#nrow(tr_hhRelRE[is.na(age_range_23)&sex!="Under 18 years"]) #7146, but go ahead and assign across sex
tr_hhSARE[is.na(match_relre),("tr_SARERela_match_id"):=
            paste0(GEOID,re_code,age_range_3,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,re_code,age_range_3,age_range_6)]
tr_hhRelRE[household=="In households"&is.na(age_range_23),("tr_SARERela_match_id"):=
             paste0(GEOID,re_code,age_range_3,age_range_6,as.character(100000+sample(1:.N))),
           by=.(GEOID,re_code,age_range_3,age_range_6)]
tr_hhSARE[is.na(match_relre),("match_relre"):= 
            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERela_match_id)]]
tr_hhRelRE[household=="In households"&is.na(age_range_23),c("age_range_23","sex"):=
             tr_hhSARE[.SD,c(list(age_range_23),list(sex)),on=.(tr_SARERela_match_id)]]
#nrow(tr_hhRelRE[household=="In households"&is.na(age_range_23)]) #3474224 just about 6.7% still not matching

#make sure it still respects age_range_3
tr_hhSARE[is.na(match_relre),("tr_SARERelb_match_id"):=
            paste0(GEOID,re_code,age_range_3,as.character(100000+sample(1:.N))),
          by=.(GEOID,re_code,age_range_3)]
tr_hhRelRE[household=="In households"&is.na(age_range_23),("tr_SARERelb_match_id"):=
             paste0(GEOID,re_code,age_range_3,as.character(100000+sample(1:.N))),
           by=.(GEOID,re_code,age_range_3)]
tr_hhSARE[is.na(match_relre),("match_relre"):= 
            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERelb_match_id)]]
tr_hhRelRE[household=="In households"&is.na(age_range_23),c("age_range_23","sex"):=
             tr_hhSARE[.SD,c(list(age_range_23),list(sex)),on=.(tr_SARERelb_match_id)]]
nrow(tr_hhRelRE[household=="In households"&is.na(age_range_23)]) #614788

#table(tr_hhSARE[,re_code],tr_hhSARE[,age_range_6],useNA = "ifany")-table(tr_hhRelRE[household=="In households",re_code],tr_hhRelRE[household=="In households",age_range_6],useNA = "ifany")
#table(tr_hhRelRE[household=="In households"&is.na(age_range_23),age_range_6])
#table(tr_hhRelRE[is.na(age_range_23)&household=="In households",age_range_6],useNA = "ifany")
#table(tr_hhSARE[is.na(match_relre),age_range_23],useNA = "ifany")
#nrow(tr_hhSARE[is.na(match_relre)])==nrow(tr_hhRelRE[is.na(age_range_23)&household=="In households"])

tr_hhRelRE_inst <- tr_hhRelRE[str_detect(role,"nstit")]
tr_hhRelRE <- tr_hhRelRE[!str_detect(role,"nstit")]
tr_hhRelRE <- tr_hhRelRE[order(match(role,c("Biological child","Grandchild","Adopted child","Stepchild","Foster child",
                                                "Householder","Opposite-sex spouse","Opposite-sex unmarried partner",
                                                "Same-sex spouse","Same-sex unmarried partner","Son-in-law or daughter-in-law",
                                                "Other nonrelatives","Other relatives","Brother or sister","Parent",
                                                "Parent-in-law")))]
tr_hhSARE <- tr_hhSARE[order(match(age_range_6,c("Under 18 years","18 to 24 years","18 to 34 years","18 to 64 years",
                                                 "55 to 64 years","65 years and over")))]
#pick up last 1%
tr_hhSARE[is.na(match_relre),("tr_SARERelc_match_id"):=
            paste0(GEOID,re_code,as.character(100000+(1:.N))), #re_code per GEOID should still match, then just keep order when assigning count
          by=.(GEOID,re_code)]
tr_hhRelRE[household=="In households"&is.na(age_range_23),("tr_SARERelc_match_id"):=
             paste0(GEOID,re_code,as.character(100000+(1:.N))),
           by=.(GEOID,re_code)]
tr_hhSARE[is.na(match_relre),("match_relre"):= 
            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERelc_match_id)]]
tr_hhRelRE[household=="In households"&is.na(age_range_23),c("age_range_23","sex"):=
             tr_hhSARE[.SD,c(list(age_range_23),list(sex)),on=.(tr_SARERelc_match_id)]]
nrow(tr_hhRelRE[is.na(age_range_23)&household=="In households"]) #0

#move tr_hh18Rel and bgGQ up here, then move to kid_age_range_1 and track group quarters and ages better
#need to track the _tr stuff and test!!!!


#pull in bg_SARE for matching by age and re_code_14 / created by Sam_1_bg_sare.R
file_path <- valid_file_path(censusdir,vintage,state,county = "*",api_type="dec",geo_type = "block_group",
                             groupname="bgSARE",path_suff="wrk")
#"~/University Of Houston/Engaged Data Science - Data/Census/2020/state_48/2020_48_dec_block_group_bgSARE_wrk.RDS"
bg_SARE <- readRDS(file_path)
#need to ensure bg_SARE has tract, age_range_23, who it has sex for, and how to do race and ethnicity at the same time...
bg_SARE[,("re_code_7"):=fcase(re_code=="I" | re_code=="P","A",
                              re_code=="J" | re_code=="Q","B",
                              re_code=="K" | re_code=="R","C",
                              re_code=="L" | re_code=="S","D",
                              re_code=="M" | re_code=="T","E",
                              re_code=="N" | re_code=="U","F",
                              re_code=="O" | re_code=="V","G",
                              default = "unknown")]
bg_SARE[,("HvL"):=fcase(re_code=="P" |
                          re_code=="Q" | 
                          re_code=="R" | 
                          re_code=="S" | 
                          re_code=="T" | 
                          re_code=="U" | 
                          re_code=="V","Hispanic or Latino",
                          default = "Not Hispanic or Latino")]
bg_SARE[,("age_range_3"):=fcase(age_num<18,"Under 18 years",
                                age_num>64,"65 years and over",
                                default = "18 to 64 years")]
#fiddling with percentages to come close...
bg_SARE[,("age_range_6"):=fcase(age_num<18,"Under 18 years",
                                age_num>17 & age_num<25,sample(c("18 to 24 years","18 to 64 years"),.N,c(.6,.4),replace=TRUE),
                                age_num>24 & age_num<35,sample(c("18 to 34 years","18 to 64 years"),.N,c(.2,.8),replace=TRUE),
                                age_num>34 & age_num<55,sample(c("55 to 64 years","18 to 64 years"),.N,c(.05,.95),replace=TRUE),#start with more, .1,.9 gets you close to matching total numbers
                                age_num>64,"65 years and over",default = "18 to 64 years"
)]
bg_SARE[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]

#nrow(tr_hhRelRE[is.na(sex)&household=="In households"])==0
#could compare with expanding bg_SARE to re_code_9...

#joining bg_SARE and tr_hhRelRE, with Group Quarters put back in
tr_hhRelRE[,("tr_SARERelc_match_id"):=NULL]
tr_hhRelRE <- rbind(tr_hhRelRE,tr_hhRelRE_inst)
rm(tr_hhRelRE_inst)
#because we need to move individuals only once, need to match tr_hhRelR with tr_hhSAE
tr_hhRelR <- tr_hhRelRE[!re_code%in%c("H","I")] 
tr_hhRelH <- tr_hhRelRE[re_code=="H"]
tr_hhRelI <- tr_hhRelRE[re_code=="I"]
tr_hhRelR[re_code=="A",("tr_SARI_match_id"):=
          paste0(GEOID,household,sex,role_7,age_range_23,as.character(100000+sample(1:.N))),
        by=.(GEOID,household,sex,role_7,age_range_23)]
tr_hhRelI[,("tr_SARI_match_id"):=
            paste0(GEOID,household,sex,role_7,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,sex,role_7,age_range_23)]
tr_hhRelR[re_code=="A",("re_code_I"):= #all should equal "I"
            tr_hhRelI[.SD,list(re_code),on=.(tr_SARI_match_id)]]
#tr_hhRelR[,("re_code_14"):=fcase(re_code=="A"&is.na(re_code_I),"P",
#                                 default = re_code_I)]
tr_hhRelI[,("matched_trSAI"):=
           tr_hhRelR[.SD,list(re_code),on=.(tr_SARI_match_id)]]
#nrow(tr_hhRelI[is.na(matched_trSAI)]) #845421/nrow(tr_hhRelI) - 7%
#match on only age_range_6
tr_hhRelR[re_code=="A"&is.na(re_code_I),("tr_SARIa_match_id"):=
            paste0(GEOID,household,sex,role_7,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,sex,role_7,age_range_6)]
tr_hhRelI[is.na(matched_trSAI),("tr_SARIa_match_id"):=
            paste0(GEOID,household,sex,role_7,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,sex,role_7,age_range_6)]
tr_hhRelR[re_code=="A"&is.na(re_code_I),("re_code_I"):= #all should equal "I"
            tr_hhRelI[.SD,list(re_code),on=.(tr_SARIa_match_id)]]
#tr_hhRelR[,("re_code_14"):=fcase(re_code=="A"&is.na(re_code_I),"P",
#                                 default = re_code_I)]
tr_hhRelI[is.na(matched_trSAI),("matched_trSAI"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARIa_match_id)]]
#nrow(tr_hhRelI[is.na(matched_trSAI)])#85991

#pick up last couple
tr_hhRelR[re_code=="A"&is.na(re_code_I),("tr_SARIb_match_id"):=
            paste0(GEOID,household,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,age_range_6)]
tr_hhRelI[is.na(matched_trSAI),("tr_SARIb_match_id"):=
            paste0(GEOID,household,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,age_range_6)]
tr_hhRelR[re_code=="A"&is.na(re_code_I),("re_code_I"):= #all should equal "I"
            tr_hhRelI[.SD,list(re_code),on=.(tr_SARIb_match_id)]]
#tr_hhRelR[,("re_code_14"):=fcase(re_code=="A"&is.na(re_code_I),"P",
#                                 default = re_code_I)]
tr_hhRelI[is.na(matched_trSAI),("matched_trSAI"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARIb_match_id)]]
#nrow(tr_hhRelI[is.na(matched_trSAI)]) #86? leave unmatched?

#do I to bg_SARE, then H, then rest of re_code_7; remember is.na(sex) & is.na(age_range) for group quarters
tr_hhRelI[,("tr_bg_SAI_match_id"):=
            paste0(GEOID,sex,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,sex,age_range_23)]
bg_SARE[re_code=="I",("tr_bg_SAI_match_id"):=
          paste0(tract,sex,age_range,as.character(100000+sample(1:.N))),
        by=.(tract,sex,age_range)]
tr_hhRelI[,("re_code_7"):= #should all be "A"
            bg_SARE[.SD,list(re_code_7),on=.(tr_bg_SAI_match_id)]]
bg_SARE[re_code=="I",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelI[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SAI_match_id)]]
#nrow(tr_hhRelI[is.na(re_code_7)]) == nrow(tr_hhRelI[household=="In group quarters"])
#nrow(bg_SARE[!is.na(role_tr)])==nrow(tr_hhRelI[household=="In households"])

#need to match from bg_SARE to tr_hhRelH, then tr_hhRelH back to tr_hhRelR
tr_hhRelH[,("tr_bg_SAE_match_id"):=
           paste0(GEOID,sex,age_range_23,as.character(100000+sample(1:.N))),
         by=.(GEOID,sex,age_range_23)]
bg_SARE[HvL=="Hispanic or Latino",("tr_bg_SAE_match_id"):= 
           paste0(tract,sex,age_range,as.character(100000+sample(1:.N))),
         by=.(tract,sex,age_range)]
tr_hhRelH[,("re_code_7"):=
           bg_SARE[.SD,list(re_code_7),on=.(tr_bg_SAE_match_id)]]
bg_SARE[HvL=="Hispanic or Latino",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelH[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SAE_match_id)]]
#nrow(tr_hhRelH[is.na(re_code_7)])#172849 is number in group quarters; no further matching needed.

#put onto tr_hhRelR
tr_hhRelR[is.na(re_code_I),("tr_SARH_match_id"):=
            paste0(GEOID,household,role,re_code,sex,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,re_code,sex,age_range_23)]
tr_hhRelH[,("tr_SARH_match_id"):=
            paste0(GEOID,household,role,re_code_7,sex,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,re_code_7,sex,age_range_23)]
tr_hhRelR[is.na(re_code_I),("re_code_H"):= 
            tr_hhRelH[.SD,list(re_code_7),on=.(tr_SARH_match_id)]]
tr_hhRelH[,("matched_trSAH"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARH_match_id)]]
#nrow(tr_hhRelH[is.na(matched_trSAH)]) #2557110 
#pick up more by relaxing age_range and role
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("tr_SARHa_match_id"):=
            paste0(GEOID,household,role_7,re_code,sex,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,re_code,sex,age_range_6)]
tr_hhRelH[is.na(matched_trSAH),("tr_SARHa_match_id"):=
            paste0(GEOID,household,role_7,re_code_7,sex,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,re_code_7,sex,age_range_6)]
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("re_code_H"):= 
            tr_hhRelH[.SD,list(re_code_7),on=.(tr_SARHa_match_id)]]
tr_hhRelH[is.na(matched_trSAH),("matched_trSAH"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARHa_match_id)]]
#nrow(tr_hhRelH[is.na(matched_trSAH)]) #1015162 
#without role, with idea that age_range_6 carries some of that
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("tr_SARHb_match_id"):=
            paste0(GEOID,household,re_code,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,re_code,age_range_6)]
tr_hhRelH[is.na(matched_trSAH),("tr_SARHb_match_id"):=
            paste0(GEOID,household,re_code_7,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,re_code_7,age_range_6)]
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("re_code_H"):= 
            tr_hhRelH[.SD,list(re_code_7),on=.(tr_SARHb_match_id)]]
tr_hhRelH[is.na(matched_trSAH),("matched_trSAH"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARHb_match_id)]]
#nrow(tr_hhRelH[is.na(matched_trSAH)]) #342888 
#get total number per tract right, even if matching on re_code and role is a bit off
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("tr_SARHc_match_id"):=
            paste0(GEOID,household,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,age_range_6)]
tr_hhRelH[is.na(matched_trSAH),("tr_SARHc_match_id"):=
            paste0(GEOID,household,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,age_range_6)]
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("re_code_H"):= 
            tr_hhRelH[.SD,list(re_code_7),on=.(tr_SARHc_match_id)]]
tr_hhRelH[is.na(matched_trSAH),("matched_trSAH"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARHc_match_id)]]
#nrow(tr_hhRelH[is.na(matched_trSAH)]) # 935 
#create re_code_14 on tr_hhRelR
tr_hhRelR[,("re_code_14"):=fcase(re_code=="A" & is.na(re_code_H),"I",
                                 re_code=="B" & is.na(re_code_H),"J",
                                 re_code=="C" & is.na(re_code_H),"K",
                                 re_code=="D" & is.na(re_code_H),"L",
                                 re_code=="E" & is.na(re_code_H),"M",
                                 re_code=="F" & is.na(re_code_H),"N",
                                 re_code=="G" & is.na(re_code_H),"O",
                                 re_code=="A" & re_code_H=="A","P",
                                 re_code=="B" & re_code_H=="B","Q",
                                 re_code=="C" & re_code_H=="C","R",
                                 re_code=="D" & re_code_H=="D","S",
                                 re_code=="E" & re_code_H=="E","T",
                                 re_code=="F" & re_code_H=="F","U",
                                 re_code=="G" & re_code_H=="G","V",
                                 default = "unknown")]
tr_hhRelR[,("re_code_14"):=fcase(re_code_14=="unknown"&re_code=="A","P",
                                 re_code_14=="unknown"&re_code=="B","Q",
                                 re_code_14=="unknown"&re_code=="C","R",
                                 re_code_14=="unknown"&re_code=="D","S",
                                 re_code_14=="unknown"&re_code=="E","T",
                                 re_code_14=="unknown"&re_code=="F","U",
                                 re_code_14=="unknown"&re_code=="G","V",
                                 default = re_code_14)]
tr_hhRelR[,("HvL"):=fcase(re_code_14%in%c("P","Q","R","S","T","U","V"),"Hispanic or Latino",
                          default = "Not Hispanic or Latino")]
#table(bg_SARE[,re_code_7],bg_SARE[,re_code])
#table(tr_hhRelR[,re_code],tr_hhRelR[,re_code_14]) #Q,S,T all less than zero

#do P by itself; having trouble preventing double-dipping for some reason
#tr_hhRelR[re_code_14=="P",("tr_bg_SARp_match_id"):=
#            paste0(GEOID,re_code,sex,age_range_23,as.character(100000+sample(1:.N))),
#          by=.(GEOID,re_code,sex,age_range_23)]
#bg_SARE[is.na(role_tr)&re_code=="P",("tr_bg_SARp_match_id"):=
#          paste0(tract,re_code_7,sex,age_range,as.character(100000+sample(1:.N))),
#        by=.(tract,re_code_7,sex,age_range)]
#tr_hhRelR[re_code_14=="P",("re_code_H"):=
#            bg_SARE[.SD,list(re_code),on=.(tr_bg_SARp_match_id)]]
#bg_SARE[is.na(role_tr)&re_code=="P",c("alone_tr","role_tr","role_7_tr","household_tr"):=
#          tr_hhRelR[.SD,c(list(alone),list(role),list(role_7),list(household)),on=.(tr_bg_SARp_match_id)]]
#table(tr_hhRelR[is.na(re_code_H),household]) #22521403
#nrow(bg_SARE[!is.na(role_tr)])-nrow(tr_hhRelR)
#table(bg_SARE[,re_code_7])-table(bg_SARE[!is.na(household_tr),re_code_7])

#do rest of re_code_7 for re_code_14 match
tr_hhRelR[HvL=="Not Hispanic or Latino"&re_code_14!="I",("tr_bg_SARh_match_id"):=
            paste0(GEOID,re_code_14,sex,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,re_code_14,sex,age_range_23)]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I",("tr_bg_SARh_match_id"):=
          paste0(tract,re_code,sex,age_range,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,sex,age_range)]
tr_hhRelR[HvL=="Not Hispanic or Latino"&re_code_14!="I",("match_7"):=
            bg_SARE[.SD,list(re_code),on=.(tr_bg_SARh_match_id)]]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelR[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SARh_match_id)]]
#nrow(tr_hhRelR[is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code_14!="I"]) #559365
#nrow(bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I"]) #503742
#match on age_range_6, with 
tr_hhRelR[is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code!="A",("tr_bg_SARha3_match_id"):=
            paste0(GEOID,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,age_range_6)]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code_7!="A",("tr_bg_SARha3_match_id"):=
          paste0(tract,age_range_6,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_6)]
tr_hhRelR[is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code!="A",("match_7"):=
            bg_SARE[.SD,list(re_code),on=.(tr_bg_SARha3_match_id)]]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code_7!="A",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelR[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SARha3_match_id)]]
#nrow(tr_hhRelR[is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code_14!="I"]) #293788
#nrow(bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I"]) #238165

#this doesn't have RE data, so joining afterwards to help with block_group distribution
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
  bg_hhRel_data_from_census[str_detect(label_2,"child")&is.na(label_3),("label_5"):="over_17"] #some of these are wrong
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
bg_hhRel_melted[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
bg_hhRel <- as.data.table(lapply(bg_hhRel_melted[,.SD],rep,bg_hhRel_melted[,codom_hhRel]))
#sum(test_total_pop[,.SD,.SDcols = Geoids])==nrow(bg_hhRel)
rm(bg_hhRel_data)
rm(bg_hhRel_data_from_census)
rm(bg_hhRel_melted)
#no group_quarter information except institutionalized vs. non-institutionalized

bg_SARE[,("household_tr"):=fcase(is.na(household_tr),"In group quarters",
                                 default = household_tr)] #still 83k off...
bg_hhRel[,("age_range_2"):=fcase(is.na(age_range_2),"over_17",default = age_range_2)]

#put some potential matches on from tr_hhRelR
bg_hhRel[household=="In households",("tr_bg_Rel_match_id"):=
           paste0(tract,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,sex,role,alone,age_range_2)]
tr_hhRelR[,("tr_bg_Rel_match_id"):=
            paste0(GEOID,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
          by=.(GEOID,sex,role,alone,age_range_2)]
bg_hhRel[household=="In households",c("re_code","race",
                                      "age_range_6","age_range_23","HvL"):=
           tr_hhRelR[.SD,c(list(re_code),list(race),
                           list(age_range_6),list(age_range_23),list(HvL)),
                     on=.(tr_bg_Rel_match_id)]]
tr_hhRelR[,("match_bgRel"):=
            bg_hhRel[.SD,list(household),on=.(tr_bg_Rel_match_id)]]
#nrow(tr_hhRelR[!is.na(match_bgRel)])
#b/c sex was only for about a third of the table, have to take it out for rest
bg_hhRel[household=="In households"&is.na(re_code),("tr_bg_Rels_match_id"):=
           paste0(tract,role,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,role,alone,age_range_2)]
tr_hhRelR[is.na(match_bgRel),("tr_bg_Rels_match_id"):=
            paste0(GEOID,role,alone,age_range_2,as.character(100000+sample(1:.N))),
          by=.(GEOID,role,alone,age_range_2)]
bg_hhRel[household=="In households"&is.na(re_code),c("re_code","race","sex",
                                      "age_range_6","age_range_23","HvL"):=
           tr_hhRelR[.SD,c(list(re_code),list(race),list(sex),
                           list(age_range_6),list(age_range_23),list(HvL)),
                     on=.(tr_bg_Rels_match_id)]]
tr_hhRelR[is.na(match_bgRel),("match_bgRel"):=
            bg_hhRel[.SD,list(household),on=.(tr_bg_Rels_match_id)]]
nrow(tr_hhRelR[is.na(match_bgRel)])==nrow(bg_hhRel[household=="In group quarters"])

#should we start with age_range_23? No ground truth is given for age_range on relatives, so have to move over keeping what we can then fixing
#match from move from tract to bg; match on re_code and HvL, then on re_code, then on HvL
bg_hhRel[,("bg_Rel_match_id"):=
           paste0(tract,household,sex,role,alone,age_range_23,HvL,re_code,as.character(100000+sample(1:.N))),
         by=.(tract,household,sex,role,alone,age_range_23,HvL,re_code)]
bg_SARE[,("bg_Rel_match_id"):=
          paste0(tract,household_tr,sex,role_tr,alone_tr,age_range,HvL,re_code_7,as.character(100000+sample(1:.N))),
        by=.(tract,household_tr,sex,role_tr,alone_tr,age_range,HvL,re_code_7)]
bg_hhRel[,("re_code_14"):=
           bg_SARE[.SD,list(re_code),on=.(bg_Rel_match_id)]]
bg_SARE[,c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(GEOID)),on=.(bg_Rel_match_id)]]
#nrow(bg_hhRel[is.na(re_code_14)]) #5022690 (about 18% not matched)
#table(bg_SARE[,age_range],bg_SARE[,role],useNA = "ifany")

#then on age_range_6
bg_hhRel[is.na(re_code_14),("bg_Rel6_match_id"):=
           paste0(tract,household,sex,role,alone,age_range_6,HvL,re_code,as.character(100000+sample(1:.N))),
         by=.(tract,household,sex,role,alone,age_range_6,HvL,re_code)]
bg_SARE[is.na(household),("bg_Rel6_match_id"):=
          paste0(tract,household_tr,sex,role_tr,alone_tr,age_range_6,HvL,re_code_7,as.character(100000+sample(1:.N))),
        by=.(tract,household_tr,sex,role_tr,alone_tr,age_range_6,HvL,re_code_7)]
bg_hhRel[is.na(re_code_14),("re_code_14"):=
           bg_SARE[.SD,list(re_code),on=.(bg_Rel6_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(GEOID)),on=.(bg_Rel6_match_id)]]
#nrow(bg_hhRel[is.na(re_code_14)]) #2787664 (about 10% not matched)
#because bg_hhRel doesn't have re_code originally, assuming lots of mismatch from that
#table(bg_SARE[,age_range],bg_SARE[,role],useNA = "ifany")

#go back to tr_hhRelR to get different additional matches
#first exclude already matched 
bg_hhRel[,("trbg2_Rel_match_id"):=
           paste0(tract,household,re_code_14,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,household,re_code_14,sex,role,alone,age_range_2)]
tr_hhRelR[,("trbg2_Rel_match_id"):=
            paste0(GEOID,household,re_code_14,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,re_code_14,sex,role,alone,age_range_2)]
tr_hhRelR[,c("match_trbgRel","match_bgSARE","bg_GEOID"):=
            bg_hhRel[.SD,c(list(household),list(re_code_14),list(GEOID)),on=.(trbg2_Rel_match_id)]]
bg_hhRel[,("re_code_match"):= #just for testing
           tr_hhRelR[.SD,list(re_code),on=.(trbg2_Rel_match_id)]]
#nrow(tr_hhRelR[is.na(match_trbgRel)])
#nrow(tr_hhRelR[is.na(match_bgSARE)])

#then remix 
bg_hhRel[is.na(re_code_14),("trbg_Rela_match_id"):= 
           paste0(tract,household,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,household,sex,role,alone,age_range_2)]
tr_hhRelR[is.na(match_bgSARE),("trbg_Rela_match_id"):=
          paste0(GEOID,household,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
        by=.(GEOID,household,sex,role,alone,age_range_2)]
tr_hhRelR[is.na(match_bgSARE),c("match_trbgRel","match_bgSARE","bg_GEOID"):=
    bg_hhRel[.SD,c(list(household),list(re_code_14),list(GEOID)),on=.(trbg_Rela_match_id)]]
bg_hhRel[is.na(re_code_14),c("re_code_tr","re_code_14_tr","age_range_6_tr"):= 
            tr_hhRelR[.SD,c(list(re_code),list(re_code_14),list(age_range_6)),on=.(trbg_Rela_match_id)]]
#nrow(tr_hhRelR[is.na(match_trbgRel)])#0

#matching to tr_hhRelR with the bg_GEOID from bg_hhRel written over
bg_SARE[is.na(household),("bg_RelSARE_match_id"):=
           paste0(tract,re_code,household_tr,sex,role_tr,alone_tr,age_range_6,as.character(100000+sample(1:.N))),
         by=.(tract,re_code,household_tr,sex,role_tr,alone_tr,age_range_6)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("bg_RelSARE_match_id"):=
            paste0(GEOID,re_code_14_tr,household,sex,role,alone,age_range_6_tr,as.character(100000+sample(1:.N))),
          by=.(GEOID,re_code_14_tr,household,sex,role,alone,age_range_6_tr)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("re_code_14"):=
            bg_SARE[.SD,list(re_code),on=.(bg_RelSARE_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelSARE_match_id)]]
#nrow(bg_SARE[is.na(household)])#2787664
#nrow(bg_hhRel[is.na(re_code_14)]) #2787664

bg_SARE[,("age_range_3"):=fcase(age_num>64,"65 years and over",
                                age_num<65&age_num>17,"18 to 64 years",
                                default = "17 years and under")]
bg_hhRel[,("age_range_3"):=fcase(as.numeric(substr(age_range_23,1,2))>64,"65 years and over",
                                 as.numeric(substr(age_range_23,1,2))<65&as.numeric(substr(age_range_23,1,2))>17,"18 to 64 years",
                                default = "17 years and under")]
#NAs are introduced because of "Under", but doesn't seem to be a problem
#table(bg_hhRel[,age_range_3],bg_hhRel[,age_range_23],useNA = "ifany")

#finish as much re_code as possible, all at tract level
bg_SARE[is.na(household),("bg_RelARE_match_id"):=
          paste0(tract,re_code,age_range_3,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,age_range_3)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("bg_RelARE_match_id"):=
           paste0(tract,re_code_14_tr,age_range_3,as.character(100000+sample(1:.N))),
         by=.(tract,re_code_14_tr,age_range_3)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("re_code_14"):=
           bg_SARE[.SD,list(re_code),on=.(bg_RelARE_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelARE_match_id)]]
nrow(bg_SARE[is.na(household)]) #865211

#order on age range and then do last bit 
bg_hhRel <- bg_hhRel[order(match(role,c("Biological child","Grandchild","Adopted child","Stepchild","Foster child",
                                            "Householder","Opposite-sex spouse","Opposite-sex unmarried partner",
                                            "Same-sex spouse","Same-sex unmarried partner","Son-in-law or daughter-in-law",
                                            "Other nonrelatives","Other relatives","Brother or sister","Parent",
                                            "Parent-in-law")))]
bg_SARE <- bg_SARE[order(age_num)]

bg_SARE[is.na(household),("bg_RelRb_match_id"):=
          paste0(tract,re_code_7,as.character(100000+(1:.N))),
        by=.(tract,re_code_7)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("bg_RelRb_match_id"):=
            paste0(tract,re_code,as.character(100000+(1:.N))),
          by=.(tract,re_code)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("re_code_14"):=
            bg_SARE[.SD,list(re_code),on=.(bg_RelRb_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelRb_match_id)]]
#nrow(bg_SARE[is.na(household)])#638258
#nrow(bg_hhRel[is.na(re_code_14)])#638258 
#table(bg_SARE[,age_range_6],bg_SARE[,role],useNA = "ifany")

#for last 600k, just move over on re_code_7?
#bg_SARE[is.na(household),("bg_RelRc_match_id"):=
#          paste0(tract,re_code_7,as.character(100000+(1:.N))),
#        by=.(tract,re_code_7)]
#bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("bg_RelRc_match_id"):=
#            paste0(tract,re_code,as.character(100000+(1:.N))),
#          by=.(tract,re_code)]
#bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("re_code_14"):=
#            bg_SARE[.SD,list(re_code),on=.(bg_RelRc_match_id)]]
#bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
#          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelRc_match_id)]]
#nrow(bg_SARE[is.na(household)])#801898
#nrow(bg_hhRel[is.na(re_code_14)])#801898 

#last bit on GEOID only, with age_range still ordered in background
bg_SARE[is.na(household),("bg_RelRd_match_id"):=
          paste0(tract,as.character(100000+(1:.N))),
        by=.(tract)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("bg_RelRd_match_id"):=
            paste0(tract,as.character(100000+(1:.N))),
          by=.(tract)]
bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("re_code_14"):=
            bg_SARE[.SD,list(re_code),on=.(bg_RelRd_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelRd_match_id)]]
#nrow(bg_SARE[is.na(household)])#0
#nrow(bg_hhRel[is.na(re_code_14)])#0 
#table(bg_SARE[,age_range],bg_SARE[,role],useNA = "ifany") #this test does not depend on bg_GEOID
#table(bg_SARE[,role])-table(bg_hhRel[,role],useNA = "ifany") #all zeros
#table(bg_SARE[,role])-table(tr_hhRelR[,role],useNA = "ifany") #all zeros
#table(bg_SARE[,age_range_3])-table(tr_hhRelR[,age_range_3],useNA = "ifany") #not sure what's going on

#fix roles and roles_7 by age - typically off in the hundreds to low thousands across whole
#switch householders with child and add to other relatives?
#nrow(bg_SARE[str_detect(role," child")&age_num>64&sex=="Male"])
#nrow(bg_SARE[role=="Householder"&age_num<18])
bg_SARE[,("role"):=fcase(role=="Householder"&age_num<18,"Biological child",
  str_detect(role,"-sex")&age_num<18,"Biological child",
  str_detect(role,"Parent")&age_num<18,"Biological child",
  str_detect(role,"Parent")&age_num>17&age_num<54,"Other relatives",
  str_detect(role," child")&age_num>64&sex=="Male","Householder",
  str_detect(role," child")&age_num>64&sex=="Female","Opposite-sex spouse",
  role=="Grandchild"&age_num>44,"Other relatives",
  default = role
  )]


#
#add under18 info; this includes some group quarter info; not doing above, because doesn't have re_code
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
  label_c1 <- c("household","child_role","kid_age_range") 
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
tr_hhRel18_melted <- melt(tr_hhRel18_data, id.vars = c("household","child_role","kid_age_range"), measure.vars = Geoids,
                          value.name = "codom_tr_hhRel18", variable.name = "GEOID")
tr_hhRel18 <- as.data.table(lapply(tr_hhRel18_melted[,.SD],rep,tr_hhRel18_melted[,codom_tr_hhRel18]))
rm(tr_hhRel18_data_from_census)
rm(tr_hhRel18_melted)
rm(tr_hhRel18_data)

tr_hhRel18[,("kid_age_range_1"):=fcase(kid_age_range=="Under 3 years" |
                                         kid_age_range=="3 and 4 years", "Under 5 years",
                                       kid_age_range=="5 years" |
                                         kid_age_range=="14 years" |
                                         kid_age_range=="12 and 13 years" |
                                         kid_age_range=="6 to 11 years", "5 to 14 years",
                                       default = kid_age_range)] #also gets 15 to 17 years for both 
bg_SARE[,("kid_age_range_1"):=fcase(age_range=="5 to 9 years" |
                                      age_range=="10 to 14 years", "5 to 14 years",
                                    default = age_range)]
table(bg_SARE[,kid_age_range_1],bg_SARE[,role])
#5988 more 15 to 17 years in bg_SARE than in tr_hhRel18; 5 to 14 and under 5 match; all straight from census...
#nrow(bg_SARE[age_num<18])-nrow(tr_hhRel18) #5988 - not at all sure what to do with that; are they perhaps the spouses and householders?
#need to check if household or household_tr should be used here on bg_SARE
bg_SARE[,("bg_RelRd_match_id"):=
          paste0(tract,household,kid_age_range_1,as.character(100000+(1:.N))),
        by=.(tract)]
tr_hhRel18[,("bg_RelRd_match_id"):=
           paste0(tract,as.character(100000+(1:.N))),
         by=.(tract)]
tr_hhRel18[,("re_code_14"):=
           bg_SARE[.SD,list(re_code),on=.(bg_RelRd_match_id)]]
bg_SARE[,c("alone","role","household","bg_GEOID"):=
          tr_hhRel18[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelRd_match_id)]]

#need to pare down number of gq kids and then re-assign them relations...


tr_hhRel18[,("bg_Rel18_match_id"):=
           paste0(GEOID,household,kid_age_range_1,as.character(100000+sample(1:.N))),
         by=.(GEOID,household,kid_age_range_1)]
bg_SARE[,("bg_Rel18_match_id"):=
          paste0(tract,household,kid_age_range_1,as.character(100000+sample(1:.N))),
        by=.(tract,household,kid_age_range_1)]
tr_hhRel18[,("match_SARE"):=
           bg_SARE[.SD,list(re_code),on=.(bg_Rel18_match_id)]]
bg_SARE[,c("child_role","kid_age_range"):=
          tr_hhRel18[.SD,c(list(child_role),list(kid_age_range)),
                   on=.(bg_Rel18_match_id)]]
nrow(tr_hhRel18[is.na(match_SARE)]) #46458
nrow(bg_SARE[age_num<18&is.na(child_role)]) #52446 - 5988 = 46458
table(tr_hhRel18[is.na(match_SARE),kid_age_range])
#check for this vs. end
table(bg_SARE[,kid_age_range],bg_SARE[,role])
table(bg_SARE[,kid_age_range_1],bg_SARE[,role])
#if these are right, then just make sure age_range (etc.) is also pulled own in line 677  

#without household, but then pulling tr_hhRel18 household down
tr_hhRel18[is.na(match_SARE),("bg_Rel18a_match_id"):=
             paste0(GEOID,kid_age_range_1,as.character(100000+sample(1:.N))),
           by=.(GEOID,kid_age_range_1)]
bg_SARE[is.na(child_role),("bg_Rel18a_match_id"):=
          paste0(tract,kid_age_range_1,as.character(100000+sample(1:.N))),
        by=.(tract,kid_age_range_1)]
tr_hhRel18[is.na(match_SARE),c("match_SARE","role","role_7"):=
             bg_SARE[.SD,c(list(re_code),list(role),list(role_7)),on=.(bg_Rel18a_match_id)]]
bg_SARE[is.na(child_role),c("child_role","kid_age_range","household","role","role_7"):=
          tr_hhRel18[.SD,c(list(child_role),list(kid_age_range),list(household),list(role),list(role_7)),
                     on=.(bg_Rel18a_match_id)]]
nrow(tr_hhRel18[is.na(match_SARE)])==0
nrow(bg_SARE[age_num<18&is.na(child_role)]) #5988
table(bg_SARE[,kid_age_range],bg_SARE[,role])
table(bg_SARE[,kid_age_range_1],bg_SARE[,role])

#add group quarters info
groupname <- "P18" #GROUP QUARTERS POPULATION BY SEX BY AGE BY MAJOR GROUP QUARTERS TYPE
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_gq_age_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_gq_age_data_from_census)[11]=="label_1"){ #not sure what they changed to make it at 11, not 6
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
Geoids <- colnames(bgGQ_data[,15:(ncol(bgGQ_data)-2)]) #check if it may be -2
bgGQ_melted <- melt(bgGQ_data, id.vars = c("sex","age_range","gq_institution","gq_type","gq_type_6"), measure.vars = Geoids,
                    value.name = "codom_GQSAT", variable.name = "GEOID")
bgGQ <- as.data.table(lapply(bgGQ_melted[,.SD],rep,bgGQ_melted[,codom_GQSAT]))
rm(bgGQ_data)
rm(bgGQ_melted)
bgGQ[,("beg_age_gq"):=fcase(age_range=="Under 18 years", as.numeric(0),
                            age_range=="18 to 64 years", as.numeric(18),
                            age_range=="65 years and over", as.numeric(65))]

#pull in bg_hhSARETT for householders, and create the overall [should this be in a new file?]



#get bg_hhRel (P17); move alone_tr, etc., up from bg_SARE to bg_hhRel, but match at tract level and test - if they all match, then move bg from bg_hhRel to bg_SARE


#get under 18 and group quarters data


#delete #s below, if above == TRUE

#do we need this, or can we use bg_SARE directly???

#back to tr_hhRelR then over to bg_SARE, matching on HvL and re_code_7
#tr_hhRelR[is.na(re_code_14),("tr_SARE7_match_id"):=
#           paste0(GEOID,re_code,sex,age_range_23,as.character(100000+sample(1:.N))),
#         by=.(GEOID,re_code,sex,age_range_23)]
#tr_hhRelH[,("tr_SARE7_match_id"):=
#           paste0(GEOID,re_code_7,sex,age_range_23,as.character(100000+sample(1:.N))),
#         by=.(GEOID,re_code_7,sex,age_range_23)]
#tr_hhRelR[is.na(re_code_14),("HvL"):=
#            tr_hhRelH[.SD,list(re_code),on=.(tr_SARE7_match_id)]]
#tr_hhRelH[,("matched_trSAE"):=
#           tr_hhRelR[.SD,list(re_code),on=.(tr_SARE7_match_id)]]
#nrow(tr_hhRelH[household=="In households"&is.na(matched_trSAE)]) #(.3%)
#table(tr_hhRelH[household=="In households"&is.na(matched_trSAE),age_range_3])
#table(tr_hhRelH[household=="In households"&is.na(matched_trSAE),re_code_7])
#table(tr_hhRelR[,re_code_14])
#
##pick up last .3%
##back to tr_hhSAR to track re_code_14, then over to bg_SARE, matching on re_code_14
#tr_hhRelR[is.na(re_code_14)&is.na(HvL),("tr_SARE7a_match_id"):=
#           paste0(GEOID,re_code,age_range_3,as.character(100000+sample(1:.N))),
#         by=.(GEOID,re_code,age_range_3)]
#tr_hhRelH[is.na(matched_trSAE),("tr_SARE7a_match_id"):=
#           paste0(GEOID,re_code_7,age_range_3,as.character(100000+sample(1:.N))),
#         by=.(GEOID,re_code_7,age_range_3)]
#tr_hhRelR[is.na(re_code_14)&is.na(HvL),("re_code_14"):=
#           tr_hhRelH[.SD,list(re_code),on=.(tr_SARE7a_match_id)]]
#tr_hhRelH[is.na(matched_trSAE),("matched_trSAE"):=
#           tr_hhRelR[.SD,list(re_code),on=.(tr_SARE7a_match_id)]]
#nrow(tr_hhRelH[is.na(matched_trSAE)]) #.2% - let ground from bgSARE be final match
#
#
#
#
##match on re_code_7 and HvL 
#bg_SARE[HvL=="Not Hispanic or Latino",("bg_SAR_match_id"):=
#            paste0(GEOID,sex,age_range_23,as.character(100000+sample(1:.N))),
#          by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
#tr_hhSAE[,("bg_SAR_match_id"):=
#            paste0(GEOID,household,role,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
#          by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
#bg_SARE[HvL=="Not Hispanic or Latino",c("re_code_HvL","codom_hhRelRE"):=
#          tr_hhSAE[.SD,c(list(re_code),list(codom_hhRelRE)),on=.(bg_SAR_match_id)]]
#tr_hhSAE[,("matched_SARE"):=
#            bg_SARE[.SD,list(re_code),on=.(bg_SAR_match_id)]]
##for rest without sex?
#nrow(tr_hhSARE[!is.na(matched_SARE)])
##for not HvL
#bg_SARE[HvL=="Hispanic or Latino" & is.na(household),("bg_SAE_match_id"):=
#          paste0(GEOID,sex,age_range_23,re_code_14,as.character(100000+sample(1:.N))),
#        by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
#tr_hhSARE[re_code=="H" & is.na(matched_SARE),("bg_SAE_match_id"):=
#            paste0(GEOID,household,role,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
#          by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
#bg_SARE[HvL=="Hispanic or Latino" & is.na(household),c("re_code_HvL","codom_hhRelRE"):=
#          tr_hhSARE[.SD,c(list(re_code),list(codom_hhRelRE)),on=.(bg_SAe_match_id)]]
#tr_hhSARE[re_code=="H" & is.na(matched_SARE),("matched_SARE"):=
#            bg_SARE[.SD,list(re_code),on=.(bg_SAE_match_id)]]
#for rest without sex?
#missing 61 and 62 year old Hispanic women need to be designated; group_quarter needs to be designated



#once we have ages and GQ added to bg_hhRel, will want to order by age on Rel and Type


#test<-table(tr_hhRelRE[re_code%in%c(LETTERS[1:7]),household],tr_hhRelRE[re_code%in%c(LETTERS[1:7]),role],
#      tr_hhRelRE[re_code%in%c(LETTERS[1:7]),alone],tr_hhRelRE[re_code%in%c(LETTERS[1:7]),sex],tr_hhRelRE[re_code%in%c(LETTERS[1:7]),age_range_2])==
#  table(bg_hhRel[,household],bg_hhRel[,role],bg_hhRel[,alone],bg_hhRel[,sex],bg_hhRel[,age_range_2])
#length(test[test==FALSE])
#codes and races
#R 59175    AMERICAN INDIAN AND ALASKA NATIVE ALONE, HISPANIC OR LATINO HOUSEHOLDER 
#K 32755    AMERICAN INDIAN AND ALASKA NATIVE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER [C (91957) is both (should've been 91930), 27 more in bg]
#S 5804     ASIAN ALONE, HISPANIC OR LATINO HOUSEHOLDER
#L 511372   ASIAN ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER [D (517172) is both (should've been 517176), 4 more in bg]
#Q 23299    BLACK OR AFRICAN AMERICAN ALONE, HISPANIC OR LATINO HOUSEHOLDER
#J 1302024  BLACK OR AFRICAN AMERICAN ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER [B (1325283) is both (should've been 1325323), 40 more in bg]
#T 1514     NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE, HISPANIC OR LATINO HOUSEHOLDER
#M 8109     NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER [E (9733) is both (should've been 9623), 110 fewer in bg]
#U 1086680  SOME OTHER RACE ALONE, HISPANIC OR LATINO HOUSEHOLDER
#N 37967    SOME OTHER RACE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER [F (1124717) is both (should've been 1124647), 70 fewer in bg]
#V 1282580  TWO OR MORE RACES, HISPANIC OR LATINO HOUSEHOLDER
#O 289592   TWO OR MORE RACES, NOT HISPANIC OR LATINO HOUSEHOLDER [G (1572219) is both (should've been 1572172), 47 fewer in bg]
#P 903545   WHITE ALONE, HISPANIC OR LATINO HOUSEHOLDER [A (5850183) is all White Alone (should've been 5850276), 93 more in bg]
#I 4946731  WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER [in both (4946645 in bg, 86 fewer)]
#nrow(bg_hhTypeRE)-nrow(tr_hhRelRE[re_code%in%c(LETTERS[1:7])&role_7=="Householder"]) 117 different??? 

#do without sex, but with age_range_23...
#for every role with a child in it, is.na(sex) should equal over_18

#move the ethnicity over with the codom info for sorting
tr_hhRelR <- tr_hhRelRE[re_code%in%c(LETTERS[1:7])]
tr_hhRelI <- tr_hhRelRE[re_code=="I"]
tr_hhRelR[re_code=="A",("tr_rel_match_id"):=
            paste0(GEOID,household,role,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
tr_hhRelI[,("tr_rel_match_id"):=
           paste0(GEOID,household,role,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
         by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
tr_hhRelR[re_code=="A",c("re_code_HvL","codom_hhRelRE"):=
           tr_hhRelI[.SD,c(list(re_code),list(codom_hhRelRE)),on=.(tr_rel_match_id)]]
tr_hhRelI[,("matched_R"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_rel_match_id)]]
#26682 not matching because of code_7 lost info for over_64
tr_hhRelR[re_code=="A"&is.na(re_code_HvL),("tr_rel1_match_id"):=
            paste0(GEOID,household,role_7,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,sex,alone,age_range_2,over_64)]
tr_hhRelI[is.na(matched_R),("tr_rel1_match_id"):=
            paste0(GEOID,household,role_7,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,sex,alone,age_range_2,over_64)]
tr_hhRelR[re_code=="A"&is.na(re_code_HvL),c("re_code_HvL","codom_hhRelRE"):=
            tr_hhRelI[.SD,c(list(re_code),list(codom_hhRelRE)),on=.(tr_rel1_match_id)]]
tr_hhRelI[is.na(matched_R),("matched_R"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_rel1_match_id)]]
tr_hhRelR[,("re_code_HvL"):=fcase(re_code=="A"&is.na(re_code_HvL),"P",default = re_code_HvL)]
#nrow(tr_hhRelR[re_code=="A"&is.na(re_code_HvL)])==0
#make them all the HvL re_code to match within, below
#tr_hhRelR[re_code!="A",("re_code_HvL"):=fcase(re_code=="B","Q",
#                                              re_code=="C","R",
#                                              re_code=="D","S",
#                                              re_code=="E","T",
#                                              re_code=="F","U",
#                                              re_code=="G","V",
#                                              default = re_code_HvL)]
#
#at this point, tr_hhRelR has I and P correctly
#match for IvP back to tr_hhRelH, so that we have only the non-White Hispanics in H
tr_hhRelH <- tr_hhRelRE[re_code=="H"]
#match all the H's to the P's and take them out
tr_hhRelR[re_code_HvL=="P",("bg_relH_match_id"):=
            paste0(GEOID,household,role,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
tr_hhRelH[,("bg_relH_match_id"):=
            paste0(GEOID,household,role,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,sex,alone,age_range_2,over_64)]
tr_hhRelH[,("re_code_HvL"):=
            tr_hhRelR[.SD,list(re_code_HvL),on=.(bg_relH_match_id)]]
tr_hhRelR[re_code_HvL=="P",("matchedI_HvL"):=
            tr_hhRelH[.SD,list(re_code),on=.(bg_relH_match_id)]]
#7473 not matched; assuming it's because of role_7 mismatches
tr_hhRelR[is.na(matchedI_HvL)&re_code_HvL=="P",("bg_relH1_match_id"):=
            paste0(GEOID,household,role_7,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,sex,alone,age_range_2,over_64)]
tr_hhRelH[is.na(re_code_HvL),("bg_relH1_match_id"):=
            paste0(GEOID,household,role_7,sex,alone,age_range_2,over_64,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,sex,alone,age_range_2,over_64)]
tr_hhRelH[is.na(re_code_HvL),("re_code_HvL"):=
            tr_hhRelR[.SD,list(re_code_HvL),on=.(bg_relH1_match_id)]]
tr_hhRelR[is.na(matchedI_HvL)&re_code_HvL=="P",("matchedI_HvL"):=
            tr_hhRelH[.SD,list(re_code),on=.(bg_relH1_match_id)]]
#nrow(tr_hhRelR[is.na(matchedI_HvL)&re_code_HvL=="P"])==0
#save data for Hispanic v Latino, but not white
tr_hhRelHnotP <- tr_hhRelH[is.na(re_code_HvL)]
#will finish matching, below, after getting bg distribution by Q-V
#annoyingly, bg by household don't match fro bg_hhTypeRE (all households) and bg_hhRel (everyone in hh, including "Householders")
#point is to get a directional match and then make sure final is taken from exact matches where possible; hh is really off!!

#tract is not perfect, but fewer problems... still need an overall approach; doing first 100000 because they don't have same tracts (non-conformable arrays)
#will need tract on bg_hhRel in any case, so test here

#test_hhT <- table(bg_hhTypeRE[,tract][1:100000])-table(bg_hhRel[role=="Householder",tract][1:100000])
#sum(test_hhT) #0
#sum(abs(test_hhT)) #432
#mean(abs(test_hhT)) #6.857143
#max(abs(test_hhT)) #88



#match the HvL, not P, to the bg_hhRel (i.e., Q-V); will then match it within bg_hhTypeRE, then match rest (after getting all codomains, will sort and match)
#bg_hhRel does not have "over_64" - may be able to preserve with the codom_
tr_hhRelHnotP[,("bg_HnotP_match_id"):=
            paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,sex,alone,age_range_2)]
bg_hhRel[,("bg_HnotP_match_id"):=
           paste0(tract,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,household,role,sex,alone,age_range_2)]
bg_hhRel[,c("copath_HvL","copath_over_64","codom_hhRelH","codom_tr_hh65RelRE"):=
           tr_hhRelHnotP[.SD,c(list(re_code),list(over_64),list(codom_hhRelRE),list(codom_tr_hh65RelRE)),on=.(bg_HnotP_match_id)]]
#tr_hhRelHnotP[,c("match_HvL","match_role"):=
#                bg_hhRel[.SD,c(list(household),list(role)),on=.(bg_HnotP_match_id)]]
#nrow(bg_hhRel[!is.na(copath_HvL)])==nrow(tr_hhRelHnotP)

#also get the I (tried with and without, and the matching by order seems to be same either way)
tr_hhRelI[,("bg_I_match_id"):=
                paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
              by=.(GEOID,household,role,sex,alone,age_range_2)]
bg_hhRel[is.na(copath_HvL),("bg_I_match_id"):=
           paste0(tract,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,household,role,sex,alone,age_range_2)]
bg_hhRel[is.na(copath_HvL),c("copath_HvL","copath_over_64","codom_hhRelH","codom_tr_hh65RelRE"):=
           tr_hhRelI[.SD,c(list(re_code),list(over_64),list(codom_hhRelRE),list(codom_tr_hh65RelRE)),on=.(bg_I_match_id)]]
tr_hhRelI[,c("match_HvL","match_role"):=
                bg_hhRel[.SD,c(list(household),list(role)),on=.(bg_I_match_id)]]
#nrow(bg_hhRel[copath_HvL=="I"])==nrow(tr_hhRelI)
#rm(tr_hhRelH)
#rm(tr_hhRelI)
#rm(tr_hhRelHnotP)

#rest of re_code_HvL from tract level
#bg_hhRel[,("copath_re_code"):=ifelse(!is.na(copath_HvL),"A","Not known")]
tr_hhRelR[,("bg_notA_match_id"):=
            paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role,sex,alone,age_range_2)]
bg_hhRel[,("bg_notA_match_id"):=
           paste0(tract,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,household,role,sex,alone,age_range_2)]
bg_hhRel[,c("copath_re_AG","copath_over_64","codom_bg_hhRelRE","codom_tr_hh65RelRE"):=
           tr_hhRelR[.SD,c(list(re_code),list(over_64),list(codom_hhRelRE),list(codom_tr_hh65RelRE)),on=.(bg_notA_match_id)]]
tr_hhRelR[,c("match_HvL","match_role"):=
            bg_hhRel[.SD,c(list(household),list(role)),on=.(bg_notA_match_id)]]
#nrow(bg_hhRel[is.na(copath_re_AG)])
#this just lets you compare; much worse on AG than on ordering
#bg_hhRel[,("copath_re_code_HvL"):=fcase(copath_HvL=="I","I",
#                                        is.na(copath_HvL)&copath_re_AG=="B","J",
#                                        is.na(copath_HvL)&copath_re_AG=="C","K",
#                                        is.na(copath_HvL)&copath_re_AG=="D","L",
#                                        is.na(copath_HvL)&copath_re_AG=="E","M",
#                                        is.na(copath_HvL)&copath_re_AG=="F","N",
#                                        is.na(copath_HvL)&copath_re_AG=="G","O",
#                                        copath_HvL=="H"&copath_re_AG=="A","P",
#                                        copath_HvL=="H"&copath_re_AG=="B","Q",
#                                        copath_HvL=="H"&copath_re_AG=="C","R",
#                                        copath_HvL=="H"&copath_re_AG=="D","S",
#                                        copath_HvL=="H"&copath_re_AG=="E","T",
#                                        copath_HvL=="H"&copath_re_AG=="F","U",
#                                        copath_HvL=="H"&copath_re_AG=="G","V",
#                                        default = "not matched")]
#



#get bg_hhSARETT
file_path <- valid_file_path(censusdir,vintage,state,county="*",api_type="dec",geo_type="block_group",
                             groupname="bg_hhSARETT",path_suff="wrk")
#"~/University Of Houston/Engaged Data Science - Data/Census/2020/state_48/2020_48_dec_block_group_bg_hhSARETT_wrk.RDS"
if(file.exists(file_path)){bg_hhTypeRE <- readRDS(file_path)}else{print("bg_hhTypeRE does not exist at this location")}

#test_hh <- table(bg_hhRel[role=="Householder",GEOID])-table(bg_hhTypeRE[,GEOID])
#sum(test_hh) #117
#sum(abs(test_hh)) #207091
#mean(abs(test_hh)) #11.1112
#max(abs(test_hh)) #107

#do we need these for alone and sex?
#about 5k na coming in on bg_hhTypeRE$alone, but all should be "Living alone"
bg_hhTypeRE[,("alone"):=fcase(is.na(alone),"Living alone",
                              default = alone)]
bg_hhTypeRE[,("copath_over_64"):=fcase(age_range_3=="Householder 65 years and over",
                                       "over_64",default = "under_65")]
bg_hhTypeRE[,("sex"):=fcase(sex=="Sex not known","Male",default = sex)] #up through 2020, hh head is assumed male unless told otherwise
table(bg_hhRel[role=="Householder",sex]) #doesn't work! way off other tables provided by decennial!
#Female    Male 
#5088692 5402572
table(bg_hhTypeRE[,alone],bg_hhTypeRE[,age_range_3])
table(bg_hhRel[role=="Householder",alone],bg_hhRel[role=="Householder",copath_over_64])

#age_range_2 in bg_hhRel is under 18 and only for roles associated with children, not for hh
#losing householders under 18
#match on re_code_14 letters for copath_re_code in Rel, leftovers being the HvL? Don't have to do the ordering...
#bg_hhRel[,("copath_re_code_14"):=lapply(copath_re_AG,function(x) LETTERS[which(LETTERS==x)+9])] #super slow
bg_hhRel[,("re_code_14"):=fcase(copath_re_AG=="A","I",
                                     copath_re_AG=="B","J",
                                     copath_re_AG=="C","K",
                                     copath_re_AG=="D","L",
                                     copath_re_AG=="E","M",
                                     copath_re_AG=="F","N",
                                     copath_re_AG=="G","O",
                              default = copath_re_AG)]
#could try to give the ids some inherent order, but at this point leaning into random
bg_hhTypeRE[,("hh_id"):=paste0(paste0(GEOID,as.character(100000+sample(1:.N)))),
            by=.(GEOID)]
bg_hhRel[,("individual_id"):=paste0(paste0(GEOID,as.character(100000+sample(1:.N)))), #largest block_group has 19522 individuals
         by=.(GEOID)]
#match on re_code_14 first, then rest will be the leftover with HvL
#test <- table(bg_hhTypeRE[,GEOID])-table(bg_hhRel[role=="Householder",GEOID])
#max(test)
#] 72
#min(test)
#] -107
#mean(test)
#] -0.006277498
#median(test)
#] 0
#length(test[test==0])
#] 721
#length(test)
#] 18638
#test <- table(bg_hhTypeRE[,GEOID],bg_hhTypeRE[,re_code_7])-table(bg_hhRel[role=="Householder",GEOID],bg_hhRel[role=="Householder",copath_re_AG])
#max(test)
#] 302
#min(test)
#] -301
#mean(test)
#] -0.0008967854
#median(test)
#] 0
#length(test[test==0])
#] 20081 #i.e., 15% are exact matches...
#length(test)
#] 130466

#order on age for both

bg_hhRel[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
bg_hhTypeRE[,("bg_RT14_match_id"):=
              paste0(GEOID,re_code_14,copath_over_64,alone,sex,as.character(1:.N)),
            by=.(GEOID,re_code_14,copath_over_64,alone,sex)]
bg_hhRel[role=="Householder",("bg_RT14_match_id"):=
           paste0(GEOID,re_code_14,copath_over_64,alone,sex,as.character(1:.N)),
         by=.(GEOID,re_code_14,copath_over_64,alone,sex)]
bg_hhRel[role=="Householder",c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                               "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                               "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                               "age_range_3","age_range_9","anyone_60",
                               "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RT14_match_id)]]
bg_hhTypeRE[,("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RT14_match_id)]]
nrow(bg_hhRel[role=="Householder"])-nrow(bg_hhTypeRE) #missing 117
table(bg_hhTypeRE[is.na(hh_individual_id),re_code_14]) #looking through O
table(bg_hhTypeRE[is.na(hh_individual_id),re_code_14],bg_hhTypeRE[is.na(hh_individual_id),no_spouse_sex],bg_hhTypeRE[is.na(hh_individual_id),copath_over_64])

#now for re_code_7
bg_hhTypeRE[is.na(hh_individual_id),("bg_RT7_match_id"):=
              paste0(GEOID,re_code_7,copath_over_64,alone,sex,as.character(1:.N)),
            by=.(GEOID,re_code_7,copath_over_64,alone,sex)]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RT7_match_id"):=
           paste0(GEOID,copath_re_AG,copath_over_64,alone,sex,as.character(1:.N)),
         by=.(GEOID,copath_re_AG,copath_over_64,alone,sex)]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                               "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                               "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                               "age_range_3","age_range_9","anyone_60",
                               "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RT7_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RT7_match_id)]]
nrow(bg_hhTypeRE[is.na(hh_individual_id)]) #5382406
#table(bg_hhTypeRE[is.na(hh_individual_id),re_code_14]) #looking through O; seems pretty balanced what's missing
#table(bg_hhTypeRE[is.na(hh_individual_id),re_code_14],bg_hhTypeRE[is.na(hh_individual_id),no_spouse_sex],bg_hhTypeRE[is.na(hh_individual_id),copath_over_64])
#table(bg_hhTypeRE[,copath_over_64])

#re_code_7 without sex and at tract level
bg_hhTypeRE[is.na(hh_individual_id),("bg_RT7s_match_id"):=
              paste0(tract,re_code_7,copath_over_64,alone,as.character(1:.N)),
            by=.(tract,re_code_7,copath_over_64,alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RT7s_match_id"):=
           paste0(tract,copath_re_AG,copath_over_64,alone,as.character(1:.N)),
         by=.(tract,copath_re_AG,copath_over_64,alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RT7s_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RT7s_match_id)]]
nrow(bg_hhTypeRE[is.na(hh_individual_id)]) #GEOID - 1134554 / tract - 456618 / 4.35%

#no re_code
bg_hhTypeRE[is.na(hh_individual_id),("bg_RT0_match_id"):=
              paste0(tract,copath_over_64,alone,as.character(1:.N)),
            by=.(tract,copath_over_64,alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RT0_match_id"):=
           paste0(tract,copath_over_64,alone,as.character(1:.N)),
         by=.(tract,copath_over_64,alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RT0_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RT0_match_id)]]
#need all householders to match - check
nrow(bg_hhTypeRE[is.na(hh_individual_id)]) #168223 / 1.6%

#no over_64
bg_hhTypeRE[is.na(hh_individual_id),("bg_RTa_match_id"):=
              paste0(tract,alone,as.character(1:.N)),
            by=.(tract,alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RTa_match_id"):=
           paste0(tract,alone,as.character(1:.N)),
         by=.(tract,alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RTa_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RTa_match_id)]]
#need all householders to match - check
nrow(bg_hhTypeRE[is.na(hh_individual_id)]) #66838 / .06%

#tract only
bg_hhTypeRE[is.na(hh_individual_id),("bg_RTt_match_id"):=
              paste0(tract,as.character(1:.N)),
            by=.(tract)]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RTt_match_id"):=
           paste0(tract,as.character(1:.N)),
         by=.(tract)]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RTt_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RTt_match_id)]]
nrow(bg_hhTypeRE[is.na(hh_individual_id)]) #19278 / .018%

#go ahead and pick them up at county level?
bg_hhTypeRE[,("county"):=substr(tract,1,5)]
bg_hhRel[,("county"):=substr(tract,1,5)]
bg_hhTypeRE[is.na(hh_individual_id),("bg_RTc_match_id"):=
              paste0(county,as.character(1:.N)),
            by=.(county)]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RTc_match_id"):=
           paste0(county,as.character(1:.N)),
         by=.(county)]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RTc_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RTc_match_id)]]
nrow(bg_hhTypeRE[is.na(hh_individual_id)]) #1151 / .01%
#test <- table(bg_hhRel[role=="Householder",county])-table(bg_hhTypeRE[,county])
#then just to get them all
bg_hhTypeRE[is.na(hh_individual_id),("bg_RToa_match_id"):=
              paste0(alone,as.character(1:.N)),
            by=.(alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RToa_match_id"):=
           paste0(alone,as.character(1:.N)),
         by=.(alone)]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RToa_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RToa_match_id)]]
nrow(bg_hhTypeRE[is.na(hh_individual_id)])
table(bg_hhTypeRE[is.na(hh_individual_id),alone])

#Get last, with assumption that point is to get everyone matches with bg_hhTypeRE as ground
bg_hhTypeRE[is.na(hh_individual_id),("bg_RTca_match_id"):=
              paste0(as.character(1:.N)),
            by=.()]
bg_hhRel[role=="Householder" & is.na(hh_id),("bg_RTca_match_id"):=
           paste0(as.character(1:.N)),
         by=.()]
bg_hhRel[role=="Householder" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RTca_match_id)]]
bg_hhTypeRE[is.na(hh_individual_id),("hh_individual_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RTca_match_id)]]
#nrow(bg_hhTypeRE[is.na(hh_individual_id)])==0

#table(bg_hhRel[,role])
#test <- table(bg_hhTypeRE[same_sex=="Same-sex unmarried partner households",GEOID])-table(bg_hhRel[role=="Same-sex unmarried partner",GEOID])
#length(test[test==0])#5012
#length(test)#18638

#for Same-sex unmarried partner
bg_hhTypeRE[same_sex=="Same-sex unmarried partner households",("bg_RTssup_match_id"):=
              paste0(alone,as.character(1:.N)),
            by=.(alone)]
bg_hhRel[role=="Same-sex unmarried partner",("bg_RTssup_match_id"):=
           paste0(alone,as.character(1:.N)),
         by=.(alone)]
bg_hhRel[role=="Same-sex unmarried partner",c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RTssup_match_id)]]
bg_hhTypeRE[same_sex=="Same-sex unmarried partner households",("hh_partner_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RTssup_match_id)]]
nrow(bg_hhTypeRE[is.na(hh_partner_id)])
#get rest

#for Opposite-sex unmarried partner
bg_hhTypeRE[same_sex=="Opposite-sex unmarried partner household" & is.na(hh_partner_id),("bg_RTssup_match_id"):=
              paste0(alone,as.character(1:.N)),
            by=.(alone)]
bg_hhRel[role=="Opposite-sex unmarried partner" & is.na(hh_id),("bg_RTssup_match_id"):=
           paste0(alone,as.character(1:.N)),
         by=.(alone)]
bg_hhRel[role=="Opposite-sex unmarried partner" & is.na(hh_id),c("hh_id","hh_race","hh_family","hh_family_type","hh_no_spouse_sex",
                                              "hh_match_type_5","hh_rent_own","hh_rel_in_house","own_kids",
                                              "hh_sex","hh_same_sex","couple_gender","hh_size_7","tenure_mortgage",
                                              "age_range_3","age_range_9","anyone_60",
                                              "codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(hh_id),list(race),list(family),list(family_type),list(no_spouse_sex),
                             list(match_type_5),list(rent_own),list(rel_in_house),list(own_kids),
                             list(sex),list(same_sex),list(couple_gender),list(hh_size_7),list(tenure),
                             list(age_range_3),list(age_range_9),list(anyone_60),
                             list(codom_hhTypeRE)),on=.(bg_RTssup_match_id)]]
bg_hhTypeRE[same_sex=="Opposite-sex unmarried partner household" & is.na(hh_partner_id),("hh_partner_id"):=
              bg_hhRel[.SD,list(individual_id),on=.(bg_RTssup_match_id)]]
nrow(bg_hhTypeRE[is.na(hh_partner_id)])







#table(bg_hhRel[,role],bg_hhRel[,age_range_2])
#order by HvL then not HvL (letters follow already) #sometimes seems to not order if only in i
bg_hhTypeRE <- bg_hhTypeRE[order(GEOID,-re_code_14,alone,sex)]
bg_hhRel <- bg_hhRel[order(GEOID,-copath_HvL,alone,sex)]

#Counting out, not sampling; could hold off for matching on codom later, and go ahead and match with copath_re_AG first; then only order for last bit?
bg_hhTypeRE[,("bg_RT_match_id"):=
              paste0(GEOID,alone,sex,as.character(1:.N)),
          by=.(GEOID,alone,sex)]
bg_hhRel[role=="Householder",("bg_RT_match_id"):=
           paste0(GEOID,alone,sex,as.character(1:.N)),
          by=.(GEOID,alone,sex)]
#putting codom for re_code so we can order later numerically with cell sizes from codom_etc.; a co-path is the factor name with the codom
bg_hhRel[role=="Householder",c("copath_re_code","copath_race","family","family_type","no_spouse_sex","codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(re_code_14),list(race),list(family),list(family_type),
                             list(no_spouse_sex),list(codom_hhTypeRE)),on=.(bg_RT_match_id)]]
bg_hhTypeRE[,("matched_rel"):=
              bg_hhRel[.SD,list(copath_re_code),on=.(bg_RT_match_id)]]
#nrow(bg_hhRel[role=="Householder"])-nrow(bg_hhRel[!is.na(copath_re_code)]) #8321400
#table(bg_hhTypeRE[is.na(matched_rel),sex]) #only 65 with sex not matched

#same, without sex - basically distributing according to that remnant of not living alone without sex (married and some non-family)
bg_hhTypeRE[is.na(matched_rel),("bg_RTA_match_id"):=
              paste0(GEOID,alone,as.character(1:.N)),
            by=.(GEOID,alone)]
bg_hhRel[role=="Householder"&is.na(copath_re_code),("bg_RTA_match_id"):=
           paste0(GEOID,alone,as.character(1:.N)),
         by=.(GEOID,alone)]
bg_hhRel[role=="Householder"&is.na(copath_re_code),c("copath_re_code","copath_race","family","family_type","no_spouse_sex","codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(re_code_14),list(race),list(family),list(family_type),
                             list(no_spouse_sex),list(codom_hhTypeRE)),on=.(bg_RTA_match_id)]]
bg_hhTypeRE[is.na(matched_rel),("matched_rel"):=
              bg_hhRel[.SD,list(copath_re_code),on=.(bg_RTA_match_id)]]
#nrow(bg_hhTypeRE[is.na(matched_rel)]) #194858, less than two percent, but close to total diff. by bg for hh (207091)
#need all of bg_hhTypeRE info to move over, but after getting the tract Relative data, so it lines up with households

test_bg_re_code <- table(bg_hhRel[,copath_re_code],bg_hhRel[,GEOID])-table(bg_hhTypeRE[,re_code_14],bg_hhTypeRE[,GEOID])
sum(abs(test_bg_re_code)) #194858
mean(abs(test_bg_re_code)) #.7467
max(abs(test_bg_re_code)) #72 - i.e., small amounts per re_code difference for each bg
sum(table(bg_hhTypeRE[,re_code_14])-(table(bg_hhRel[,copath_re_code])))+117==nrow(bg_hhRel[role=="Householder"])-nrow(bg_hhRel[!is.na(copath_re_code)])
table(bg_hhTypeRE[,re_code_14])/(table(bg_hhRel[,copath_re_code])) #(with ordering by copath_HvL)
#remember it would never be a perfect fit b/c of householders being defined differently. 
#       I        J        K        L        M        N        O        P        Q        R        S        T        U        V 
#.  1.037865 1.005096 1.006545 1.001022 1.003962 1.005003 1.001432 1.006900 1.000172 1.000456 1.000172 1.000661 1.000142 1.000024 
##running it without reordering by copath_HvL is close for I, O, R, and U (even better, by a bit), but much better for most to have the order
#without ordering:
#       I        J        K        L        M        N        O        P        Q        R        S        T        U        V 
#   1.002726 1.027935 1.569854 1.008784 1.451405 1.557237 1.000028 1.131656 1.493909 1.000034 1.011502 1.421596 1.000011 1.000169 
#just matching on the race without constraining on H is much worse, but you can still get an answer


#how many of 194858 can be found just by moving to tract level? - might be good to wait on this, too? There are other bg tables, after all...
#bg_hhTypeRE[is.na(matched_rel),("tr_RTA_match_id"):=
#              paste0(tract,alone,as.character(1:.N)),
#            by=.(tract,alone)]
#bg_hhRel[role=="Householder"&is.na(copath_re_code),("tr_RTA_match_id"):=
#           paste0(tract,alone,as.character(1:.N)),
#         by=.(tract,alone)]
#bg_hhRel[role=="Householder"&is.na(copath_re_code),c("copath_re_code","copath_race","family","family_type","no_spouse_sex","codom_hhTypeRE"):=
#           bg_hhTypeRE[.SD,c(list(re_code),list(race),list(family),list(family_type),
#                             list(no_spouse_sex),list(codom_hhTypeRE)),on=.(tr_RTA_match_id)]]
#bg_hhTypeRE[is.na(matched_rel),("matched_rel"):=
#              bg_hhRel[.SD,list(copath_re_code),on=.(tr_RTA_match_id)]]
#nrow(bg_hhTypeRE[is.na(matched_rel)]) #66838
#nrow(bg_hhRel[role=="Householder"&is.na(copath_re_code)]) #194975
#table(bg_hhTypeRE[,re_code])/(table(bg_hhRel[,copath_re_code]))
#     I        J        K        L        M        N        O        P        Q        R        S        T        U        V 
#1.012974 1.001277 1.001835 1.000245 1.000864 1.001636 1.000411 1.001560 1.000043 1.000118 1.000000 1.000000 1.000026 1.000003

#Leaving 194975 unfinished, till we have some more paths to match

rm(tr_hhRelH)
rm(tr_hhRelHnotP)
rm(tr_hhRelI)
rm(tr_hhRelR)
rm(tr_hhRelRE)
rm(tr_hh65RelE)
rm(tr_hh65RelR)
#broken, below here...

#bgSARE has 23 age-groups, re_code is 14, and 2 for sex - move bg_hhRel and household stuff over?? Need a plan!!!

#move rest of householders over - this is broken
bg_hhTypeRE[is.na(matched_rel),("bg_RTF_match_id"):=
              paste0(GEOID,alone,as.character(100000+sample(1:.N))),
            by=.(GEOID,alone)]
bg_hhRel[is.na(re_code)&is.na(sex),("bg_RTF_match_id"):=
           paste0(GEOID,alone,as.character(100000+sample(1:.N))),
         by=.(GEOID,alone)]
#look at how the other factors fill so it still matches? 
bg_hhRel[is.na(re_code)&is.na(sex),c("re_code","race","family","family_type","no_spouse_sex","codom_hhTypeRE"):=
           bg_hhTypeRE[.SD,c(list(re_code),list(race),list(family),list(family_type),
                             list(no_spouse_sex),list(codom_hhTypeRE)),on=.(bg_RTF_match_id)]]
bg_hhTypeRE[is.na(matched_rel),("matched_rel"):=
              bg_hhRel[.SD,list(re_code),on=.(bg_RTF_match_id)]]


#match all the way back up to bg_hhTypeTenure
#bg_hhTypeTenure[,("bg_TTre_match_id"):=
#                  paste0(GEOID,no_spouse_sex,as.character(100000+sample(1:.N))),
#                by=.(GEOID,no_spouse_sex)]
#bg_hhTypeRE[,("bg_TTre_match_id"):=
#               paste0(GEOID,match_type_5,as.character(100000+sample(1:.N))),
#             by=.(GEOID,match_type_5)]
#bg_hhTypeRE[,c("rent_own","rel_in_house","own_kids","age_range_3",
#                   "sex","same_sex","couple_gender","alone","family",
#                   "family_type_4","family_type_7"):=
#              bg_hhTypeTenure[.SD,c(list(rent_own),list(rel_in_house),list(own_kids),list(age_range_3),
#                                     list(sex),list(same_sex),list(couple_gender),list(alone),
#                                     list(family),list(family_type),list(family_type_7)),on=.(bg_TTre_match_id)]]
#bg_hhTypeTenure[,("match_TTre"):=
#               bg_hhTypeRE[.SD,list(match_type_5),on=.(bg_TTre_match_id)]]
#nrow(bg_hhTypeTenure[is.na(match_TTre)])



#match rest of H to the Hispanic or Latino, then bg will pull out the ones that don't match
#tr_hhRelR[re_code!="A",("bg_relEth_match_id"):=
#            paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
#          by=.(GEOID,household,role,sex,alone,age_range_2)]
#tr_hhRelH[is.na(re_code_HvL),("bg_relEth_match_id"):=
#            paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
#          by=.(GEOID,household,role,sex,alone,age_range_2)]
#tr_hhRelR[re_code!="A",c("re_code_HvL","codom_hhRelEth"):=
#            tr_hhRelH[.SD,c(list(re_code_HvL),list(codom_hhRelRE)),on=.(bg_relEth_match_id)]]


#match then move up to P16 - need to think through codomain and copath logic for matching
#tr_hhRelR[,("bg_relBG_match_id"):=
#             paste0(GEOID,household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
#           by=.(GEOID,household,role,sex,alone,age_range_2)]
#bg_hhRel[,("bg_relBG_match_id"):=
#               paste0(substr(GEOID,1,13),household,role,sex,alone,age_range_2,as.character(100000+sample(1:.N))),
#             by=.(substr(GEOID,1,13),household,role,sex,alone,age_range_2)]
#bg_hhRel[,c("tract","codom_hhRelRE"):=
#           tr_hhRelR[.SD,c(list(GEOID),list(codom_hhRelRE)),on=.(bg_relBG_match_id)]]
##test <- table(bg_hhRel[,tract])==table(tr_hhRelRE[,GEOID])
##length(test[test==FALSE])
#rm(tr_hhRelRE)

#nrow(bg_hhTypeRE)-nrow(tr_hhRelRE[re_code%in%c(LETTERS[1:7])&role_7=="Householder"]) 117 different??? - see above












#Put tenure re_code_14 and size_tenure together, then order on re_code_14 for multi_gen with size decreasing by order


#redundant with H4, which has by block_group
#groupname <- "HCT1" #TENURE BY HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER BY RACE OF HOUSEHOLDER - gives everything at tract keeps from guessing about race/eth
#geo_type <- "tract"
#api_type <- "dec/dhc"
#path_suff <- "est"
#tr_hhTenureRE_data_from_census <- 
#  census_tract_get(censusdir, vintage, state, censuskey, 
#                   groupname,county = "*",
#                   api_type,path_suff)
#if(names(tr_hhTenureRE_data_from_census)[11]=="label_1"){
#  #labels determined by hand; didn't do re_code in name for some reason; owner_occupied, Latino, AIAN missing (about 33k in TX)
#  label_c1 <- c("tenure","HvL","race") 
#  #row_c1 by hand
#  tr_hhTenureRE_data_from_census[,"label_3":=fcase(label_1=="Owner occupied"&
#                                                     label_2=="Hispanic or Latino householder"&
#                                                     is.na(label_3),"Householder who is American Indian and Alaska Native alone",default = label_3)]
#  row_c1 <- c(unique(tr_hhTenureRE_data_from_census[!is.na(label_3),name]))
#  test_total_pop <- tests_download_data(tr_hhTenureRE_data_from_census,label_c1,row_c1,state=state) #seems to not get right total row!!
#  tr_hhTenureRE_data <- relabel(tr_hhTenureRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
#  write_relabel(tr_hhTenureRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
#}else{
#  print("Using already given labels; no rewrite.")
#  tr_hhTenureRE_data <- tr_hhTenureRE_data_from_census
#}
##reshape a bit and make list of individuals
#Geoids <- colnames(tr_hhTenureRE_data[,.SD,.SDcols = startsWith(names(tr_hhTenureRE_data),state)])
#tr_hhTenureRE_melted <- melt(tr_hhTenureRE_data, id.vars = c("tenure","HvL","race"), measure.vars = Geoids,
#                           value.name = "codom_tr_hhTenureRE", variable.name = "GEOID")
#tr_hhTenureRE_melted[,("codom_tr_hhTenureRE"):=fcase(tenure=="Owner occupied"&HvL=="Hispanic or Latino householder"&
#                                                       race=="Householder who is American Indian and Alaska Native alone",
#                                                     as.numeric(.SD[tenure=="Owner occupied"&HvL=="Hispanic or Latino householder"&
#                                                                      race=="Householder who is American Indian and Alaska Native alone",codom_tr_hhTenureRE])-
#                                                       sum(as.numeric(.SD[tenure=="Owner occupied"&HvL=="Hispanic or Latino householder"&
#                                                                        race!="Householder who is American Indian and Alaska Native alone",codom_tr_hhTenureRE]),na.rm = TRUE),
#                                                     default=as.numeric(codom_tr_hhTenureRE)),by=.(GEOID)]
#tr_hhTenureRE <- as.data.table(lapply(tr_hhTenureRE_melted[,.SD],rep,tr_hhTenureRE_melted[,codom_tr_hhTenureRE])) #right number
#tr_hhTenureRE[,("re_code"):=fcase(HvL=="Hispanic or Latino householder"&race=="Householder who is White alone","P",
#                                  HvL=="Not Hispanic or Latino householder"&race=="Householder who is White alone","I",
#                                  HvL=="Hispanic or Latino householder"&race=="Householder who is Black or African American alone","Q",
#                                  HvL=="Not Hispanic or Latino householder"&race=="Householder who is Black or African American alone","J",
#                                  HvL=="Hispanic or Latino householder"&race=="Householder who is American Indian and Alaska Native alone","R",
#                                  HvL=="Not Hispanic or Latino householder"&race=="Householder who is American Indian and Alaska Native alone","K",
#                                  HvL=="Hispanic or Latino householder"&race=="Householder who is Asian alone","S",
#                                  HvL=="Not Hispanic or Latino householder"&race=="Householder who is Asian alone","L",
#                                  HvL=="Hispanic or Latino householder"&race=="Householder who is Native Hawaiian and Other Pacific Islander alone","T",
#                                  HvL=="Not Hispanic or Latino householder"&race=="Householder who is Native Hawaiian and Other Pacific Islander alone","M",
#                                  HvL=="Hispanic or Latino householder"&race=="Householder who is Some Other Race alone","U",
#                                  HvL=="Not Hispanic or Latino householder"&race=="Householder who is Some Other Race alone","N",
#                                  HvL=="Hispanic or Latino householder"&race=="Householder who is Two or More Races","V",
#                                  HvL=="Not Hispanic or Latino householder"&race=="Householder who is Two or More Races","O",
#                                  default = "not found")]
#rm(tr_hhTenureRE_data_from_census)
#rm(tr_hhTenureRE_data)
#rm(tr_hhTenureRE_melted)

#moved from schematic_sam.Rmd - moved to data.table only

