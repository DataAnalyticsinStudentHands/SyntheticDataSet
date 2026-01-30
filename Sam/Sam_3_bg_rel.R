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

#match for over_64 
tr_hhRelRE[,("age_range_2"):=fcase(is.na(age_range_2)&household=="In households","over_17",
                                   household=="In group quarters","unknown_GQ",
                                   default = age_range_2)]
tr_hh65RelRE[,("age_range_2"):=fcase(household=="In group quarters","unknown_GQ",default = "over_17")]
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
#table(tr_hhRelRE[,over_64])==table(tr_hh65RelRE[,re_code]) #using re_code to test all moved over
#table(tr_hhRelRE[,household],tr_hhRelRE[,over_64])
#nrow(tr_hhRelRE[!is.na(over_64)])==nrow(tr_hh65RelRE)
#make sure not to lose group quarters!!!
tr_hhRelRE[,("over_64"):=fcase(!is.na(over_64),"over_64",default = "under_65")]
#age in sex is better than in age_range_2 (roles like householders may have under 18 that are not listed)
tr_hhRelRE[,("age_range_3"):=fcase(age_range_2=="unknown_GQ","unknown_GQ",
                                   age_range_2=="under_18","Under 18 years",
                                over_64=="over_64","65 years and over",
                                default = "18 to 64 years")]

#a little artificial, but don't have much to go on
tr_hhRelRE[,("age_range_6"):=fcase(age_range_2=="under_18","Under 18 years",
                                   age_range_2=="over_17" & str_detect(role,"child"),"18 to 24 years",
                                   over_64=="over_64","65 years and over",
                                   over_64=="under_65" & role=="Householder","18 to 64 years",
                                   over_64=="under_65" & str_detect(role,"sex"),"18 to 64 years", #all 4 spouse/partner designations
                                   #need to match on institutionalized separately because some are under_18 but not given in age_range_2
                                   over_64=="under_65" & str_detect(role,"population"),"unknown_GQ",#"Institutionalized under 65 years",
                                   over_64=="under_65" & str_detect(role,"relatives"),"18 to 64 years",
                                   over_64=="under_65" & str_detect(role,"Son-in"),"18 to 34 years",
                                   over_64=="under_65" & str_detect(role,"Brother or "),"18 to 64 years",
                                   over_64=="under_65" & str_detect(role,"Parent"),"55 to 64 years",
                                   default = "default 35 to 64 years")]
#table(tr_hhRelRE[,sex],tr_hhRelRE[,role],useNA = "ifany")
#following old rules on assigning male to household head, if male present in household; sex is only given for Householder role
#numbers don't add up if all opposite sex spouse/partners are assigned female; there are too many females to match the male householders. 
#tr_hhRelRE[,("sex"):=fcase(str_detect(role,"Opposite"),"Female",default = sex)]
#table(tr_hhRelRE[,role],tr_hhRelRE[,age_range_6],useNA = "ifany")
#rm(tr_hh65RelRE)

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
#nrow(tr_hhSARE)==nrow(tr_hhRelRE[household=="In households"]) #FALSE; True after fixes, below
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
#5221 more in tr_hhRelRE than expected for matching in tr_hhSARE
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
#table(tr_hhRelRE[household=="In households"&!is.na(age_range_23),sex])-table(tr_hhRelRE[sex!="Under 18 years",sex]) #-2979, -3928 - perhaps householders under18?
tr_hhRelRE[,("age_range_3"):=fcase(household=="In households"&is.na(age_range_23)&sex!="Under 18 years","Under 18 years",
                                   default = age_range_3)]
tr_hhRelRE[,("age_range_6"):=fcase(household=="In households"&is.na(age_range_23)&sex!="Under 18 years","Under 18 years",
                                   default = age_range_6)]
tr_hhRelRE[,("age_range_23"):=fcase(household=="In households"&is.na(age_range_23)&sex!="Under 18 years","17 years",
                                    default = age_range_23)]
#do a match for 6907 17yo householders to get the match_relre
#tr_hhSARE[age_range_23=="15 to 17 years",("tr_SARERel1_match_id"):=
#            paste0(GEOID,re_code,sex,as.character(100000+sample(1:.N))),
#          by=.(GEOID,re_code,sex)]
#tr_hhRelRE[age_range_23=="17 years",("tr_SARERel1_match_id"):=
#             paste0(GEOID,re_code,sex,as.character(100000+sample(1:.N))),
#           by=.(GEOID,re_code,sex)]
#tr_hhSARE[age_range_23=="15 to 17 years",("match_relre"):= 
#            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERel1_match_id)]]

#table(tr_hhSARE[!is.na(match_relre),re_code],tr_hhSARE[!is.na(match_relre),age_range_6],useNA = "ifany")-table(tr_hhRelRE[household=="In households"&!is.na(age_range_23),re_code],tr_hhRelRE[household=="In households"&!is.na(age_range_23),age_range_6],useNA = "ifany")
#all difference is in Under 18 
#opposite sex spouse and partner is more than total male householders, so some of them must be male, with female head of household (breaking rule b4 2020); in 2020, householder is first adult listed in census response and/or person on lease/homeowner
#do not assign sex to opposite sex partner/spouse in 2020 or later

#tr_hhSARE[is.na(match_relre),("tr_SARERela_match_id"):=
#            paste0(GEOID,re_code,age_range_3,age_range_6,as.character(100000+sample(1:.N))),
#          by=.(GEOID,re_code,age_range_3,age_range_6)]
#tr_hhRelRE[household=="In households"&is.na(age_range_23),("tr_SARERela_match_id"):=
#             paste0(GEOID,re_code,age_range_3,age_range_6,as.character(100000+sample(1:.N))),
#           by=.(GEOID,re_code,age_range_3,age_range_6)]
#tr_hhSARE[is.na(match_relre),("match_relre"):= 
#            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERela_match_id)]]
#tr_hhRelRE[household=="In households"&is.na(age_range_23),c("age_range_23","sex"):=
#             tr_hhSARE[.SD,c(list(age_range_23),list(sex)),on=.(tr_SARERela_match_id)]]
##nrow(tr_hhRelRE[household=="In households"&is.na(age_range_23)]) #5558 just about .01% still not matching
#
##make sure it still respects age_range_3 on last little bit
#tr_hhSARE[is.na(match_relre),("tr_SARERelb_match_id"):=
#            paste0(GEOID,re_code,age_range_3,as.character(100000+sample(1:.N))),
#          by=.(GEOID,re_code,age_range_3)]
#tr_hhRelRE[household=="In households"&is.na(age_range_23),("tr_SARERelb_match_id"):=
#             paste0(GEOID,re_code,age_range_3,as.character(100000+sample(1:.N))),
#           by=.(GEOID,re_code,age_range_3)]
#tr_hhSARE[is.na(match_relre),("match_relre"):= 
#            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERelb_match_id)]]
#tr_hhRelRE[household=="In households"&is.na(age_range_23),c("age_range_23","sex"):=
#             tr_hhSARE[.SD,c(list(age_range_23),list(sex)),on=.(tr_SARERelb_match_id)]]
#nrow(tr_hhRelRE[household=="In households"&is.na(age_range_23)]) #1294 - stop trying for last bit?

#table(tr_hhSARE[,re_code],tr_hhSARE[,age_range_6],useNA = "ifany")-table(tr_hhRelRE[household=="In households",re_code],tr_hhRelRE[household=="In households",age_range_6],useNA = "ifany")
#table(tr_hhRelRE[household=="In households"&is.na(age_range_23),age_range_6])
#table(tr_hhRelRE[is.na(age_range_23)&household=="In households",age_range_6],useNA = "ifany")
#table(tr_hhSARE[is.na(match_relre),age_range_23],useNA = "ifany")
#nrow(tr_hhSARE[is.na(match_relre)])==nrow(tr_hhRelRE[is.na(age_range_23)&household=="In households"])

#tr_hhRelRE_inst <- tr_hhRelRE[str_detect(role,"nstit")]
#tr_hhRelRE <- tr_hhRelRE[!str_detect(role,"nstit")]
#tr_hhRelRE <- tr_hhRelRE[order(match(role,c("Biological child","Grandchild","Adopted child","Stepchild","Foster child",
#                                                "Householder","Opposite-sex spouse","Opposite-sex unmarried partner",
#                                                "Same-sex spouse","Same-sex unmarried partner","Son-in-law or daughter-in-law",
#                                                "Other nonrelatives","Other relatives","Brother or sister","Parent",
#                                                "Parent-in-law")))]
#tr_hhSARE <- tr_hhSARE[order(match(age_range_6,c("Under 18 years","18 to 24 years","18 to 34 years","18 to 64 years",
#                                                 "55 to 64 years","65 years and over")))]
##pick up last 1%
#tr_hhSARE[is.na(match_relre),("tr_SARERelc_match_id"):=
#            paste0(GEOID,re_code,as.character(100000+(1:.N))), #re_code per GEOID should still match, then just keep order when assigning count
#          by=.(GEOID,re_code)]
#tr_hhRelRE[household=="In households"&is.na(age_range_23),("tr_SARERelc_match_id"):=
#             paste0(GEOID,re_code,as.character(100000+(1:.N))),
#           by=.(GEOID,re_code)]
#tr_hhSARE[is.na(match_relre),("match_relre"):= 
#            tr_hhRelRE[.SD,list(re_code),on=.(tr_SARERelc_match_id)]]
#tr_hhRelRE[household=="In households"&is.na(age_range_23),c("age_range_23","sex"):=
#             tr_hhSARE[.SD,c(list(age_range_23),list(sex)),on=.(tr_SARERelc_match_id)]]
#nrow(tr_hhRelRE[is.na(age_range_23)&household=="In households"]) #0

#tr_hhRelRE[,("tr_SARERelc_match_id"):=NULL]
#tr_hhRelRE <- rbind(tr_hhRelRE,tr_hhRelRE_inst)

#rm(tr_hhRelRE_inst)

#move tr_hh18Rel with kid_age_range
#need to track the _tr stuff and test!!!!
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

#tr_hhRel18[,("kid_age_range_1"):=fcase(kid_age_range=="Under 3 years" |
#                                         kid_age_range=="3 and 4 years", "Under 5 years",
#                                       kid_age_range=="5 years" |
#                                         kid_age_range=="14 years" |
#                                         kid_age_range=="12 and 13 years" |
#                                         kid_age_range=="6 to 11 years", "5 to 14 years",
#                                       default = kid_age_range)] #also gets 15 to 17 years for both 
#
#5988 more 15 to 17 years in bg_SARE than in tr_hhRel18; 5 to 14 and under 5 match; all straight from census...
#nrow(bg_SARE[age_num<18])-nrow(tr_hhRel18) #5988 - not at all sure what to do with that; are they perhaps the spouses and householders?

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
bg_SARE[,("kid_age_range"):=fcase(age_num==0,sample(c("Under 3 years","3 and 4 years"),.N,c(.576,.423),replace=TRUE),
                                  age_num==5,sample(c("5 years","6 to 11 years"),.N,c(.2,.8),replace=TRUE),
                                  age_num==10,sample(c("6 to 11 years","12 and 13 years","14 years"),.N,c(.395,.41,.195),replace=TRUE),
                                  age_num==15,"15 to 17 years",
                                default = age_range)]
#fiddling with percentages to come close, but it simply does not match
bg_SARE[,("age_range_6"):=fcase(age_num<18,"Under 18 years",
                                age_num>17 & age_num<25,sample(c("18 to 24 years","18 to 64 years"),.N,c(.6,.4),replace=TRUE),
                                age_num>24 & age_num<35,sample(c("18 to 34 years","18 to 64 years"),.N,c(.2,.8),replace=TRUE),
                                age_num>34 & age_num<55,sample(c("55 to 64 years","18 to 64 years"),.N,c(.05,.95),replace=TRUE),#start with more, .1,.9 gets you close to matching total numbers
                                age_num>64,"65 years and over",default = "18 to 64 years"
)]
bg_SARE[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]


#get matches by age for tr_hhRel18 bg_SARE; really only distributing child_role by age and tract
#note: nrow(bg_SARE[age_num<18])-nrow(tr_hhRel18) = 5988
bg_SARE[age_num<18,("bg_Rel18_match_id"):=
          paste0(tract,kid_age_range,as.character(100000+(1:.N))),
        by=.(tract,kid_age_range)]
tr_hhRel18[,("bg_Rel18_match_id"):=
             paste0(GEOID,kid_age_range,as.character(100000+(1:.N))),
           by=.(GEOID,kid_age_range)]
tr_hhRel18[,("re_code_14"):=
             bg_SARE[.SD,list(re_code),on=.(bg_Rel18_match_id)]]
bg_SARE[age_num<18,("child_role_tr"):=
          tr_hhRel18[.SD,list(child_role),on=.(bg_Rel18_match_id)]]
#nrow(tr_hhRel18[is.na(re_code_14)]) #209979 (3%)
#nrow(bg_SARE[age_num<18&is.na(child_role)]) #215967 - 5988 = 209979
#pick up remaining, no need for matching on age, since child_role is very broad
bg_SARE[is.na(child_role_tr)&age_num<18,("bg_Rel18a_match_id"):=
          paste0(tract,as.character(100000+(1:.N))),
        by=.(tract)]
tr_hhRel18[is.na(re_code_14),("bg_Rel18a_match_id"):=
             paste0(GEOID,as.character(100000+(1:.N))),
           by=.(GEOID)]
tr_hhRel18[is.na(re_code_14),("re_code_14"):=
             bg_SARE[.SD,list(re_code),on=.(bg_Rel18a_match_id)]]
bg_SARE[is.na(child_role_tr)&age_num<18,("child_role_tr"):=
          tr_hhRel18[.SD,list(child_role),on=.(bg_Rel18a_match_id)]]
#nrow(tr_hhRel18[is.na(re_code_14)]) #0
#nrow(bg_SARE[age_num<18&is.na(child_role_tr)]) #5988 #need to be sure that household_tr is respected for child_role later; number in GQ matches for tr_hhRel18

#this will assign too many own child and other children, so have to take them out again for gq kids; there are 5,988 individuals under 18 in bg_SARE 
#not in tr_hhRel18, but looks like under_18 GQ matches with GQ numbers by age, so distribute among Other children and Own at tract 
bg_SARE[,("child_role_tr"):=fcase(is.na(child_role_tr)&age_num<18,sample(c("Other children","Own child"),.N,c(.16,.84),replace=TRUE),
                               is.na(child_role_tr)&age_num>17,"Not child",
                               default = child_role_tr)]
#letting age sort itself out in age_range match, since only own children are listed over_17
tr_hhRelRE[,("child_role_tr"):=fcase(role_7=="Biological child","Own child",
                                  role_7=="Adopted child" | role_7=="Foster child" | role_7=="Grandchild" | role_7=="Stepchild","Other children",
                                  str_detect(role_7,"population"),role_7,
                                  default = "Not child")]
#table(tr_hhRelRE[is.na(age_range_23),household],tr_hhRelRE[is.na(age_range_23),age_range_3],useNA = "ifany")

tr_hhRelRE[,("age_range_23"):=fcase(is.na(age_range_23),age_range_3,default = age_range_23)]

tr_hhRelR <- tr_hhRelRE[!re_code%in%c("H","I")] 
tr_hhRelH <- tr_hhRelRE[re_code=="H"]
tr_hhRelI <- tr_hhRelRE[re_code=="I"]
#I doesn't completely match inside of "A" on sex, role, age_range_23, on original file! which breaks the definitions!; leaving last .7 % unmatched
#can't figure out if the problem is relative at re_code or way totals were generated; try doing best possible on households and then gq????
#don't need household=="In households" because sex is NA and age_range_6 as unknown_GQ for both I and R
#Should be 3024768 P and 11584597 I to make 14609365 A
tr_hhRelR[re_code=="A",("tr_SARI_match_id"):=
          paste0(GEOID,sex,role_7,age_range_23,as.character(100000+sample(1:.N))),
        by=.(GEOID,sex,role_7,age_range_23)]
tr_hhRelI[,("tr_SARI_match_id"):=
            paste0(GEOID,sex,role_7,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,sex,role_7,age_range_23)]
tr_hhRelR[re_code=="A",("re_code_I"):= #all should equal "I" and 
            tr_hhRelI[.SD,list(re_code),on=.(tr_SARI_match_id)]]
tr_hhRelI[,("matched_trSAI"):=
           tr_hhRelR[.SD,list(re_code),on=.(tr_SARI_match_id)]]
#nrow(tr_hhRelI[is.na(matched_trSAI)]) #180346/nrow(tr_hhRelI)# 1.5%; 
#quick try to get more without sex
tr_hhRelR[re_code=="A"&is.na(re_code_I),("tr_SARIa_match_id"):=
            paste0(GEOID,role_7,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,role_7,age_range_23)]
tr_hhRelI[is.na(matched_trSAI),("tr_SARIa_match_id"):=
            paste0(GEOID,role_7,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,role_7,age_range_23)]
tr_hhRelR[re_code=="A"&is.na(re_code_I),("re_code_I"):= #all should equal "I" and 
            tr_hhRelI[.SD,list(re_code),on=.(tr_SARIa_match_id)]]
tr_hhRelI[is.na(matched_trSAI),("matched_trSAI"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARIa_match_id)]]
#nrow(tr_hhRelI[is.na(matched_trSAI)]) #106477/nrow(tr_hhRelI)# .9%
#table(tr_hhRelI[is.na(matched_trSAI),age_range_23],useNA = "ifany") #seems to have matched all Under 18 and UnknownGQ, missing other years
#just allow the 106477 to not match for I, picking up with matches on A, later

#move I to bg_SARE, then H, then rest of re_code_7; remember is.na(sex) & is.na(age_range) for group quarters

#only households, first, because we don't have ages in group quarters
tr_hhRelI[household=="In households",("tr_bg_SAI_match_id"):=
            paste0(GEOID,sex,child_role_tr,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,sex,child_role_tr,age_range_23)]
bg_SARE[re_code=="I",("tr_bg_SAI_match_id"):=
          paste0(tract,sex,child_role_tr,age_range,as.character(100000+sample(1:.N))),
        by=.(tract,sex,child_role_tr,age_range)]
tr_hhRelI[household=="In households",("re_code_7"):= #should all be "A"
            bg_SARE[.SD,list(re_code_7),on=.(tr_bg_SAI_match_id)]]
bg_SARE[re_code=="I",c("alone_tr","role_tr","role_7_tr","household_tr"):=
          tr_hhRelI[.SD,c(list(alone),list(role),list(role_7),list(household)),on=.(tr_bg_SAI_match_id)]]
#nrow(tr_hhRelI[is.na(re_code_7)]) #6639643
#nrow(bg_SARE[!is.na(role_tr)])/nrow(tr_hhRelI) #43%


#finish without child_role; group quarters do not have sex or age_range, and only Householders have sex and full age_range_23
#age_range_3 on tr_hhRelI has: 17 years, 18 to 64 years, 65 years and over, Under 18 years, unknown_GQ
#age_range_3 on bg_SARE has 18 to 64 years, 65 years and over,    Under 18 years
tr_hhRelI[household=="In households"&is.na(re_code_7),("tr_bg_SAIa_match_id"):=
            paste0(GEOID,age_range_3,as.character(100000+sample(1:.N))),
          by=.(GEOID,age_range_3)]
bg_SARE[re_code=="I"&is.na(household_tr),("tr_bg_SAIa_match_id"):=
          paste0(tract,age_range_3,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3)]
tr_hhRelI[household=="In households"&is.na(re_code_7),("re_code_7"):= #should all be "A"
            bg_SARE[.SD,list(re_code_7),on=.(tr_bg_SAIa_match_id)]]
bg_SARE[re_code=="I"&is.na(household_tr),c("alone_tr","role_tr","household_tr","child_role_tr"):=
          tr_hhRelI[.SD,c(list(alone),list(role),list(household),list(child_role_tr)),on=.(tr_bg_SAIa_match_id)]]
#table(bg_SARE[,household_tr],bg_SARE[,child_role])
#table(tr_hhRel18[,household],tr_hhRel18[,child_role])
#nrow(tr_hhRelI[is.na(re_code_7)&household=="In households"]) #60496
#nrow(tr_hhRelI[household=="In group quarters"])
#nrow(bg_SARE[!is.na(role_tr)])

#need to match from bg_SARE to tr_hhRelH, then tr_hhRelH back to tr_hhRelR
tr_hhRelH[household=="In households",("tr_bg_SAE_match_id"):=
           paste0(GEOID,sex,child_role_tr,age_range_23,as.character(100000+sample(1:.N))),
         by=.(GEOID,sex,child_role_tr,age_range_23)]
bg_SARE[HvL=="Hispanic or Latino",("tr_bg_SAE_match_id"):= 
           paste0(tract,sex,child_role_tr,age_range,as.character(100000+sample(1:.N))),
         by=.(tract,sex,child_role_tr,age_range)]
tr_hhRelH[household=="In households",("re_code_7"):=
           bg_SARE[.SD,list(re_code_7),on=.(tr_bg_SAE_match_id)]]
bg_SARE[HvL=="Hispanic or Latino",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelH[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SAE_match_id)]]
#nrow(tr_hhRelH[is.na(re_code_7)])#8079375. Matching almost all (-317) householders; didn't have any 17 yo householders
#table(tr_hhRelH[is.na(re_code_7),role])


tr_hhRelH[household=="In households"&is.na(re_code_7),("tr_bg_SAEa_match_id"):=
            paste0(GEOID,age_range_3,as.character(100000+sample(1:.N))),
          by=.(GEOID,age_range_3)]
bg_SARE[HvL=="Hispanic or Latino"&is.na(household_tr),("tr_bg_SAEa_match_id"):= 
          paste0(tract,age_range_3,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3)]
tr_hhRelH[household=="In households"&is.na(re_code_7),("re_code_7"):=
            bg_SARE[.SD,list(re_code_7),on=.(tr_bg_SAEa_match_id)]]
bg_SARE[HvL=="Hispanic or Latino"&is.na(household_tr),c("alone_tr","role_tr","household_tr","child_role_tr"):=
          tr_hhRelH[.SD,c(list(alone),list(role),list(household),list(child_role_tr)),on=.(tr_bg_SAEa_match_id)]]
#nrow(tr_hhRelH[is.na(re_code_7)&household=="In households"])#173126 #about 1.5%; don't try to pick up
 
#put onto tr_hhRelR, so that we're not drawing from I or P-V inappropriately
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
#nrow(tr_hhRelH[is.na(matched_trSAH)]) #1368020 
#pick up more by relaxing age_range and role
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("tr_SARHa_match_id"):=
            paste0(GEOID,household,role_7,re_code,sex,age_range_3,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,re_code,sex,age_range_3)]
tr_hhRelH[is.na(matched_trSAH),("tr_SARHa_match_id"):=
            paste0(GEOID,household,role_7,re_code_7,sex,age_range_3,as.character(100000+sample(1:.N))),
          by=.(GEOID,household,role_7,re_code_7,sex,age_range_3)]
tr_hhRelR[is.na(re_code_I)&is.na(re_code_H),("re_code_H"):= 
            tr_hhRelH[.SD,list(re_code_7),on=.(tr_SARHa_match_id)]]
tr_hhRelH[is.na(matched_trSAH),("matched_trSAH"):=
            tr_hhRelR[.SD,list(re_code),on=.(tr_SARHa_match_id)]]
#nrow(tr_hhRelH[is.na(matched_trSAH)]) #885153 
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
#nrow(tr_hhRelH[is.na(matched_trSAH)&household=="In households"]) #319896 stop matching and have the rest pick up from R matches primarily

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
#table(bg_SARE[,re_code_7],bg_SARE[,re_code],useNA="ifany")
#table(tr_hhRelR[,re_code],tr_hhRelR[,re_code_14],useNA="ifany") #Q,S,T all less than zero

#move rest of RelR to bg_SARE, only in households 
tr_hhRelR[household=="In households"&HvL=="Not Hispanic or Latino"&re_code_14!="I",("tr_bg_SARh_match_id"):=
            paste0(GEOID,re_code_14,sex,child_role_tr,age_range_23,as.character(100000+sample(1:.N))),
          by=.(GEOID,re_code_14,sex,child_role_tr,age_range_23)]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I",("tr_bg_SARh_match_id"):=
          paste0(tract,re_code,sex,child_role_tr,age_range,as.character(100000+sample(1:.N))),
        by=.(tract,re_code,sex,child_role_tr,age_range)]
tr_hhRelR[household=="In households"&HvL=="Not Hispanic or Latino"&re_code_14!="I",("match_7"):=
            bg_SARE[.SD,list(re_code),on=.(tr_bg_SARh_match_id)]]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelR[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SARh_match_id)]]
#nrow(bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I"]) #4034265 
#table(bg_SARE[HvL=="Not Hispanic or Latino"&re_code!="I",re_code],useNA = "ifany")
#table(tr_hhRelR[HvL=="Not Hispanic or Latino"&re_code_14!="I"&is.na(match_7),re_code_14],useNA = "ifany")
#match on age_range_6 & child_role 
tr_hhRelR[household=="In households"&is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code_14!="I",("tr_bg_SARha3_match_id"):=
            paste0(GEOID,child_role_tr,age_range_6,as.character(100000+sample(1:.N))),
          by=.(GEOID,child_role_tr,age_range_6)]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code_7!="A",("tr_bg_SARha3_match_id"):=
          paste0(tract,child_role_tr,age_range_6,as.character(100000+sample(1:.N))),
        by=.(tract,child_role_tr,age_range_6)]
tr_hhRelR[household=="In households"&is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code_14!="I",("match_7"):=
            bg_SARE[.SD,list(re_code),on=.(tr_bg_SARha3_match_id)]]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code_7!="A",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelR[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SARha3_match_id)]]
#nrow(tr_hhRelR[is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code_14!="I"]) #1183179
#nrow(bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I"]) #852283
tr_hhRelR[household=="In households"&is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code!="A",("tr_bg_SARha4_match_id"):=
            paste0(GEOID,age_range_3,as.character(100000+sample(1:.N))),
          by=.(GEOID,age_range_3)]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code_7!="A",("tr_bg_SARha4_match_id"):=
          paste0(tract,age_range_3,as.character(100000+sample(1:.N))),
        by=.(tract,age_range_3)]
tr_hhRelR[household=="In households"&is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code!="A",("match_7"):=
            bg_SARE[.SD,list(re_code),on=.(tr_bg_SARha4_match_id)]]
bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code_7!="A",c("alone_tr","role_tr","household_tr"):=
          tr_hhRelR[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SARha4_match_id)]]
#nrow(tr_hhRelR[is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code_14!="I"]) #544692
#nrow(bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I"]) #213796
#table(tr_hhRelR[,household],useNA = "ifany")
#table(bg_SARE[,household_tr],useNA = "ifany")



##RETHINK GETTING GROUP QUARTERS HERE - MAYBE ONLY FROM GQ, BELOW??? - and remember that _bg needs to trump for gq
##to get just group quarters
#tr_hhRelR[is.na(match_7),("tr_bg_SARha5_match_id"):=
#            paste0(GEOID,age_range_3,as.character(100000+sample(1:.N))),
#          by=.(GEOID,age_range_3)]
#bg_SARE[is.na(role_tr),("tr_bg_SARha5_match_id"):=
#          paste0(tract,age_range_3,as.character(100000+sample(1:.N))),
#        by=.(tract,age_range_3)]
#tr_hhRelR[is.na(match_7),("match_7"):=
#            bg_SARE[.SD,list(re_code),on=.(tr_bg_SARha5_match_id)]]
#bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code_7!="A",c("alone_tr","role_tr","household_tr"):=
#          tr_hhRelR[.SD,c(list(alone),list(role),list(household)),on=.(tr_bg_SARha5_match_id)]]
##nrow(tr_hhRelR[is.na(match_7)&HvL=="Not Hispanic or Latino"&re_code_14!="I"]) #249133
##nrow(bg_SARE[is.na(role_tr)&HvL=="Not Hispanic or Latino"&re_code!="I"]) #40176

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
#no group_quarter information except institutionalized vs. non-institutionalized; 

#set all na on household to group quarters, which is over, but gives us a start
bg_SARE[,("household_tr"):=fcase(is.na(household_tr),"In group quarters",
                                 default = household_tr)] #still 83k off...
bg_hhRel[,("age_range_2"):=fcase(is.na(age_range_2),"over_17",default = age_range_2)]

#put some potential matches on from tr_hhRelR; household is encoded in role, and all in households ended up matching
bg_hhRel[,("tr_bg_Rel_match_id"):=
           paste0(tract,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
         by=.(tract,sex,role,alone,age_range_2)]
tr_hhRelR[,("tr_bg_Rel_match_id"):=
            paste0(GEOID,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
          by=.(GEOID,sex,role,alone,age_range_2)]
bg_hhRel[,c("re_code","race","age_range_6","age_range_23","HvL"):=
           tr_hhRelR[.SD,c(list(re_code),list(race),
                           list(age_range_6),list(age_range_23),list(HvL)),
                     on=.(tr_bg_Rel_match_id)]]
tr_hhRelR[,("match_bgRel"):=
            bg_hhRel[.SD,list(household),on=.(tr_bg_Rel_match_id)]]
#nrow(tr_hhRelR[is.na(match_bgRel)]) #606045, which is all group quarters
#table(tr_hhRelR[is.na(match_bgRel),household])
#b/c sex was only for about a third of the table, have to take it out for rest
#bg_hhRel[is.na(re_code),("tr_bg_Rels_match_id"):=
#           paste0(tract,role,alone,age_range_2,as.character(100000+sample(1:.N))),
#         by=.(tract,role,alone,age_range_2)]
#tr_hhRelR[is.na(match_bgRel),("tr_bg_Rels_match_id"):=
#            paste0(GEOID,role,alone,age_range_2,as.character(100000+sample(1:.N))),
#          by=.(GEOID,role,alone,age_range_2)]
#bg_hhRel[is.na(re_code),c("re_code","race","sex",
#                                      "age_range_6","age_range_23","HvL"):=
#           tr_hhRelR[.SD,c(list(re_code),list(race),list(sex),
#                           list(age_range_6),list(age_range_23),list(HvL)),
#                     on=.(tr_bg_Rels_match_id)]]
#tr_hhRelR[is.na(match_bgRel),("match_bgRel"):=
#            bg_hhRel[.SD,list(household),on=.(tr_bg_Rels_match_id)]]
#
#

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
#check on child_role_tr
#nrow(bg_hhRel[is.na(re_code_14)]) #9145923; only householders matched, under_18 doesn't have sex; ~30% not matched; no GQ match
#table(bg_SARE[,age_range],bg_SARE[,role],useNA = "ifany")

#then on age_range_6, without sex, with the bg_GEOID from bg_hhRel written over for testing
bg_hhRel[is.na(re_code_14),("bg_Rel6_match_id"):=
           paste0(tract,household,role,alone,age_range_6,HvL,re_code,as.character(100000+sample(1:.N))),
         by=.(tract,household,role,alone,age_range_6,HvL,re_code)]
bg_SARE[is.na(household),("bg_Rel6_match_id"):=
          paste0(tract,household_tr,role_tr,alone_tr,age_range_6,HvL,re_code_7,as.character(100000+sample(1:.N))),
        by=.(tract,household_tr,role_tr,alone_tr,age_range_6,HvL,re_code_7)]
bg_hhRel[is.na(re_code_14),("re_code_14"):=
           bg_SARE[.SD,list(re_code),on=.(bg_Rel6_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(GEOID)),on=.(bg_Rel6_match_id)]]
nrow(bg_hhRel[is.na(re_code_14)]) #5602017 (about 20% not matched)
#because bg_hhRel doesn't have re_code originally, assuming lots of mismatch from that
#table(bg_SARE[,age_range],bg_SARE[,role],useNA = "ifany")
##NEED TO DO SOMETHING TO CAPTURE NON-HOUSEHOLDERS???


##SOMETHING'S GONE WRONG BETWEEN HERE AND TIME AFTER REMIX - MAYBE NO REMIX???; even fixed, less than 100k gained
#
##go back to tr_hhRelR to get different additional matches
##first exclude already matched by moving over all the ones that have matched for re_code_14 already
#bg_hhRel[,("trbg2_Rel_match_id"):=
#           paste0(tract,household,re_code_14,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
#         by=.(tract,household,re_code_14,sex,role,alone,age_range_2)]
#tr_hhRelR[,("trbg2_Rel_match_id"):=
#            paste0(GEOID,household,re_code_14,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
#          by=.(GEOID,household,re_code_14,sex,role,alone,age_range_2)]
#tr_hhRelR[,c("match_trbgRel","match_bgSARE","bg_GEOID"):=
#            bg_hhRel[.SD,c(list(household),list(re_code_14),list(GEOID)),on=.(trbg2_Rel_match_id)]]
#bg_hhRel[,("re_code_match"):= #just for testing
#           tr_hhRelR[.SD,list(re_code),on=.(trbg2_Rel_match_id)]]
##nrow(tr_hhRelR[is.na(match_trbgRel)])
##nrow(tr_hhRelR[is.na(match_bgSARE)])
#
##then remix without re_code matching, keeping only re_code from tr_hhRelR
#bg_hhRel[is.na(re_code_14),("trbg_Rela_match_id"):= 
#           paste0(tract,household,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
#         by=.(tract,household,sex,role,alone,age_range_2)]
#tr_hhRelR[is.na(match_bgSARE),("trbg_Rela_match_id"):=
#          paste0(GEOID,household,sex,role,alone,age_range_2,as.character(100000+sample(1:.N))),
#        by=.(GEOID,household,sex,role,alone,age_range_2)]
#tr_hhRelR[is.na(match_bgSARE),c("match_trbgRel","match_bgSARE","bg_GEOID"):=
#    bg_hhRel[.SD,c(list(household),list(re_code_14),list(GEOID)),on=.(trbg_Rela_match_id)]]
#bg_hhRel[is.na(re_code_14),c("re_code_tr","re_code_14_tr","age_range_6_tr"):= 
#            tr_hhRelR[.SD,c(list(re_code),list(re_code_14),list(age_range_6)),on=.(trbg_Rela_match_id)]]
##nrow(tr_hhRelR[is.na(match_trbgRel)])#606045 - that is, just group quarters
##table(bg_hhRel[,re_code_14],bg_hhRel[,re_code_14_tr],useNA = "ifany")
#
##matching to remixed re_code on bg_hhRel
#bg_SARE[is.na(household),("bg_RelSARE_match_id"):=
#           paste0(tract,re_code,household_tr,role_tr,alone_tr,age_range_6,as.character(100000+sample(1:.N))),
#         by=.(tract,re_code,household_tr,role_tr,alone_tr,age_range_6)]
#bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("bg_RelSARE_match_id"):=
#            paste0(tract,re_code_14_tr,household,role,alone,age_range_6_tr,as.character(100000+sample(1:.N))),
#          by=.(tract,re_code_14_tr,household,role,alone,age_range_6_tr)]
#bg_hhRel[is.na(re_code_14)&!is.na(re_code_14_tr),("re_code_14"):=
#            bg_SARE[.SD,list(re_code),on=.(bg_RelSARE_match_id)]]
#bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
#          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelSARE_match_id)]]
#nrow(bg_SARE[is.na(household)])#5524373, after remix... 
#nrow(bg_hhRel[is.na(re_code_14)]) #5524373

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
          paste0(tract,re_code_7,age_range_3,as.character(100000+sample(1:.N))),
        by=.(tract,re_code_7,age_range_3)]
bg_hhRel[is.na(re_code_14)&household=="In households",("bg_RelARE_match_id"):=
           paste0(tract,re_code,age_range_3,as.character(100000+sample(1:.N))),
         by=.(tract,re_code,age_range_3)]
bg_hhRel[is.na(re_code_14)&household=="In households",("re_code_14"):=
           bg_SARE[.SD,list(re_code),on=.(bg_RelARE_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelARE_match_id)]]
#nrow(bg_SARE[is.na(household)]) #912530 
#table(bg_SARE[,household_tr],bg_SARE[,household],useNA = "ifany") 
#nrow(bg_hhRel[is.na(re_code_14)&household=="In households"]) #306485 ~1%

#order on age range and then do last 1% 
bg_hhRel <- bg_hhRel[order(match(role,c("Biological child","Grandchild","Adopted child","Stepchild","Foster child",
                                            "Householder","Opposite-sex spouse","Opposite-sex unmarried partner",
                                            "Same-sex spouse","Same-sex unmarried partner","Son-in-law or daughter-in-law",
                                            "Other nonrelatives","Other relatives","Brother or sister","Parent",
                                            "Parent-in-law")))]
bg_SARE <- bg_SARE[order(age_num)]

bg_SARE[is.na(household),("bg_RelRb_match_id"):=
          paste0(tract,re_code_7,as.character(100000+(1:.N))),
        by=.(tract,re_code_7)]
bg_hhRel[is.na(re_code_14)&household=="In households",("bg_RelRb_match_id"):=
            paste0(tract,re_code,as.character(100000+(1:.N))),
          by=.(tract,re_code)]
bg_hhRel[is.na(re_code_14)&household=="In households",("re_code_14"):=
            bg_SARE[.SD,list(re_code),on=.(bg_RelRb_match_id)]]
bg_SARE[is.na(household),c("alone","role","household","bg_GEOID"):=
          bg_hhRel[.SD,c(list(alone),list(role),list(household),list(bg_GEOID)),on=.(bg_RelRb_match_id)]]
#nrow(bg_SARE[is.na(household)])#606045
#nrow(bg_hhRel[is.na(re_code_14)])#606045 
#table(bg_SARE[,age_range_6],bg_SARE[,role],useNA = "ifany") #some ages are off, but try to keep re_code_14 when re-matching


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

bgGQ_data[,("gq_type_7"):=fcase(str_detect(gq_type,"College"), #to make easier to display
                                "College/University student housing",
                                str_detect(gq_type,"Nursing"),
                                "Nursing facilities",
                                gq_type=="Military quarters (601-602)",
                                "Military quarters",
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
bgGQ_melted <- melt(bgGQ_data, id.vars = c("sex","age_range","gq_institution","gq_type","gq_type_7"), measure.vars = Geoids,
                    value.name = "codom_GQSAT", variable.name = "GEOID")
bgGQ <- as.data.table(lapply(bgGQ_melted[,.SD],rep,bgGQ_melted[,codom_GQSAT]))
rm(bgGQ_data)
rm(bgGQ_melted)
table(bgGQ[,gq_type_7],bgGQ[,age_range],useNA = "ifany")
#table(tr_hh65RelRE[,household]) has 202039 over 65 in GQ; bgGQ has 108248 - VERY FRUSTRATING; straight from census table
#and length(unique(bgGQ[,GEOID])) #4012
#length(unique(bg_SARE[,GEOID])) #18561
#table(bgGQ[,gq_type_7],bgGQ[,age_range],useNA = "ifany")
#table(bgGQ[is.na(age_range),gq_type_7],useNA = "ifany")
#nrow(bgGQ[is.na(age_range)]) #118182
#table(bg_SARE[,household_tr],bg_SARE[,age_range_3],useNA = "ifany") #if all 14k under_18, not a bad match...

#how make sure we're not losing some of the correctional facilities for adults in 18 to 21 and 50 to 64? sample?? second match?
#some of these will be very imprecise, in any case - trying to err on the conservative side
bgGQ[,("age_range_3a"):=fcase(gq_type_7=="College/University student housing","18 to 21 years",
                             gq_type_7=="Correctional facilities for adults"&age_range=="Under 18 years","15 to 17 years",
                             gq_type_7=="Juvenile facilities","15 to 17 years",
                             gq_type_7=="Military quarters","18 to 64 years",
                             gq_type_7=="Nursing facilities"&age_range=="18 to 64 years","50 to 64 years",
                             age_range=="Under 18 years","17 years and under",default = age_range)]
bg_SARE[,("age_range_3a"):=fcase(age_range=="18 and 19 years" | age_range=="20 years" | age_range=="21 years","18 to 21 years",
                                 age_range=="15 to 17 years","15 to 17 years",
                                 age_range=="50 to 54 years" | age_range=="55 to 59 years" | age_range=="60 and 61 years" | 
                                   age_range=="62 to 64 years","50 to 64 years",
                                 default = age_range_3)]
bgGQ[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
bg_SARE[is.na(household),("bg_GQ_match_id"):=
           paste0(GEOID,sex,age_range_3a,as.character(100000+sample(1:.N))),
         by=.(GEOID,sex,age_range_3a)]
bgGQ[,("bg_GQ_match_id"):=
       paste0(GEOID,sex,age_range_3a,as.character(100000+sample(1:.N))),
     by=.(GEOID,sex,age_range_3a)]
bg_SARE[is.na(household),c("gq_institution","gq_type","gq_type_7","bgGQ_GEOID"):=
           bgGQ[.SD,c(list(gq_institution),list(gq_type),list(gq_type_7),list(GEOID)),
                on=.(bg_GQ_match_id)]]
bgGQ[,("match_bgRel"):=
       bg_SARE[.SD,list(gq_institution),on=.(bg_GQ_match_id)]]
nrow(bgGQ[is.na(match_bgRel)]) #260868 - just over 40% not matched
table(bgGQ[is.na(match_bgRel),age_range_3a],useNA = "ifany")

#on bg_SARE that was in group quarters in household_tr
bg_SARE[is.na(gq_institution)&household_tr=="In group quarters",("bg_GQ1_match_id"):=
          paste0(tract,sex,age_range_3a,as.character(100000+sample(1:.N))),
        by=.(tract,sex,age_range_3a)]
bgGQ[is.na(match_bgRel),("bg_GQ1_match_id"):=
       paste0(tract,sex,age_range_3a,as.character(100000+sample(1:.N))),
     by=.(tract,sex,age_range_3a)]
bg_SARE[is.na(gq_institution)&household_tr=="In group quarters",c("gq_institution","gq_type","gq_type_7","bgGQ_GEOID"):=
          bgGQ[.SD,c(list(gq_institution),list(gq_type),list(gq_type_7),list(GEOID)),
               on=.(bg_GQ1_match_id)]]
bgGQ[is.na(match_bgRel),c("match_bgRel","household","re_code","age_range","alone","role"):=
       bg_SARE[.SD,c(list(gq_institution),list(household),list(re_code),list(age_range),
                     list(alone),list(role)),on=.(bg_GQ1_match_id)]]
nrow(bgGQ[is.na(match_bgRel)]) #158212 - about 26% not matched; .5% of total
table(bgGQ[is.na(match_bgRel),age_range_3a],useNA = "ifany")

#TRY TO MATCH JUST FOR EVERY BLOCK-GROUP; HAVE TO DO IT MUCH EARLIER, BUT HOW DOES IT RELATE TO.

#problem is still that there are lots of empty tracts that don't match

#track so that household and GQ are right totals and everything has all the bg_SARE info from last things moved to GQ


#fix roles and roles_7 by age - typically off in the hundreds to low thousands across whole
#switch householders with child and add to other relatives?
#nrow(bg_SARE[str_detect(role," child")&age_num>64&sex=="Male"])
#nrow(bg_SARE[role=="Householder"&age_num<18])
bg_SARE[,("role"):=fcase(role=="Householder"&age_num<18&alone=="Not living alone","Biological child",
                         role=="Householder"&age_num<18&alone=="Living alone","Institutionalized population",
  str_detect(role,"-sex")&age_num<18,"Biological child",
  str_detect(role,"Parent")&age_num<18,"Biological child",
  str_detect(role,"Parent")&age_num>17&age_num<54,"Other relatives",
  str_detect(role," child")&age_num>64&sex=="Male","Householder",
  str_detect(role," child")&age_num>64&sex=="Female","Opposite-sex spouse",
  role=="Grandchild"&age_num>44,"Other relatives",
  default = role
  )]

#fidget with group quarters by age? Something up above got it pretty whack - maybe it's because it's most likely to be the last matches?
table(bg_SARE[household=="In group quarters",sex],bg_SARE[household=="In group quarters",age_range_3],useNA = "ifany")-table(bgGQ[,sex],bgGQ[,age_range],useNA = "ifany")
#17 years and under 18 to 64 years 65 years and over
#Female            -121046          71361             63445
#Male              -327928         276982             37186

