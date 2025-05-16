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

#replacing schematic_sam_dec
#b/c ordering by relations does not respect ordering by households (even though householder is called out), do all the households first, then relations
#do things that prioritize ordering where the matching is known, while carrying the codomain info about race/eth forward for later merge
#for first couple, no loss if at tract level it's just filling in within the distribution (ordering internally)

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
tr_nfhh_data_from_census <- 
  census_tract_get(censusdir, vintage, state, censuskey, 
                   groupname,county = "*",
                   api_type,path_suff)
if(names(tr_nfhh_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("sex","alone","age_range_2") 
  #row_c1 by hand
  row_c1 <- c(unique(tr_nfhh_data_from_census[!is.na(label_3),name]))
  test_total_pop <- tests_download_data(tr_nfhh_data_from_census,label_c1,row_c1,state=state) #not right because male-male unmarried is off
  tr_nfhh_data <- relabel(tr_nfhh_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_nfhh_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_nfhh_data <- tr_nfhh_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(tr_nfhh_data[,.SD,.SDcols = startsWith(names(tr_nfhh_data),state)])
tr_nfhh_melted <- melt(tr_nfhh_data, id.vars = c("sex","alone","age_range_2"), measure.vars = Geoids,
                           value.name = "codom_tr_nfhh", variable.name = "GEOID")
tr_nfhh <- as.data.table(lapply(tr_nfhh_melted[,.SD],rep,tr_nfhh_melted[,codom_tr_nfhh])) #right number
rm(tr_nfhh_data_from_census)
rm(tr_nfhh_data)
rm(tr_nfhh_melted)
tr_nfhh[,("sex"):=str_remove(sex," householder")] #at this point, sex is just for nonfamily
tr_hhCouple[,("sex"):=fcase(str_detect(couple_gender,"Male"),"Male",
                            str_detect(couple_gender,"Female"),"Female",
                            str_detect(same_sex,"Opposite"),"Male",
                            default = "Sex not known")]

#just distribute inside of hh_type_3=="All other households", since there are no power set combinations excluded
tr_hhCouple[hh_type_3=="All other households",("tr_couple_nf_match_id"):=
                    paste0(GEOID,as.character(100000+sample(1:.N))),
                  by=.(GEOID)]
tr_nfhh[,("tr_couple_nf_match_id"):=
                    paste0(GEOID,as.character(100000+sample(1:.N))),
                  by=.(GEOID)]
tr_hhCouple[hh_type_3=="All other households",c("sex","alone","hh_over_64"):=
                    tr_nfhh[.SD,c(list(sex),list(alone),list(age_range_2)),on=.(tr_couple_nf_match_id)]]
tr_nfhh[,("match_nf"):=
          tr_hhCouple[.SD,list(hh_type_3),on=.(tr_couple_nf_match_id)]]
#nrow(tr_nfhh[is.na(match_nf)]) #7074
#test <- table(tr_nfhh[,GEOID])-table(tr_hhCouple[hh_type_3=="All other households",GEOID])
#nrow(test[test>0])# 130
#sum(test[test>0]) #7074 (.2%)
#max(test[test>0]) #231
#just to distribute all of them, create a county
tr_nfhh[,("county"):=substr(GEOID,1,5)]
tr_hhCouple[,("county"):=substr(GEOID,1,5)]
tr_hhCouple[hh_type_3=="All other households"&is.na(alone),("county_couple_nf_match_id"):=
              paste0(county,as.character(100000+sample(1:.N))),
            by=.(county)]
tr_nfhh[is.na(match_nf),("county_couple_nf_match_id"):=
          paste0(county,as.character(100000+sample(1:.N))),
        by=.(county)]
tr_hhCouple[hh_type_3=="All other households"&is.na(alone),c("sex","alone","hh_over_64"):=
              tr_nfhh[.SD,c(list(sex),list(alone),list(age_range_2)),on=.(county_couple_nf_match_id)]]
tr_nfhh[is.na(match_nf),("match_nf"):=
          tr_hhCouple[.SD,list(hh_type_3),on=.(county_couple_nf_match_id)]]
#nrow(tr_nfhh[is.na(match_nf)]) #0
#test <- table(tr_nfhh[,GEOID],tr_nfhh[,sex],tr_nfhh[,alone],tr_nfhh[,age_range_2])-
#  table(tr_hhCouple[hh_type_3=="All other households",GEOID],tr_hhCouple[hh_type_3=="All other households",sex],
#        tr_hhCouple[hh_type_3=="All other households",alone],tr_hhCouple[hh_type_3=="All other households",hh_over_64])
#length(test[test!=0]) #4259 (of 55168) or 93% of GEOIDS match completely; without GEOIDs, they all match on tables
rm(tr_nfhh)

#add within; no loss around any power set since just ordering within
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
  label_c1 <- c("hh_size_2","family","family_type","sex_spouse","own_kids","sex")
  tr_hhSizeTypeOwnKids_data_from_census[,("label_5"):=fcase(str_detect(label_2,"householder") | 
                                                              str_detect(label_3,"householder"),
                                                            "No own children under 18 years",
                                                            str_detect(label_4,"under"),
                                                            label_4,
                                                            default=label_5)]
  tr_hhSizeTypeOwnKids_data_from_census[,("label_6"):=fcase(str_detect(label_2,"Female") |
                                                              str_detect(label_3,"Female") | str_detect(label_4,"Female"),
                                                            "Female",
                                                            str_detect(label_2,"Male") | 
                                                              str_detect(label_3,"Male") | str_detect(label_4,"Male"),
                                                            "Male",
                                                            default="Sex not known")]
  #row_c1 determined by hand 
  row_c1 <- c(unique(tr_hhSizeTypeOwnKids_data_from_census[!is.na(label_5),name])) 
  test_total_pop <- tests_download_data(tr_hhSizeTypeOwnKids_data_from_census,label_c1,row_c1,state=state) 
  tr_hhSizeTypeOwnKids_data <- relabel(tr_hhSizeTypeOwnKids_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(tr_hhSizeTypeOwnKids_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  tr_hhSizeTypeOwnKids_data <- tr_hhSizeTypeOwnKids_data_from_census
}

#reshape a bit and make list of individuals
Geoids <- colnames(tr_hhSizeTypeOwnKids_data[,.SD,.SDcols = startsWith(names(tr_hhSizeTypeOwnKids_data),state)])
tr_hhSizeTypeOwnKids_melted <- melt(tr_hhSizeTypeOwnKids_data, id.vars = c("hh_size_2","family","family_type","sex","own_kids"), measure.vars = Geoids,
                                    value.name = "codom_tr_hhSizeTypeOwnKids", variable.name = "GEOID")
tr_hhSizeTypeOwnKids <- as.data.table(lapply(tr_hhSizeTypeOwnKids_melted[,.SD],rep,tr_hhSizeTypeOwnKids_melted[,codom_tr_hhSizeTypeOwnKids]))
rm(tr_hhSizeTypeOwnKids_data_from_census)
rm(tr_hhSizeTypeOwnKids_data)
rm(tr_hhSizeTypeOwnKids_melted)

tr_hhSizeTypeOwnKids[,("alone"):=fcase(hh_size_2=="1-person household","Living alone",
                                       hh_size_2=="2-or-more-person household" &
                                         family=="Nonfamily households",
                                       "Not living alone",
                                       default = "Not living alone")]
tr_hhSizeTypeOwnKids[,("hh_type_3"):=fcase(family_type=="Married couple family",
                                           "Married couple",
                                       str_detect(family_type,"householder"), #not alone
                                          "Unmarried-partner", #11253 short of unmarried partners in couples?
                                       default = "All others")]
#hh_type_3 does not match with hh_couple, which is the ground truth for hh_type_3; extra "Unmarried-partners" have to move to "All others"
tr_hhSizeTypeOwnKids[,("family"):=str_replace(family,"holder","holder (solitary)")]
tr_hhSizeTypeOwnKids[,("family_type"):=str_replace(family_type,"holder","holder (not alone)")]
tr_hhCouple[,("hh_type_3"):=str_remove(hh_type_3," household")]
tr_hhCouple[,("alone"):=fcase(is.na(alone),
                              "Not living alone",default = alone)]
#match on hh_type_3,alone,sex (will drop same_sex married couples); age_range with own_kids on P20 is only for seniors living alone
#hh_over_64 has small number not living alone - the relatives files show people living with adult children, so can have that override later
tr_hhCouple[,("tr_STOK_match_id"):=
              paste0(GEOID,hh_type_3,alone,sex,as.character(100000+sample(1:.N))),
            by=.(GEOID,hh_type_3,alone,sex)]
tr_hhSizeTypeOwnKids[,("tr_STOK_match_id"):=
          paste0(GEOID,hh_type_3,alone,sex,as.character(100000+sample(1:.N))),
        by=.(GEOID,hh_type_3,alone,sex)]
tr_hhCouple[,c("hh_size_2","own_kids","family","family_type"):=
              tr_hhSizeTypeOwnKids[.SD,c(list(hh_size_2),list(own_kids),
                                         list(family),list(family_type)),on=.(tr_STOK_match_id)]]
tr_hhSizeTypeOwnKids[,("match_STOK"):=
          tr_hhCouple[.SD,list(hh_type_3),on=.(tr_STOK_match_id)]]
#nrow(tr_hhSizeTypeOwnKids[is.na(match_STOK)])#6954885 #not matching sex on married couples and overdeterm on unmarried partners
#without sex, top pick up the married couples, including same_sex
tr_hhCouple[is.na(hh_size_2),("tr_STOKta_match_id"):=
              paste0(GEOID,hh_type_3,alone,as.character(100000+sample(1:.N))),
            by=.(GEOID,hh_type_3,alone)]
tr_hhSizeTypeOwnKids[is.na(match_STOK),("tr_STOKta_match_id"):=
                       paste0(GEOID,hh_type_3,alone,as.character(100000+sample(1:.N))),
                     by=.(GEOID,hh_type_3,alone)]
tr_hhCouple[is.na(hh_size_2),c("hh_size_2","own_kids","family","family_type","sex"):=
              tr_hhSizeTypeOwnKids[.SD,c(list(hh_size_2),list(own_kids),
                                         list(family),list(family_type),list(sex)),on=.(tr_STOKta_match_id)]]
tr_hhSizeTypeOwnKids[is.na(match_STOK),("match_STOK"):=
                       tr_hhCouple[.SD,list(hh_type_3),on=.(tr_STOKta_match_id)]]
#nrow(tr_hhSizeTypeOwnKids[is.na(match_STOK)])#205566 #without sex
tr_hhCouple[is.na(hh_size_2),("tr_STOKt_match_id"):=
              paste0(GEOID,alone,as.character(100000+sample(1:.N))),
            by=.(GEOID,alone)]
tr_hhSizeTypeOwnKids[is.na(match_STOK),("tr_STOKt_match_id"):=
                       paste0(GEOID,alone,as.character(100000+sample(1:.N))),
                     by=.(GEOID,alone)]
tr_hhCouple[is.na(hh_size_2),c("hh_size_2","own_kids","family","family_type","sex"):=
              tr_hhSizeTypeOwnKids[.SD,c(list(hh_size_2),list(own_kids),
                                         list(family),list(family_type),list(sex)),on=.(tr_STOKt_match_id)]]
tr_hhSizeTypeOwnKids[is.na(match_STOK),("match_STOK"):=
                       tr_hhCouple[.SD,list(hh_type_3),on=.(tr_STOKt_match_id)]]
#nrow(tr_hhSizeTypeOwnKids[is.na(match_STOK)])#10484
#table(tr_hhSizeTypeOwnKids[is.na(match_STOK),hh_type_3],tr_hhSizeTypeOwnKids[is.na(match_STOK),alone]) 
#just GEOID for those that are left; will make sure it's not last value for individual; keep by county
tr_hhSizeTypeOwnKids[,("county"):=substr(GEOID,1,5)]
tr_hhCouple[is.na(hh_size_2),("tr_STOKts_match_id"):=
              paste0(county,alone,as.character(100000+sample(1:.N))),
            by=.(county,alone)]
tr_hhSizeTypeOwnKids[is.na(match_STOK),("tr_STOKts_match_id"):=
                       paste0(county,alone,as.character(100000+sample(1:.N))),
                     by=.(county,alone)]
tr_hhCouple[is.na(hh_size_2),c("hh_size_2","own_kids","family","family_type","sex","alone"):=
              tr_hhSizeTypeOwnKids[.SD,c(list(hh_size_2),list(own_kids),
                                         list(family),list(family_type),list(sex),list(alone)),on=.(tr_STOKts_match_id)]]
tr_hhSizeTypeOwnKids[is.na(match_STOK),("match_STOK"):=
                       tr_hhCouple[.SD,list(hh_type_3),on=.(tr_STOKts_match_id)]]
#nrow(tr_hhSizeTypeOwnKids[is.na(match_STOK)])#0
#test <- table(tr_hhCouple[,GEOID],tr_hhCouple[,own_kids],tr_hhCouple[,family_type],tr_hhCouple[,sex],tr_hhCouple[,alone])-
#  table(tr_hhSizeTypeOwnKids[,GEOID],tr_hhSizeTypeOwnKids[,own_kids],tr_hhSizeTypeOwnKids[,family_type],
#        tr_hhSizeTypeOwnKids[,sex],tr_hhSizeTypeOwnKids[,alone])
#length(test[test!=0])#1311 on county; 39608 on GEOID;  when matched on GEOID, not alone in step 3, get 6604, (of 331008, or less than 2, without GEOID = 0)
#a handful of sex don't match on table test...
rm(tr_hhSizeTypeOwnKids)
tr_hhCouple[,("family_type"):=fcase(hh_type_3=="Unmarried-partner"&family_type=="Other family"&sex=="Female",
                                    "Female householder (not alone)",
                                    hh_type_3=="Unmarried-partner"&family_type=="Other family"&sex=="Male",
                                    "Male householder (not alone)",
                                    default = family_type)]

#could tweak this distribution: table(tr_hhCouple[,own_kids],tr_hhCouple[,same_sex]) with national averages, but only a few thousand
#https://www.statista.com/statistics/325049/same-sex-couples-in-the-us-by-age-of-householder/#:~:text=In%202022%2C%20about%2025.4%20percent,old%20in%20that%20same%20year.
#could also tweak:table(tr_hhCouple[,own_kids],tr_hhCouple[,hh_over_64]), but lots to account for and numbers aren't too bad
#can see difficulties with: table(tr_hhCouple[,own_kids],tr_hhCouple[,hh_over_64],tr_hhCouple[,hh_type_3])
#https://www.statista.com/statistics/242074/percentages-of-us-family-households-with-children-by-type/
#may have opportunity to tweak better later with bg_hhRel, etc.


#potential loss from projection to bg; do this, then HCT2, H14 (tenure own kids), then start picking up re_code
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
  label_c1 <- c("household_type_4","rel_in_house","age_range_2_sr") 
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
bg_hhOwnKids_melted <- melt(bg_hhOwnKids_data, id.vars = c("household_type_4","rel_in_house","age_range_2_sr"), measure.vars = Geoids,
                            value.name = "codom_bg_hhOwnKids", variable.name = "GEOID")
bg_hhOwnKids_melted[,("codom_bg_hhOwnKids"):=ifelse(household_type_4=="Male householder, no spouse or partner present" &
                                                      rel_in_house=="Living alone" & is.na(age_range_2_sr),
                                                    as.numeric(.SD[household_type_4=="Male householder, no spouse or partner present" &
                                                                     rel_in_house=="Living alone" & is.na(age_range_2_sr),codom_bg_hhOwnKids])-
                                                      as.numeric(.SD[household_type_4=="Male householder, no spouse or partner present" & 
                                                                       rel_in_house=="Living alone" & 
                                                                       age_range_2_sr=="65 years and over",codom_bg_hhOwnKids]),codom_bg_hhOwnKids),by=.(GEOID)]
bg_hhOwnKids_melted[,("codom_bg_hhOwnKids"):=ifelse(household_type_4=="Female householder, no spouse or partner present" &
                                                      rel_in_house=="Living alone" & is.na(age_range_2_sr),
                                                    as.numeric(.SD[household_type_4=="Female householder, no spouse or partner present" &
                                                                     rel_in_house=="Living alone" & is.na(age_range_2_sr),codom_bg_hhOwnKids])-
                                                      as.numeric(.SD[household_type_4=="Female householder, no spouse or partner present" & 
                                                                       rel_in_house=="Living alone" & 
                                                                       age_range_2_sr=="65 years and over",codom_bg_hhOwnKids]),codom_bg_hhOwnKids),by=.(GEOID)]
bg_hhOwnKids_melted[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
bg_hhOwnKids <- as.data.table(lapply(bg_hhOwnKids_melted[,.SD],rep,bg_hhOwnKids_melted[,codom_bg_hhOwnKids]))
rm(bg_hhOwnKids_data_from_census)
rm(bg_hhOwnKids_melted)
rm(bg_hhOwnKids_data)

#make a hh_type_4 to capture the possible matches... - redo with more categories, if needed
#what if make all married couples male hh, then back out for the ones that are same_sex female after fail to match?
#this is broken!!!!!!
tr_hhCouple[,("hh_type_4"):=fcase(str_detect(family,"Female") | str_detect(family_type,"Female") |
                                    sex=="Female" & hh_type_3=="All others", #i.e., nonfamily
                                    "Female householder, no spouse or partner present",
                                  str_detect(family,"Male") | str_detect(family_type,"Male") |
                                    sex=="Male" & hh_type_3=="All others",
                                    "Male householder, no spouse or partner present",
                                  hh_type_3=="Unmarried-partner",
                                    "Cohabiting couple household",
                                  hh_type_3=="Married couple",
                                    "Married couple household",
                                  default = hh_type_3)]
#a little bit off, need the bg_hhOwnKids to be ground
#age_range is a little off b/c ownKids only 
bg_hhOwnKids[,("hh_over_64"):=fcase(!is.na(age_range_2_sr),"Householder 65 years and over",
                                    default = "Householder age not known")]
bg_hhOwnKids[,("own_kids"):=fcase(rel_in_house=="With own children under 18","With own children under 18 years",
                                  default = "No own children under 18 years")]
bg_hhOwnKids[,("sex"):=fcase(household_type_4=="Female householder, no spouse or partner present","Female",
                             household_type_4=="Male householder, no spouse or partner present","Male",
                             default = "Sex not known")]
tr_hhCouple[,("bg_OK_match_id"):=
              paste0(GEOID,hh_type_4,alone,sex,hh_over_64,own_kids,as.character(100000+sample(1:.N))),
            by=.(GEOID,hh_type_4,alone,sex,hh_over_64,own_kids)]
bg_hhOwnKids[,("bg_OK_match_id"):=
                       paste0(tract,household_type_4,rel_in_house,sex,hh_over_64,own_kids,as.character(100000+sample(1:.N))),
                     by=.(tract,household_type_4,rel_in_house,sex,hh_over_64,own_kids)]
bg_hhOwnKids[,c("hh_type_3","same_sex","couple_gender","alone","family","family_type"):=
               tr_hhCouple[.SD,c(list(hh_type_3),list(same_sex),list(couple_gender),list(alone),
                                         list(family),list(family_type)),on=.(bg_OK_match_id)]]
tr_hhCouple[,("match_OK"):=
                       bg_hhOwnKids[.SD,list(household_type_4),on=.(bg_OK_match_id)]]
#nrow(tr_hhCouple[is.na(match_OK)])#9599449 

#nrow(bg_hhOwnKids[is.na(hh_type_3)&hh_over_64=="Householder 65 years and over"]) #863
#taking out hh_over_64
tr_hhCouple[is.na(match_OK),("bg_OKso_match_id"):=
              paste0(GEOID,hh_type_4,alone,sex,own_kids,as.character(100000+sample(1:.N))),
            by=.(GEOID,hh_type_4,alone,sex,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),("bg_OKso_match_id"):=
               paste0(tract,household_type_4,rel_in_house,sex,own_kids,as.character(100000+sample(1:.N))),
             by=.(tract,household_type_4,rel_in_house,sex,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),c("hh_type_3","same_sex","couple_gender","alone","family","family_type"):=
               tr_hhCouple[.SD,c(list(hh_type_3),list(same_sex),list(couple_gender),list(alone),
                                 list(family),list(family_type)),on=.(bg_OKso_match_id)]]
tr_hhCouple[is.na(match_OK),("match_OK"):=
              bg_hhOwnKids[.SD,list(household_type_4),on=.(bg_OKso_match_id)]]
#nrow(tr_hhCouple[is.na(match_OK)])#7883453
#nrow(tr_hhCouple[is.na(match_OK)&alone=="Living alone"]) #5217

tr_hhCouple[is.na(match_OK),("bg_OKso_match_id"):=
              paste0(GEOID,hh_type_4,sex,own_kids,as.character(100000+sample(1:.N))),
            by=.(GEOID,hh_type_4,sex,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),("bg_OKso_match_id"):=
               paste0(tract,household_type_4,sex,own_kids,as.character(100000+sample(1:.N))),
             by=.(tract,household_type_4,sex,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),c("hh_type_3","same_sex","couple_gender","alone","family","family_type"):=
               tr_hhCouple[.SD,c(list(hh_type_3),list(same_sex),list(couple_gender),list(alone),
                                 list(family),list(family_type)),on=.(bg_OKso_match_id)]]
tr_hhCouple[is.na(match_OK),("match_OK"):=
              bg_hhOwnKids[.SD,list(household_type_4),on=.(bg_OKso_match_id)]]
#nrow(tr_hhCouple[is.na(match_OK)])# 694463
#without sex to pick up couples with sex named, but move over where known; need to compare with originals
tr_hhCouple[is.na(match_OK),("bg_OKs_match_id"):=
              paste0(GEOID,hh_type_4,own_kids,as.character(100000+sample(1:.N))),
            by=.(GEOID,hh_type_4,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),("bg_OKs_match_id"):=
               paste0(tract,household_type_4,own_kids,as.character(100000+sample(1:.N))),
             by=.(tract,household_type_4,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),c("hh_type_3","sex","same_sex","couple_gender","alone","family","family_type"):=
               tr_hhCouple[.SD,c(list(hh_type_3),list(sex),list(same_sex),list(couple_gender),list(alone),
                                 list(family),list(family_type)),on=.(bg_OKs_match_id)]]
tr_hhCouple[is.na(match_OK),("match_OK"):=
              bg_hhOwnKids[.SD,list(household_type_4),on=.(bg_OKs_match_id)]]
#nrow(tr_hhCouple[is.na(match_OK)])# 639952 is 6% of whole not matching on own children 
#using bg_hhOwnKids as ground, matching own_kids
tr_hhCouple[is.na(match_OK),("bg_OKo_match_id"):=
              paste0(GEOID,own_kids,as.character(100000+sample(1:.N))),
            by=.(GEOID,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),("bg_OKo_match_id"):=
               paste0(tract,own_kids,as.character(100000+sample(1:.N))),
             by=.(tract,own_kids)]
bg_hhOwnKids[is.na(hh_type_3),c("hh_type_3","sex","same_sex","couple_gender","alone","family","family_type"):=
               tr_hhCouple[.SD,c(list(hh_type_3),list(sex),list(same_sex),list(couple_gender),list(alone),
                                 list(family),list(family_type)),on=.(bg_OKo_match_id)]]
tr_hhCouple[is.na(match_OK),("match_OK"):=
              bg_hhOwnKids[.SD,list(household_type_4),on=.(bg_OKo_match_id)]]
#nrow(tr_hhCouple[is.na(match_OK)]) #5050
#finish up last .05%
tr_hhCouple[is.na(match_OK),("bg_OKo_match_id"):=
              paste0(GEOID,as.character(100000+sample(1:.N))),
            by=.(GEOID)]
bg_hhOwnKids[is.na(hh_type_3),("bg_OKo_match_id"):=
               paste0(tract,as.character(100000+sample(1:.N))),
             by=.(tract)]
bg_hhOwnKids[is.na(hh_type_3),c("hh_type_3","sex","same_sex","couple_gender","alone","family","family_type"):=
               tr_hhCouple[.SD,c(list(hh_type_3),list(sex),list(same_sex),list(couple_gender),list(alone),
                                 list(family),list(family_type)),on=.(bg_OKo_match_id)]]
tr_hhCouple[is.na(match_OK),("match_OK"):=
              bg_hhOwnKids[.SD,list(household_type_4),on=.(bg_OKo_match_id)]]
#nrow(tr_hhCouple[is.na(match_OK)]) #0
#fix rogue matches (just 11 of them) on sex for no spouse present households; prioritizing bg_hhOwnKids distribution
bg_hhOwnKids[,("sex"):=fcase(str_detect(household_type_4,"Female"),"Female",
             str_detect(household_type_4,"Male"),"Male",
             default = sex)]
#table(bg_hhOwnKids[,family_type],bg_hhOwnKids[,rel_in_house]) #abuot 5k off total
bg_hhOwnKids[,("rel_in_house"):=fcase(str_detect(family_type,"not")&rel_in_house=="Living alone",
                                      "With own children under 18",
                                      default = rel_in_house)]
rm(tr_hhCouple)
#fix up a few stragglers and create a better combined family_type
#table(bg_hhOwnKids[,household_type_4],bg_hhOwnKids[,family_type])# (16 wrongly assigned)
bg_hhOwnKids[,("family_type"):=fcase(str_detect(household_type_4,"Female") & 
                                  rel_in_house!="Living alone",
                                "Female householder (not alone)",
                                str_detect(household_type_4,"Male") &
                                  rel_in_house!="Living alone",
                                "Male householder (not alone)",
                                default = family_type)]
#table(bg_hhOwnKids[,household_type_4],bg_hhOwnKids[,family])#about 1k off
#cohabiting couples might have children, so not always nonfamily, but sometimes...
#bg_hhOwnKids[,("family"):=fcase(str_detect(family,"solitary") & 
#                                       household_type_4=="Cohabiting couple household",
#                                     "Nonfamily households",
#                                     default = family)]
bg_hhOwnKids[,("family_type_7"):=fcase(str_detect(family,"Nonfamily") | 
                                         family_type=="Married couple family",
                                     family_type,
                             family_type=="Other family",
                             household_type_4,
                             default = family)]
#table(bg_hhOwnKids[,household_type_4],bg_hhOwnKids[,family_type_7])
#there's a weird overlap between cohabiting couple household and family households that could be figured out, but maybe just work around...


#add tenure here, where it should be floating pretty well, still....
groupname <- "H14" #HOUSEHOLDER TYPE / TENURE
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhTypeTenure_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhTypeTenure_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("rent_own","family","family_type","no_spouse_sex","age_range_3") #follow above, but will have to divide
  #row_c1 determined by hand 
  row_c1 <- c(unique(bg_hhTypeTenure_data_from_census[str_detect(label_5,"years") | str_detect(label_4,"years"),name]))
  test_total_pop <- tests_download_data(bg_hhTypeTenure_data_from_census,label_c1,row_c1,state=state)
  bg_hhTypeTenure_data <- relabel(bg_hhTypeTenure_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhTypeTenure_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhTypeTenure_data <- bg_hhTypeTenure_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhTypeTenure_data[,.SD,.SDcols = startsWith(names(bg_hhTypeTenure_data),state)])
bg_hhTypeTenure_melted <- melt(bg_hhTypeTenure_data, id.vars = c("rent_own","family","family_type","no_spouse_sex","age_range_3"), measure.vars = Geoids,
                               value.name = "codom_hhTenure", variable.name = "GEOID")
bg_hhTypeTenure <- as.data.table(lapply(bg_hhTypeTenure_melted[,.SD],rep,bg_hhTypeTenure_melted[,codom_hhTenure]))
bg_hhTypeTenure[,("age_range_3"):=fcase(str_detect(no_spouse_sex,"years"),no_spouse_sex,
                                        default = age_range_3)]
bg_hhTypeTenure[,("no_spouse_sex"):=fcase(str_detect(no_spouse_sex,"years"),family_type,
                                          default = no_spouse_sex)]
#bg_hhTypeTenure[,("alone"):=]
rm(bg_hhTypeTenure_data_from_census)
rm(bg_hhTypeTenure_data)
rm(bg_hhTypeTenure_melted)
#clean up a little for matching with bg_hhOwnKids
#bg_hhOwnKids f_t6, needs 7, with Family Households broken out to Married and ...?
bg_hhOwnKids[,("family_2"):=fcase(str_detect(family,"Family"),family,
                                  default = "Nonfamily households")]
bg_hhTypeTenure[,("sex_match"):=fcase(str_detect(family_type,"Female"),"Female",
                                      str_detect(family_type,"Male"),"Male",
                                      str_detect(no_spouse_sex,"Female"),"Female",
                                      str_detect(no_spouse_sex,"Male"),"Male",
                                      default = "Sex not known")]
bg_hhTypeTenure[,("family_type_7"):=fcase(str_detect(family_type,"Female")&
                                            no_spouse_sex=="Living alone",
                                          "Female householder (solitary)",
                                          str_detect(family_type,"Male")&
                                            no_spouse_sex=="Living alone",
                                          "Male householder (solitary)",
                                          no_spouse_sex=="Female householder, no spouse present",
                                          "Female householder (family, no spouse, not alone)", #b/c it deals with family households in a funny way
                                          no_spouse_sex=="Male householder, no spouse present",
                                          "Male householder (family, no spouse, not alone)",
                                          family_type=="Married couple",
                                          "Married couple family",
                                          str_detect(family_type,"householder")&
                                            no_spouse_sex=="Not living alone"&
                                            family=="Family households",
                                          "Family households",
                                          default = family_type)]

#do with hh_over_64 first; keeping bg_hhTypeTenure age_range for end...
bg_hhTypeTenure[,("bg_TT_match_id"):=
              paste0(GEOID,family,family_type_7,sex_match,age_range_3,as.character(100000+sample(1:.N))),
            by=.(GEOID,family,family_type_7,sex_match,age_range_3)]
bg_hhOwnKids[,("bg_TT_match_id"):=
               paste0(GEOID,family_2,family_type_7,sex,hh_over_64,as.character(100000+sample(1:.N))),
             by=.(GEOID,family_2,family_type_7,sex,hh_over_64)]
bg_hhTypeTenure[,c("hh_type_3","hh_type_4","rel_in_house","own_kids",
                   "sex","same_sex","couple_gender","alone","family_4","family_type_4"):=
                  bg_hhOwnKids[.SD,c(list(hh_type_3),list(household_type_4),list(rel_in_house),list(own_kids),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                 list(family),list(family_type)),on=.(bg_TT_match_id)]]
bg_hhOwnKids[,("match_TT"):=
                  bg_hhTypeTenure[.SD,list(hh_type_3),on=.(bg_TT_match_id)]]
#nrow(bg_hhOwnKids[is.na(match_TT)])#9597807 #within a couple dozen of all hh_over_64 matched
#then without age 
bg_hhTypeTenure[is.na(hh_type_3),("bg_TTa_match_id"):=
                  paste0(GEOID,family,family_type_7,sex_match,as.character(100000+sample(1:.N))),
                by=.(GEOID,family,family_type_7,sex_match)]
bg_hhOwnKids[is.na(match_TT),("bg_TTa_match_id"):=
               paste0(GEOID,family_2,family_type_7,sex,as.character(100000+sample(1:.N))),
             by=.(GEOID,family_2,family_type_7,sex)]
bg_hhTypeTenure[is.na(hh_type_3),c("hh_type_3","hh_type_4","rel_in_house","own_kids",
                   "sex","same_sex","couple_gender","alone","family_4","family_type_4"):=
                  bg_hhOwnKids[.SD,c(list(hh_type_3),list(household_type_4),list(rel_in_house),list(own_kids),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                     list(family),list(family_type)),on=.(bg_TTa_match_id)]]
bg_hhOwnKids[is.na(match_TT),("match_TT"):=
               bg_hhTypeTenure[.SD,list(hh_type_3),on=.(bg_TTa_match_id)]]
#nrow(bg_hhOwnKids[is.na(match_TT)])#2858399
bg_hhOwnKids[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
bg_hhTypeTenure[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
##same at tract level - for family_type_7 no additional matches by moving to tract! That's surprising, but looks like things just matched
#just block group; get sex on householder for each that is available, then keep that part of the match?
bg_hhTypeTenure[is.na(hh_type_3),("bg_TTbg_match_id"):=
                  paste0(GEOID,family,sex_match,as.character(100000+sample(1:.N))),
                by=.(GEOID,family,sex_match)]
bg_hhOwnKids[is.na(match_TT),("bg_TTbg_match_id"):=
               paste0(GEOID,family_2,sex,as.character(100000+sample(1:.N))),
             by=.(GEOID,family_2,sex)]
bg_hhTypeTenure[is.na(hh_type_3),c("hh_type_3","hh_type_4","rel_in_house","own_kids",
                                   "sex","same_sex","couple_gender","alone","family_4","family_type_4"):=
                  bg_hhOwnKids[.SD,c(list(hh_type_3),list(household_type_4),list(rel_in_house),list(own_kids),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                     list(family),list(family_type)),on=.(bg_TTbg_match_id)]]
bg_hhOwnKids[is.na(match_TT),("match_TT"):=
               bg_hhTypeTenure[.SD,list(hh_type_3),on=.(bg_TTbg_match_id)]]
#nrow(bg_hhOwnKids[is.na(match_TT)])#165682 
#with sex, but at tract
bg_hhTypeTenure[is.na(hh_type_3),("tr_TTbg_match_id"):=
                  paste0(tract,family,sex_match,as.character(100000+sample(1:.N))),
                by=.(tract,family,sex_match)]
bg_hhOwnKids[is.na(match_TT),("tr_TTbg_match_id"):=
               paste0(tract,family_2,sex,as.character(100000+sample(1:.N))),
             by=.(tract,family_2,sex)]
bg_hhTypeTenure[is.na(hh_type_3),c("hh_type_3","hh_type_4","rel_in_house","own_kids",
                                   "sex","same_sex","couple_gender","alone","family_4","family_type_4"):=
                  bg_hhOwnKids[.SD,c(list(hh_type_3),list(household_type_4),list(rel_in_house),list(own_kids),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                     list(family),list(family_type)),on=.(tr_TTbg_match_id)]]
bg_hhOwnKids[is.na(match_TT),("match_TT"):=
               bg_hhTypeTenure[.SD,list(hh_type_3),on=.(tr_TTbg_match_id)]]
#nrow(bg_hhOwnKids[is.na(match_TT)])#60795
#without sex
bg_hhTypeTenure[is.na(hh_type_3),("bg_TTbg_match_id"):=
                  paste0(GEOID,family,as.character(100000+sample(1:.N))),
                by=.(GEOID,family)]
bg_hhOwnKids[is.na(match_TT),("bg_TTbg_match_id"):=
               paste0(GEOID,family_2,as.character(100000+sample(1:.N))),
             by=.(GEOID,family_2)]
bg_hhTypeTenure[is.na(hh_type_3),c("hh_type_3","hh_type_4","rel_in_house","own_kids",
                                   "sex","same_sex","couple_gender","alone","family_4","family_type_4"):=
                  bg_hhOwnKids[.SD,c(list(hh_type_3),list(household_type_4),list(rel_in_house),list(own_kids),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                     list(family),list(family_type)),on=.(bg_TTbg_match_id)]]
bg_hhOwnKids[is.na(match_TT),("match_TT"):=
               bg_hhTypeTenure[.SD,list(hh_type_3),on=.(bg_TTbg_match_id)]]
#nrow(bg_hhOwnKids[is.na(match_TT)])#24314 
#at tract
bg_hhTypeTenure[is.na(hh_type_3),("tr_TTbg_match_id"):=
                  paste0(tract,family,as.character(100000+sample(1:.N))),
                by=.(tract,family)]
bg_hhOwnKids[is.na(match_TT),("tr_TTbg_match_id"):=
               paste0(tract,family_2,as.character(100000+sample(1:.N))),
             by=.(tract,family_2)]
bg_hhTypeTenure[is.na(hh_type_3),c("hh_type_3","hh_type_4","rel_in_house","own_kids",
                                   "sex","same_sex","couple_gender","alone","family_4","family_type_4"):=
                  bg_hhOwnKids[.SD,c(list(hh_type_3),list(household_type_4),list(rel_in_house),list(own_kids),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                     list(family),list(family_type)),on=.(tr_TTbg_match_id)]]
bg_hhOwnKids[is.na(match_TT),("match_TT"):=
               bg_hhTypeTenure[.SD,list(hh_type_3),on=.(tr_TTbg_match_id)]]
#nrow(bg_hhOwnKids[is.na(match_TT)])#10496
#table(bg_hhOwnKids[is.na(match_TT),family_2]) 
#doing at tract - had 357 not match at block_group with no other match
bg_hhTypeTenure[is.na(hh_type_3),("tr_TT_match_id"):=
                  paste0(tract,as.character(100000+sample(1:.N))),
                by=.(tract)]
bg_hhOwnKids[is.na(match_TT),("tr_TT_match_id"):=
               paste0(tract,as.character(100000+sample(1:.N))),
             by=.(tract)]
bg_hhTypeTenure[is.na(hh_type_3),c("hh_type_3","hh_type_4","rel_in_house","own_kids",
                                   "sex","same_sex","couple_gender","alone","family_4","family_type_4"):=
                  bg_hhOwnKids[.SD,c(list(hh_type_3),list(household_type_4),list(rel_in_house),list(own_kids),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                     list(family),list(family_type)),on=.(tr_TT_match_id)]]
bg_hhOwnKids[is.na(match_TT),("match_TT"):=
               bg_hhTypeTenure[.SD,list(hh_type_3),on=.(tr_TT_match_id)]]
#nrow(bg_hhOwnKids[is.na(match_TT)])
#need to do the little clean up on things that are weird
#table(bg_hhTypeTenure[,sex],bg_hhTypeTenure[,family_type_7]) #almost 800 screwed up on sex 
bg_hhTypeTenure[,("sex"):=fcase(str_detect(family_type_7,"Female"),"Female",
                                str_detect(family_type_7,"Male"),"Male",
                                default = sex)]
#alone is broken in the several thousand range
bg_hhTypeTenure[,("alone"):=fcase(str_detect(family_type_7,"solitary"),"Living alone",
                                  str_detect(family_type_7,"not alone"),"Not living alone",
                                  str_detect(family_type_7,"Married"),"Not living alone",
                                  default = alone)]
#table(bg_hhTypeTenure[,family_type_4],bg_hhTypeTenure[,family_type_7])
#seems to be something wrong with hh_type_4
#family_type_4, hh_type, etc. has a few things that don't match: mostly a couple of dozen some closer to 800 off.
#some of the other hh_type things are further off; have to think about the strategy for retrieving them... is it codom? or just redo?
#need to draw out what needs to be saved; what if the ones that don't match are considered clues for secondary elements in the ordering/matching?
rm(bg_hhOwnKids)

#add to P16, then put in relations
#one of the two distributions with full race/eth
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
bg_hhTypeRE[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
rm(bg_hhTypeRE_data_from_census)
rm(bg_hhTypeRE_data)
rm(bg_hhTypeRE_melted)
#test <- table(bg_hhTypeRE[,GEOID],bg_hhTypeRE[,family])==
#  table(bg_hhTypeTenure[,GEOID],bg_hhTypeTenure[,family])
#length(test[test==FALSE])==0
#clean up for matching
bg_hhTypeRE[,("match_type_5"):=fcase(family_type=="Other family",no_spouse_sex,
                                     family_type=="Householder not living alone",
                                     "Not living alone",
                                     family_type=="Householder living alone",
                                     "Living alone",
                                     family_type=="Married couple family",
                                     "Married couple",
                                  default = "family_type")]

#move bg_hhTypeTenure over
bg_hhTypeTenure[,("bg_TTre_match_id"):=
                  paste0(GEOID,no_spouse_sex,as.character(100000+sample(1:.N))),
                by=.(GEOID,no_spouse_sex)]
bg_hhTypeRE[,("bg_TTre_match_id"):=
               paste0(GEOID,match_type_5,as.character(100000+sample(1:.N))),
             by=.(GEOID,match_type_5)]
bg_hhTypeRE[,c("rent_own","rel_in_house","own_kids","age_range_3",
                   "sex","same_sex","couple_gender","alone","family",
                   "family_type_4","family_type_7"):=
              bg_hhTypeTenure[.SD,c(list(rent_own),list(rel_in_house),list(own_kids),list(age_range_3),
                                     list(sex),list(same_sex),list(couple_gender),list(alone),
                                     list(family),list(family_type),list(family_type_7)),on=.(bg_TTre_match_id)]]
bg_hhTypeTenure[,("match_TTre"):=
               bg_hhTypeRE[.SD,list(match_type_5),on=.(bg_TTre_match_id)]]
nrow(bg_hhTypeTenure[is.na(match_TTre)])==0 #0
rm(bg_hhTypeTenure)

#get own_kids with race/eth and family_type; will create a codomain 
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
#create a matching variable for bg_hhTypeRE, below
tr_hhTypeOwnKidsRE[,("family_type_7"):=fcase(family_type=="Female householder, no spouse present",
                                             "Female householder (family, no spouse, not alone)",
                                             family_type=="Male householder, no spouse present",
                                             "Male householder (family, no spouse, not alone)",
                                             family_2=="Married couple family",
                                             "Married couple family",
                                             default = family_2)] #others are nonfamily
tr_hhTypeOwnKidsR <- tr_hhTypeOwnKidsRE[!re_code %in% c("H","I")] #is right number...
tr_hhTypeOwnKidsH <- tr_hhTypeOwnKidsRE[re_code == "H"]
tr_hhTypeOwnKidsI <- tr_hhTypeOwnKidsRE[re_code == "I"]
rm(tr_hhTypeOwnKidsRE_data_from_census)
rm(tr_hhTypeOwnKidsRE_data)
rm(tr_hhTypeOwnKidsRE_melted)

#add I to the A in tr_hhTypeOwnKidsR, 
tr_hhTypeOwnKidsR[re_code=="A",("tr_ownkidsRI_match_id"):=
                    paste0(GEOID,family_2,family_type,own_kids,kid_age_2,as.character(100000+sample(1:.N))),
                  by=.(GEOID,family_2,family_type,own_kids,kid_age_2)]
tr_hhTypeOwnKidsI[,("tr_ownkidsRI_match_id"):=
                    paste0(GEOID,family_2,family_type,own_kids,kid_age_2,as.character(100000+sample(1:.N))),
                  by=.(GEOID,family_2,family_type,own_kids,kid_age_2)]
tr_hhTypeOwnKidsR[re_code=="A",c("copath_re_code","codom_tr_hhTypeOwnKidsRE"):=
                    tr_hhTypeOwnKidsI[.SD,c(list(re_code),list(codom_tr_hhTypeOwnKidsRE)),on=.(tr_ownkidsRI_match_id)]]
tr_hhTypeOwnKidsI[,("match_R"):=
                    tr_hhTypeOwnKidsR[.SD,list(re_code),on=.(tr_ownkidsRI_match_id)]]
#nrow(tr_hhTypeOwnKidsI[is.na(match_R)])==0
tr_hhTypeOwnKidsR[,c("copath_re_code"):=fcase(re_code=="A"&is.na(copath_re_code),"P",default = copath_re_code)]
#I and P match family in bg_hhTypeRE
rm(tr_hhTypeOwnKidsI)
#find HnotP for later ordering merge with re_code_14 distribution
tr_hhTypeOwnKidsH[,("tr_ownkidsH_match_id"):=
                    paste0(GEOID,family_2,family_type,own_kids,kid_age_2,as.character(100000+sample(1:.N))),
                  by=.(GEOID,family_2,family_type,own_kids,kid_age_2)]
tr_hhTypeOwnKidsR[copath_re_code=="P",("tr_ownkidsH_match_id"):=
                    paste0(GEOID,family_2,family_type,own_kids,kid_age_2,as.character(100000+sample(1:.N))),
                  by=.(GEOID,family_2,family_type,own_kids,kid_age_2)]
tr_hhTypeOwnKidsH[,c("copath_re_code","codom_tr_hhTypeOwnKidsRE"):=
                    tr_hhTypeOwnKidsR[.SD,c(list(copath_re_code),list(codom_tr_hhTypeOwnKidsRE)),on=.(tr_ownkidsH_match_id)]]
tr_hhTypeOwnKidsR[copath_re_code=="P",("match_H"):= #just for housekeeping, mixes re_code_14
                    tr_hhTypeOwnKidsH[.SD,list(re_code),on=.(tr_ownkidsH_match_id)]]
tr_hhTypeOwnKidsHnotP <- tr_hhTypeOwnKidsH[is.na(copath_re_code)]
#matches total for Q-V family households in bg_hhTypeRE
rm(tr_hhTypeOwnKidsH)

#add HnotP to bg_hhTypeRE[re_code%in%c("Q","R","S","T","U","V") #need to check that family_type_7 replaces: &family=="Family households"]
tr_hhTypeOwnKidsHnotP[,("tr_ownkidsHnotP_match_id"):=
                        paste0(GEOID,family_type_7,own_kids,as.character(100000+sample(1:.N))),
                      by=.(GEOID,family_type_7,own_kids)]
bg_hhTypeRE[re_code%in%c("Q","R","S","T","U","V"),("tr_ownkidsHnotP_match_id"):=
              paste0(tract,family_type_7,own_kids,as.character(100000+sample(1:.N))),
            by=.(tract,family_type_7,own_kids)]
bg_hhTypeRE[re_code%in%c("Q","R","S","T","U","V"),("kid_age_2"):=
              tr_hhTypeOwnKidsHnotP[.SD,list(kid_age_2),on=.(tr_ownkidsHnotP_match_id)]]
tr_hhTypeOwnKidsHnotP[,("match_bg"):=
                        bg_hhTypeRE[.SD,list(family_type_7),on=.(tr_ownkidsHnotP_match_id)]]
nrow(tr_hhTypeOwnKidsHnotP[is.na(match_bg)]) #151348, which is better than one might expect given that the source of own_kids (P20) didn't have RE
#distribute again, writing over the own_kids on bg_hhTypeRE
tr_hhTypeOwnKidsHnotP[is.na(match_bg),("tr_ownkidsHnotP_match2_id"):=
                        paste0(GEOID,family_type_7,as.character(100000+sample(1:.N))),
                      by=.(GEOID,family_type_7)]
bg_hhTypeRE[re_code%in%c("Q","R","S","T","U","V")&is.na(kid_age_2),("tr_ownkidsHnotP_match2_id"):=
              paste0(tract,family_type_7,as.character(100000+sample(1:.N))),
            by=.(tract,family_type_7)]
bg_hhTypeRE[re_code%in%c("Q","R","S","T","U","V")&is.na(kid_age_2),("kid_age_2"):=
              tr_hhTypeOwnKidsHnotP[.SD,list(kid_age_2),on=.(tr_ownkidsHnotP_match2_id)]]
tr_hhTypeOwnKidsHnotP[is.na(match_bg),("match_bg"):=
                        bg_hhTypeRE[.SD,list(family_type_7),on=.(tr_ownkidsHnotP_match2_id)]]
nrow(tr_hhTypeOwnKidsHnotP[is.na(match_bg)])==0
rm(tr_hhTypeOwnKidsHnotP)

#distribute for I and P
#nrow(tr_hhTypeOwnKidsR[!is.na(copath_re_code)])==nrow(bg_hhTypeRE[re_code%in%c("I","P")&family=="Family households"])
tr_hhTypeOwnKidsR[re_code=="A",("tr_ownkidsIP_match_id"):=
                        paste0(GEOID,family_type_7,own_kids,as.character(100000+sample(1:.N))),
                      by=.(GEOID,family_type_7,own_kids)]
bg_hhTypeRE[re_code%in%c("I","P"),("tr_ownkidsIP_match_id"):=
              paste0(tract,family_type_7,own_kids,as.character(100000+sample(1:.N))),
            by=.(tract,family_type_7,own_kids)]
bg_hhTypeRE[re_code%in%c("I","P"),("kid_age_2"):=
              tr_hhTypeOwnKidsR[.SD,list(kid_age_2),on=.(tr_ownkidsIP_match_id)]]
tr_hhTypeOwnKidsR[re_code=="A",("match_bgIP"):=
                        bg_hhTypeRE[.SD,list(re_code),on=.(tr_ownkidsIP_match_id)]]
nrow(tr_hhTypeOwnKidsR[re_code=="A"])-nrow(tr_hhTypeOwnKidsR[!is.na(match_bgIP)]) #170226, which is better than one might expect given that the source of own_kids (P20) didn't have RE
#distribute again, writing over the own_kids on bg_hhTypeRE
tr_hhTypeOwnKidsR[re_code=="A"&is.na(match_bgIP),("tr_ownkidsIP_match2_id"):=
                        paste0(GEOID,family_type_7,as.character(100000+sample(1:.N))),
                      by=.(GEOID,family_type_7)]
bg_hhTypeRE[re_code%in%c("I","P")&is.na(kid_age_2),("tr_ownkidsIP_match2_id"):=
              paste0(tract,family_type_7,as.character(100000+sample(1:.N))),
            by=.(tract,family_type_7)]
bg_hhTypeRE[re_code%in%c("I","P")&is.na(kid_age_2),("kid_age_2"):=
              tr_hhTypeOwnKidsR[.SD,list(kid_age_2),on=.(tr_ownkidsIP_match2_id)]]
tr_hhTypeOwnKidsR[re_code=="A"&is.na(match_bgIP),("match_bgIP"):=
                        bg_hhTypeRE[.SD,list(family_type_7),on=.(tr_ownkidsIP_match2_id)]]
nrow(tr_hhTypeOwnKidsR[re_code=="A"])-nrow(tr_hhTypeOwnKidsR[!is.na(match_bgIP)]) 
rm(tr_hhTypeOwnKidsR)
rm(tr_hhTypeOwnKidsRE)

groupname <- "H13" #HOUSEHOLDER AGE / TENURE / RACE / ETHx2
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhTenureARE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
#tract level with same groupname does not have more categories
if(names(bg_hhTenureARE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("rent_own","age_range_9")
  #row_c1 determined by hand 
  row_c1 <- c(unique(bg_hhTenureARE_data_from_census[!is.na(label_2) & concept!="TENURE BY AGE OF HOUSEHOLDER",name])) #test with:  & !str_detect(concept,"HISPANIC")
  #test_total_pop <- tests_download_data(bg_hhTenureARE_data_from_census,label_c1,row_c1,state=state)
  #do this with HvL, to divide later, since don't have whole population by both
  bg_hhTenureARE_data <- relabel(bg_hhTenureARE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhTenureARE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhTenureARE_data <- bg_hhTenureARE_data_from_census
}
bg_hhTenureARE_data[,("re_code") := substr(name,4,4)][
  ,("race") := str_replace(concept,"TENURE BY AGE OF HOUSEHOLDER \\(","")][
    ,("race") := str_replace(race,"\\)","")]

#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhTenureARE_data[,.SD,.SDcols = startsWith(names(bg_hhTenureARE_data),state)])
bg_hhTenureARE_melted <- melt(bg_hhTenureARE_data, id.vars = c("re_code","race","rent_own","age_range_9"), measure.vars = Geoids,
                              value.name = "codom_hhAge", variable.name = "GEOID")
bg_hhTenureARE <- as.data.table(lapply(bg_hhTenureARE_melted[,.SD],rep,bg_hhTenureARE_melted[,codom_hhAge]))
rm(bg_hhTenureARE_data)
rm(bg_hhTenureARE_data_from_census)
rm(bg_hhTenureARE_melted)
bg_hhTenureARE[,("age_range_3"):=fcase(as.numeric(substr(age_range_9,13,14))<35,
                                       "Householder 15 to 34 years",
                                       as.numeric(substr(age_range_9,13,14))>34&as.numeric(substr(age_range_9,13,14))<65,
                                       "Householder 35 to 64 years",
                                       default = "Householder 65 years and over")]
bg_hhTenureARE[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
bg_hhTenureAR <- bg_hhTenureARE[!re_code %in% c("H","I")]
bg_hhTenureAE <- bg_hhTenureARE[re_code %in% c("H","I")]
rm(bg_hhTenureARE)

#NEED TO RETHINK A BIT OF BELOW!!!
#move I over to R
bg_hhTenureAR[re_code=="A",("bg_hhTenureRI_match_id"):=
                    paste0(GEOID,rent_own,age_range_9,as.character(100000+sample(1:.N))),
                  by=.(GEOID,rent_own,age_range_9)]
bg_hhTenureAE[re_code=="I",("bg_hhTenureRI_match_id"):=
              paste0(GEOID,rent_own,age_range_9,as.character(100000+sample(1:.N))),
            by=.(GEOID,rent_own,age_range_9)]
bg_hhTenureAR[re_code=="A",("re_code_14"):=
                bg_hhTenureAE[.SD,list(re_code),on=.(bg_hhTenureRI_match_id)]]
bg_hhTenureAE[re_code=="I",("match_bgTIR"):=
                bg_hhTenureAR[.SD,list(re_code),on=.(bg_hhTenureRI_match_id)]]
nrow(bg_hhTenureAE[re_code=="I"])-nrow(bg_hhTenureAE[!is.na(match_bgTIR)])==0
#maybe not do that step, since it is easier to make all the HvL on bg_hhType
bg_hhTenureAR[,("re_code_14"):=fcase(re_code=="A"&is.na(re_code_14),"P",default = re_code_14)]
#have to still distribute H at end



#HvL is broken!!! maybe before this, too!!!!


bg_hhTypeRE[,("HvL"):=fcase(re_code%in%c("P","Q","R","S","T","U","V"),"H",default = "Not H")]
bg_hhTypeRE[,("re_code_7"):=fcase(re_code%in%c("I","P"),"A",
                                  re_code%in%c("J","Q"),"B",
                                  re_code%in%c("K","R"),"C",
                                  re_code%in%c("L","S"),"D",
                                  re_code%in%c("M","T"),"E",
                                  re_code%in%c("N","U"),"F",
                                  re_code%in%c("O","V"),"G",
                                  default = "Not given")]

#move P out of H
bg_hhTenureAR[re_code_14=="P",("bg_hhTenureRP_match_id"):=
                paste0(GEOID,rent_own,age_range_9,as.character(100000+sample(1:.N))),
              by=.(GEOID,rent_own,age_range_9)]
bg_hhTenureAE[re_code=="H",("bg_hhTenureRP_match_id"):=
                paste0(GEOID,rent_own,age_range_9,as.character(100000+sample(1:.N))),
              by=.(GEOID,rent_own,age_range_9)]
bg_hhTenureAR[re_code_14=="P",("HvL"):=
                bg_hhTenureAE[.SD,list(re_code),on=.(bg_hhTenureRP_match_id)]]
bg_hhTenureAE[re_code=="H",("match_bgTH"):=
                bg_hhTenureAR[.SD,list(re_code),on=.(bg_hhTenureRP_match_id)]]
nrow(bg_hhTenureAR[re_code_14=="P"])-nrow(bg_hhTenureAE[!is.na(match_bgTH)])==0
#move over rest of H to Q, R, S, T, U, V
bg_hhTenureAR[re_code!="A",("bg_hhTenureRH_match_id"):=
                paste0(GEOID,rent_own,age_range_9,as.character(100000+sample(1:.N))),
              by=.(GEOID,rent_own,age_range_9)]
bg_hhTenureAE[re_code=="H" & is.na(match_bgTH),("bg_hhTenureRH_match_id"):=
                paste0(GEOID,rent_own,age_range_9,as.character(100000+sample(1:.N))),
              by=.(GEOID,rent_own,age_range_9)]
bg_hhTenureAR[re_code!="A",("HvL"):=
                bg_hhTenureAE[.SD,list(re_code),on=.(bg_hhTenureRH_match_id)]]
bg_hhTenureAE[re_code=="H" & is.na(match_bgTH),("match_bgTH"):=
                bg_hhTenureAR[.SD,list(re_code),on=.(bg_hhTenureRH_match_id)]]
bg_hhTenureAR[,("re_code_14"):=fcase(!is.na(HvL)&re_code=="B","Q",
                                     !is.na(HvL)&re_code=="C","R",
                                     !is.na(HvL)&re_code=="D","S",
                                     !is.na(HvL)&re_code=="E","T",
                                     !is.na(HvL)&re_code=="F","U",
                                     !is.na(HvL)&re_code=="G","V",
                                     is.na(HvL)&re_code=="B","J",
                                     is.na(HvL)&re_code=="C","K",
                                     is.na(HvL)&re_code=="D","L",
                                     is.na(HvL)&re_code=="E","M",
                                     is.na(HvL)&re_code=="F","N",
                                     is.na(HvL)&re_code=="G","O",
                                     default = re_code_14)]
table(bg_hhTenureAR[,re_code_14])
#add to H4 here, to get re_code_14 right and then add to bg_hhTypeRE
#then H4 to start assigning the full re_code_14
groupname <- "H4" #Tenure - by race and mortgage
geo_type <- "block_group"
api_type <- "dec/dhc"
path_suff <- "est"
bg_hhTenureRE_data_from_census <- 
  census_block_get(censusdir, vintage, state, censuskey, 
                   groupname,county_num = "*",
                   api_type,path_suff)
if(names(bg_hhTenureRE_data_from_census)[11]=="label_1"){
  #labels determined by hand
  label_c1 <- c("tenure_1","tenure","re_code","race") 
  bg_hhTenureRE_data_from_census[,("re_code") := substr(name,3,3)][
    ,("race") := str_replace(concept,"TENURE \\(","")][
      ,("race") := str_replace(race,"\\)","")]
  #row_c1 by hand
  row_c1 <- c(unique(bg_hhTenureRE_data_from_census[label!="!!" & label!="Geography" & 
                                                      concept!="TENURE (HISPANIC OR LATINO HOUSEHOLDER)" & 
                                                      concept!="TENURE" & str_detect(concept,"HISP"),name]))
  test_total_pop <- tests_download_data(bg_hhTenureRE_data_from_census,label_c1,row_c1,state=state) #seems to not get right total row!!
  bg_hhTenureRE_data <- relabel(bg_hhTenureRE_data_from_census[!is.na(label)],label_c1,row_c1,groupname)
  write_relabel(bg_hhTenureRE_data,censusdir,vintage,state,censuskey,geo_type,groupname,county_num=county,api_type,path_suff)
}else{
  print("Using already given labels; no rewrite.")
  bg_hhTenureRE_data <- bg_hhTenureRE_data_from_census
}
#reshape a bit and make list of individuals
Geoids <- colnames(bg_hhTenureRE_data[,.SD,.SDcols = startsWith(names(bg_hhTenureRE_data),state)])
bg_hhTenureRE_melted <- melt(bg_hhTenureRE_data, id.vars = c("tenure_1","tenure","re_code","race"), measure.vars = Geoids,
                             value.name = "codom_bg_hhTenureRE", variable.name = "GEOID")
bg_hhTenureRE <- as.data.table(lapply(bg_hhTenureRE_melted[,.SD],rep,bg_hhTenureRE_melted[,codom_bg_hhTenureRE])) #all re_codes
rm(bg_hhTenureRE_data_from_census)
rm(bg_hhTenureRE_data)
rm(bg_hhTenureRE_melted)
#tenure_1 and tenure are the same - weirdness about how they set up the table
bg_hhTenureRE[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")] #checked with HCT1, which has less info but same references
bg_hhTenureRE[,("rent_own"):=fcase(str_detect(tenure,"Renter"),"Renter occupied",
                                   default = "Owner occupied")]
bg_hhTenureRE[,("re_code_7"):=fcase(re_code%in%c("I","P"),"A",
                                  re_code%in%c("J","Q"),"B",
                                  re_code%in%c("K","R"),"C",
                                  re_code%in%c("L","S"),"D",
                                  re_code%in%c("M","T"),"E",
                                  re_code%in%c("N","U"),"F",
                                  re_code%in%c("O","V"),"G",
                                  default = "Not given")]

bg_hhTenureAR[,("bg_hhTTARE_match_id"):=
                paste0(GEOID,re_code_14,rent_own,as.character(100000+sample(1:.N))),
              by=.(GEOID,re_code_14,rent_own)]
bg_hhTenureRE[,("bg_hhTTARE_match_id"):=
              paste0(GEOID,re_code,rent_own,as.character(100000+sample(1:.N))),
            by=.(GEOID,re_code,rent_own)]
bg_hhTenureAR[,("tenure"):=
                bg_hhTenureRE[.SD,list(tenure),on=.(bg_hhTTARE_match_id)]]
bg_hhTenureRE[,("match_bgTenure"):=
              bg_hhTenureAR[.SD,list(re_code_14),on=.(bg_hhTTARE_match_id)]]
nrow(bg_hhTenureAR[is.na(tenure)]) #got ~90%
#table(bg_hhTenureAR[is.na(tenure),re_code_14]) #I and P are fully matched
#write ground re_code_14 from TenureRE, but may have loss of some age details by re_code
bg_hhTenureAR[is.na(tenure),("bg_hhTTARE1_match_id"):=
                paste0(GEOID,re_code,rent_own,as.character(100000+sample(1:.N))),
              by=.(GEOID,re_code,rent_own)]
bg_hhTenureRE[is.na(match_bgTenure),("bg_hhTTARE1_match_id"):=
                paste0(GEOID,re_code_7,rent_own,as.character(100000+sample(1:.N))),
              by=.(GEOID,re_code_7,rent_own)]
bg_hhTenureAR[is.na(tenure),c("tenure","re_code_14"):=
                bg_hhTenureRE[.SD,c(list(tenure),list(re_code)),on=.(bg_hhTTARE1_match_id)]]
bg_hhTenureRE[is.na(match_bgTenure),("match_bgTenure"):=
                bg_hhTenureAR[.SD,list(re_code_14),on=.(bg_hhTTARE1_match_id)]]
nrow(bg_hhTenureAR[is.na(tenure)])==0

#move over to bg_hhTypeRE; want to keep the bg_hhTenure for rent_own by age and race, but need the codom for family_type
bg_hhTenureAR[,("bg_hhTTIP_match_id"):=
                paste0(GEOID,re_code_14,rent_own,age_range_3,as.character(100000+sample(1:.N))),
              by=.(GEOID,re_code_14,rent_own,age_range_3)]
bg_hhTypeRE[,("bg_hhTTIP_match_id"):=
                paste0(GEOID,re_code,rent_own,age_range_3,as.character(100000+sample(1:.N))),
              by=.(GEOID,re_code,rent_own,age_range_3)]
bg_hhTenureAR[,("match_bgIP"):=
                bg_hhTypeRE[.SD,list(re_code),on=.(bg_hhTTIP_match_id)]]
bg_hhTypeRE[,("age_range_9"):=
                bg_hhTenureAR[.SD,list(age_range_9),on=.(bg_hhTTIP_match_id)]]
nrow(bg_hhTenureAR)-nrow(bg_hhTypeRE[!is.na(age_range_9)]) #1235318/10491147 = .117 not matching; when not doing the re_code from bg_hhTenureRE, it was 18%
nrow(bg_hhTenureAR[re_code=="A"])-nrow(bg_hhTypeRE[re_code_7=="A" & !is.na(age_range_9)]) #482137/10491147 = .046; basically same with and without bg_hhTenureRE
#without matching on re_code; move AR over, since TypeRE was assigned with some error
#if just rent_own, age_range_3, can finish matching in one step.
#with re_code_7 (is there a rationale for Hvl first?)
bg_hhTenureAR[is.na(match_bgIP),("bg_hhTTr_match_id"):=
                paste0(GEOID,re_code,rent_own,age_range_3,as.character(100000+sample(1:.N))),
              by=.(GEOID,re_code,rent_own,age_range_3)]
bg_hhTypeRE[is.na(age_range_9),("bg_hhTTr_match_id"):=
              paste0(GEOID,re_code_7,rent_own,age_range_3,as.character(100000+sample(1:.N))),
            by=.(GEOID,re_code_7,rent_own,age_range_3)]
bg_hhTenureAR[is.na(match_bgIP),("match_bgIP"):=
                bg_hhTypeRE[.SD,list(re_code),on=.(bg_hhTTr_match_id)]]
bg_hhTypeRE[is.na(age_range_9),c("age_range_9","HvL"):=
              bg_hhTenureAR[.SD,c(list(age_range_9),list(HvL)),on=.(bg_hhTTr_match_id)]]
nrow(bg_hhTenureAR)-nrow(bg_hhTypeRE[!is.na(age_range_9)]) #1011303

#with HvL
bg_hhTenureAR[is.na(match_bgIP),("bg_hhTTe_match_id"):=
                paste0(GEOID,HvL,rent_own,age_range_3,as.character(100000+sample(1:.N))),
              by=.(GEOID,HvL,rent_own,age_range_3)]
bg_hhTypeRE[is.na(age_range_9),("bg_hhTTe_match_id"):=
              paste0(GEOID,HvL,rent_own,age_range_3,as.character(100000+sample(1:.N))),
            by=.(GEOID,HvL,rent_own,age_range_3)]
bg_hhTenureAR[is.na(match_bgIP),("match_bgIP"):=
                bg_hhTypeRE[.SD,list(re_code),on=.(bg_hhTTe_match_id)]]
bg_hhTypeRE[is.na(age_range_9),c("age_range_9","re_code_7"):=
              bg_hhTenureAR[.SD,c(list(age_range_9),list(re_code)),on=.(bg_hhTTe_match_id)]]
nrow(bg_hhTenureAR)-nrow(bg_hhTypeRE[!is.na(age_range_9)]) #431113 / became 412215 with bg_hhTenureRE
#just rent_own and age
bg_hhTenureAR[is.na(match_bgIP),("bg_hhTTa_match_id"):=
                paste0(GEOID,rent_own,age_range_3,as.character(100000+sample(1:.N))),
              by=.(GEOID,rent_own,age_range_3)]
bg_hhTypeRE[is.na(age_range_9),("bg_hhTTa_match_id"):=
              paste0(GEOID,rent_own,age_range_3,as.character(100000+sample(1:.N))),
            by=.(GEOID,rent_own,age_range_3)]
bg_hhTenureAR[is.na(match_bgIP),("match_bgIP"):=
                bg_hhTypeRE[.SD,list(re_code),on=.(bg_hhTTa_match_id)]]
bg_hhTypeRE[is.na(age_range_9),c("age_range_9","re_code_7","HvL"):=
              bg_hhTenureAR[.SD,c(list(age_range_9),list(re_code),list(HvL)),on=.(bg_hhTTa_match_id)]]
nrow(bg_hhTenureAR)-nrow(bg_hhTypeRE[!is.na(age_range_9)]) == 0
#need to reset re_code on bg_hhTypeRE to match each other -

bg_hhTypeRE[,("re_code_14"):=fcase(HvL=="H"&re_code_7=="B","Q",
                                     HvL=="H"&re_code_7=="C","R",
                                     HvL=="H"&re_code_7=="D","S",
                                     HvL=="H"&re_code_7=="E","T",
                                     HvL=="H"&re_code_7=="F","U",
                                     HvL=="H"&re_code_7=="G","V",
                                     HvL!="H"&re_code_7=="B","J",
                                     HvL!="H"&re_code_7=="C","K",
                                     HvL!="H"&re_code_7=="D","L",
                                     HvL!="H"&re_code_7=="E","M",
                                     HvL!="H"&re_code_7=="F","N",
                                     HvL!="H"&re_code_7=="G","O",
                                     default = re_code)]
#test <- table(bg_hhTypeRE[,GEOID],
#              bg_hhTypeRE[,re_code_7],
#              bg_hhTypeRE[,rent_own],
#              bg_hhTypeRE[,age_range_9])==
#  table(bg_hhTenureAR[,GEOID],
#        bg_hhTenureAR[,re_code],
#        bg_hhTenureAR[,rent_own],
#        bg_hhTenureAR[,age_range_9])
#length(test[test==FALSE])==0
#
#test <- table(bg_hhTenureRE[,GEOID],
#              bg_hhTenureRE[,re_code],
#              bg_hhTenureRE[,rent_own])==
#  table(bg_hhTenureAR[,GEOID],
#        bg_hhTenureAR[,re_code_14],
#        bg_hhTenureAR[,rent_own])
#length(test[test==FALSE])==0

test <- table(bg_hhTypeRE[,GEOID],
              bg_hhTypeRE[,re_code_14],
              bg_hhTypeRE[,rent_own])==
  table(bg_hhTenureRE[,GEOID],
        bg_hhTenureRE[,re_code],
        bg_hhTenureRE[,rent_own])
length(test[test==FALSE]) #265800 (521864 cells) totals are right, but lots not matching in GEOID  

test <- table(bg_hhTypeRE[,re_code_7],
              bg_hhTypeRE[,rent_own])==
  table(bg_hhTenureRE[,re_code_7],
        bg_hhTenureRE[,rent_own])
length(test[test==FALSE])==0

test <- table(bg_hhTypeRE[,re_code],
              bg_hhTypeRE[,rent_own])==
  table(bg_hhTenureAR[,re_code_14],
        bg_hhTenureAR[,rent_own])
length(test[test==FALSE]) #28 wrong of 28...

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
tr_hh65RelR <- tr_hh65RelRE[!re_code %in% c("H","I")]
tr_hh65RelE <- tr_hh65RelRE[re_code %in% c("H","I")]
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
tr_hhRelRE_melted[,("age_range_2"):=ifelse(role=="Foster child","under_18",age_range_2)] #since it doesn't say, we assume only until 18.
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

#match for over64 (some potential info loss because of role_7 instead of role_18, but everything has a match and no other tables overlap on role and age)
tr_hhRelRE[,("re65_match_id"):=
      paste0(GEOID,re_code,household,role_7,sex,alone,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code,household,role_7,sex,alone)]
tr_hh65RelRE[,("re65_match_id"):=
      paste0(GEOID,re_code,household,role,sex,alone,as.character(100000+sample(1:.N))),
    by=.(GEOID,re_code,household,role,sex,alone)]
tr_hhRelRE[,c("over_64","codom_tr_hh65RelRE"):=
            tr_hh65RelRE[.SD,c(list(re_code),list(codom_tr_hh65RelRE)),on=.(re65_match_id)]]
#table(tr_hhRelRE[,over_64])==table(tr_hh65RelRE[,re_code]) #using re_code to test all moved over
#nrow(tr_hhRelRE[!is.na(over_64)])==nrow(tr_hh65RelRE)
tr_hhRelRE[,("over_64"):=fcase(!is.na(over_64),"over_64",default = "under_65")]
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
test_hh <- table(bg_hhRel[role=="Householder",GEOID])-table(bg_hhTypeRE[,GEOID])
sum(test_hh) #117
sum(abs(test_hh)) #207091
mean(abs(test_hh)) #11.1112
max(abs(test_hh)) #107
#tract is not perfect, but fewer problems... still need an overall approach; doing first 100000 because they don't have same tracts (non-conformable arrays)
#will need tract on bg_hhRel in any case, so test here
bg_hhRel[,("tract"):=str_remove_all(substr(GEOID,1,13),"_")]
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
#get the little bits of overlap right...
bg_hhTypeRE[,("alone"):=fcase(family_type=="Householder living alone","Living alone",
                              default = "Not living alone")]
bg_hhTypeRE[,("sex"):=fcase(no_spouse_sex=="Female householder, no spouse present","Female",
                            no_spouse_sex=="Male householder, no spouse present","Male",
                              default = "Not known")]
#order by HvL then not HvL (letters follow already) #sometimes seems to not order if only in i
bg_hhTypeRE <- bg_hhTypeRE[order(GEOID,-re_code,alone,sex)]
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
           bg_hhTypeRE[.SD,c(list(re_code),list(race),list(family),list(family_type),
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
           bg_hhTypeRE[.SD,c(list(re_code),list(race),list(family),list(family_type),
                             list(no_spouse_sex),list(codom_hhTypeRE)),on=.(bg_RTA_match_id)]]
bg_hhTypeRE[is.na(matched_rel),("matched_rel"):=
              bg_hhRel[.SD,list(copath_re_code),on=.(bg_RTA_match_id)]]
#nrow(bg_hhTypeRE[is.na(matched_rel)]) #194858, less than two percent, but close to total diff. by bg for hh (207091)
#need all of bg_hhTypeRE info to move over, but after getting the tract Relative data, so it lines up with households

#test_bg_re_code <- table(bg_hhRel[,copath_re_code],bg_hhRel[,GEOID])-table(bg_hhTypeRE[,re_code],bg_hhTypeRE[,GEOID])
#sum(abs(test_bg_re_code)) #194858
#mean(abs(test_bg_re_code)) #.7467
#max(abs(test_bg_re_code)) #72 - i.e., small amounts per re_code difference for each bg
#sum(table(bg_hhTypeRE[,re_code])-(table(bg_hhRel[,copath_re_code])))+117==nrow(bg_hhRel[role=="Householder"])-nrow(bg_hhRel[!is.na(copath_re_code)])
#table(bg_hhTypeRE[,re_code])/(table(bg_hhRel[,copath_re_code])) #(with ordering by copath_HvL)
#remember it would never be a perfect fit b/c of householders being defined differently. 
#       I        J        K        L        M        N        O        P        Q        R        S        T        U        V 
#.  1.037912 1.004984 1.005896 1.001059 1.004211 1.004870 1.001504 1.006802 1.000215 1.000710 1.000000 1.000000 1.000124 1.000030 
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

#broken, below here...


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



#tract level for PCT13 - more ages
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
tr_hhSARE[,("hh_age_range_9"):=fcase(age_range_23=="15 to 17 years" |
                                       age_range_23=="18 and 19 years" |
                                       age_range_23=="20 years" |
                                       age_range_23=="21 years" |
                                       age_range_23=="22 to 24 years","15 to 24 years",
                                     age_range_23=="25 to 29 years" |
                                       age_range_23=="30 to 34 years","25 to 34 years",
                                     age_range_23=="35 to 39 years" |
                                       age_range_23=="40 to 44 years","35 to 44 years",
                                     age_range_23=="45 to 49 years" |
                                       age_range_23=="50 to 54 years","45 to 54 years",
                                     age_range_23=="55 to 59 years","55 to 59 years",
                                     age_range_23=="60 and 61 years" |
                                       age_range_23=="62 to 64 years","60 to 64 years",
                                     age_range_23=="65 and 66 years" |
                                       age_range_23=="67 to 69 years" |
                                       age_range_23=="70 to 74 years","65 to 74 years",
                                     age_range_23=="75 to 79 years" |
                                       age_range_23=="80 to 84 years","75 to 84 years",
                                     age_range_23=="85 years and over","85 years and over",default="not householder")]
#why doesn't it have same number total as tr_hhRelRE? (check for whether it's just GQ...)still 99921 different for households (no GQ)
#should we bias 15-24 towards older for matching on hh???
#tr_hhSAR <- tr_hhSARE[!re_code %in% c("H","I")]
#tr_hhSAE <- tr_hhSARE[re_code %in% c("H","I")]
rm(tr_hhSARE_data_from_census)
rm(tr_hhSARE_data)
rm(tr_hhSARE_melted)



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


#do through the multi-gen as a group, then add to above; doing tracts together first, then to block-group
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

#the missing folks from PCT4 are all from Non-family households, so it doesn't change any other factors
#they also lost the nonfamily designation for a bunch of the tr_hh60SizeType
tr_hh60SizeType[is.na(family_type),("family_type"):="Nonfamily households"]
#also want to catch matches for no_spouse, so be sure to get them
#UGH - b/c it's important to know how many nonfamily householders are present, let's keep joining to larger group...
#join tr_hh60SizeType to tr_hh60Type
tr_hh60SizeType[,("tr_60ST_match_id"):=
                  paste0(GEOID,household_60,household,as.character(100000+sample(1:.N))),
                by=.(GEOID,household_60,household)]
tr_hh60Type[,("tr_60ST_match_id"):=
               paste0(GEOID,household_60,family_type,as.character(100000+sample(1:.N))),
             by=.(GEOID,household_60,family_type)]
tr_hh60SizeType[,c("family_type","no_spouse"):=
                  tr_hh60Type[.SD,c(list(household_type_5),list(spouse)),on=.(tr_60ST_match_id)]]
tr_hh60Type[,("match_60ST"):=
                  tr_hh60SizeType[.SD,list(hh_size_2),on=.(tr_60ST_match_id)]]
nrow(tr_hh60Type[is.na(match_60ST)])


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



#multigen is hard; having parent only counts if you also have kids; having grandkids only counts if their parents are there...
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
