#reapportioning things where the small cells don't make sense at the tract level - which is almost everything, unfortunately.

library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(forcats)

#get HCAD_residences from HCAD_merge or from housingdir main level for desired vintage (by most recent date)
#get others from household and individual generator scripts


#depends on where pulling it from
HCAD_dt <- as.data.table(HCAD_residences)
HCAD_dt[,tract:=droplevels(tract)]
HCAD_dt <- HCAD_dt[!is.na(improv_typ)] #returns 1815741


#a faster sample algorithm is available, with interesting papers on approaches to be uploaded along with
#: https://stackoverflow.com/questions/15113650/faster-weighted-sampling-without-replacement
#we're really not sampling as much anymore - could look at for later time

#getting household_type_size_data from householdsGenerator.R - num_family_id == 1 : 1562813, which is same as nrow in for hh; numeric_in_family lets you expand
hh_size_dt <- as.data.table(household_type_size_data)
#add number of workers per household
hh_workers_dt <- as.data.table(household_workers_data)
hh_size_dt[,("number_in_hh"):=if_else(as.numeric(substr(number_in_family,1,1))>3,"4-or-more-person household",number_in_family)]
hh_size_dt[,("num_workers_id"):=paste0(tract,number_in_hh,as.character(1000000+seq.int(1:.N))),by=.(tract,number_in_hh)]
hh_workers_dt[,("num_workers_id"):=paste0(tract,number_in_hh,as.character(1000000+seq.int(1:.N))),by=.(tract,number_in_hh)]
hh_size_dt[,c("number_workers_in_hh") := hh_workers_dt[.SD, list(number_workers_in_hh), on = .(tract,number_in_hh,num_workers_id)]]

hh_type_race_dt <- as.data.table(household_type_race_data)
hh_type_race_dt[,c("household_id","num_hh_intract") := list(paste0(state,county,tract,as.character(2000000+seq.int(1:.N))),.N),by=.(tract)]
hh_type_race_dt[,("size_id"):=paste0(tract,family,as.character(1000000+seq.int(1:.N))),by=.(tract,family)]
hh_size_dt[,("size_id"):=paste0(tract,family,as.character(1000000+seq.int(1:.N))),by=.(tract,family)]
hh_type_race_dt[,c("family_size","number_workers_in_hh") := hh_size_dt[.SD, c(list(number_in_family),list(number_workers_in_hh)), on = .(tract,family,size_id)]]

#unmarried partners who are not householders are not counted here!!
hh_partner_dt <- as.data.table(household_type_partners_data)
hh_partner_dt[is.na(partner_type),("partner_type") := "Not a partner household"] 
hh_partner_dt[,("num_hh_intract") := .N,by=.(tract)]
hh_partner_dt[,("percent_partners_intract") := .N/num_hh_intract,by=.(tract,partner_type)]
hh_partner_exp <- bind_rows(hh_type_race_dt,hh_partner_dt)
hh_partner_exp[is.na(percent_partners_intract),("percent_partners_intract") := as.numeric(0.000000001)]

hh_partner_exp[,("partner_hh_type") := 
                 sample(rep(.SD[is.na(household_id),.(partner_type)][[1]],2),size=.N,
                        replace = FALSE,prob = rep((percent_partners_intract*2)/.N,1)),
               by=.(tract)]

hh_partner_exp <- hh_partner_exp[!is.na(household_id)]
hh_partner_exp <- hh_partner_exp %>% select(-name, -label, -size_id, -unmarried, -partner_type, -concept, -number_sams, -partner_id, -percent_partners_intract)
hh_partner_exp[partner_hh_type == "Female householder and female partner",("sex_hh") := "Female"]
hh_partner_exp[partner_hh_type == "Female householder and female partner",("sex_partner") := "Female"]
hh_partner_exp[partner_hh_type == "Male householder and male partner",("sex_hh") := "Male"]
hh_partner_exp[partner_hh_type == "Male householder and male partner",("sex_partner") := "Male"]
hh_partner_exp[partner_hh_type == "Male householder and female partner",("sex_hh") := "Male"]
hh_partner_exp[partner_hh_type == "Male householder and female partner",("sex_partner") := "Female"]
hh_partner_exp[partner_hh_type == "Female householder and male partner",("sex_hh") := "Female"]
hh_partner_exp[partner_hh_type == "Female householder and male partner",("sex_partner") := "Male"]
saveRDS(hh_partner_exp,file = paste0(housingdir, vintage, "/hh_partner_exp_",Sys.Date(),".RDS"))


hh_units_dt <- as.data.table(household_type_units_data)
hh_units_dt[,("units_fam_role") := if_else(fam_role_units=="Female householder no husband present" | fam_role_units=="Male householder no wife present",
                                           "Other family",if_else(fam_role_units=="Nonfamily households","Other households",fam_role_units))]
hh_partner_exp[,("units_fam_role") := if_else(family_type=="Householder living alone" | family_type=="Householder not living alone",
                                              "Other households",family_type)]

#trying trick to merge by ids I assign to capture full course of possible positions
hh_partner_exp[,("units_id"):=paste0(tract,units_fam_role,as.character(1000000+seq.int(1:.N))),by=.(tract,units_fam_role)]
hh_units_dt[,("units_id"):=paste0(tract,units_fam_role,as.character(1000000+seq.int(1:.N))),by=.(tract,units_fam_role)]
hh_partner_exp[,c("num_structures") := hh_units_dt[.SD, list(num_structures), on = .(tract,units_fam_role,units_id)]]

saveRDS(hh_partner_exp,file = paste0(housingdir, vintage, "/hh_partner_exp_",Sys.Date(),".RDS"))

#family_role has everything and without NAs adds up to 4525519; with Householder at 1562813
hh_relation_dt <- as.data.table(household_type_relation_data)
hh_relation_dt[family_role=="Householder",("hh_id"):=paste0(state,county,tract,as.character(2000000+seq.int(1:.N))),by=.(tract)]
hh_partner_exp[,("hh_id"):=paste0(state,county,tract,as.character(2000000+seq.int(1:.N))),by=.(tract)]
hh_relation_dt[family_role=="Householder",c("family","family_type","family_role","race","num_hh_intract",
                                            "family_size","number_workers_in_hh","partner_hh_type","sex_hh",
                                            "sex_partner","num_structures") := 
                 hh_partner_exp[.SD, c(list(family),list(family_type),list(family_role),list(race),list(num_hh_intract),
                      list(family_size),list(number_workers_in_hh),list(partner_hh_type),
                      list(sex_hh),list(sex_partner),list(num_structures)), on = .(hh_id)]]
#and test whether the place I do it above is correct or not - should make all four type of householder different??? 
#hh_relation_dt[!is.na(hh_id)] #gets Households

saveRDS(hh_relation_dt,file = paste0(housingdir, vintage, "/hh_relation_dt_",Sys.Date(),".RDS"))



hh_sam <- hh_partner_exp


#want to get age of adults by relation_hh and age of person
hh_adults <- as.data.table(household_adults_relation_data)
#hh_partner_exp has perhaps younger than 18 and married? has family_role=="Spouse" of 733235; "Householder" at 1562813 

hh_adults[,("relation") := relation_hh]
hh_adults[relation_hh=="Householder living with spouse or spouse of householder" & family_id %% 2 == 0, ("relation_hh") := "Spouse of Householder"]
hh_adults[relation_hh=="Householder living with spouse or spouse of householder", ("relation_hh") := "Householder"]
hh_adults[relation_hh=="Householder living with unmarried partner or unmarried partner of householder" & family_id %% 2 == 0, ("relation_hh") := "Unmarried partner of Householder"]
hh_adults[relation_hh=="Householder living with unmarried partner or unmarried partner of householder", ("relation_hh") := "Householder"]
hh_adults[relation_hh=="Lives alone", ("relation_hh") := "Householder"]
hh_adults[is.na(relation_hh),("relation_hh") := "Group Quartered"] #should be a way of having automatically with factors, but couldn't figure it out


#moved family_roles into subcategories that helped with age...
hh_sam[family_role=="Householder",("relation_hh") := "Householder"]
hh_sam[family_role=="Adopted child" | family_role=="Stepchild" | family_role=="Foster child" | family_role=="Son-in-law or daughter-in-law" | 
         family_role=="Biological child" | family_role=="Grandchild",("relation_hh") := "Child of householder"] #but this should only be for the subset of such over 18 in hh
hh_sam[family_role=="Spouse",("relation_hh") := "Spouse of Householder"]
hh_sam[family_role=="Unmarried partner",("relation_hh") := "Unmarried partner of Householder"]
hh_sam[family_role=="Parent" | family_role=="Brother or sister" | family_role=="Other relatives" ,("relation_hh") := "Other relatives"]
hh_sam[family_role=="Housemate or roommate" | 
       family_role=="Parent-in-law" | family_role=="Roomer or boarder" | #these should get different ages!!!
         family_role=="Other nonrelatives",("relation_hh") := "Other nonrelatives"]
hh_sam[is.na(relation_hh),("relation_hh") := "Group Quartered"]

#add an id
hh_adults[,("adults_id"):=paste0(tract,relation_hh,as.character(100000+seq.int(1:.N))),by=.(tract,relation_hh)]
hh_sam[,("adults_id"):=paste0(tract,relation_hh,as.character(100000+seq.int(1:.N))),by=.(tract,relation_hh)]
#assign - this works as long as everything for hh_sam is still independent of age, except for implicit things like kids living at home over 17...
hh_sam[,c("age_range") := hh_adults[.SD, list(age_range), on = .(tract,relation_hh,adults_id)]]

#pick up missing ones by going to one for whole
hh_adults[,("pu_adults_id"):=paste0(relation_hh,as.character(1000000+seq.int(1:.N))),by=.(relation_hh)]
hh_sam[,("pu_adults_id"):=paste0(relation_hh,as.character(1000000+seq.int(1:.N))),by=.(relation_hh)]
#try to pick up half of the remaining - not quite perfect, but close enough for now
hh_sam[is.na(age_range) & as.numeric(str_sub(adults_id,-1,-1)) %% 2 ==1,c("age_range") := hh_adults[.SD, list(age_range), on = .(relation_hh,pu_adults_id)]]

saveRDS(hh_sam,file = paste0(housingdir, vintage, "/hh_sam_",Sys.Date(),".RDS"))








#add age and race
sex_age_race_latinx_dt[,("num_age_intract") := .N,by=.(tract)]
sex_age_race_latinx_dt[,("percent_sex_intract") := .N/num_age_intract,by=.(tract,sex)]
sex_age_race_latinx_dt[,("percent_age_intract") := .N/num_age_intract,by=.(tract,sex,age_range)]
sex_age_race_latinx_dt[,("percent_race_intract") := .N/num_age_intract,by=.(tract,sex,age_range,race)]
sex_age_race_latinx_dt[,("percent_latinx_intract") := .N/num_age_intract,by=.(tract,sex,age_range,race,latinx)]
sex_age_race_latinx_dt[,("age_id"):=paste0(tract,as.character(1000000+seq.int(1:.N))),by=.(tract)]

hh_sam_age <- bind_rows(hh_sam,sex_age_race_latinx_dt)
hh_sam_age[is.na(percent_sex_intract),("percent_sex_intract") := as.numeric(0.000000001)]
hh_sam_age[is.na(percent_age_intract),("percent_age_intract") := as.numeric(0.000000001)]
hh_sam_age[is.na(percent_race_intract),("percent_race_intract") := as.numeric(0.000000001)]
hh_sam_age[is.na(percent_latinx_intract),("percent_latinx_intract") := as.numeric(0.000000001)]

###how to get this to do sex, age_range, race, latinx !!!!!!
#could do each by percent, and previous one, till getting latinx after 4 times through...
hh_sam_age[,c("sex") := 
             sample(rep(.SD[is.na(individual_id),.(sex)][[1]],2),size=.N,
                    replace = FALSE,prob = rep((percent_sex_intract*2)/.N,1)),
           by=.(tract)]
hh_sam_age[,c("age_range") := 
             sample(rep(.SD[is.na(individual_id),.(age_range)][[1]],2),size=.N,
                    replace = FALSE,prob = rep((percent_age_intract*2)/.N,1)),
           by=.(tract,sex)]
hh_sam_age[,c("race") := 
             sample(rep(.SD[is.na(individual_id),.(race)][[1]],2),size=.N,
                    replace = FALSE,prob = rep((percent_race_intract*2)/.N,1)),
           by=.(tract,sex)]
hh_sam_age[,c("latinx") := 
             sample(rep(.SD[is.na(individual_id),.(latinx)][[1]],2),size=.N,
                    replace = FALSE,prob = rep((percent_latinx_intract*2)/.N,1)),
           by=.(tract,sex)]  #total number is lower by 20820?? I = 1910535 - 1889715 after this - would be different number if rerun
#https://medium.com/@ThinkNowTweets/progressive-latino-pollster-trust-me-latinos-do-not-identify-with-latinx-63229adebcea
hh_sam_age <- hh_sam_age %>% rename(hispanic = latinx)
hh_sam_age[is.na(hispanic), c("hispanic") := 
             sample(c(rep(TRUE,20820),rep(FALSE,nrow(.SD[is.na(hispanic)])-20820)),size = .N,
                    replace = FALSE,prob = rep(1/.N,.N))
                    ]

hh_sam_age <- hh_sam_age[!is.na(individual_id)]
saveRDS(hh_sam_age,file = paste0(housingdir, vintage, "/hh_sam_age_",Sys.Date(),".RDS"))
hh_sam_age[age>17,c("adult_rel"):=
             sample(c(.SD[relation_hh==hh_adults$relation_hh,list(relation_hh)][[1]]),size = .N,
                    )]

#hh_sams_exp <- bind_rows(hh_sam_age,hh_adults)
#hh_sams_exp[is.na(percent_adult_rels_intract),("percent_adult_rels_intract") := as.numeric(0.000000001)]
#HAVE TO MAKE BOTH SIDES THE SAME SIZE - no group quarters in hh_adults means no tract for 412100 and 312100, so can't match...
#maybe create something as a dt that has tract, other categories that are known, and then the total numbers, etc. - and just look up instead of bind_rows?? 
#hh_sams_exp[as.numeric(substr(age_range,1,2))>17,c("adult_rels") := 
 #             sample(c(rep(.SD[is.na(individual_id),list(relation_hh)][[1]],1),rep("not adult relation",nrow(.SD[!is.na(individual_id)]))),size=.N,
  #                   replace = FALSE,prob = rep(percent_adult_rels_intract/.N,1)),
   #         by=.(tract)]

hh_sams_exp <- hh_sams_exp[!is.na(individual_id)]

#sample inside relation_hh on both??

#add age


#sex_age_race_latinx_dt


#this could have zip and super mixed up, but we won't be saving them - will use the zip super from HCAD at end
for (tr in hh_type_race_dt$tract){
  hh_type_race_dt[tract==tr,("super") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==tr,.(superneighborhood)][.N][[1]]),
                                                size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
  hh_type_race_dt[tract==tr,("zip") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==tr,.(zip)][.N][[1]]),
                                              size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
}
#add cities for places without super; for some reason 
#unique(hh_type_race_dt[is.na(super)]$tract) - "542800" "550401" "550402" "551100" "552001" - all in Cypress, Katy or Spring, but 18094 hh
#the NAs don't seem to be because of stopping - although I did just run the for loops, and then stop it after painful waiting; I think it was just stuck in exit of loop
for (tr in hh_type_race_dt$tract){
  hh_type_race_dt[tract==tr & is.na(super),("super") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==z,.(city)][.N][[1]]),
                                                               size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
}
saveRDS(hh_type_race_dt,file = paste0(housingdir, vintage, "/hh_type_",Sys.Date(),".RDS"))
hh_type_race_dt[is.na(super) & tract == c("542800"),("super") := "KATY"]
hh_type_race_dt[is.na(super) & tract == c("552001"),("super") := "CYPRESS"]
hh_type_race_dt[is.na(super) & tract == c("550401"),("super") := "SPRING"]  #each lists Houston as city, but is in Spring ISD 
hh_type_race_dt[is.na(super) & tract == c("550402"),("super") := "SPRING"]
hh_type_race_dt[is.na(super) & tract == c("551100"),("super") := "SPRING"]

#unique(hh_type_race_dt[is.na(zip)]$tract) : "230500" (77093) "332800" (77087) "341302" (77058) "433201" (77036) 
#"450802" (77042) "452802" (77072) "521100" (77080) "542800" (77493) "552001" (77065) "330400" (77053) 
#some have two zip codes, and I just picked one by eye.
hh_type_race_dt[is.na(zip) & tract == c("230500"),("zip") := "77093"]
hh_type_race_dt[is.na(zip) & tract == c("332800"),("zip") := "77087"]
hh_type_race_dt[is.na(zip) & tract == c("341302"),("zip") := "77058"]
hh_type_race_dt[is.na(zip) & tract == c("433201"),("zip") := "77036"]
hh_type_race_dt[is.na(zip) & tract == c("450802"),("zip") := "77042"]
hh_type_race_dt[is.na(zip) & tract == c("452802"),("zip") := "77072"]
hh_type_race_dt[is.na(zip) & tract == c("521100"),("zip") := "77080"]
hh_type_race_dt[is.na(zip) & tract == c("542800"),("zip") := "77493"]
hh_type_race_dt[is.na(zip) & tract == c("552001"),("zip") := "77065"]
hh_type_race_dt[is.na(zip) & tract == c("330400"),("zip") := "77053"]


#start with sex_by_age_race from individuals_generator. Race seems reasonable, but age does not (lower score is smoother).
#see discussion at https://stats.stackexchange.com/questions/24607/how-to-measure-smoothness-of-a-time-series-in-r
sd(diff(table(sex_by_age_race_data$race))) / abs(mean(diff(table(sex_by_age_race_data$race)))) #= 8.339956
sd(diff(table(sex_by_age_race_data$race,sex_by_age_race_data$tract))) / abs(mean(diff(table(sex_by_age_race_data$race,sex_by_age_race_data$tract)))) #= 13.187
sd(diff(table(sex_by_age_race_data$age_range,sex_by_age_race_data$tract))) / abs(mean(diff(table(sex_by_age_race_data$age_range,sex_by_age_race_data$tract)))) #= 199.1819
cor(table(sex_by_age_race_data$race)[-length(table(sex_by_age_race_data$race))],table(sex_by_age_race_data$race)[-1]) #=.3401369
cor(table(sex_by_age_race_data$age_range)[-length(table(sex_by_age_race_data$age_range))],table(sex_by_age_race_data$age_range)[-1]) #= .4854334




hh_type_race_dt[,("num_role_intract") := .N,by=.(tract,family_role)]
hh_type_race_dt[,("percent_race_role_intract") := .N/num_role_intract,by=.(tract,race,family_role)]
hh_exp <- bind_rows(hh_type_race_dt,hh_size_dt)
hh_exp[is.na(family_or_non),("family_or_non") := if_else(nonfamily,"Nonfamily households","Family households")]
hh_exp[is.na(percent_race_role_intract),("percent_race_role_intract") := as.numeric(0.001)]
hh_exp[num_family_id==1 | is.na(num_family_id),("num_in_family") := sample(rep(.SD[!is.na(num_family_id),.(numeric_in_family)][[1]],2),size=.N,
                                                                           replace = FALSE,prob = rep((percent_race_role_intract*2)/.N,1)),
       by=.(tract,family_or_non)]
#got a warning that "Invalid .internal.selfref detected and fixed by taking a (shallow) copy..." not sure how to fix; I am using set except with bind_rows...
hh_expanded <- hh_exp[is.na(family_id)]
hh_expanded[,("family_role") := "Householder"]