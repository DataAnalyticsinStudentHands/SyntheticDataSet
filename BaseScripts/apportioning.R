#reapportioning things where the small cells don't make sense at the tract level - which is almost everything, unfortunately.

library(tidyr)
library(dplyr)
library(stringr)
library(data.table)

#get HCAD_residences from HCAD_merge or from housingdir main level for desired vintage (by most recent date)
#get others from household and individual generator scripts

#start with sex_by_age_race from individuals_generator. Race seems reasonable, but age does not (lower score is smoother).
#see discussion at https://stats.stackexchange.com/questions/24607/how-to-measure-smoothness-of-a-time-series-in-r
sd(diff(table(sex_by_age_race_data$race))) / abs(mean(diff(table(sex_by_age_race_data$race)))) #= 8.339956
sd(diff(table(sex_by_age_race_data$race,sex_by_age_race_data$tract))) / abs(mean(diff(table(sex_by_age_race_data$race,sex_by_age_race_data$tract)))) #= 13.187
sd(diff(table(sex_by_age_race_data$age_range,sex_by_age_race_data$tract))) / abs(mean(diff(table(sex_by_age_race_data$age_range,sex_by_age_race_data$tract)))) #= 199.1819
cor(table(sex_by_age_race_data$race)[-length(table(sex_by_age_race_data$race))],table(sex_by_age_race_data$race)[-1]) #=.3401369
cor(table(sex_by_age_race_data$age_range)[-length(table(sex_by_age_race_data$age_range))],table(sex_by_age_race_data$age_range)[-1]) #= .4854334


HCAD_dt <- as.data.table(HCAD_residences)
HCAD_dt[,tract:=droplevels(tract)]
HCAD_dt <- HCAD_dt[!is.na(improv_typ)] #returns 1815741

#HCAD_ext <- bind_rows(HCAD_not_vacant,sex_age_race_latinx_dt)
#need to rerun having taken out the ones that don't have improv_typ 
hh_type_race_dt <- as.data.table(household_type_race_data)
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

#some things that don't need zip and super - idea would be same logic as by=.(tract,etc) but with either zip or super
#getting household_type_size_data from householdsGenerator.R - num_family_id == 1 : 1562813, which is same as nrow in for hh; numeric_in_family lets you expand
hh_size_dt <- as.data.table(household_type_size_data)
#or get from OneDrive
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
hh_expanded[,c("household_id","num_hh_intract") := list(paste0(state,county,tract,as.character(200000+seq.int(1:.N))),.N),by=.(tract)]

hh_relation_dt <- as.data.table(household_type_relation_data)
#family_role has everything and without NAs adds up to 4525519; with Householder at 1562813
hh_relation_dt[,("num_relat_intract") := .N,by=.(tract,family_role)]
hh_relation_dt[,("percent_relat_intract") := num_relat_intract/.N,by=.(tract)]
hh_relation_dt[,("individual_id") := paste0(state,county,tract,as.character(100000+seq.int(1:.N))),by=.(tract)] #may want to redo to encode more info
hh_relation <- hh_relation_dt  %>% select(individual_id,tract,family_role,percent_relat_intract)
hh_expanded <- hh_expanded %>% select(household_id,tract,family_type,family_role,num_in_family,num_hh_intract,race,super,zip)  #super and zip are only if used - would be supplanted by HCAD
hh_merge_rel <- bind_cols(hh_relation[family_role=="Householder"],hh_expanded)  #do by tract, in case they got mixed above??
hh_merge_exp <- merge(hh_relation,hh_merge_rel,by="individual_id",all = TRUE)
hh_merge_exp <- hh_merge_exp %>% select(individual_id,household_id,tract = tract.x,family_role = family_role.x,
                                        family_type,percent_relat_intract = percent_relat_intract.x,num_hh_intract,
                                             num_in_family,race,super,zip)

saveRDS(hh_merge_exp,file = paste0(housingdir, vintage, "/hh_merge_exp_",Sys.Date(),".RDS"))
#hh_merge_exp[family_role=="Householder"] gets you the hh group - 1532813

hh_partner_dt <- as.data.table(household_type_partners_data)
hh_partner_dt[is.na(partner_type),("partner_type") := "Not a partner households"]
hh_partner_dt[,("num_hh_intract") := .N,by=.(tract)]
hh_partner_dt[,("percent_partners_intract") := .N/num_hh_intract,by=.(tract,partner_type)]
hh_partner_exp <- bind_rows(hh_merge_exp,hh_partner_dt)
hh_partner_exp[is.na(percent_partners_intract),("percent_partners_intract") := as.numeric(0.000000001)]
hh_partner_exp[family_role=="Householder" | is.na(individual_id),("partner_hh_type") := 
                 sample(rep(.SD[is.na(individual_id),.(partner_type)][[1]],2),size=.N,
                        replace = FALSE,prob = rep((percent_partners_intract*2)/.N,1)),
       by=.(tract)]
hh_partner_exp <- hh_partner_exp[!is.na(individual_id)]
hh_partner_exp <- hh_partner_exp %>% select(-name, -label, -unmarried, -partner_type, -concept, -number_sams, -partner_id, -percent_partners_intract)
saveRDS(hh_partner_exp,file = paste0(housingdir, vintage, "/hh_partner_exp_",Sys.Date(),".RDS"))

hh_units_dt <- as.data.table(household_type_units_data)
hh_units_dt[,("units_fam_role") := if_else(fam_role_units=="Female householder no husband present" | fam_role_units=="Male householder no wife present",
                                          "Other family",if_else(fam_role_units=="Nonfamily households","Other households",fam_role_units))]
hh_partner_exp[,("units_fam_role") := if_else(family_type=="Householder living alone" | family_type=="Householder not living alone",
                                              "Other households",family_type)]
#trying trick to merge by ids, but something is wrong...
hh_partner_exp[family_role=="Householder",("units_id"):=paste0(tract,units_fam_role,as.character(100000+seq.int(1:.N))),by=.(tract,units_fam_role)]
hh_units_dt[,("units_id"):=paste0(tract,units_fam_role,as.character(100000+seq.int(1:.N))),by=.(tract,units_fam_role)]
hh_units_merge <- merge(hh_partner_exp[family_role=="Householder"],hh_units_dt,all.y=TRUE,by=c("units_id"))
hh_units_full <- merge(hh_partner_exp,hh_units_merge,by="individual_id")  







#for each percent_relat_intract - can I create IDs that they would match on??
hh_merge_exp_test[,("family_id_wrk") := paste0(100000+num_hh_intract,100000+seq.int(1:.N)),
                  by=.(tract,family_role)]
#or just do one at a time through the types???, with a counter for how many in the family are left??? maybe dplyr


#sample from .SD - search by family_roles - should balance by race afterwards - here race is only for the householder
hh_merge_exp_test[!is.na(household_id),("spouse_id") := if_else(family_type=="Married-couple family",
                                                                sample(rep(.SD[family_role=="Spouse",.(individual_id)][[1]],1)),
                                                                "No spouse"), #are they only the ones not separated?? should look??
                  by=.(tract)]
hh_merge_exp_test[is.na(household_id),("household_id") := "test",
                  by=.(tract,family_role)]

#bind rows with hh_expanded; then go backwards with everything going to the full sam size - have to think about mixed race families....

#hh_rel_exp <- bind_rows(hh_expanded,hh_relation_dt)

#hh_rel_exp[,("household_id") := sample(rep(.SD[!is.na(role_id),.(numeric_in_family)][[1]],2),size=.N,
 #                                                                          replace = FALSE,prob = rep((percent_race_role_intract*2)/.N,1)),
  #     by=.(tract,family_or_non)]

#for hh_type_race_data$family_role, householder living alone and Household not living alone are the two types of non-family households
#check hh_type_size and age_race to see if it has same rows vs tract problems... if it does match before distributing by zip / super
#apportion by getting percentages by row for subsets and using those as prob weights...
#use dt to get sums by .N for race ?? 
for (type in hh_type_race_dt$family_type){
  #only non-GQ hh
    #sum over zip and sum over super add and then divide by two
  #get percentages that matter by super, then use as weights for prob
}

