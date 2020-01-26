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
#the stuff below writes NAs to everything except the last one it goes through... all the hh go to Webster...
hh_type_race_dt <- as.data.table(household_type_race_data)
#this could have zip and super mixed up, but we won't be saving them - will use the zip super from HCAD at end
for (tr in hh_type_race_dt$tract){
  hh_type_race_dt[tract==tr,("super") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==tr,.(superneighborhood)][.N][[1]]),
                                       size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
  hh_type_race_dt[tract==tr,("zip") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==tr,.(zip)][.N][[1]]),
                                                size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
}
#add cities for places without super, but trying to pick up smaller cities by zip adjacency;
for (z in hh_type_race_dt$zip){
  hh_type_race_dt[zip==z & is.na(super),("super") := sample(list(HCAD_dt[.SD[,.(zip)][[1]]==z,.(city)][.N][[1]]),
                                                   size = 1,replace = TRUE,prob = c(1/.N)),by = .(zip)]
}
#add a marker of income by tract from the HCAD?? check for missing zip
 
#check hh_type_size and age_race to see if it has same rows vs tract problems... if it does match before distributing by zip / super
#apportion by getting percentages by row for subsets and using those as prob weights...
#use dt to get sums by .N for race ?? 
for (type in hh_type_race_dt$family_type){
  #only non-GQ hh
    #sum over zip and sum over super add and then divide by two
  #get percentages that matter by super, then use as weights for prob
}

