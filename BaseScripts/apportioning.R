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
HCAD_not_vacant <- HCAD_dt[!is.na(improv_typ)] #returns 1815741

HCAD_ext <- bind_rows(HCAD_not_vacant,sex_age_race_latinx_dt)
#trying to get super and city in same place, but respecting that not all addresses in a tract are in the super/city
#should look at how the supers align with tracts, most are within, I think.
#A[,("super") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==.(tract),.(superneighborhood)][[1]]),
A[,("super") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==.(tract),.(superneighborhood)][[1]]),
                       size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
hh_type_race_dt <- as.data.table(household_type_race_data)
hh_type_race_dt[,("super") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==.(tract),.(superneighborhood)][[1]]),
                                              size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
hh_type_race_dt[is.na(super),("super") := sample(list(HCAD_dt[.SD[,.(tract)][[1]]==.(tract),.(city)][[1]]),
                                                 size = 1,replace = TRUE,prob = c(1/.N)),by = .(tract)]
#do pca at superneighbor level to then mix tract info better...
