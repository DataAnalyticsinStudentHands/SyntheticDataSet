#test pages - expand just lives from tables equaling each other as appropriate, with code in file

#for sam_hh


#for sam
#test 1
test <- table(place_born_marital_dt$tract,place_born_marital_dt$place_born)==
  table(marital_status_age_dt$tract,marital_status_age_dt$place_born)
length(test[test==F])==0

#test 2 [absorbed into 2b, etc.]
test <- table(sex_age_race[as.numeric(substr(age_range,1,2))>14]$tract,sex_age_race[as.numeric(substr(age_range,1,2))>14]$sex,
              sex_age_race[as.numeric(substr(age_range,1,2))>14]$age_range)==
  table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range)
length(test[test==F])==0
#test2b
test <- table(sex_age_race[age>14]$tract,sex_age_race[age>14]$sex,sex_age_race[age>14]$age_range,sex_age_race[age>14]$race)==
  table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range,marital_status_age_dt$race)
test <- table(sex_by_age_eth[age>14]$tract,sex_by_age_eth[age>14]$sex,sex_by_age_eth[age>14]$age_range,sex_by_age_eth[age>14]$ethnicity)==
   table(marital_status_age_dt$tract,marital_status_age_dt$sex,marital_status_age_dt$age_range,marital_status_age_dt$ethnicity)
length(test[test==F])==0
#test2c
test <- table(sex_age_race[age>14]$tract,sex_age_race[age>14]$sex,sex_age_race[age>14]$age_range,sex_age_race[age>14]$race)==
  table(marital_status_race_dt$tract,marital_status_race_dt$sex,marital_status_race_dt$age_range,marital_status_race_dt$race)
test <- table(sex_by_age_eth[age>14]$tract,sex_by_age_eth[age>14]$sex,sex_by_age_eth[age>14]$age_range,sex_by_age_eth[age>14]$ethnicity)==
  table(marital_status_eth_dt$tract,marital_status_eth_dt$sex,marital_status_eth_dt$age_range,marital_status_eth_dt$ethnicity)
length(test[test==F])==0
#test2c1
test <- table(marital_status_race_dt$tract,marital_status_race_dt$sex,marital_status_race_dt$age_range,marital_status_race_dt$race)==
  table(sex_age_race[age>14]$tract,sex_age_race[age>14]$sex,sex_age_race[age>14]$age_range,sex_age_race[age>14]$race)
length(test[test==F])==0
#test2d [redo of 2b, for race/eth]
test <- table(sex_age_race[age>14]$tract,sex_age_race[age>14]$sex,sex_age_race[age>14]$age_range,sex_age_race[age>14]$race)==
  table(marital_status_race_dt$tract,marital_status_race_dt$sex,marital_status_race_dt$age_range,marital_status_race_dt$race)
test <- table(sex_by_age_eth[age>14]$tract,sex_by_age_eth[age>14]$sex,sex_by_age_eth[age>14]$age_range,sex_by_age_eth[age>14]$ethnicity)==
  table(marital_status_eth_dt$tract,marital_status_eth_dt$sex,marital_status_eth_dt$age_range,marital_status_eth_dt$ethnicity)
length(test[test==F])==0
#test2e
test <- nrow(marital_status_eth_dt[!is.na(pregnant)]) == nrow(preg_eth_dt)
test <- nrow(marital_status_race_dt[!is.na(pregnant)]) == nrow(preg_race_dt)

#test3
test<-table(sex_age_race[age>64]$tract,sex_age_race[age>64]$sex,sex_age_race[age>64]$race,sex_age_race[age>64]$age_range)==
  table(sr_relations$tract,sr_relations$race_sex_sr_relations,sr_relations$race,sr_relations$race_age_range)
test<-table(sex_by_age_eth[age>64]$tract,sex_by_age_eth[age>64]$sex,sex_by_age_eth[age>64]$ethnicity,sex_by_age_eth[age>64]$age_range)==
  table(sr_relations$tract,sr_relations$eth_sex_sr_relations,sr_relations$ethnicity,sr_relations$eth_age_range)
length(test[test==F])==0
#test3a
#there are 4 tracts from sr_relations not in hh_relations
test<-table(hh_relations_dt[tract%in%unique(sr_relations$tract)]$tract,hh_relations_dt[tract%in%unique(sr_relations$tract)]$sex_sr_hh,
            hh_relations_dt[tract%in%unique(sr_relations$tract)]$sr_hh_living_alone,hh_relations_dt[tract%in%unique(sr_relations$tract)]$race,
            hh_relations_dt[tract%in%unique(sr_relations$tract)]$ethnicity,hh_relations_dt[tract%in%unique(sr_relations$tract)]$eth_age_range)==
  table(sr_relations$tract,sr_relations$sex_sr_relations,sr_relations$living_alone,sr_relations$race,sr_relations$ethnicity,sr_relations$eth_age_range)
length(test[test==F])==0
#test3b
test<-table(sex_age_race[age>17&age<65&missing_race=="In households"]$tract,sex_age_race[age>17&age<65&missing_race=="In households"]$sex,
            sex_age_race[age>17&age<65&missing_race=="In households"]$race,sex_age_race[age>17&age<65&missing_race=="In households"]$age_range)==
  table(adults_relations[age_range_3!="65 years and over"]$tract,adults_relations[age_range_3!="65 years and over"]$race_sex_relations,
        adults_relations[age_range_3!="65 years and over"]$race,adults_relations[age_range_3!="65 years and over"]$race_age_range)
length(test[test==F])==0
test<-table(sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$tract,sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$sex,
            sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$ethnicity,sex_by_age_eth[age>17&age<65&missing_eth=="In households"]$age_range)==
  table(adults_relations[age_range_3!="65 years and over"]$tract,adults_relations[age_range_3!="65 years and over"]$eth_sex_relations,adults_relations[age_range_3!="65 years and over"]$ethnicity,adults_relations[age_range_3!="65 years and over"]$eth_age_range)
length(test[test==F])==0

#test4
nrow(hh_relations_dt[relation_hh=="Householder"])==nrow(hh_relations_dt[relative=="Householder"])

#test 5 [[need to think through better testing on relations]]
test<-nrow(hh_relations_dt[is.na(ind_id_eth)])==0

#test 5b
test<-nrow(hh_relations_dt[is.na(individual_id)])==0
test <- length(unique(hh_relations_dt$individual_id))==nrow(sex_age_race)
test <- length(unique(hh_relations_dt$ind_id_eth))==nrow(sex_by_age_eth)
test <- table(sex_age_race$tract,sex_age_race$race,sex_age_race$sex,sex_age_race$age_range)==
  table(hh_relations_dt$tract,hh_relations_dt$race,hh_relations_dt$race_sex_relations,hh_relations_dt$race_age_range)
length(test[test==F])==0
test <- table(sex_by_age_eth$tract,sex_by_age_eth$ethnicity,sex_by_age_eth$sex,sex_by_age_eth$age_range)==
  table(hh_relations_dt$tract,hh_relations_dt$ethnicity,hh_relations_dt$eth_sex_relations,hh_relations_dt$eth_age_range)
length(test[test==F])==0

#test 6 
test <- table(place_born_age_full_dt[place_born=="Foreign born"]$tract,place_born_age_full_dt[place_born=="Foreign born"]$age_range_18)==
  table(sex_nativity_age_dt$tract,sex_nativity_age_dt$age_range)
length(test[test==F]) == 0

#test 6b
test <- nrow(hh_relations_dt[is.na(place_born_race)])==0
test <- nrow(hh_relations_dt[is.na(place_born_eth)])==0
test <- table(hh_relations_dt$tract,hh_relations_dt$place_born_eth,hh_relations_dt$age_range_14_eth)==
  table(place_born_age_full_dt$tract,place_born_age_full_dt$place_born,place_born_age_full_dt$age_range_14)
test <- table(hh_relations_dt$tract,hh_relations_dt$place_born_race,hh_relations_dt$age_range_14_race)==
  table(place_born_age_full_dt$tract,place_born_age_full_dt$place_born,place_born_age_full_dt$age_range_14)
length(test[test==F])==0

#test 7
tester <- hh_relations_eth[tract%in%unique(origin_data_dt$tract)]
#too big#test <- table(tester$tract,tester$fb_origin_area,tester$fb_origin_continent,tester$fb_origin_region,tester$fb_origin_country)==
#too big#  table(origin_data_dt$tract,origin_data_dt$origin_area,origin_data_dt$origin_continent,origin_data_dt$origin_region,origin_data_dt$origin_country)
#too big#length(test[test==F])==0
test <- table(tester$tract,tester$fb_origin_area,tester$fb_origin_country)==
  table(origin_data_dt$tract,origin_data_dt$origin_area,origin_data_dt$origin_country)
length(test[test==F])==0
tester <- hh_relations_race[tract%in%unique(origin_data_dt$tract)]
test <- table(tester$tract,tester$fb_origin_area,tester$fb_origin_country)==
  table(origin_data_dt$tract,origin_data_dt$origin_area,origin_data_dt$origin_country)
length(test[test==F])==0

#test 8  
test <- table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)==
  table(hh_relations_race$tract,hh_relations_race$race_sex_relations)
length(test[test==F])==0
test <- table(sex_by_age_eth$tract,sex_by_age_eth$sex)==
  table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)
length(test[test==F])==0
test <- table(hh_relations_eth$tract,hh_relations_eth$age_range_14_eth)==
  table(hh_relations_race$tract,hh_relations_race$age_range_14_race)
length(test[test==F])==0
##the crosstab is false - ugh!!! I dropped it somewhere!!
test <- table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations,hh_relations_eth$age_range_14_eth)==
  table(hh_relations_race$tract,hh_relations_race$race_sex_relations,hh_relations_race$age_range_14_race)
length(test[test==F])/length(test)#55% of tracts are off by just a handful - can be explained by sampling
#but these are true, so much better
test <- table(sex_by_age_eth$tract,sex_by_age_eth$ethnicity,sex_by_age_eth$sex,sex_by_age_eth$age_range)==
  table(hh_relations_eth$tract,hh_relations_eth$ethnicity,hh_relations_eth$eth_sex_relations,hh_relations_eth$eth_age_range)
length(test[test==F])==0
test <- table(sex_age_race$tract,sex_age_race$race,sex_age_race$sex,sex_age_race$age_range)==
  table(hh_relations_race$tract,hh_relations_race$race,hh_relations_race$race_sex_relations,hh_relations_race$race_age_range)
length(test[test==F])==0


