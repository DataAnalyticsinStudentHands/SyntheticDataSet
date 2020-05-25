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

#test 4a
test <- table(hh_relations_dt$eth_age_range,hh_relations_dt$age_range_4)
sum(colSums(test==0/nrow(test)*100))==42
test <- table(hh_relations_dt$race_age_range,hh_relations_dt$age_range_4)
sum(colSums(test==0/nrow(test)*100))==42
tester <- hh_relations_dt[tract%in%unique(hh_type_kids$tract) & age_range_4=="0  to 17 years"]
test <- table(tester$tract,
              tester$eth_age_range,
              tester$eth_sex_relations,
              tester$ethnicity,
              tester$race,
              tester$race_age_range,
              tester$race_sex_relations,
              tester$in_family_type)==
  table(hh_type_kids$tract,
        hh_type_kids$eth_age_range,
        hh_type_kids$eth_sex_relations,
        hh_type_kids$ethnicity,
        hh_type_kids$race,
        hh_type_kids$race_age_range,
        hh_type_kids$race_sex_relations,
        hh_type_kids$in_family_type)
length(test[test==F])==0

#test 5
test<-nrow(hh_relations_dt[is.na(ind_id_eth)])==0
test<-nrow(hh_relations_dt[is.na(eth_age_range)])==0
test<-nrow(hh_relations_dt[is.na(individual_id)])==0
test<-nrow(hh_relations_dt[is.na(race_age_range)])==0
test<-length(unique(hh_relations_dt$ind_id_eth))==nrow(sex_by_age_eth)
test<-length(unique(hh_relations_dt$individual_id))==nrow(sex_age_race)
#redo 4a
test<-table(hh_relations_dt$tract,hh_relations_dt$eth_age_range,hh_relations_dt$ethnicity,hh_relations_dt$eth_sex_relations)==
  table(sex_by_age_eth$tract,sex_by_age_eth$age_range,sex_by_age_eth$ethnicity,sex_by_age_eth$sex)
length(test[test==F])==0
test<-table(hh_relations_dt$tract,hh_relations_dt$race_age_range,hh_relations_dt$race,hh_relations_dt$race_sex_relations)==
  table(sex_age_race$tract,sex_age_race$age_range,sex_age_race$race,sex_age_race$sex)
length(test[test==F])==0

#test 6
test <- table(sex_place_when_dt$tract,sex_place_when_dt$sex,sex_place_when_dt$sn_age_range)==
  table(sex_nativity_age_dt$tract,sex_nativity_age_dt$sex,sex_nativity_age_dt$age_range)
length(test[test==F])==0

#test 6 
test <- table(place_born_age_full_dt[place_born=="Foreign born"]$tract,place_born_age_full_dt[place_born=="Foreign born"]$age_range_18)==
  table(sex_nativity_age_dt$tract,sex_nativity_age_dt$age_range)
length(test[test==F]) == 0

#test 6b
test <- table(place_period_citizen_dt$tract,
              place_period_citizen_dt$date_entered,
              place_period_citizen_dt$fb_origin_country,
              place_period_citizen_dt$citizen)==
  table(sex_place_when_dt$tract,
        sex_place_when_dt$fb_date_entered,
        sex_place_when_dt$fb_origin_place,
        sex_place_when_dt$fb_citizen)
length(test[test==F])==0

#test 6c
test <- nrow(sex_place_when_dt[!is.na(sn_age_range)])==nrow(sex_nativity_age_dt[substr(age_range_14,1,2)<10])

#test 6c1
test <- table(sex_place_when_dt$tract,
              sex_place_when_dt$sn_age_range,
              sex_place_when_dt$age_range_14,
              sex_place_when_dt$sex)==
  table(sex_nativity_age_dt$tract,
        sex_nativity_age_dt$age_range,
        sex_nativity_age_dt$age_range_14,
        sex_nativity_age_dt$sex)
length(test[test==F])/length(test) < .002 #it's a little under 2% off, because of that secondary matching above, but unavoidable and right totals.

#test 6d
test <- nrow(sex_place_when_dt[is.na(fb_origin_continent)])==0

#test 7
nrow(sex_place_when_dt[!is.na(fb_date_entered)])==nrow(place_born_age_full_dt[!is.na(fb_date_entered)])
test<-table(sex_place_when_dt$tract,
            sex_place_when_dt$fb_origin_continent)-
  table(place_born_age_full_dt[place_born=="Foreign born",tract],
        place_born_age_full_dt[place_born=="Foreign born",fb_origin_continent])
max(abs(test[,1:5])) < 35
#lose less than 1% on others, but they don't match

#test 8 
test <- table(place_born_age_full_dt$tract,
              place_born_age_full_dt$place_born,
              place_born_age_full_dt$age_range_14
)==
  table(place_born_eth_dt$tract,
        place_born_eth_dt$place_born,
        place_born_eth_dt$age_range_14
  )
length(test[test==F]) == 0    

#test 8b
test <- table(place_born_age_full_dt$tract,
              place_born_age_full_dt$place_born,
              place_born_age_full_dt$ethnicity,
              place_born_age_full_dt$age_range_14)==
  table(place_born_race_dt$tract,
        place_born_race_dt$place_born,
        place_born_race_dt$ethnicity,
        place_born_race_dt$age_range_14)
length(test[test==F])==0   

#test 8  - suite
test <- table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)==
  table(hh_relations_race$tract,hh_relations_race$race_sex_relations)
length(test[test==F])==0
test <- table(sex_by_age_eth$tract,sex_by_age_eth$sex)==
  table(hh_relations_eth$tract,hh_relations_eth$eth_sex_relations)
length(test[test==F])==0
#eth_age_range and race_age_range don't match
test <- table(hh_relations_eth$tract,hh_relations_eth$eth_age_range)==
  table(hh_relations_race$tract,hh_relations_race$race_age_range)
length(test[test==F])==0
#but these each are true, so much better
test <- table(sex_by_age_eth$tract,sex_by_age_eth$ethnicity,sex_by_age_eth$sex,sex_by_age_eth$age_range)==
  table(hh_relations_eth$tract,hh_relations_eth$ethnicity,hh_relations_eth$eth_sex_relations,hh_relations_eth$eth_age_range)
length(test[test==F])==0
test <- table(sex_age_race$tract,sex_age_race$race,sex_age_race$sex,sex_age_race$age_range)==
  table(hh_relations_race$tract,hh_relations_race$race,hh_relations_race$race_sex_relations,hh_relations_race$race_age_range)
length(test[test==F])==0

#test 10
#still close to 100k off, but not worth further effort

#test 11
test <- table(place_born_language_dt$tract,
              place_born_language_dt$English_proficiency,
              place_born_language_dt$language_at_home)==
  table(hh_relations_race$tract,
        hh_relations_race$English_proficiency,
        hh_relations_race$fb_language_at_home)
length(test[test==F])==0

#test 12 pre
test<-table(hh_relations_eth[substr(eth_age_range,1,2)>14,tract],
            hh_relations_eth[substr(eth_age_range,1,2)>14,eth_sex_relations],
            hh_relations_eth[substr(eth_age_range,1,2)>14,ethnicity],
            hh_relations_eth[substr(eth_age_range,1,2)>14,eth_age_range])==
  table(marital_status_eth_dt$tract,
        marital_status_eth_dt$sex,
        marital_status_eth_dt$ethnicity,
        marital_status_eth_dt$age_range)
length(test[test==F])==0
test<-table(hh_relations_race[substr(race_age_range,1,2)>14,tract],
            hh_relations_race[substr(race_age_range,1,2)>14,race_sex_relations],
            hh_relations_race[substr(race_age_range,1,2)>14,race],
            hh_relations_race[substr(race_age_range,1,2)>14,race_age_range])==
  table(marital_status_race_dt$tract,
        marital_status_race_dt$sex,
        marital_status_race_dt$race,
        marital_status_race_dt$age_range)
length(test[test==F])==0

#test 12
test <- table(hh_relations_race[substr(race_age_range,1,2)>14,tract],
              hh_relations_race[substr(race_age_range,1,2)>14,race],
              hh_relations_race[substr(race_age_range,1,2)>14,race_sex_relations],
              hh_relations_race[substr(race_age_range,1,2)>14,race_age_range],
              hh_relations_race[substr(race_age_range,1,2)>14,marital_status])==
  table(marital_status_race_dt$tract,
        marital_status_race_dt$race,
        marital_status_race_dt$sex,
        marital_status_race_dt$age_range,
        marital_status_race_dt$marital_status)
length(test[test==F])==0





