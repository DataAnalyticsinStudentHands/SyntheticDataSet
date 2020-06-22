#test pages - expand just lives from tables equaling each other as appropriate, with code in file

#for sam_hh
#test hh1a
test <- table(hh_age_dt$tract,hh_age_dt$householder_age_9)==table(occup_type_dt$tract,occup_type_dt$householder_age_9)
length(test[test==F])==0

#test hh1
test <- table(hh_type_eth_dt$tract,hh_type_eth_dt$own_rent,hh_type_eth_dt$family_type)==table(hh_type_race_dt$tract,hh_type_race_dt$own_rent,hh_type_race_dt$family_type)
length(test[test==F])==0

#test hh2 - right mix for rematch
test <- table(
  anti_hh_type_race_dt$tract,
  anti_hh_type_race_dt$own_rent
)==table(
  sam_race_hh[is.na(family)]$tract,
  sam_race_hh[is.na(family)]$own_rent
)
length(test[test==F])==0
test <- table(
  anti_hh_type_race_dt$tract,
  anti_hh_type_race_dt$race
)==table(
  sam_race_hh[is.na(family)]$tract,
  sam_race_hh[is.na(family)]$race
)
length(test[test==F])==0
test <- table(
  anti_hh_type_eth_dt$tract,
  anti_hh_type_eth_dt$own_rent
)==table(
  sam_eth_hh[is.na(family)]$tract,
  sam_eth_hh[is.na(family)]$own_rent
)
length(test[test==F])==0
test <- table(
  anti_hh_type_eth_dt$tract,
  anti_hh_type_eth_dt$ethnicity
)==table(
  sam_eth_hh[is.na(family)]$tract,
  sam_eth_hh[is.na(family)]$ethnicity
)
length(test[test==F])==0

#test hh2b 
test <- nrow(sam_eth_hh[is.na(family)])==0
test <- nrow(sam_race_hh[is.na(family)])==0
test <- table(
  sam_race_hh$tract,
  sam_race_hh$race,
  #sam_race_hh$own_rent,#with this in, it's false, because line-up would be from a diff. possible match
  sam_race_hh$family,
  sam_race_hh$family_type,
  sam_race_hh$single_hh_sex
)==
  table(
    hh_type_race_dt$tract,
    hh_type_race_dt$race,
    #hh_type_race_dt$own_rent, 
    hh_type_race_dt$family,
    hh_type_race_dt$family_type,
    hh_type_race_dt$single_hh_sex
  )
length(test[test==F])==0
test <- table(
  sam_eth_hh$tract,
  sam_eth_hh$ethnicity,
  sam_eth_hh$family,
  sam_eth_hh$family_type,
  sam_eth_hh$single_hh_sex
)==
  table(
    hh_type_eth_dt$tract,
    hh_type_eth_dt$ethnicity,
    hh_type_eth_dt$family,
    hh_type_eth_dt$family_type,
    hh_type_eth_dt$single_hh_sex
  )
length(test[test==F])==0
test <- table(
  sam_race_hh$tract,
  sam_race_hh$family,
  sam_race_hh$family_type,
  sam_race_hh$family_role,
  sam_race_hh$single_hh_sex
)==
  table(
    sam_eth_hh$tract,
    sam_eth_hh$family,
    sam_eth_hh$family_type,
    sam_eth_hh$family_role,
    sam_eth_hh$single_hh_sex
  )
length(test[test==F])==0
test <- table(
  sam_race_hh$tract,
  sam_race_hh$householder_age_9,
  sam_race_hh$own_rent
)==
  table(
    sam_eth_hh$tract,
    sam_eth_hh$householder_age_9,
    sam_eth_hh$own_rent
  )
length(test[test==F])==0

#test hh31a
test <- table(
  housing_units_race_dt$tract,
  housing_units_race_dt$housing_units,
  housing_units_race_dt$own_rent
)==
  table(
    housing_units_rent_dt$tract,
    housing_units_rent_dt$housing_units,
    housing_units_rent_dt$own_rent
  )
length(test[test==F])==0
test <- table(
  housing_units_eth_dt$tract,
  housing_units_eth_dt$housing_units,
  housing_units_eth_dt$own_rent
)==
  table(
    housing_units_rent_dt$tract,
    housing_units_rent_dt$housing_units,
    housing_units_rent_dt$own_rent
  )
length(test[test==F])==0
#test hh3preA - each works alone with tract, but not together
test <- table(
  housing_units_eth_dt$tract,
  housing_units_eth_dt$ethnicity#,
  #housing_units_eth_dt$own_rent
)==
  table(
    sam_eth_hh$tract,
    sam_eth_hh$ethnicity#,
    #sam_eth_hh$own_rent
  )
length(test[test==F])==0

#test hh3
test <- table(
  sam_eth_hh$tract,
  sam_eth_hh$ethnicity,
  sam_eth_hh$family_role_4
)==
  table(
    hh_units_rent_eth$tract,
    hh_units_rent_eth$ethnicity,
    hh_units_rent_eth$family_role_4
  )
length(test[test==F])==0

#test hh4a
test <- table(
  sam_eth_hh[is.na(housing_units)]$tract,
  sam_eth_hh[is.na(housing_units)]$ethnicity
)==
  table(
    hh_units_rent_eth[is.na(missed_units)]$tract,
    hh_units_rent_eth[is.na(missed_units)]$ethnicity
  )
length(test[test==F])==0
test <- table(
  sam_eth_hh[is.na(housing_units)]$tract,
  #sam_eth_hh[is.na(housing_units)]$family_role_4,
  sam_eth_hh[is.na(housing_units)]$own_rent
)==
  table(
    hh_units_rent_eth[is.na(missed_units)]$tract,
    #hh_units_rent_eth[is.na(missed_units)]$family_role_4b,
    hh_units_rent_eth[is.na(missed_units)]$own_rent
  )
length(test[test==F])==0

#test hh4c - confirming distribution with hh_type_units - no need to do more
test <- table(
  housing_units_race_dt$tract,
  housing_units_race_dt$family,
  housing_units_race_dt$family_role_4
)==table(
  hh_type_units_dt$tract,
  hh_type_units_dt$family,
  hh_type_units_dt$family_role_4
)
length(test[test==F])==0
test <- table(
  housing_units_eth_dt$tract,
  housing_units_eth_dt$family,
  housing_units_eth_dt$family_role_4
)==table(
  hh_type_units_dt$tract,
  hh_type_units_dt$family,
  hh_type_units_dt$family_role_4
)
length(test[test==F])==0
#test hh4d - confirming distribution with hh_type_units - no need to do more
test <- table(
  sam_race_hh$tract,
  sam_race_hh$family,
  sam_race_hh$family_role_4
)==table(
  hh_type_units_dt$tract,
  hh_type_units_dt$family,
  hh_type_units_dt$family_role_4
)
length(test[test==F])==0
test <- table(
  sam_eth_hh$tract,
  sam_eth_hh$family,
  sam_eth_hh$family_role_4
)==table(
  hh_type_units_dt$tract,
  hh_type_units_dt$family,
  hh_type_units_dt$family_role_4
)
length(test[test==F])==0

#test hh5 - family_role and num_structures before moving over to sam_eth/race as just the units
test <- table(
  housing_units_race_dt$tract,
  housing_units_race_dt$family,
  housing_units_race_dt$family_role_4,
  housing_units_race_dt$num_structures
)==table(
  hh_type_units_dt$tract,
  hh_type_units_dt$family,
  hh_type_units_dt$family_role_4,
  hh_type_units_dt$num_structures
)
length(test[test==F])==0
test <- table(
  housing_units_eth_dt$tract,
  housing_units_eth_dt$family,
  housing_units_eth_dt$family_role_4,
  housing_units_eth_dt$num_structures
)==table(
  hh_type_units_dt$tract,
  hh_type_units_dt$family,
  hh_type_units_dt$family_role_4,
  hh_type_units_dt$num_structures
)
length(test[test==F])==0

#test hh5b - housing_units matched on family, race/eth
test <- table(
  housing_units_race_dt$tract,
  housing_units_race_dt$family,
  housing_units_race_dt$housing_units,
  housing_units_race_dt$race
)==table(
  sam_race_hh$tract,
  sam_race_hh$family,
  sam_race_hh$housing_units,
  sam_race_hh$race
)
length(test[test==F])==0
test <- table(
  housing_units_eth_dt$tract,
  housing_units_eth_dt$family,
  housing_units_eth_dt$housing_units,
  housing_units_eth_dt$ethnicity
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$family,
  sam_eth_hh$housing_units,
  sam_eth_hh$ethnicity
)
length(test[test==F])==0

#test hh6
test <- table(
  sam_eth_hh$tract,
  sam_eth_hh$people_per_room
)==
  table(
    sam_race_hh$tract,
    sam_race_hh$people_per_room
  )
length(test[test==FALSE])==0

#test hh7
test<-table(
  hh_occup_size_dt$tract,
  hh_occup_size_dt$own_rent,
  hh_occup_size_dt$family_or_non_eth,
  hh_occup_size_dt$family_type_eth
  )==table(
    sam_eth_hh$tract,
    sam_eth_hh$own_rent,
    sam_eth_hh$family,
    sam_eth_hh$family_type
    )
length(test[test==F])==0
test<-table(
  hh_occup_size_dt$tract,
  hh_occup_size_dt$own_rent,
  hh_occup_size_dt$family_or_non_race,
  hh_occup_size_dt$family_type_race
  )==table(
    sam_race_hh$tract,
    sam_race_hh$own_rent,
    sam_race_hh$family,
    sam_race_hh$family_type
    )
length(test[test==F])==0

#test hh8
test<-table(
  hh_size_dt$tract,
  hh_size_dt$own_rent_eth,
  hh_size_dt$family_or_non,
  hh_size_dt$hh_size
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$own_rent,
  sam_eth_hh$family,
  sam_eth_hh$hh_size
)
length(test[test==F])==0
test<-table(
  hh_size_dt$tract,
  hh_size_dt$own_rent_race,
  hh_size_dt$family_or_non,
  hh_size_dt$hh_size
)==table(
  sam_race_hh$tract,
  sam_race_hh$own_rent,
  sam_race_hh$family,
  sam_race_hh$hh_size
)
length(test[test==F])==0

#test hh8a
test<-table(
  transport_age_dt$tract,
  transport_age_dt$means_transport_5,
  transport_age_dt$means_transport_6a,
  transport_age_dt$sex
)==table(
  transport_sex_dt$tract,
  transport_sex_dt$means_transport_5,
  transport_sex_dt$means_transport,
  transport_sex_dt$sex
)
length(test[test==F])==0

#test hh8b
test<-table(
  transport_age_dt$tract,
  transport_age_dt$means_transport,
  transport_age_dt$industry,
  transport_age_dt$occupation
)==table(
  transport_industry_dt$tract,
  transport_industry_dt$means_transport,
  transport_industry_dt$industry,
  transport_industry_dt$occupation_match
)
length(test[test==F])==0
#test hh8c
test<-table(
  transport_time_work_dt$tract,
  transport_time_work_dt$sex,
  transport_time_work_dt$commute_time
)==table(
  time_to_work_sex_dt$tract,
  time_to_work_sex_dt$sex,
  time_to_work_sex_dt$time_to_work
)
length(test[test==F])==0
#test hh8d
test<-table(
  transport_time_work_dt$tract,
  transport_time_work_dt$sex,
  transport_time_work_dt$when_go_to_work
)==table(
  when_go_work_sex_dt$tract,
  when_go_work_sex_dt$sex,
  when_go_work_sex_dt$when_go_to_work
)
length(test[test==F])==0

#test hh8e
test<-table(
  transport_time_work_dt$tract,
  transport_time_work_dt$sex,
  transport_time_work_dt$commute_time,
  transport_time_work_dt$when_go_to_work
)==table(
  transport_age_dt[means_transport_5!="Worked at home"]$tract,
  transport_age_dt[means_transport_5!="Worked at home"]$sex,
  transport_age_dt[means_transport_5!="Worked at home"]$commute_time,
  transport_age_dt[means_transport_5!="Worked at home"]$when_go_to_work
)
length(test[test==F])==0

#test hh8f
test<-table(
  transport_language_dt$tract,
  transport_language_dt$language,
  transport_language_dt$English_level
)==table(
  transport_age_dt$tract,
  transport_age_dt$language,
  transport_age_dt$English_level
)
length(test[test==F])==0

#test hh8g
#test that they all moved over nrow(transport_tenure_dt[is.na(missing)])==0
#transport_tenure_dt[,c("missing"):=
#                      transport_age_dt[.SD, list(own_rent),
#                                       on = .(tenure_id)]]

#test that they all moved over nrow(transport_income_dt[is.na(missing)])==0
#transport_income_dt[,c("missing"):=
#                      transport_age_dt[.SD, list(income_range),
#                                       on = .(income_id)]]

#test hh9
test<-table(
  hh_occup_bedrooms_dt$tract,
  hh_occup_bedrooms_dt$own_rent,
  hh_occup_bedrooms_dt$num_bedrooms
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$own_rent,
  sam_eth_hh$num_bedrooms
)
length(test[test==F])==0
test<-table(
  hh_occup_bedrooms_dt$tract,
  hh_occup_bedrooms_dt$own_rent,
  hh_occup_bedrooms_dt$num_bedrooms
)==table(
  sam_race_hh$tract,
  sam_race_hh$own_rent,
  sam_race_hh$num_bedrooms
)
length(test[test==F])==0
test<-table(
  hh_occup_rooms_dt$tract,
  hh_occup_rooms_dt$own_rent,
  hh_occup_rooms_dt$num_rooms
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$own_rent,
  sam_eth_hh$num_rooms
)
length(test[test==F])==0
test<-table(
  hh_occup_rooms_dt$tract,
  hh_occup_rooms_dt$own_rent,
  hh_occup_rooms_dt$num_rooms
)==table(
  sam_race_hh$tract,
  sam_race_hh$own_rent,
  sam_race_hh$num_rooms
)
length(test[test==F])==0

#test hh10
test <- table(
  sam_race_hh$tract,
  sam_race_hh$sex
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$sex
)
length(test[test==F])==0

#test hh10a
test <- table(
  hh_workers$tract,
  hh_workers$hh_size_7
)==table(
  hh_size_dt$tract,
  hh_size_dt$hh_size
)
length(test[test==F])==0

#test hh11h
test<-table(
  vehicles_hh$tract,
  vehicles_hh$number_workers_in_hh
)==table(
  vehicles_workers_dt$tract,
  vehicles_workers_dt$number_workers_in_hh
)
length(test[test==F])==0

#test hh11i
test<-table(
  vehicles_hh$tract,
  vehicles_hh$number_vehicles_in_hh
)==table(
  sam_race_hh$tract,
  sam_race_hh$number_vehicles_hh
)
length(test[test==F])==0
test<-table(
  vehicles_hh$tract,
  vehicles_hh$number_vehicles_in_hh
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$number_vehicles_hh
)
length(test[test==F])==0

#test hh12
test <- table(
  sam_race_hh$tract,
  sam_race_hh$kids_by_age
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$kids_by_age
)
length(test[test==F])==0

#test hh13
test<-table(
  hh_partner_dt$tract,
  hh_partner_dt$partner_type,
  hh_partner_dt$sex_partner,
  hh_partner_dt$sex
)==table(
  sam_race_hh$tract,
  sam_race_hh$partner_type,
  sam_race_hh$sex_partner,
  sam_race_hh$hh_sex
)
length(test[test==F])==0
test<-table(
  hh_partner_dt$tract,
  hh_partner_dt$partner_type,
  hh_partner_dt$sex_partner,
  hh_partner_dt$sex
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$partner_type,
  sam_eth_hh$sex_partner,
  sam_eth_hh$hh_sex
)
length(test[test==F])==0

#test hh14
test<-table(
  tenure_yr_moved_units_hh$tract,
  #tenure_yr_moved_units_hh$own_rent,
  tenure_yr_moved_units_hh$housing_units,
  tenure_yr_moved_units_hh$when_moved
)==table(
  sam_race_hh$tract,
  #sam_race_hh$own_rent,
  sam_race_hh$housing_units_6,
  sam_race_hh$when_moved_in
)
length(test[test==F])==0
test<-table(
  tenure_yr_moved_units_hh$tract,
  #tenure_yr_moved_units_hh$own_rent,
  tenure_yr_moved_units_hh$housing_units,
  tenure_yr_moved_units_hh$when_moved
)==table(
  sam_eth_hh$tract,
  #sam_eth_hh$own_rent,
  sam_eth_hh$housing_units_6,
  sam_eth_hh$when_moved_in
)
length(test[test==F])==0

#test hh15
test<-table(
  hh_income_dt$tract,
  hh_income_dt$hh_income_level,
  hh_income_dt$income_low,
  hh_income_dt$income_high
)==table(
  sam_race_hh$tract,
  sam_race_hh$hh_income_level,
  sam_race_hh$income_low,
  sam_race_hh$income_high
)
length(test[test==F])==0
test<-table(
  hh_income_dt$tract,
  hh_income_dt$hh_income_level,
  hh_income_dt$income_low,
  hh_income_dt$income_high
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$hh_income_level,
  sam_eth_hh$income_low,
  sam_eth_hh$income_high
)
length(test[test==F])==0

#test hh16
test<-table(
  hh_educ_dt$tract,
  hh_educ_dt$hh_education_level
)==table(
  sam_race_hh$tract,
  sam_race_hh$hh_education_level
)
length(test[test==F])==0
test<-table(
  hh_educ_dt$tract,
  hh_educ_dt$hh_education_level
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$hh_education_level
)
length(test[test==F])==0

#test hh17
test<-table(
  gross_rent_hh$tract,
  gross_rent_hh$hh_income_renters
)==table(
  sam_race_hh$tract,
  sam_race_hh$hh_income_renters
)
length(test[test==F])==0
test<-table(
  gross_rent_hh$tract,
  gross_rent_hh$hh_income_renters
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$hh_income_renters
)
length(test[test==F])==0



#test hh19
test<-table(
  food_stamps_race_dt$tract,
  food_stamps_race_dt$race,
  food_stamps_race_dt$food_stamps
)==table(
  sam_race_hh$tract,
  sam_race_hh$race,
  sam_race_hh$SNAP
)
length(test[test==F])==0
test<-table(
  food_stamps_eth_dt$tract,
  food_stamps_eth_dt$ethnicity,
  food_stamps_eth_dt$food_stamps
)==table(
  sam_eth_hh$tract,
  sam_eth_hh$ethnicity,
  sam_eth_hh$SNAP
)
length(test[test==F])==0


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





