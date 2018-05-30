parcels_2014=readRDS("2014 HCAD stuff/valid_parcels_for_simulation.RDS")
parcels_2015=readRDS("2015 HCAD stuff/valid_parcels_for_simulation.RDS")

parcels_2014$ACCOUNT=paste0(parcels_2014$ACCOUNT,"_",parcels_2014$BUILDING_NUMBER)
parcels_2015$ACCOUNT=paste0(parcels_2015$ACCOUNT,"_",parcels_2015$BUILDING_NUMBER)

buildings_no_longer_available=parcels_2014$ACCOUNT[which(!parcels_2014$ACCOUNT %in% parcels_2015$ACCOUNT)]
new_buildings=parcels_2015$ACCOUNT[which(!parcels_2015$ACCOUNT %in% parcels_2014$ACCOUNT)]

sample_set=readRDS("complete_sample_set2018-05-29.RDS")

