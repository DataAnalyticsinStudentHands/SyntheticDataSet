#Read in Valid Parcels
library(sf)
parcels <- st_read("Parcels.shp")
parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")

#Read in HUD data
HUD_data=read.csv("MF_Properties_with_Assistance_&_Sec8_Contracts.csv")

#Subset Houston
Houston_HUD_data=subset(HUD_data,HUD_data$city_name_text=="Houston")
Houston_HUD_data$LocAddr=toupper(Houston_HUD_data$address_line1_text)

#Read in Valid Parcels
validparcels=readRDS("validparcels.RDS")
#complete_sample_set=readRDS("complete_sample_set.RDS")
#Join
library(tigris)
Houston_HUD_buildings=geo_join(validparcels,Houston_HUD_data,by="LocAddr",how="inner")

#Houston_HUD_simulated_people=subset(complete_sample_set,complete_sample_set$LocAddr %in% Houston_HUD_data$LocAddr)
#some houses went missing?
Houston_HUD_buildings2=geo_join(parcels,Houston_HUD_data,by="LocAddr",how="inner")
#still missing?
missing_addresses=setdiff(Houston_HUD_data$LocAddr,parcels$LocAddr)

library(sf)
Houston_HUD_buildings2=st_transform(Houston_HUD_buildings2,crs='+proj=longlat +datum=WGS84')

library(leaflet)

map = leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Houston_HUD_buildings2, 
              color = "red", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2)

map

saveRDS(Houston_HUD_buildings2,"Recent_HUD_Buildings")




#Trying to match with the validparcels our dataframe is built off of instead of the latest
#Read in HUD data
HUD_data=read.csv("MF_Properties_with_Assistance_&_Sec8_Contracts.csv")

#Subset Houston
Houston_HUD_data=subset(HUD_data,HUD_data$city_name_text=="Houston")
Houston_HUD_data$LocAddr=toupper(Houston_HUD_data$address_line1_text)

validparcels=readRDS("validparcels1.RDS")

Houston_HUD_buildings=rbind(validparcels[grep(paste(toupper(Houston_HUD_data$owner_organization_name), collapse='|'), validparcels$CurrOwner, ignore.case=TRUE),],
                            validparcels[grep(paste(toupper(Houston_HUD_data$address_line1_text), collapse='|'), validparcels$LocAddr, ignore.case=TRUE),],
                            validparcels[grep(paste(toupper(Houston_HUD_data$property_name_text), collapse='|'), validparcels$LocName, ignore.case=TRUE),])

