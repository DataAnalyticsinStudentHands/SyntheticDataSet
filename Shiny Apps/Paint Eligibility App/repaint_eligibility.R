library(sf)
parcels <- st_read("hcadparcelstuff/Parcels/Parcels.shp")

parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")


#Merge with building features for residential houses

#Read in data
buildingfeatures=read.table('hcadparcelstuff/building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE)#, ,colClasses = "character",stringsAsFactors = FALSE
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

#validparceldataframe=as.data.frame(validparcels)

#validparceldataframe2=merge(buildingfeatures,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

#Maybe this is better than that
library(tigris)
buildingfeatures$"HCAD_NUM"=buildingfeatures$ACCOUNT

validparcel2=geo_join(validparcels,buildingfeatures,by="HCAD_NUM",how="inner")

#Homes must be built before 1978
validparcel2$DATE_ERECTED=as.numeric(as.character(validparcel2$DATE_ERECTED))
houses_eligible=subset(validparcel2,validparcel2$DATE_ERECTED<1978)
houses_too_young=subset(validparcel2,validparcel2$DATE_ERECTED>=1978)
houses_too_young=data.frame(LocAddr=houses_too_young$LocAddr,zip=houses_too_young$zip)
write.csv(houses_too_young,"houses_too_young.csv")


#Homes must be in city limits
#I'm only interested in looking at Houston so
#library(rgdal)
TxDOT_cities <- st_read("TxDOT_City_Boundaries")
Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]
Houston_bounds <- st_transform(Houston_bounds, st_crs(validparcels))

#houses_eligible=as(houses_eligible, "Spatial")
#Houston_bounds=st_as_sf(Houston_bounds)
houses_eligible=houses_eligible[Houston_bounds,]
houses_addresses=houses_eligible[,c("LocAddr","zip")]
#Houses can't be in a Flood Zone

#FEMA 2015
FEMA2015floodplains=st_read("FEMA_Floodplains_NFHL_2015.gdb")
FEMA2015floodplains <- st_transform(FEMA2015floodplains,st_crs(validparcels))
FEMA2015floodplains <- FEMA2015floodplains[(FEMA2015floodplains$ZONE_SUBTY=="0.2 PCT ANNUAL CHANCE FLOOD HAZARD"|FEMA2015floodplains$ZONE_SUBTY=="FLOODWAY"),]
FEMA2015floodplains <- FEMA2015floodplains[Houston_bounds,]

indexes_of_eligbile_houses=st_intersects(houses_eligible, FEMA2015floodplains)
indexes_of_eligbile_houses=rapply(indexes_of_eligbile_houses,function(x) ifelse(length(x)==0,TRUE,FALSE), how = "replace")
indexes_of_eligbile_houses=unlist(indexes_of_eligbile_houses)
houses_eligible2=houses_eligible[indexes_of_eligbile_houses,]

eligible_addresses=houses_eligible[indexes_of_eligbile_houses,c("LocAddr","zip")]
eligible_addresses$geometry=NULL

write.csv(eligible_addresses,"eligible_addresses.csv")

not_eligible_in_floodplain=houses_eligible[!indexes_of_eligbile_houses,c("LocAddr","zip")]
not_eligible_in_floodplain$geometry=NULL

write.csv(not_eligible_in_floodplain,"floodplain_addresses.csv")
library(leaflet)

#To projection leaflet likes
FEMA2015floodplains <- st_transform(FEMA2015floodplains,"+proj=longlat +datum=WGS84")
houses_eligible2 <- st_transform(houses_eligible2,"+proj=longlat +datum=WGS84")

#map<-leaflet() %>%
 # addPolygons(data = FEMA2015floodplains, 
  #            fillColor = "blue", 
   #           fillOpacity = 0.7, 
    #          weight = 1, 
     #         smoothFactor = 0.2)%>%
  #addPolygons(data=houses_eligible2,
   #           fillColor = "red",
    #          fillOpacity = 0.7, 

     #         weight = 1, 
      #        smoothFactor = 0.2)
#map
not_in_houston=houses_addresses[!(houses_addresses$LocAddr %in% eligible_addresses$LocAddr),]
not_in_houston=not_in_houston[!(not_in_houston$LocAddr %in% not_eligible_in_floodplain$LocAddr),]
not_in_houston$geometry=NULL

write.csv(not_in_houston,"not_in_houston.csv")