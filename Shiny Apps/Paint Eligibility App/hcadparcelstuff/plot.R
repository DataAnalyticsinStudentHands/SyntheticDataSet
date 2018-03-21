library(sf)

#Read in HCAD parcel data and subset ones with valid geometry
parcels <- st_read("Parcels/Parcels.shp")
parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")
rm(parcels)

#Read in Census tract polygons ans set them to the same projection
TXCensusTracts <- st_read("TexasCensusTractShapefiles/gz_2010_48_140_00_500k.shp")
TXCensusTracts <- st_transform(TXCensusTracts,st_crs(validparcels))

#Get which Census Tract each HCAD parcel is located in
CensusTractforHCADParcels=st_within(validparcels,TXCensusTracts)
CensusTractforHCADParcelsunlisted=rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
CensusTractforHCADParcelsunlisted=unlist(CensusTractforHCADParcelsunlisted)

validparcels$COUNTY=TXCensusTracts$COUNTY[CensusTractforHCADParcelsunlisted]
validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]

rm(CensusTractforHCADParcels)
rm(CensusTractforHCADParcelsunlisted)

#load data on residential parcels
buildingfeaturescharacters=read.table('building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE)
colnames(buildingfeaturescharacters)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

#Join residential parcel data with valid parcels
validparceldataframe=as.data.frame(validparcels)
buildingfeaturescharacters$HCAD_NUM=buildingfeaturescharacters$ACCOUNT
validparceldataframe2=merge(buildingfeaturescharacters,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")
validparcelsf=st_as_sf(validparceldataframe2)

rm(validparcels)
rm(validparceldataframe)

#Plot map of hcad parcels with different colors for different census tracts
library(leaflet)

validparcelsf$TRACT=as.character(validparcelsf$TRACT)
validparcelsf$TRACT=as.numeric(validparcelsf$TRACT)

validparcelsf=st_transform(validparcelsf,'+proj=longlat +datum=WGS84')

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = validparcelsf$TRACT
)

map2<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = validparcelsf, 
              fillColor = ~pal(TRACT), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2)

rm(buildingfeaturescharacters)
rm(TXCensusTracts)
rm(validparceldataframe2)

map2
