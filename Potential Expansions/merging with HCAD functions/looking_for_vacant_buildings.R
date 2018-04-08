#I ran this stuff before but am putting it here so you can see it
#library(sf)
#parcels <- st_read("../hcadparcelstuff/Parcels/Parcels.shp")

#parcels$valid=st_is_valid(parcels, reason = TRUE)
#validparcels=subset(parcels,parcels$valid=="Valid Geometry")

#Read in Texas Census Tract Files
#TXCensusTracts <- st_read("../hcadparcelstuff/TexasCensusTractShapefiles/gz_2010_48_140_00_500k.shp")
#Put them in same CRS as Parcels
#TXCensusTracts <- st_transform(TXCensusTracts,st_crs(validparcels))

#Get Census Tracts for each parcel
#CensusTractforHCADParcels=st_within(validparcels,TXCensusTracts)
#CensusTractforHCADParcelsunlisted=rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
#CensusTractforHCADParcelsunlisted=unlist(CensusTractforHCADParcelsunlisted)

#validparcels$COUNTY=TXCensusTracts$COUNTY[CensusTractforHCADParcelsunlisted]
#validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]

validparcels=readRDS("validparcels.RDS")

#So we need the non residential files
buildingfeatures=read.table('../hcadparcelstuff/building_other.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE,quote = "")#, ,colClasses = "character",stringsAsFactors = FALSE
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","NOTICED_DEPR_VALUE","DEPRECIATION_VALUE","MS_REPLACEMENT_COST","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","CATEGORY","CATEGORY_DESC","PROPERTY_NAME","UNITS","NET_RENT_AREA","LEASE_RATE","OCCUPANCY","TOTAL_INCOME")

buildingfeatures$"HCAD_NUM"=buildingfeatures$ACCOUNT

library(tigris)
validparcels3=geo_join(validparcels,buildingfeatures,by="HCAD_NUM",how="inner")

#subset by what I believe are code for residential properties
places_people_live=subset(validparcels3,validparcels3$BUILDING_STYLE_CODE %in% c("660","8321","8324","8393","8424","8451","8589","101","107","108","109","125","8177","8178","8179",
                                                                                 "8338","8351","8354","8401","8548","8549","8550","8986","8988","102","103","104","105","8300","8352","8338","8459",
                                                                                 "8493","8546","8547","8596","8984","8987","8989"))
unique(places_people_live$OCCUPANCY)

#Merge to get Land Use code
land_stuff=read.table('../hcadparcelstuff/land.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE,quote = "")#, ,colClasses = "character",stringsAsFactors = FALSE
colnames(land_stuff)=c("ACCOUNT","LINE_NUMBER","LAND_USE_CODE","SITE_CD","SITE_CD_DSCR","SITE_ADJ","UNIT_TYPE","UNITS","SIZE_FACTOR","SITE_FACT","APPR_OVERRIDE_FACTOR","APPR_OVERRIDE_REASON","TOT_ADJ","UNIT_PRICE","ADJ_UNIT_PRICE","VALUE","OVERRIDE_VALUE")

land_stuff$"HCAD_NUM"=land_stuff$ACCOUNT

validparcelsagain=geo_join(validparcels,land_stuff,by="HCAD_NUM",how="inner")

#Subset codes that have vacant in their description
#http://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-land-codes/
maybe_this_is_what_youre_asking_for=subset(validparcelsagain,validparcelsagain$LAND_USE_CODE %in% c("1000","1002","2000","2002","7000","9999"))
saveRDS(places_people_live,"places_people_live_that_have_occupancy_rates.RDS")
saveRDS(maybe_this_is_what_youre_asking_for,"vacant_land_by_land_use_code.RDS")
