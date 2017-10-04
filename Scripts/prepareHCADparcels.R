#Read in Parcels

library(sf)
parcels <- st_read("../hcadparcelstuff/Parcels/Parcels.shp")

parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")

#Read in Texas Census Tract Files
TXCensusTracts <- st_read("../hcadparcelstuff/TexasCensusTractShapefiles/gz_2010_48_140_00_500k.shp")
#Put them in same CRS as Parcels
TXCensusTracts <- st_transform(TXCensusTracts,st_crs(validparcels))

#Get Census Tracts for each parcel
CensusTractforHCADParcels=st_within(validparcels,TXCensusTracts)
CensusTractforHCADParcelsunlisted=rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
CensusTractforHCADParcelsunlisted=unlist(CensusTractforHCADParcelsunlisted)

validparcels$COUNTY=TXCensusTracts$COUNTY[CensusTractforHCADParcelsunlisted]
validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]

#Merge with building features for residential houses

#Read in data
buildingfeatures=read.table('../hcadparcelstuff/building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE)#, ,colClasses = "character",stringsAsFactors = FALSE
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

#validparceldataframe=as.data.frame(validparcels)

#validparceldataframe2=merge(buildingfeatures,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

#Maybe this is better than that
library(tigris)
buildingfeatures$"HCAD_NUM"=buildingfeatures$ACCOUNT

validparcel2=geo_join(validparcels,buildingfeatures,by="HCAD_NUM",how="inner")

#Convert to data frame
validparceldataframe2=as.data.frame(validparcel2)
#validparceldataframe2=unlist(validparceldataframe2)
#Write to tab delimited file
#csv is a bad idea because of geometry column

saveRDS(validparceldataframe2,"validparceldataframe2.RDS")
#Write to tab delimited file
