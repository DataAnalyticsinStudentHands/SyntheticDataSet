#Generate appropriate number of households per tract

#read in data
households=read.csv("household_type_for_error.csv")
#get total number of households per tract
households$total=rowSums(households[4:8])

#Simulate Houston people
households=subset(households,households$county==201)

seed=1

tracts=households$tract
number.of.households=households$total

sample.set=data.frame()
for (index in 1:length(tracts)){
  sample.set=rbind(sample.set,master(201,tracts[index],number.of.households[index],seed))
}

write.csv(sample.set)

#Merge with HCAD data

#Read in Parcels

library(sf)
parcels <- st_read("Parcels/Parcels.shp")
parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")

#Read in Texas Census Tract Files
TXCensusTracts <- st_read("TexasCensusTractShapefiles/gz_2010_48_140_00_500k.shp")
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
buildingfeatures=read.table('building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE)#, ,colClasses = "character",stringsAsFactors = FALSE
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

validparceldataframe=as.data.frame(validparcels)

validparceldataframe2=merge(buildingfeatures,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

#Merge households with houses
sample.set$ACCOUNT=NA

for (tract in tracts){
  tracthouses=subset(validparceldataframe2,validparceldataframe2$TRACT=tract)
  householdIDs=unique(subset(sample.set,sample.set$tract==tract)$householdID)
  
  
}






