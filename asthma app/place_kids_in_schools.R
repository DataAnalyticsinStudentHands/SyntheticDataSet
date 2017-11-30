#Chose instead to simulate asthma from BRFSS data and Asthma call back survey
#Scripts for this with accompanying data are in the folder BRSSR
#the script is called organize_and_simulate_asthma.R

#read in simulation
syntheticdataset=readRDS("asthma_simulation.RDS")

#read in house locations
library(sf)
parcels <- st_read("../hcadparcelstuff/Parcels/Parcels.shp")
#this was downloaded from HCAD

parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")


#Read in data
buildingfeatures=read.table('../hcadparcelstuff/building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE,quote = "")#, ,colClasses = "character",stringsAsFactors = FALSE
#buildingfeatures=buildingfeatures[-(1:371770),]
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")


#Maybe this is better than that
library(tigris)
buildingfeatures$"HCAD_NUM"=buildingfeatures$ACCOUNT

validparcel2=geo_join(validparcels,buildingfeatures,by="HCAD_NUM",how="inner")

#School zones were collected from this site
#http://cohgis-mycity.opendata.arcgis.com/datasets
#In 11/24/2017

#Read in elementary school zones
elementary_school_zones <- st_read("HISD_Elementary_Boundary.shp")
#Put them in same CRS as Parcels
elementary_school_zones <- st_transform(elementary_school_zones,st_crs(validparcel2))

#Get elementary school for each parcel
ElementarySchoolforHCADParcels=st_within(validparcel2,elementary_school_zones)
ElementarySchoolforHCADParcelsunlisted=rapply(ElementarySchoolforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
ElementarySchoolforHCADParcelsunlisted=unlist(ElementarySchoolforHCADParcelsunlisted)

validparcel2$Elementary_School=elementary_school_zones$Elementary[ElementarySchoolforHCADParcelsunlisted]
#validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]

#Read in middle school zones
middle_school_zones <- st_read("HISD_Middle_School_Boundary.shp")
#Put them in same CRS as Parcels
middle_school_zones <- st_transform(middle_school_zones,st_crs(validparcel2))

#Get elementary school for each parcel
MiddleSchoolforHCADParcels=st_within(validparcel2,middle_school_zones)
MiddleSchoolforHCADParcelsunlisted=rapply(MiddleSchoolforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
MiddleSchoolforHCADParcelsunlisted=unlist(MiddleSchoolforHCADParcelsunlisted)

validparcel2$Middle_School=middle_school_zones$Middle_Sch[MiddleSchoolforHCADParcelsunlisted]

#Read in high school zones
high_school_zones <- st_read("HISD_High_School_Boundary.shp")
#Put them in same CRS as Parcels
high_school_zones <- st_transform(high_school_zones,st_crs(validparcel2))

#Get elementary school for each parcel
HighSchoolforHCADParcels=st_within(validparcel2,high_school_zones)
HighSchoolforHCADParcelsunlisted=rapply(HighSchoolforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
HighSchoolforHCADParcelsunlisted=unlist(HighSchoolforHCADParcelsunlisted)

validparcel2$High_School=high_school_zones$High_Schoo[HighSchoolforHCADParcelsunlisted]

#Remove no longer necessary variables
rm(validparcels)
rm(buildingfeatures,parcels,ElementarySchoolforHCADParcels,ElementarySchoolforHCADParcelsunlisted,HighSchoolforHCADParcels,HighSchoolforHCADParcelsunlisted,MiddleSchoolforHCADParcels,MiddleSchoolforHCADParcelsunlisted)

#Subset children in public schools
syntheticdataset=subset(syntheticdataset,syntheticdataset$school.enrollment=="Public School")

#Place public school children in public schools based on appropriate age
elementary_school_kids=subset(syntheticdataset,syntheticdataset$age=="5 to 9")
elementary_schools_for_merge=data.frame(ACCOUNT=validparcel2$HCAD_NUM,School=validparcel2$Elementary_School)
elementary_school_kids=merge(elementary_school_kids,elementary_schools_for_merge,all.x = TRUE)

middle_school_kids=subset(syntheticdataset,syntheticdataset$age=="10 to 14")
middle_schools_for_merge=data.frame(ACCOUNT=validparcel2$HCAD_NUM,School=validparcel2$Middle_School)
middle_school_kids=merge(middle_school_kids,middle_schools_for_merge,all.x = TRUE)

high_school_kids=subset(syntheticdataset,syntheticdataset$age=="15 to 17")
high_schools_for_merge=data.frame(ACCOUNT=validparcel2$HCAD_NUM,School=validparcel2$High_School)
high_school_kids=merge(high_school_kids,high_schools_for_merge,all.x = TRUE)

