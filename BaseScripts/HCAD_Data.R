library(sf)

# Make sure the required files downlaoded and in the proper directory before running the script. Some files may have to be downloaded from http://pdata.hcad.org/GIS/index.html and http://pdata.hcad.org/download/2014.html . Check the script for more details.

getHCADParcels <- function(){
  # The Parcels.shp file can be downloaded from http://pdata.hcad.org/GIS/index.html
  parcels <- st_read("Parcels.shp")

  parcels$valid=st_is_valid(parcels, reason = TRUE)
  validparcels=subset(parcels,parcels$valid=="Valid Geometry")

  #Read in Texas Census Tract Files
  TXCensusTracts <- st_read("/Data/gz_2010_48_140_00_500k.shp")
  #Put them in same CRS as Parcels
  TXCensusTracts <- st_transform(TXCensusTracts,st_crs(validparcels))

  #Get Census Tracts for each parcel
  CensusTractforHCADParcels=st_within(validparcels,TXCensusTracts)
  CensusTractforHCADParcelsunlisted=rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  CensusTractforHCADParcelsunlisted=unlist(CensusTractforHCADParcelsunlisted)

  validparcels$COUNTY=TXCensusTracts$COUNTY[CensusTractforHCADParcelsunlisted]
  validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]

  #Save this thing because it takes a long time to load
  saveRDS(validparcels,"validparcels.RDS")
  #Merge with building features for residential houses

  #Read in data
  # The "building_res.txt" file can be downloaded from http://pdata.hcad.org/download/2014.html
  buildingfeatures=read.table('building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE,quote = "")#, ,colClasses = "character",stringsAsFactors = FALSE
  colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

  #Maybe this is better than that
  library(tigris)
  buildingfeatures$"HCAD_NUM"=buildingfeatures$ACCOUNT

  validparcel2=geo_join(validparcels,buildingfeatures,by="HCAD_NUM",how="inner")

  #Convert to data frame
  validparceldataframe2=as.data.frame(validparcel2)
  
  #Write to tab delimited file
  #csv is a bad idea because of geometry column

  saveRDS(validparceldataframe2,"validparceldataframe2.RDS")
  saveRDS(validparcel2,"validparcelsforhouseholds.RDS")
  #Write to tab delimited file

  #So places where people may live in for group quarters I think are more likely to be in the commercial file
  #then residential because it's commercial property-dorms, assisted living facilities etc

  #So we need the non residential files
  # The "building_other.txt" file can be downloaded from http://pdata.hcad.org/download/2014.html
  buildingfeatures=read.table('building_other.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE,quote = "")#, ,colClasses = "character",stringsAsFactors = FALSE
  colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","NOTICED_DEPR_VALUE","DEPRECIATION_VALUE","MS_REPLACEMENT_COST","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","CATEGORY","CATEGORY_DESC","PROPERTY_NAME","UNITS","NET_RENT_AREA","LEASE_RATE","OCCUPANCY","TOTAL_INCOME")

  buildingfeatures$"HCAD_NUM"=buildingfeatures$ACCOUNT

  validparcels3=geo_join(validparcels,buildingfeatures,by="HCAD_NUM",how="inner")
  saveRDS(validparcels3,"validparcelsforgroupquartersmaybe.RDS")
  #Convert to data frame
  validparceldataframe3=as.data.frame(validparcels3)
  
  #Write to tab delimited file
  #csv is a bad idea because of geometry column

  saveRDS(validparceldataframe3,"validparceldataframe3forgroupquarters.RDS")

  vp1=readRDS("validparcelsforhouseholds.RDS")
  vp2=readRDS("validparcelsforgroupquartersmaybe.RDS")
  library(plyr)
  fullvalidparcelsweneed=rbind.fill(vp1,vp2)
  saveRDS(fullvalidparcelsweneed,"valid_parcels_for_simulation.RDS")
  
  return (fullvalidparcelsweneed)
}

#add account number, remove duplictes, filter for building style codes
prepareHCAD <- function() {
  # create building account numbers
  validparceldataframe2$ACCOUNT = paste0(validparceldataframe2$ACCOUNT, "_", validparceldataframe2$BUILDING_NUMBER)
  validparceldataframe2 = validparceldataframe2[!duplicated(validparceldataframe2$ACCOUNT), ]
  
  # From the buildings in the tract, subset for only the ones with living spaces
  potential_buildings=subset(validparceldataframe2,validparceldataframe2$"BUILDING_STYLE_CODE" %in% c("101","102", "103", "104", "107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988","105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989", "660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8309","8489","8311","8491","8514"))
  
  
}
