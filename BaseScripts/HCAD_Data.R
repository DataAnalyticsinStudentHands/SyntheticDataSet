library(sf)
library(tigris)
library(stringr)
library(dplyr)

#' getHCADParcels function
#'
#' This function reads parcels from HCAD (Harris County Aprraisal District), adds tracts information (county, tract) and remove unnecessary columns.
#'
#' @param hcadDataDir the folder with the HCAD raw parcels
#' @param HCAD_parcelsRDS determines wheter to read preprocessed HCAD data from RDS file
getHCADParcels <- function(hcadDataDir){
  
  # the Parcels.shp file can be downloaded from http://pdata.hcad.org/GIS/index.html
  parcels <- st_read(paste0(hcadDataDir, "Parcels/Parcels.shp"))
  
  # remove invalid parcels
  parcels$valid <- st_is_valid(parcels)
  HCAD_parcels <- subset(parcels,parcels$valid)
  HCAD_parcels$valid <- NULL
  rm(parcels)
  
  # add tract information from Texas Census Tract Files
  TXCensusTracts <- st_read(paste0(hcadDataDir, "gz_2010_48_150_00_500k/gz_2010_48_150_00_500k.shp"))
  # put them in same CRS as Parcels
  TXCensusTracts <- st_transform(TXCensusTracts, st_crs(HCAD_parcels))
  
  # match parcels with tracts
  CensusTractforHCADParcels <- st_within(HCAD_parcels, TXCensusTracts)
  CensusTractforHCADParcelsunlisted <- rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  CensusTractforHCADParcelsunlisted <- unlist(CensusTractforHCADParcelsunlisted)
  
  # add census tract information to each parcel
  HCAD_parcels$COUNTY=TXCensusTracts$COUNTY[CensusTractforHCADParcelsunlisted]
  HCAD_parcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]
  
  #cleanup
  rm(TXCensusTracts)
  rm(CensusTractforHCADParcels)
  rm(CensusTractforHCADParcelsunlisted)
  
  
  # prepare parcels
  HCAD_parcels <- HCAD_parcels %>% select(-BLK_NUM,-LOT_NUM,-CONDO_FLAG,-parcel_typ,-CurrOwner)
  # save as RDS
  print("Writing HCAD RDS file ...")
  saveRDS(HCAD_parcels,paste0(hcadDataDir, "PreprocessedRDS/HCAD_parcels.RDS"))
  print("Done.")
  
  return(HCAD_parcels)
}

#' getresBuildings function
#'
#' This function reads residential building information from HCAD (Harris County Aprraisal District) data files, joins valid parcels to create subset of residential building parcels and removes unnecessary columns.
#'
#' @param HCAD_parcels set of preprocessed parcels
getResBuildings <- function(buildingDataDir, HCAD_parcels) {
  # Prepare building features for residential houses downloaded from https://hcad.org/pdata/pdata-property-downloads.html by year
  HCAD_res_build <- read.table(paste0(buildingDataDir, "building_res.txt"), sep="\t", header=FALSE, fill=TRUE, colClasses = "character", strip.white = TRUE, quote = "")
  # colnames are exported from HCAD access.zip
  colnames(HCAD_res_build) <- readLines("Mappings/HCAD_ResBuildingFeaturesColumnNames.txt")
  
  HCAD_res_build$ACCOUNT <- str_remove_all(HCAD_res_build$ACCOUNT, " ")
  HCAD_res_build <- HCAD_res_build %>%
    select(ACCOUNT, IMPRV_TYPE, DEPRECIATION_VALUE, QUALITY_DESCRIPTION, DATE_ERECTED, YR_REMODEL,
           EFFECTIVE_AREA, NBHD_FACTOR, SIZE_INDEX)
  
  # get residential parcels
  Residential_Parcels <- tigris::geo_join(HCAD_parcels, HCAD_res_build, by_sp = "HCAD_NUM", by_df = "ACCOUNT", how = "inner")
  
  return(Residential_Parcels)
}

#' getrealBuildings function
#'
#' This function reads other building information from HCAD (Harris County Aprraisal District) data files, joins with valid parcels to create subset of real building parcels and removes unnecessary columns.
#'
#' @param HCAD_parcels set of preprocessed parcels
getRealBuildings <- function(buildingDataDir, HCAD_parcels) {
  # Prepare building features for other buildings e.g. dormitories etc. downloaded from http://pdata.hcad.org/download/2014.html
  HCAD_real_build <- read.table(paste0(buildingDataDir, "building_other.txt"), sep="\t", header=FALSE, fill=TRUE, colClasses = "character", strip.white = TRUE, quote = "")
  # colnames are exported from HCAD access.zip
  colnames(HCAD_real_build)  <- readLines("Mappings/HCAD_OthersBuildingFeaturesColumnNames.txt")
  
  HCAD_real_build$ACCOUNT <- str_remove_all(HCAD_real_build$ACCOUNT, " ")
  HCAD_real_build <- HCAD_real_build %>% 
    select(ACCOUNT,IMPRV_TYPE,DEPRECIATION_VALUE,QUALITY_DESCRIPTION,DATE_ERECTED,YR_REMODEL,BASE_AREA,
           PERIMETER,PROPERTY_NAME,UNITS,LEASE_RATE,OCCUPANCY,TOTAL_INCOME)
  
  Realbuildings_Parcels <- tigris::geo_join(HCAD_parcels, HCAD_real_build, by_sp = "HCAD_NUM", by_df = "ACCOUNT", how="inner")
  
  # remove duplicates by creating building account numbers
  Realbuildings_Parcels$ACCOUNT <- paste0(Realbuildings_Parcels$ACCOUNT, "_", Realbuildings_Parcels$BUILDING_NUMBER)
  Realbuildings_Parcels <- Realbuildings_Parcels[!duplicated(Realbuildings_Parcels$ACCOUNT), ]
  
  return(Realbuildings_Parcels)
}



#add account number, remove duplicates, filter for building style codes TO DO: Move into get .... Buildings function(s)
filterBuidingsHCAD <- function() {
  
  
  # From the buildings in the tract, subset for only the ones with living spaces
  potential_buildings=subset(validparceldataframe2,validparceldataframe2$"BUILDING_STYLE_CODE" %in% c("101","102", "103", "104", "107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988","105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989", "660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8309","8489","8311","8491","8514"))
  
  
  
  # Depending on the USE_CODE some buildings will only have one unit while others will have multiple units. If a building doesn't list units then use the average number of units for that building's USE_CODE
  potential_buildings$UNITS[potential_buildings$USE_CODE %in% c("1D1", "A1", "A2", "A3", "A4", "B2", "B3", "B4", "C1", "C2", "C3", "E1", "M3", "O1", "O2", "U0", "X4", "XG", "XI", "XL", "Z4") | is.na(potential_buildings$UNITS)]= 1
  for(x in c("B1", "F1", "F2", "X1", "X2", "X3", "XE", "XJ", "XU")){
    avg_units = round(mean(as.integer(potential_buildings[potential_buildings$USE_CODE == x & !potential_buildings$UNITS %in% c("", "A", "B", "C", "D", "1/2", "1/1"),]$UNITS), na.rm = T))
    potential_buildings$UNITS[potential_buildings$USE_CODE == x & potential_buildings$UNITS %in% c("", "A", "B", "C", "D", "1/1", "1/2")] = avg_units
  }
  
  
}
