library(sf)
library(reader)
library(tigris)

#' getHCADParcels function
#'
#' This function reads parcels from HCAD (Harris County Aprraisal District), adds tracts information (county, tract) and remove unnecessary columns.
#'
#' @param hcadDataDir the folder with the HCAD raw parcels
#' @param HCAD_parcelsRDS determines wheter to read preprocessed HCAD data from RDS file
getHCADParcels <- function(hcadDataDir, HCAD_parcelsRDS = FALSE){
  
  if (HCAD_parcelsRDS) {
    # the Parcels.shp file can be downloaded from http://pdata.hcad.org/GIS/index.html
    parcels <- st_read(paste0(hcadDataDir, "Parcels/Parcels.shp"))
    
    # remove invalid parcels
    parcels$valid <- st_is_valid(parcels)
    HCAD_parcels <- subset(parcels,parcels$valid == "Valid Geometry")
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
    HCAD_parcelss$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]
    
    # save as RDS
    saveRDS(HCAD_parcels,paste0(hcadDataDir, "PreprocessedRDS/HCAD_parcels.RDS"))
    
    #cleanup
    rm(TXCensusTracts)
    rm(CensusTractforHCADParcels)
    rm(CensusTractforHCADParcelsunlisted)
    
  } else {
    HCAD_parcels <- readRDS(paste0(hcadDataDir, "PreprocessedRDS/HCAD_parcels.RDS"))
  }
  
  # prepare parcels
  HCAD_parcels <- HCAD_parcels %>% select(-BLK_NUM,-LOT_NUM,-CONDO_FLAG,-parcel_typ,-CurrOwner)
  
  return(HCAD_parcels)
}

#' getresBuildings function
#'
#' This function reads residential building information from HCAD (Harris County Aprraisal District) data files, joins valid parcels to create subset of residential building parcels and removes unnecessary columns.
#'
#' @param HCAD_parcels set of preprocessed parcels
getresBuildings <- function(HCAD_parcels) {
  # Prepare building features for residential houses downloaded from http://pdata.hcad.org/download/2014.html
  HCAD_res_build <- read.csv2(paste0(hcadDataDir, "Real_building_land/building_res.txt"), 
                              stringsAsFactors = FALSE,
                              sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
  # colnames are exported from HCAD access.zip
  colnames(HCAD_res_build) <- readLines("Mappings/HCAD_ResBuildingFeaturesColumnNames.txt")
  
  HCAD_res_build$ACCOUNT <- str_remove_all(HCAD_res_build$ACCOUNT, " ")
  HCAD_res_build <- HCAD_res_build %>%
    select(ACCOUNT, IMPRV_TYPE, DEPRECIATION_VALUE, QUALITY_DESCRIPTION, DATE_ERECTED, YR_REMODEL,
           EFFECTIVE_AREA, NBHD_FACTOR, SIZE_INDEX)
  
  # get residential parcels
  Residentidential_Parcels <- tigris::geo_join(HCAD_parcels, HCAD_res_build_build, by_sp = "HCAD_NUM", by_df = "ACCOUNT", how = "inner")
  
  return(Residentidential_Parcels)
}

#' getrealBuildings function
#'
#' This function reads other building information from HCAD (Harris County Aprraisal District) data files, joins with valid parcels to create subset of real building parcels and removes unnecessary columns.
#'
#' @param HCAD_parcels set of preprocessed parcels
getrealBuildings <- function(HCAD_parcels) {
  # Prepare building features for other buildings e.g. dormitories etc. downloaded from http://pdata.hcad.org/download/2014.html
  HCAD_real_build <- read.csv2(paste0(hcadDataDir, "Real_building_land/building_other.txt"), 
                               sep = "\t", header = FALSE, quote="", colClasses = c("V1"="character"))
  
  # colnames are exported from HCAD access.zip
  colnames(HCAD_real_build)  <- readLines("Mappings/HCAD_OthersBuildingFeaturesColumnNames.txt")
  
  HCAD_real_build$ACCOUNT <- str_remove_all(HCAD_real_build$ACCOUNT, " ")
  HCAD_real_build <- HCAD_real_build %>% 
    select(ACCOUNT,IMPRV_TYPE,DEPRECIATION_VALUE,QUALITY_DESCRIPTION,DATE_ERECTED,YR_REMODEL,BASE_AREA,
           PERIMETER,PROPERTY_NAME,UNITS,LEASE_RATE,OCCUPANCY,TOTAL_INCOME)
  
  Realbuildigs_Parcels <- tigris::geo_join(HCAD_parcels, HCAD_real_build, by_sp = "HCAD_NUM", by_df = "ACCOUNT", how="inner")
  
  # remove duplicates by creating building account numbers
  Realbuildigs_Parcels$ACCOUNT <- paste0(Realbuildigs_Parcels$ACCOUNT, "_", Realbuildigs_Parcels2$BUILDING_NUMBER)
  Realbuildigs_Parcels <- Realbuildigs_Parcels[!duplicated(Realbuildigs_Parcels$ACCOUNT), ]
  
  return(Realbuildigs_Parcels)
}



#add account number, remove duplicates, filter for building style codes TO DO: Move into getrealBuildings function
filterBuidingsHCAD <- function() {
  
  
  # From the buildings in the tract, subset for only the ones with living spaces
  potential_buildings=subset(validparceldataframe2,validparceldataframe2$"BUILDING_STYLE_CODE" %in% c("101","102", "103", "104", "107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988","105","8300","8352","8338","8459","8493","8546","8547","8596","8984","8987","8989", "660","8321","8324","8393","8424","8451","8589","8313","8322","8330","8335","8348","8394","8156","8551","8588","8710","8331","8309","8489","8311","8491","8514"))
  
  
}
