setwd("~/Documents/SyntheticDataSet")
source('Sam/get_tools.R') #for valid_file_paths
source('Sam/GIS_tools.R')
library(stringr)
library(data.table)
library(sf)

maindir = "~/University\ Of\ Houston/Engaged\ Data\ Science\ -\ Data/" #Dan Studio
#maindir = "~/Documents/Sam_data/" #if need local
censusmapdir = paste0(maindir,"Census/Cartographic\ Boundary\ Files/") 
state = "48"
vintage = "2020"

bg_RDS <- valid_file_path(censusmapdir,vintage,state,county = "*","official","500k","block_group","combined")

if(file.exists(bg_RDS)){
  bg_census_map <- readRDS(bg_RDS)
}else{
  bg_census_map <- census_GIS_state_2020(census_GIS_state_2020)
  saveRDS(bg_census_map,bg_RDS)
}





FIPS_vector <- c("201","157","167","039","071","291","339","473","061","215","427","489") # 12 counties around Houston
subsetting_census_by_county <- function(dt,county_vec){
  #put names in differently?
  return(dt[COUNTYFP%in%county_vec])
}

census_12 <- subsetting_census_by_county(state_block_groups_DT,FIPS_vector)
#add more Houston HGAC and surrounding area data
#this can add to each block, like above
HoustonDataDir <- paste0(maindir,"HoustonCityData/")
#need to decide whether/how to attach it to the rest, with this call allowing any grouping

county_blocks <- census_Houston_area(HoustonDataDir,census_12)

hcadDataDir = paste0(maindir,"HCAD/",vintage,"/")

make_HCAD_geom <- function(hcadDataDir,censusDT){
  # the Parcels.shp file can be downloaded from http://pdata.hcad.org/GIS/index.html
  parcels <- st_read(paste0(hcadDataDir, "Tax\ Parcels\ (Shapefiles)/Parcels.shp"))
  print(paste0("Number of parcels from raw file: ",nrow(parcels)))
  # remove invalid parcels
  parcels$valid <- st_is_valid(parcels)
  HCAD_parcels <- subset(parcels,parcels$valid)
  HCAD_parcels$valid <- NULL
  print(paste0("Number of valid parcels files: ",nrow(HCAD_parcels),", which is ",nrow(parcels)-nrow(HCAD_parcels)," invalid"))
  rm(parcels)
  #add to blocks
  CensusBGforHCADParcels <- st_within(HCAD_parcels, censusDT)
  CensusBGforHCADParcelsunlisted <- rapply(CensusBGforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  CensusBGforHCADParcelsunlisted <- unlist(CensusBGforHCADParcelsunlisted)
  
  # add census tract information to each parcel
  HCAD_parcels$COUNTY=censusDT$COUNTY[CensusBGforHCADParcelsunlisted] #check if some are not HARRIS?
  HCAD_parcels$GEOID=censusDT$GEOID[CensusBGforHCADParcelsunlisted]
  
  #cleanup
  rm(CensusBGforHCADParcels)
  rm(CensusBGforHCADParcelsunlisted)
  
  #check crs!!
  HCAD_parcels <- st_as_sf(HCAD_parcels) #, crs = 3857
  #HCAD_geom <- st_transform(HCAD_geom, crs = 3857)
  #LOOK at bottom of HCAD_geo if there are others, or if we should add any others for the geometry 
  return(HCAD_parcels)
}

#have to clean up better if making into a function - parcels is ok...
make_HCAD_data <- function(hcadDataDir){
  #HAVE TO GET FOLDER NAMES RIGHT
  HCAD_res_build <- read.csv2(paste0(hcadDataDir, "/Real_building_land/building_res.txt"),
                              stringsAsFactors = FALSE,
                              sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
  HCAD_real_build <- read.csv2(paste0(hcadDataDir, "/Real_building_land/building_other.txt"),
                               stringsAsFactors = FALSE,
                               sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
  #there are no account numbers in common between res_build and real_build, but some codes are in both
  
  HCAD_real_acct <- read.csv2(paste0(hcadDataDir, "/Real_acct_owner/real_acct.txt"),
                              stringsAsFactors = FALSE,
                              sep = "\t", header = TRUE, quote="",colClasses = c("V1"="character"))
  HCAD_exempt <- read.csv2(paste0(hcadDataDir, "/Real_jur_exempt/jur_exempt_cd.txt"),
                           stringsAsFactors = FALSE,
                           sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
  HCAD_owner_date <- read.csv2(paste0(housingdir, vintage, "/Real_acct_owner/deeds.txt"),
                               stringsAsFactors = FALSE,
                               sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
  HCAD_fixtures <- read.csv2(paste0(housingdir, vintage, "/Real_building_land/fixtures.txt"),
                             stringsAsFactors = FALSE,
                             sep = "\t", header = FALSE, quote="",colClasses = c("V1"="character"))
  
  #rest of HCAD_merge.R
}


#HCAD_parcels <- st_read(paste0(housingdir, vintage, "/Parcels/Parcels.shp"),stringsAsFactors = FALSE)
##for 2022, had 9 unexpected geometry messages
##or
#HCAD_parcels <- st_read(paste0(housingdir, vintage, "/Parcels_", vintage, "_Oct/Parcels.shp"),stringsAsFactors = FALSE)
##then rename
#HCAD_parcels <- HCAD_parcels %>%
#  rename(account=HCAD_NUM) %>%
#  select(account,LocAddr,city,zip,geometry)
#HCAD_parcels_dt <- as.data.table(HCAD_parcels)
#HCAD_parcels_clean <- unique(HCAD_parcels_dt, by = ('account'))
#HCAD_parcels_clean[,("valid"):=st_is_valid(geometry)]
#HCAD_parcels_valid <- HCAD_parcels_clean[valid==TRUE]
#HCAD_parcels_invalid <- HCAD_parcels_clean[valid==FALSE] #to check
#HCAD_geom <- st_as_sf(HCAD_parcels_valid)
#HCAD_geom <- st_transform(HCAD_geom, crs = 3857)
##remove extras
#rm(HCAD_parcels)
#rm(HCAD_parcels_dt)
#rm(HCAD_parcels_clean)

#get centroids - about 1% of the parcels cross tracts, so need it as a point and not
#HCAD_geom$centroid <- st_centroid(HCAD_geom$geometry)

#add Harvey flood data to each house
#for Harvey flooding
#only one Harvey_layers <- st_layers("~/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/Harvey\ Flooding/FEMA_Damage_Assessments_Harvey_Public_08_30.gdb/")
#Harvey <- st_read("~/OneDrive\ -\ University\ Of\ Houston/Social\ Network\ Hypergraphs/Harvey\ Flooding/FEMA_Damage_Assessments_Harvey_Public_08_30.gdb/")

#file_path <- valid_file_path(censusdir,vintage,state,county="*",api_type="dec",geo_type="block_group",
#                             groupname="bg_SARE",path_suff="wrk")
##"~/University Of Houston/Engaged Data Science - Data/Census/2020/state_48/2020_48_dec_block_group_bg_hhSARETT_wrk.RDS"
#if(file.exists(file_path)){file.remove(file_path)}
#saveRDS(bg_SARE_reduced,file_path)

#plan 2015, 2020, and 2025 HCAD