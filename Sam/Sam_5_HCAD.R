setwd("~/Documents/SyntheticDataSet")
source('Sam/get_tools.R')
library(stringr)
library(data.table)
library(sf)

vintage = "2020"

maindir = "~/University\ Of\ Houston/Engaged\ Data\ Science\ -\ Data/" #Dan Studio
#maindir = "~/Documents/Sam_data/" #if need local
censusdir = paste0(maindir,"Census/Cartographic\ Boundary\ Files/") 

state = "48"

parcels <- st_read(paste0(hcadDataDir, "Tax\ Parcels\ (Shapefiles)/Parcels.shp"))

#put in a helper file so it can be called
census_GIS_state_2020 <- function(censusdir,state){
    file_path <- paste0(censusdir,"2020/cb_2020_us_all_500k.gdb")
    #st_layers(file_path) # gives 34 layers, earlier versions have them separated differently
    us_block_groups <- st_read(dsn = file_path,layer = "cb_2020_us_bg_500k")
    state_block_groups <- us_block_groups[us_block_groups$STATEFP=="48",] #18626 bg in Texas
    state_block_groups <- st_transform(state_block_groups,crs = 3857) #matches weirdness of HCAD, should do others, too
    state_block_groups_DT <- as.data.table(state_block_groups)
    state_block_groups_DT[,("centroid"):=st_centroid(SHAPE)] 
    rm(state_block_groups)
    
    #zipcodes
    us_zcta <- st_read(dsn = file_path,layer = "cb_2020_us_zcta520_500k") #no state boundaries given
    us_zcta <- st_transform(us_zcta,crs = 3857)
    #put zip on each block_group by centroid (some may be a little off)
    blocks4zcta <- st_within(state_block_groups_DT[,centroid], us_zcta)
    blocks4zctaunlisted <- rapply(blocks4zcta,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4zctaunlisted <- unlist(blocks4zctaunlisted)
    state_block_groups_DT[,("zipcode"):=us_zcta$ZCTA5CE20[blocks4zctaunlisted]]
    rm(blocks4zcta)
    rm(blocks4zctaunlisted)
    rm(us_zcta)
    
    #voting districts
    us_vtd <- st_read(dsn = file_path,layer = "cb_2020_us_vtd_500k") 
    state_vtd <- us_vtd[us_vtd$STATEFP20=="48",]
    state_vtd <- st_transform(state_vtd,crs = 3857)
    #put voting district on each block_group by centroid (some may be a little off)
    blocks4vtd <- st_within(state_block_groups_DT[,centroid], state_vtd)
    blocks4vtdunlisted <- rapply(blocks4vtd,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4vtdunlisted <- unlist(blocks4vtdunlisted)
    state_block_groups_DT[,("voting_district"):=state_vtd$VTDST20[blocks4vtdunlisted]]
    rm(blocks4vtd)
    rm(blocks4vtdunlisted)
    rm(us_vtd)
    rm(state_vtd)
    
    #puma 
    us_puma <- st_read(dsn = file_path,layer = "cb_2020_us_puma20_500k")
    state_puma <- us_puma[us_puma$STATEFP20=="48",] #217 PUMA divisions in Texas
    state_puma <- st_transform(state_puma,crs = 3857)
    blocks4puma <- st_within(state_block_groups_DT[,centroid], state_puma)
    blocks4pumaunlisted <- rapply(blocks4puma,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4pumaunlisted <- unlist(blocks4pumaunlisted)
    state_block_groups_DT[,("PUMA_ID"):=state_puma$PUMACE20[blocks4pumaunlisted]]
    rm(blocks4puma)
    rm(blocks4pumaunlisted)
    rm(us_puma)
    rm(state_puma)
    
    #elementary school district
    #us_elsd <- st_read(dsn = file_path,layer = "cb_2020_us_elsd_500k")
    #state_elsd <- us_elsd[us_elsd$STATEFP=="48",] # only 1 in Texas !!
    #state_elsd <- st_transform(state_elsd,crs = 3857)
    #unified school district
    us_unsd <- st_read(dsn = file_path,layer = "cb_2020_us_unsd_500k")
    state_unsd <- us_unsd[us_unsd$STATEFP=="48",] # 1019 in Texas 
    state_unsd <- st_transform(state_unsd,crs = 3857)
    blocks4unsd <- st_within(state_block_groups_DT[,centroid], state_unsd)
    blocks4unsdunlisted <- rapply(blocks4unsd,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4unsdunlisted <- unlist(blocks4unsdunlisted)
    state_block_groups_DT[,("school_district"):=state_unsd$NAME[blocks4unsdunlisted]]
    rm(blocks4unsd)
    rm(blocks4unsdunlisted)
    rm(us_unsd)
    rm(state_unsd)
    
    #place names
    us_place <- st_read(dsn = file_path,layer = "cb_2020_us_place_500k")
    state_place <- us_place[us_place$STATEFP=="48",] #1860 place names in Texas
    state_place <- st_transform(state_place,crs = 3857)
    blocks4place <- st_within(state_block_groups_DT[,centroid], state_place)
    blocks4placeunlisted <- rapply(blocks4place,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4placeunlisted <- unlist(blocks4placeunlisted)
    state_block_groups_DT[,("place_name"):=state_place$NAME[blocks4placeunlisted]]
    rm(blocks4place)
    rm(blocks4placeunlisted)
    rm(us_place)
    rm(state_place)
    
    
    #lower state legislative district
    us_sldl <- st_read(dsn = file_path,layer = "cb_2020_us_sldl_500k")
    state_sldl <- us_sldl[us_sldl$STATEFP=="48",] #150 lower house districts in Texas
    state_sldl <- st_transform(state_sldl,crs = 3857)
    blocks4sldl <- st_within(state_block_groups_DT[,centroid], state_sldl)
    blocks4sldlunlisted <- rapply(blocks4sldl,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4sldlunlisted <- unlist(blocks4sldlunlisted)
    state_block_groups_DT[,("state_leg_lower"):=state_sldl$NAMELSAD[blocks4sldlunlisted]]
    rm(blocks4sldl)
    rm(blocks4sldlunlisted)
    rm(us_sldl)
    rm(state_sldl)
    
    #upper state legislative district
    us_sldu <- st_read(dsn = file_path,layer = "cb_2020_us_sldu_500k")
    state_sldu <- us_sldu[us_sldu$STATEFP=="48",] #75 upper house districts in Texas
    state_sldu <- st_transform(state_sldu,crs = 3857)
    blocks4sldu <- st_within(state_block_groups_DT[,centroid], state_sldu)
    blocks4slduunlisted <- rapply(blocks4sldu,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4slduunlisted <- unlist(blocks4slduunlisted)
    state_block_groups_DT[,("state_leg_upper"):=state_sldu$NAMELSAD[blocks4slduunlisted]]
    rm(blocks4sldu)
    rm(blocks4slduunlisted)
    rm(us_sldu)
    rm(state_sldu)
    
    
    #national congressional districts
    us_stateleg <- st_read(dsn = file_path,layer = "cb_2020_us_cd116_500k")
    state_leg <- us_stateleg[us_stateleg$STATEFP=="48",] #31 congressional districts in Texas
    state_leg <- st_transform(state_leg,crs = 3857)
    blocks4leg <- st_within(state_block_groups_DT[,centroid], state_leg)
    blocks4legunlisted <- rapply(blocks4leg,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
    blocks4legunlisted <- unlist(blocks4legunlisted)
    state_block_groups_DT[,("US_house_district"):=state_leg$NAMELSAD[blocks4legunlisted]]
    rm(blocks4leg)
    rm(blocks4legunlisted)
    rm(us_stateleg)
    rm(state_leg)
    
    if(!dir.exists(paste0(censusdir,"2020/",state,"/",state))){
      dir.create(paste0(censusdir,"2020/",state,"/",state))
      print("creating state folder in censusdir for 2020")}
    rds_path <- paste0(censusdir,"2020/",state,"/",state,"_block_group.RDS")
    if(file.exists(rds_path)){
      rm(rds_path)
    }
    saveRDS(rds_path)
    return(rds_path)
}

FIPS_vector <- c("201","157","167","039","071","291","339","473","061","215","427","489") # 12 counties around Houston
subsetting_census_by_county <- function(dt,county_vec){
  return(dt[COUNTYFP%in%county_vec])
}

census_12 <- subsetting_census_by_county(state_block_groups_DT,FIPS_vector)
#add more Houston HGAC and surrounding area data
#this can add to each block, like above
HoustonDataDir <- paste0(maindir,"HoustonCityData/")
#need to decide whether/how to attach it to the rest, with this call allowing any grouping
census_Houston_area <- function(HoustonDataDir,censusDT){
  #adding to the RDS from above, with same unlist, etc? 
  super_neighborhoods <- st_read(paste0(HoustonDataDir,"2026/Super_Neighborhoods"))
  super_neighborhoods <- st_transform(super_neighborhoods,crs = 3857)
  blocks4sn <- st_within(censusDT[,centroid], super_neighborhoods)
  blocks4snunlisted <- rapply(blocks4sn,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  blocks4snunlisted <- unlist(blocks4snunlisted)
  censusDT[,("Super_neighborhood"):=super_neighborhoods$SNBNAME[blocks4snunlisted]]
  rm(blocks4sn)
  rm(blocks4snunlisted)
  rm(super_neighborhoods)
  
  city_council_districts <- st_read(paste0(HoustonDataDir,"2026/City_Council_Districts"))
  super_neighborhoods <- st_transform(city_council_districts,crs = 3857)
  blocks4cc <- st_within(censusDT[,centroid], city_council_districts)
  blocks4ccunlisted <- rapply(blocks4cc,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  blocks4ccunlisted <- unlist(blocks4ccunlisted)
  censusDT[,("Houston_City_council_district"):=city_council_districts$DISTRICT[blocks4ccunlisted]]
  censusDT[,("Houston_City_council_rep"):=city_council_districts$MEMBER[blocks4ccunlisted]]
  rm(blocks4cc)
  rm(blocks4ccunlisted)
  rm(city_council_districts)
  return(censusDT)
}
county_blocks <- census_Houston_area(HoustonDataDir,census_12)

hcadDataDir = paste0(maindir,"HCAD/",vintage,"/")

make_HCAD_geom <- function(hcadDataDir,censusDT){
  # the Parcels.shp file can be downloaded from http://pdata.hcad.org/GIS/index.html
  parcels <- st_read(paste0(hcadDataDir, "Tax\ Parcels\ (Shapefiles)/Parcels.shp"))
  
  # remove invalid parcels
  parcels$valid <- st_is_valid(parcels)
  HCAD_parcels <- subset(parcels,parcels$valid)
  HCAD_parcels$valid <- NULL
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