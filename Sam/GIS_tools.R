#must already be subset by state
subsetting_census_by_county <- function(dt,county_vec){
  #put names in differently?
  return(dt[COUNTYFP%in%county_vec])
}

#annoying that they keep changing details in how they store and disseminate files
#should break into separate functions and make sure state works
census_GIS_state_2020 <- function(censusmapdir,state){
  file_path <- paste0(censusmapdir,"2020/cb_2020_us_all_500k.gdb")
  #st_layers(file_path) # gives 34 layers, earlier versions have them separated differently
  us_block_groups <- st_read(dsn = file_path,layer = "cb_2020_us_bg_500k")
  state_block_groups <- us_block_groups[us_block_groups$STATEFP==state,] #18626 bg in Texas
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
  state_vtd <- us_vtd[us_vtd$STATEFP20==state,]
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
  state_puma <- us_puma[us_puma$STATEFP20==state,] #217 PUMA divisions in Texas
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
  #state_elsd <- us_elsd[us_elsd$STATEFP==state,] # only 1 in Texas !!
  #state_elsd <- st_transform(state_elsd,crs = 3857)
  #unified school district
  us_unsd <- st_read(dsn = file_path,layer = "cb_2020_us_unsd_500k")
  state_unsd <- us_unsd[us_unsd$STATEFP==state,] # 1019 in Texas 
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
  state_place <- us_place[us_place$STATEFP==state,] #1860 place names in Texas
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
  state_sldl <- us_sldl[us_sldl$STATEFP==state,] #150 lower house districts in Texas
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
  state_sldu <- us_sldu[us_sldu$STATEFP==state,] #75 upper house districts in Texas
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
  state_leg <- us_stateleg[us_stateleg$STATEFP==state,] #31 congressional districts in Texas
  state_leg <- st_transform(state_leg,crs = 3857)
  blocks4leg <- st_within(state_block_groups_DT[,centroid], state_leg)
  blocks4legunlisted <- rapply(blocks4leg,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  blocks4legunlisted <- unlist(blocks4legunlisted)
  state_block_groups_DT[,("US_house_district"):=state_leg$NAMELSAD[blocks4legunlisted]]
  rm(blocks4leg)
  rm(blocks4legunlisted)
  rm(us_stateleg)
  rm(state_leg)
  
  #can this work off of valid census path?
  return(state_block_groups_DT)
}

#reported at tract level; statename is first letter capitalized ("Texas"); has census tract name if oz (could make t/f)
add_2019_opportunity_zones <- function(oz_dir,bg_DT,statename){
  #https://www.cdfifund.gov/opportunity-zones
  #opportunity zones (opportunity-zones=8764.-9-10-2019 should be updated second Trump term)
  file_path <- paste0(oz_dir,"opportunity-zones=8764.-9-10-2019")
  us_oz <- st_read(dsn = file_path)
  state_oz <- us_oz[us_oz$STATENAME==statename,] 
  state_oz <- st_transform(state_oz,crs = 3857)
  blocks4oz <- st_within(bg_DT[,centroid], state_oz)
  blocks4ozunlisted <- rapply(blocks4oz,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  blocks4ozunlisted <- unlist(blocks4ozunlisted)
  bg_DT[,("opportunity_zone_2019"):=state_oz$CENSUSTRAC[blocks4ozunlisted]] #comes out as census tract or empty
  rm(blocks4oz)
  rm(blocks4ozunlisted)
  rm(us_oz)
  rm(state_oz)
  return(bg_DT)
}

#make three calls, with an easy list unlist call repeated?
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
  city_council_districts <- st_transform(city_council_districts,crs = 3857)
  blocks4cc <- st_within(censusDT[,centroid], city_council_districts)
  blocks4ccunlisted <- rapply(blocks4cc,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  blocks4ccunlisted <- unlist(blocks4ccunlisted)
  censusDT[,("Houston_City_council_district"):=city_council_districts$DISTRICT[blocks4ccunlisted]]
  censusDT[,("Houston_City_council_rep"):=city_council_districts$MEMBER[blocks4ccunlisted]]
  rm(blocks4cc)
  rm(blocks4ccunlisted)
  rm(city_council_districts)
  
  #https://www.houstontx.gov/ecodev/tirz/23.html 
  TIRZ <- st_read(paste0(HoustonDataDir,"2026/COH_TIRZ"))
  TIRZ <- st_transform(TIRZ,crs = 3857)
  blocks4tz <- st_within(censusDT[,centroid], TIRZ)
  blocks4tzunlisted <- rapply(blocks4tz,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  blocks4tzunlisted <- unlist(blocks4tzunlisted)
  censusDT[,("Houston_City_TIRZ"):=TIRZ$NAME[blocks4tzunlisted]]
  rm(blocks4tz)
  rm(blocks4tzunlisted)
  rm(TIRZ)
  
  return(censusDT)
}

distribute_points_geom <- function(poly,num_pts,reg_random){
  #https://www.jla-data.net/eng/creating-and-pruning-random-points-and-polygons/
  #  st_sample(multipoly, size= , type=, exact=TRUE
  #            st_cast might help for embedded, but probably want to do a process that casts people separately, in any case
              
  #            https://r-spatial.github.io/sf/reference/st_sample.html
  return(coords)
}