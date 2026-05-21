setwd("~/Documents/SyntheticDataSet")
source('Sam/get_tools.R') #for valid_file_paths
source('Sam/GIS_tools.R') #for census_GIS_state_2020
source('Sam/HCAD_tools.R') #for making HCAD
library(stringr)
library(data.table)
library(sf)

maindir = "~/University\ Of\ Houston/Engaged\ Data\ Science\ -\ Data/" #Dan Studio
#maindir = "~/Documents/Sam_data/" #if need local
censusmapdir = paste0(maindir,"Census/Cartographic\ Boundary\ Files/") 
oz_dir = paste0(maindir,"OpportunityZones/") #add_2019_opportunity_zones <- function(oz_dir,bg_DT,statename)
state = "48"
vintage = "2025"
hcadDataDir = paste0(maindir,"HCAD/",vintage,"/")
HoustonDataDir <- paste0(maindir,"HoustonCityData/")

#get census geography file - may rejoin with HCAD and at different levels, depending on use case.
bg_RDS_path <- valid_file_path(censusmapdir,vintage,state,county = "*","official","500k","block_group","combined")

if(file.exists(bg_RDS_path)){
  bg_census_map <- readRDS(bg_RDS_path)
  print(paste("Previous file read from: ",bg_RDS_path))
}else{
  bg_census_map <- census_GIS_state_2020(censusmapdir,state)
  bg_census_map <- add_2019_opportunity_zones(oz_dir,bg_census_map,"Texas")
  saveRDS(bg_census_map,bg_RDS_path)
  print(paste("New file saved to: ",bg_RDS_path))
}

HCAD_RDS_path <- paste0(hcadDataDir,"HCAD.RDS")
if(file.exists(HCAD_RDS_path)){
  HCAD <- readRDS(HCAD_RDS_path)
  print(paste("Previous file read from: ",HCAD_RDS_path))
}else{
  HCAD <- make_HCAD_geom(hcadDataDir,bg_census_map) #return parcels; have to rejoin with bg_census_map, if showing those boundaries
  HCAD <- HCAD_join_res_build_geom(hcadDataDir,HCAD) #add residential building information
  HCAD <- HCAD_join_real_build_geom(hcadDataDir,HCAD) #add businesses, including apartments
  HCAD <- HCAD_join_real_acct_geom(hcadDataDir,HCAD) #add some of the acct data on real property; have to decide what we need
  HCAD <- HCAD_join_owner_date_geom(hcadDataDir,HCAD)
  HCAD <- HCAD_join_exemptions_geom(hcadDataDir,HCAD)
  HCAD <- HCAD_join_fixtures_geom(hcadDataDir,HCAD)
  saveRDS(HCAD,HCAD_RDS_path)
  print(paste("New file saved to: ",HCAD_RDS_path))
}
#need to decide whether/how to attach it to the rest, with this call allowing any grouping

#FIPS_vector <- c("201","157","167","039","071","291","339","473","061","215","427","489") # 12 counties around Houston

#census_12 <- subsetting_census_by_county(bg_census_map,FIPS_vector)
#add more Houston HGAC and surrounding area data
#this can add to each block, like above


#have to clean up better if making into a function - parcels is ok...



#other stuff to think about!!
  #HAVE TO GET FOLDER NAMES RIGHT
  
  
  
  
  

  #joins, etc.
  #for res, they use effective area (V24) for calculating improvement value
  #they have a nbhd_factor that asthma folks could use, but it's not there for apts.

  

  

  

  

  

  #check this!!!! - what are the number of accounts, and how many have unique apt_id!!!!!
  #think about how to add geometry for each person to be separate in representations.
  
  #http://pdata.hcad.org/Desc/2015/code_desc_real.txt
  #in bldg_data -                                                                                                                                                           
  #AP0                 Apt:  Effcncy Unit                                                                                                                                                                  
  #AP1                 Apt:  1-Bedroom Unit                                                                                                                                                                
  #AP2                 Apt:  2-Bedroom Unit                                                                                                                                                                
  #AP3                 Apt:  3-Bedroom Unit                                                                                                                                                                
  #AP4                 Apt:  4-Bedroom Unit 
  #RMB                 Room:  Bedroom                                                                                                                                                                      
  #RMT                 Room:  Total
  #UNT                 Number of Apartment Units 
  
  #https://hcad.org/hcad-resources/hcad-appraisal-codes/hcad-building-type-codes/
  #perhaps still need to add logic for: (some come from improv_typ.x, some from improv_typ.y - res vs. real)
  #NOT run - just in case we want to drill down
  fourplex4 <- HCAD %>% filter(improv_typ.x == 1004) #looks like each account has it listed as 8 bedrooms, instead of as 4 apts.
  #1002 = duplex, 1003 = triplex, with same problems.
  
  #if we did NADcentroids on parcels earlier
  #st_geometry(HCAD) <- NULL
  #HCAD <- HCAD %>%
  #  select(-NADcentroids)
  HCAD <- as.data.table(HCAD)
  censusblocks <- as.data.table(censusblocks) #update its indices
  censusblocks[,("geoid"):=GEOID]
  HCAD <- as.data.table(HCAD)
  censusblocks <- as.data.table(censusblocks)
  #censusblocks[,("geoid"):=GEOID]
  HCAD <- unique(HCAD,by=c("account","apt_id")) #check - this eliminated the expansion by apartment type - 
  HCAD[geoid%in%unique(censusblocks$geoid),c("closest_food_distance","closest_food_account",
                                             "fast_food_distance_min","fast_food_account",
                                             "convenience_stores_distance","convenience_stores_account",
                                             "shop_center_distance","shop_center_account",
                                             "super_market_distance","super_market_account",
                                             "metro_stop_distance_min","metro_stop_BSID"):=
         censusblocks[.SD,c(list(food_source_distance_min),list(closest_food_account),
                            list(fast_food_distance_min),list(fast_food_account),
                            list(convenience_stores_distance),list(convenience_stores_account),
                            list(shop_center_distance),list(shop_center_account),
                            list(super_market_distance),list(super_market_account),
                            list(metro_stop_distance_min),list(metro_stop_BSID)),
                      on=("geoid")]]
  
  HCAD_residences <- HCAD %>%
    filter(is.na(improv_typ_real)) #there are a significant number that are na on improv_typ, still, but chose to keep them for wholeness here
  #cf: they seem to be things like commercial land in residential areas, some church properties, and other odds and ends
  HCAD_businesses <- HCAD %>%
    filter(!is.na(improv_typ_real))
  
  #censustracts[,("min_bus_distance_by_pop"):=]
  
  #for busstops - more than 24 hours!!
  #metro_stops <- readRDS(paste0(houstondatadir,"2022/metro_stops/busstops_tracts.RDS"))
  #metro_stops <- st_transform(metro_stops,st_crs(HCAD))
  #metro_stops_1k <- st_is_within_distance(HCAD$centroid,metro_stops$geometry,dist = 1000)
  
  #metro_stops_distance <- st_distance(HCAD$centroid,metro_stops1$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
  #metro_distance <- as.data.table(metro_stops_distance)
  #HCAD$min_h_distance <- apply(h_distance[,1:90], 1, min)
  
  #for crime - picked up 7/21/2022 - https://www.houstontx.gov/police/cs/crime-stats-archives.htm
  
  saveRDS(HCAD,file = paste0(housingdir, vintage, "/HCAD_pts_",Sys.Date(),".RDS"))
  saveRDS(HCAD_residences,file = paste0(housingdir, vintage, "/HCAD_residences_",Sys.Date(),".RDS"))
  saveRDS(HCAD_businesses,file = paste0(housingdir, vintage, "/HCAD_businesses_",Sys.Date(),".RDS"))
  
  #HCAD as a whole has all the spatial info we may need for some of the SDoH calculations 
  
  #depends on where pulling it from
  HCAD_dt <- as.data.table(HCAD_residences)
  HCAD_dt[,tract:=droplevels(tract)]
  HCAD_dt <- HCAD_dt[!is.na(improv_typ)] #returns 1815741
  HCAD_res <- HCAD_dt[!is.na(LocAddr)]
  HCAD_res <- HCAD_res[!duplicated(account) | !improv_typ=="1001"] #not sure how that happened - should be single_family
  
  HCADbus_dt <- as.data.table(HCAD_businesses)
  HCADbus_dt[,tract:=droplevels(tract)]
  HCADbus_dt <- HCADbus_dt[!is.na(improv_typ_real)]
  HCAD_bus <- HCADbus_dt[!duplicated(account)]
  HCAD_bus <- HCAD_bus[!is.na(LocAddr)]
  HCAD_bus <- HCAD_bus[!is.na(account)]
  
  #clean up HCAD_bus to have right number of units
  #https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
  #are these all owned vs. rented?
  HCAD_bus[improv_typ_real=="1001",apt_units:=1] #Residential Single Family - not sure how different from res, but not in both
  HCAD_bus[improv_typ_real=="1002",apt_units:=2] #Residential Duplex
  HCAD_bus[improv_typ_real=="1005",apt_units:=1] #Mixed use - only five, look to be single family in business
  HCAD_bus[improv_typ_real=="1008",apt_units:=1] #Mobile home 
  #only adds 96 units above
  HCAD_bus[improv_typ_real=="4108",apt_units:=1] #Commercial Mobile home
  HCAD_bus[improv_typ_real=="4209",apt_units:=as.numeric(units)] #Apartment Struct. 4-20 Units
  HCAD_bus[improv_typ_real=="4211",apt_units:=as.numeric(units)] #Apartment Garden (1 to 3 Stories) - doesn't match up with number generated from AP2, etc.
  HCAD_bus[improv_typ_real=="4212",apt_units:=as.numeric(units)] #Apartment Mid Rise (4 to 12 Stories)
  HCAD_bus[improv_typ_real=="4213" & !is.na(as.numeric(units)),apt_units:=as.numeric(units)] #Mobile Home Park
  HCAD_bus[improv_typ_real=="4213" & is.na(as.numeric(units)),apt_units:=4] #Mobile Home Park
  HCAD_bus[improv_typ_real=="4214",apt_units:=as.numeric(units)] #Apartment High Rise (13+ Stories)
  HCAD_bus[improv_typ_real=="4221",apt_units:=as.numeric(units)] #Subsidized Housing
  HCAD_bus[improv_typ_real=="4222",apt_units:=as.numeric(units)] #Apartment - Tax Credit
  HCAD_bus[improv_typ_real=="4299",apt_units:=as.numeric(units)] #Apartment Structure
  #HCAD_bus[improv_typ_real=="4313",apt_units:=as.numeric(units)] #Dormitories - can't get numbers from units
  HCAD_bus[improv_typ_real=="4316" & !is.na(as.numeric(units)),apt_units:=as.numeric(units)] #Nursing Home - some didn't have units
  HCAD_bus[improv_typ_real=="4316" & is.na(as.numeric(units)),apt_units:=40] #nursing Home
  HCAD_bus[improv_typ_real=="4317",apt_units:=as.numeric(units)] #Retirement Home
  HCAD_bus[improv_typ_real=="4318",apt_units:=as.numeric(units)] #Boarding & Rooming House
  HCAD_bus[is.na(total_income),apt_units:=as.numeric(0)]
  #HCAD_bus[improv_typ_real=="4319" & !is.na(as.numeric(units)),apt_units:=as.numeric(units)] #Commercial Bldg. - Mixed Res.
  #HCAD_bus[improv_typ_real=="4319" & is.na(as.numeric(units)),apt_units:=40] #Commercial Bldg. - Mixed Res.
  #supermarkets are 4347
  #colleges and universities (4613): 1243320010001 is TSU; 0410070130026 is UH; 1318230000001 is Rice; 
  HCAD_bus[account=="1243320010001",apt_units:=2500] #TSU - frosh dorms only?
  HCAD_bus[account=="0410070130026",apt_units:=8500] #UH - guess, since I think some are listed differently
  HCAD_bus[account=="0410070130026",apt_units:=3000] #Rice
  #correctional facilities are improv_typ_real 4670
  #for adding correctional facilities - in tract 210100 add account 0031220000001 from HCAD_bus;  The seven floor 701 Jail has 4,144 inmate beds, and a few others, it is Harris County Correctional Facility, 701 Jail
  #in tract 100000 add account 0010120000010' The facility, which has 4,156 regular beds, 124 beds for the Medical Division, and 96 beds for MHMRA, is one American football field deep and two American football fields in length. It is Harris County Correctional Facility, 1200 Jail
  HCAD_bus[account=="0031220000001",apt_units:=4500]
  HCAD_bus[account=="0010120000010",apt_units:=4200]
  
  #get each apt_units a new row
  HCAD_apts <- HCAD_bus[!is.na(apt_units) & apt_units!=0]
  HCAD_apts <- HCAD_apts %>% uncount(apt_units,.id = "apt_number_id",.remove = TRUE)
  #HCAD_homes <- rbind(HCAD_apts,HCAD_res,fill=TRUE) #choking on ptcoords!!!
  HCAD_homes <- bind_rows(HCAD_res,HCAD_apts) #Vectorizing 'sfc_POINT' elements may not preserve their attributes!  might have to do each?? looks right, but hard to be sure
  
  #fix up a few things
  HCAD_homes[,qual_descript:=if_else(is.na(qual_descript.x),qual_descript.y,qual_descript.x)]
  HCAD_homes[,yr_remodel:=if_else(is.na(yr_remodel.x),yr_remodel.y,yr_remodel.x)]
  HCAD_homes[,nbhd_factor:=if_else(is.na(nbhd_factor.x),as.numeric(nbhd_factor.y),as.numeric(nbhd_factor.x))] #or as.numeric?
  HCAD_homes <- HCAD_homes[!is.na(tract)]
  HCAD_homes[,building_val:=if_else(is.na(bldg_value),as.numeric(bldg_value_real),as.numeric(bldg_value))]
  HCAD_homes <- HCAD_homes %>% 
    select(-qual_descript.x,-qual_descript.y,-yr_remodel.x,-yr_remodel.y,-nbhd_factor.x,-nbhd_factor.y,
           -lease_rate,-occupancy_rate,-bldg_value,-bldg_value_real,valid)
  
  HCAD_homes[is.na(improv_typ),cost_per_sf:=tot_bldg_sf/improv_value]
  HCAD_homes[!is.na(improv_typ),cost_per_sf:=living_sf/improv_value] #relative value - could sort from high to low and give id (subtracting 10%?)
  HCAD_homes[is.na(improv_typ),avg_sf:=tot_bldg_sf/as.integer(units)] #size of living space to assign to
  HCAD_homes[!is.na(improv_typ),avg_sf:=living_sf] #just to complete the column for future
  HCAD_homes[,type_code:=if_else(is.na(improv_typ),improv_typ_real,improv_typ)] #also need to make one improv_typ
  
  return(HCAD)
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