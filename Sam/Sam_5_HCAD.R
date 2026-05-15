setwd("~/Documents/SyntheticDataSet")
source('Sam/get_tools.R') #for valid_file_paths
source('Sam/GIS_tools.R')
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


bg_RDS_path <- valid_file_path(censusmapdir,vintage,state,county = "*","official","500k","block_group","combined")

if(file.exists(bg_RDS_path)){
  bg_census_map <- readRDS(bg_RDS_path)
}else{
  bg_census_map <- census_GIS_state_2020(censusmapdir,state)
  bg_census_map <- add_2019_opportunity_zones(oz_dir,bg_census_map,"Texas")
  saveRDS(bg_census_map,bg_RDS_path)
}

FIPS_vector <- c("201","157","167","039","071","291","339","473","061","215","427","489") # 12 counties around Houston

census_12 <- subsetting_census_by_county(bg_census_map,FIPS_vector)
#add more Houston HGAC and surrounding area data
#this can add to each block, like above

#need to decide whether/how to attach it to the rest, with this call allowing any grouping



make_HCAD_geom <- function(hcadDataDir,censusDT){
  # the Parcels.shp file can be downloaded from http://pdata.hcad.org/GIS/index.html
  parcels <- st_read(paste0(hcadDataDir, "Tax\ Parcels\ (Shapefiles)/Parcels.shp"))
  print(paste0("Number of parcels from raw file: ",nrow(parcels)))
  # remove invalid parcels
  parcels$valid <- st_is_valid(parcels)
  HCAD_parcels <- subset(parcels,parcels$valid)
  HCAD_parcels$valid <- NULL
  print(paste0("Number of valid parcels files: ",nrow(HCAD_parcels),", which is ",nrow(parcels)-nrow(HCAD_parcels)," invalid parcel geometries"))
  rm(parcels)
  #add to blocks
  censusDT <- st_as_sf(censusDT)
  HCAD_parcels <- st_transform(HCAD_parcels,crs = st_crs(censusDT))
  CensusBGforHCADParcels <- st_within(HCAD_parcels, censusDT)
  CensusBGforHCADParcelsunlisted <- rapply(CensusBGforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
  CensusBGforHCADParcelsunlisted <- unlist(CensusBGforHCADParcelsunlisted)
  
  # add census tract information to each parcel
  HCAD_parcels$COUNTY=censusDT$COUNTY[CensusBGforHCADParcelsunlisted] #a handful have their centroid in one of 7 neighboring counties
  HCAD_parcels$GEOID=censusDT$GEOID[CensusBGforHCADParcelsunlisted]
  
  #cleanup
  rm(CensusBGforHCADParcels)
  rm(CensusBGforHCADParcelsunlisted)
  
  #check crs!!
  #HCAD_parcels <- st_as_sf(HCAD_parcels) #, crs = 3857
  #HCAD_geom <- st_transform(HCAD_geom, crs = 3857)
  #LOOK at bottom of HCAD_geo if there are others, or if we should add any others for the geometry 
  return(as.data.table(HCAD_parcels))
}

HCAD_join_res_build_geom <- function(hcadDataDir,HCAD_parcels){
  HCAD_res_build <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Building\ Information/building_res.txt"),
                              stringsAsFactors = FALSE,
                              sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  HCAD_res_build$acct <- str_remove_all(HCAD_res_build$acct, " ")
  #length(unique(HCAD_res_build$acct)) #1182396 of 1210552 - looked individually and seems like first row matters
  HCAD_res_build <- as.data.table(HCAD_res_build)
  HCAD_res_build <- unique(HCAD_res_build, by=("acct"))
  HCAD_res_build <- HCAD_res_build[,c("acct","impr_tp","structure_dscr","dpr_val","qa_cd","dscr","date_erected","yr_remodel","im_sq_ft","bld_adj")]
  
  HCAD_geom[HCAD_res_build,on='acct']
  return(HCAD_geom)
}

HCAD_join_real_build_geom <- function(hcadDataDir,HCAD){
  #there are no account numbers in common between res_build and real_build, but some codes are in both
  HCAD_real_build <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Building\ Information/building_other.txt"),
                               stringsAsFactors = FALSE, 
                               sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  #using actual_area for total_bldg_sf - get units from fixtures
  #total_income may include non-rental income?
  HCAD_real_build$acct <- str_remove_all(HCAD_real_build$acct, " ")
  HCAD_real_build <- as.data.table(HCAD_real_build)
  HCAD_real_build <- unique(HCAD_real_build, by=("acct"))
  HCAD_real_build <- HCAD_real_build[,c("acct","impr_tp","structure_dscr","Depr_val","qa_cd","dscr","date_erected","yr_remodel","im_sq_ft","prop_nm","units",
                                        "lease_rt","occ_rt","tot_inc")]
  HCAD[HCAD_real_build,on='acct']
  return(HCAD)
}

HCAD_join_real_acct_geom <- function(hcadDataDir,HCAD){
  HCAD_real_acct <- read.csv2(paste0(hcadDataDir, "Real\ Property\ Data/real_acct.txt"),
                              stringsAsFactors = FALSE,
                              sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  HCAD_real_acct$acct <- str_remove_all(HCAD_real_acct$acct, " ")
  HCAD_real_acct <- as.data.table(HCAD_real_acct)
  HCAD_real_acct <- unique(HCAD_real_acct, by=("acct"))
  #need to look at what we'd want to use
  #HCAD_real_acct <- HCAD_real_acct[,c("acct","impr_tp","structure_dscr","Depr_val","qa_cd","dscr","date_erected","yr_remodel","im_sq_ft","prop_nm","units",
  #                                      "lease_rt","occ_rt","tot_inc")]
  HCAD[HCAD_real_build,on='acct']
  return(HCAD)
}

HCAD_join_owner_date_geom <- function(hcadDataDir,HCAD){
  HCAD_owner_date <- read.csv2(paste0(hcadDataDir, "Real\ Property\ Data/deeds.txt"),
                               stringsAsFactors = FALSE,
                               sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  
  HCAD_owner_date$acct <- str_remove_all(HCAD_owner_date$acct, " ")
  HCAD_owner_date <- as.data.table(HCAD_owner_date)
  HCAD_owner_date <- unique(HCAD_owner_date, by=("acct"))
  
  #need to look at what we want to do
#  #remember to replace vintage with date, below
#  HCAD_owner_date <- HCAD_owner_date %>%
#    rename(account=V1,purchase_date=V2) %>%
#    distinct(account,.keep_all = TRUE) %>%
#    filter(!row_number()==1) %>%
#    select(account,purchase_date) #%>%
#  #  mutate(
#  #    yrs_in_house = 2022 - as.integer(substr(as.character.Date(HCAD_owner_date$V2),7,10))
#  #  )
#  HCAD <- full_join(HCAD,HCAD_owner_date,by="account")
  HCAD[HCAD_owner_date,on='acct']
  return(HCAD)
}

HCAD_join_exemptions_geom <- function(hcadDataDir,HCAD){
  HCAD_exempt <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Jurisdiction\ Information/jur_exempt_cd.txt"),
                           stringsAsFactors = FALSE,
                           sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  
  #exemptions give us some clues about owners, etc. that match up with ACS info
  #cf. jur_exemption_dscr.txt 
  #exempt_cat	exemption_dscr. #exemption_dscr is not given in same file; they seem to be using more as time goes on
  #ABT	Abatement
  #APD	Apportioned Partial Disability
  #APO	Apportioned Partial Over-65
  #APR	Apportioned Partial Residential
  #DIS	Disability
  #HIS	Historical
  #LIH	Low Income Housing
  #MCL	Methane Capture at Landfill
  #OVR	Over-65
  #PAR	Partial Residential Homestead
  #PDS	Partial Disability
  #PEX	Partial Total
  #POL	Pollution Control
  #POV	Partial Over-65
  #PRO	Prorated
  #RES	Residential Homestead
  #SOL	Solar
  #SPE	Self Provided Emergency Services (TOTAL exemption)
  #SPX	Spaceport Exemption (TOTAL exemption)
  #SSA	Surviving Spouse Active Duty
  #SSD	Surviving Spouse Disability
  #STT	Surviving Spouse Total Transfer
  #STX	Surviving Spouse Vet Disability Total Exemption
  #SUR	Surviving Spouse Over-65
  #TOT	Total
  #V11	Vet Disability #1 10-29 pct
  #V12	Vet Disability #1 30-49 pct
  #V13	Vet Disability #1 50-69 pct
  #V14	Vet Disability #1 70-100 pct
  #V21	Vet Disability #2 10-29 pct
  #V22	Vet Disability #2 30-49 pct
  #V23	Vet Disability #2 50-69 pct
  #V24	Vet Disability #2 70-100 pct
  #VCH	Vet Charitable Disability
  #VS1	Vet Survivor 10-29 pct
  #VS2	Vet Survivor 30-49 pct
  #VS3	Vet Survivor 50-69 pct
  #VS4	Vet Survivor 70-100 pct
  #VTX	Vet Disability Total Exemption
  
  HCAD_exempt$acct <- str_remove_all(HCAD_exempt$acct, " ")
  HCAD_exempt <- as.data.table(HCAD_exempt)
  #HCAD_exempt <- unique(HCAD_exempt, by=("acct")) #no duplicates in jur_exempt_cd.txt, but others do
  HCAD_exempt[,("exempt_type_homestead"):=fcase(str_detect(exempt_cat,'RES') | str_detect(exempt_cat,'PAR'),"homestead", default = "not homestead")]
  HCAD_exempt[,("exempt_type_Veteran"):=fcase(str_detect(exempt_cat,"V1") | str_detect(exempt_cat,"V2"),"Veteran",default = "not veteran")]
  HCAD_exempt[,("exempt_type_over_65"):=fcase(str_detect(exempt_cat,"APO") | str_detect(exempt_cat,"OVR") |
                                                str_detect(exempt_cat,"POV") | str_detect(exempt_cat,"SUR"),"Over 65",default = "not over 65")] #starts using more over time
  HCAD_exempt[,("exempt_type_surviving_spouse"):=fcase(str_detect(exempt_cat,"SSD") | str_detect(exempt_cat,"STT") |
                                                str_detect(exempt_cat,"STX") | str_detect(exempt_cat,"SUR") |
                                                  str_detect(exempt_cat,"VS") | str_detect(exempt_cat,"VTX"),"Surviving Spouse",default = "Not surviving spouse")] #starts using more over time
  HCAD_exempt[,("exempt_type_disabled"):=fcase(str_detect(exempt_cat,"PDS") | str_detect(exempt_cat,"DIS") |
                                                         str_detect(exempt_cat,"STX") | str_detect(exempt_cat,"APD") |
                                                         str_detect(exempt_cat,"VCH") | str_detect(exempt_cat,"VTX"),"Disabled",default = "Not disabled")] #starts using more over time
  HCAD_exempt[,("exempt_type_low_income_housing"):=fcase(str_detect(exempt_cat,"LIH"),"Low income housing", default = "not low income")]
  HCAD_exempt[,("exempt_type_abatement"):=fcase(str_detect(exempt_cat,"ABT"),"Abatement", default = "Not abatement")]
  HCAD_exempt[,("exempt_type_historical"):=fcase(str_detect(exempt_cat,"HIS"),"Historical", default = "Not historical")]
  HCAD_exempt[,("exempt_type_solar"):=fcase(str_detect(exempt_cat,"SOL"),"Solar", default = "Not solar")]
  HCAD_exempt[,("exempt_type_total"):=fcase(str_detect(exempt_cat,"TOT"),"Total", default = "Not total")] 
  
  
 # HCAD_exempt <- HCAD_exempt %>% 
 #   rename(account=V1,exempt_cat=V2) %>% 
 #   distinct(account,.keep_all = TRUE) %>%
 #   filter(!row_number()==1) %>%
 #   select(account,exempt_cat) %>% 
 #   mutate(
 #     homestead = case_when(  #trying to get at who owns...
 #       exempt_cat %in% c('RES','PAR') ~ TRUE
 #     ),
 #     vet = case_when(
 #       exempt_cat %in% c('V11','V12','V13','V14','V21','V22','V23','V24') ~ TRUE
 #     ),
 #     low_income_housing = case_when(
 #       exempt_cat == "LIH" ~ TRUE
 #     ),
 #     over_65 = case_when(
 #       exempt_cat %in% c('APO','OVR','POV','SUR') ~ TRUE
 #     ),
 #     surv_spouse = case_when(
 #       exempt_cat %in% c('SSD','STT','STX','SUR','VS1','VS2','VS3','VS4','VTX') ~ TRUE
 #     ),
 #     disabled = case_when(
 #       exempt_cat %in% c('PDS','DIS','APD','STX','V11','V12','V13','V14','V21','V22','V23','V24','VCH','VTX') ~ TRUE
 #     )
 #   )
  HCAD <- full_join(HCAD,HCAD_exempt,by="account")
  return(HCAD)
}

HCAD_join_fixtures_geom <- function(hcadDataDir,HCAD){
  HCAD_fixtures <- read.csv2(paste0(hcadDataDir, "Real\ Property\ -\ Building\ Information/fixtures.txt"),
                             stringsAsFactors = FALSE,
                             sep = "\t", header = TRUE, quote="",colClasses = c("acct"="character"))
  #for bldg_data codes: http://pdata.hcad.org/Desc/2015/code_desc_real.txt #updated: https://pdata.hcad.org/data/cama/2015/code_desc_real.txt
  HCAD_fixtures$acct <- str_remove_all(HCAD_fixtures$acct, " ")
  HCAD_fixtures <- as.data.table(HCAD_fixtures)

  #HCAD only puts bedrooms on first bld_num for apts for some reason; random ones are separated...  
  #need to do some testing! - ugh!!!
  HCAD_fixtures[,("bedrooms"):=fcase(str_detect(type,"RMB"),as.integer(units),default = as.integer(0))] #bedrooms in residence, although can be multiple buildings on plat
  HCAD_fixtures[,"efficiency_apts":=fcase(str_detect(type,"AP0"),as.integer(units)+as.integer(bedrooms),default = as.integer(0))]
  HCAD_fixtures[,"1_bedroom_apts":=fcase(str_detect(type,"AP1"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP1"),as.integer(units)+as.integer(bedrooms),default = bedrooms)]
  HCAD_fixtures[,"2_bedroom_apts":=fcase(str_detect(type,"AP2"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP2"),(as.integer(units)+as.integer(bedrooms))*as.integer(2),default = bedrooms)]
  HCAD_fixtures[,"3_bedroom_apts":=fcase(str_detect(type,"AP3"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP3"),(as.integer(units)+as.integer(bedrooms))*as.integer(3),default = bedrooms)]
  HCAD_fixtures[,"4_bedroom_apts":=fcase(str_detect(type,"AP4"),as.integer(units),default = as.integer(0))]
  HCAD_fixtures[,"bedrooms":=fcase(str_detect(type,"AP4"),(as.integer(units)+as.integer(bedrooms))*as.integer(4),default = bedrooms)]
  HCAD_fixtures[,total_bedrooms_bldg := sum(bedrooms),by = c("acct","bld_num")] #keeping bld_num for geo, and then summing for other calculations; they just use whatever bld_num the apt uses, not a count
  HCAD_fixtures[,test_total_beds := sum(bedrooms),by=(acct)]
  
  
  HCAD_fixtures <- unique(HCAD_fixtures,by = c("acct","bld_num"))
  HCAD_fixtures[,total_bedrooms := sum(bedrooms),by = (acct)]
  
  
  
  #need to total on bedrooms for each acct
  
  
  #add counts for rooms and apartments
  HCAD_fixtures <- HCAD_fixtures %>%
    rename(account=V1,bldg_num=V2,bldg_data (type)=V3,
           type_descr=V4,units=V5) %>%
    distinct(account,.keep_all = TRUE) %>%
    filter(!row_number()==1) %>%
    select(account,bldg_num,bldg_data,bldg_data_descr,num_units) %>%
    mutate(
      bedrooms = case_when(
        bldg_data == 'RMB ' ~ as.integer(num_units),
        bldg_data == 'AP0 ' ~ as.integer(0),
        bldg_data == 'AP1 ' ~ as.integer(1),
        bldg_data == 'AP2 ' ~ as.integer(2),
        bldg_data == 'AP3 ' ~ as.integer(3),
        bldg_data == 'AP4 ' ~ as.integer(4)
      ),
      total_rooms = case_when(
        bldg_data == 'RMT ' ~ as.integer(num_units)
      ),
      effcncy_apts = case_when(
        bldg_data == 'AP0 ' ~ as.integer(num_units)
      ),
      oneBR_apts = case_when(
        bldg_data == 'AP1 ' ~ as.integer(num_units)
      ),
      twoBR_apts = case_when(
        bldg_data == 'AP2 ' ~ as.integer(num_units)
      ),
      threeBR_apts = case_when(
        bldg_data == 'AP3 ' ~ as.integer(num_units)
      ),
      fourBR_apts = case_when(
        bldg_data == 'AP4 ' ~ as.integer(num_units),
        TRUE ~ as.integer(0)
      ),
      num_apts = case_when(  #maybe not use - seems to have a lot of missing!
        grepl('AP', bldg_data) ~ as.integer(num_units),
        #bldg_data == 'UNT ' ~ as.integer(num_units), #need to confirm it adds correctly
        TRUE ~ as.integer(0)
      )
    ) %>%
    filter(bedrooms>=0 | total_rooms>0 | effcncy_apts>0 | oneBR_apts>0 | 
             twoBR_apts>0 | threeBR_apts>0 | fourBR_apts>0 | num_apts>0 ) 
  
  #expand so each apt has its own record
  HCAD_fixtures <- data.frame(HCAD_fixtures[rep(seq_len(dim(HCAD_fixtures)[1]), 
                                                as.integer(HCAD_fixtures$num_apts)), 
                                            #as.integer(HCAD_fixtures$num_apts*.89)), #in case want to do occupancy this way
                                            1:dim(HCAD_fixtures)[2], drop = FALSE], row.names = NULL)
  #.89 is the overall Harris County occupancy - I have it in another place, too
  #add apt_ids
  HCAD_fixtures <- HCAD_fixtures %>%
    group_by(account) %>%
    mutate(
      apt_id = paste0(account,rep(1:n()))
    )
  HCAD <- full_join(HCAD,HCAD_fixtures,by="account")
  return(HCAD)
}
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