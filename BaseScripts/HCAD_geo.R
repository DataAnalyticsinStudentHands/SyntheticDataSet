library(sf)
library(data.table)

#put in check for HCAD_parcels_valid and readRDS it in?
#put in check for censustracts from geojson_out.R

#saveRDS(HCAD_parcels,file = paste0(censusdir, vintage, "/temp/HCAD_parcels_valid.RDS"))
#HCAD_parcels <- readRDS(paste0(censusdir, vintage, "/temp/HCAD_parcels_valid.RDS"))

geo_vintage <- "2021"

censustracts <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", 
                               geo_vintage, "_", state, "_tract_500k/cb_", geo_vintage, "_", state, "_tract_500k.shp"))
censustracts <- st_transform(censustracts,crs = 3857) 
#start with loading files we need from HCAD
#most recent parcels doesn't have the _Oct
HCAD_parcels <- st_read(paste0(housingdir, vintage, "/Parcels/Parcels.shp"),stringsAsFactors = FALSE)
#for 2022, had 9 unexpected geometry messages
#or
HCAD_parcels <- st_read(paste0(housingdir, vintage, "/Parcels_", vintage, "_Oct/Parcels.shp"),stringsAsFactors = FALSE)
#then rename
HCAD_parcels <- HCAD_parcels %>%
  rename(account=HCAD_NUM) %>%
  select(account,LocAddr,city,zip,geometry)
HCAD_parcels_dt <- as.data.table(HCAD_parcels)
HCAD_parcels_clean <- unique(HCAD_parcels_dt, by = ('account'))
HCAD_parcels_clean[,("valid"):=st_is_valid(geometry)]
HCAD_parcels_valid <- HCAD_parcels_clean[valid==TRUE]
HCAD_parcels_invalid <- HCAD_parcels_clean[valid==FALSE] #to check
HCAD_geom <- st_as_sf(HCAD_parcels_valid)
HCAD_geom <- st_transform(HCAD_geom, crs = 3857)
#remove extras
rm(HCAD_parcels)
rm(HCAD_parcels_dt)
rm(HCAD_parcels_clean)

#get centroids - about 1% of the parcels cross tracts, so need it as a point and not
HCAD_geom$centroid <- st_centroid(HCAD_geom$geometry)

#do on blocks, then just make tract id from geoid for blocks
censusblocks <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_bg_500k/cb_", geo_vintage, "_", state, "_bg_500k.shp"))
censusblocks <- st_transform(censusblocks,st_crs(HCAD_geom))
censusblocks$geoid_centroid <- st_centroid(censusblocks$geometry)
CensusBlockforHCADParcels <- st_within(HCAD_geom$centroid, censusblocks)
#unlist into vector
CensusBlockforHCADParcelsunlisted <- rapply(CensusBlockforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
CensusBlockforHCADParcelsunlisted <- unlist(CensusBlockforHCADParcelsunlisted)
# add census tract information to each parcel
HCAD_geom$geoid=censusblocks$GEOID[CensusBlockforHCADParcelsunlisted] 
#need to check if there are censusblocks not in 201.
#> censusblocks <- censusblocks[COUNTYFP=="201",]
#> length(unique(censusblocks$GEOID))
#[1] 2830
#> length(unique(HCAD$geoid))
#[1] 2876




# match parcels with tracts - should I be using st_contains?
# <- st_contains(HCAD_geom$centroid, censustracts) 
CensusTractforHCADParcels <- st_within(HCAD_geom$centroid, censustracts)
#unlist into vector
CensusTractforHCADParcelsunlisted <- rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
CensusTractforHCADParcelsunlisted <- unlist(CensusTractforHCADParcelsunlisted)
# add census tract information to each parcel
HCAD_geom$tractCE=censustracts$TRACTCE[CensusTractforHCADParcelsunlisted] 
#in 2021, 9 parcels have NAs for geometry and 64 don't match for tract and geoid, presumably because of where centroid was for each

#get HCAD stuff for a couple of the distance calculations - do either here or there...


#in 2022, CoH now has https://cohgis-mycity.opendata.arcgis.com/ - 
council_districts <- st_read(paste0(houstondatadir, "2022/City_Council/COH_ADMINISTRATIVE_BOUNDARY_-_MIL/COH_ADMINISTRATIVE_BOUNDARY_-_MIL.shp"))

#superneighborhoods - some changes in houstondata can happen - have to look at each dataset, since they don't consistently update by year
superneighborhoods <- st_read(paste0(houstondatadir, "2022/HOUSTON_LIMITS_BOUNDARIES_PACKAGE/HOUSTON_LIMITS_BOUNDARIES_PACKAGE.shp"))
#2017 had different title, but same geometry
#superneighborhoods <- st_read(paste0(houstondatadir, "2017/COH_SUPER_NEIGHBORHOODS/COH_SUPER_NEIGHBORHOODS.shp"))
superneighborhoods <- st_transform(superneighborhoods, st_crs(HCAD_geom)) #HCAD is renamed from sf_HCAD in this run - can change
super_within <- st_within(HCAD_geom$centroid, superneighborhoods)
super_within_unlist <- rapply(super_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
super_within_unlist <- unlist(super_within_unlist)
HCAD_geom$superneighborhood=superneighborhoods$SNBNAME[super_within_unlist]

#changing in 2022
city_council <- st_read(paste0(houstondatadir, "2022/City_Council/COH_ADMINISTRATIVE_BOUNDARY_-_MIL/COH_ADMINISTRATIVE_BOUNDARY_-_MIL.shp"))
city_council <- st_transform(city_council, st_crs(HCAD_geom))
council_within <- st_within(HCAD_geom$centroid,city_council)
council_within_unlist <- rapply(council_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
council_within_unlist <- unlist(council_within_unlist)
HCAD_geom$council_district=city_council$DISTRICT[council_within_unlist]

#saveRDS(HCAD,file = paste0(housingdir, vintage, "/temp/HCAD_parcels_tract_sn_coords.RDS"))

#HISD HS
hisdhs <- st_read(paste0(housingdir, "HISD/17-18_maps/High1718/HighSchool1718.shp"))
hisdhs <- st_transform(hisdhs, st_crs(HCAD))
hisdhs_within <- st_within(HCAD, hisdhs)
hisdhs_within_unlist <- rapply(hisdhs_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
hisdhs_within_unlist <- unlist(hisdhs_within_unlist)
HCAD$HISD_High_School=hisdhs$High_Schoo[hisdhs_within_unlist]

#HISD MS
hisdms <- st_read(paste0(housingdir, "HISD/17-18_maps/Middle1718/Middle1718.shp"))
hisdms <- st_transform(hisdhs, st_crs(HCAD))
hisdms_within <- st_within(HCAD, hisdms)
hisdms_within_unlist <- rapply(hisdms_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
hisdms_within_unlist <- unlist(hisdms_within_unlist)
HCAD$HISD_Middle_School=hisdms$Middle_Sch[hisdms_within_unlist]

#HISD ES
hisdes <- st_read(paste0(housingdir, "HISD/17-18_maps/Elementary1718/Elementary1718.shp"))
hisdes <- st_transform(hisdes, st_crs(HCAD))
hisdes_within <- st_within(HCAD, hisdes)
hisdes_within_unlist <- rapply(hisdes_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
hisdes_within_unlist <- unlist(hisdes_within_unlist)
HCAD$HISD_Elem_School=hisdes$Facility[hisdes_within_unlist]

other_isd <- st_read(paste0(censusdir, vintage, "/geo_census/cb_", vintage, "_", state, "_unsd_500k/cb_", vintage, "_", state, "_unsd_500k.shp"))
other_isd <- st_transform(other_isd, st_crs(HCAD))
other_isd_within <- st_within(HCAD, other_isd)
other_isd_within_unlist <- rapply(other_isd_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
other_isd_within_unlist <- unlist(other_isd_within_unlist)
HCAD$ISD=other_isd$NAME[other_isd_within_unlist]

# roads from census - also available from TXDOT - want the aadt eventually, in any case, and major_roads
#https://www.census.gov/library/reference/code-lists/route-type-codes.html
#the mtfccs2017.pdf in this folder lists the mtfcc codes, and includes distinctions between primary, secondary, and frontage roads
roads <- st_read(paste0(censusdir, vintage, "/geo_census/tl_", vintage, "_", state, county, "_roads/tl_", vintage, "_", state, county, "_roads.shp"))
roads <- st_transform(roads, st_crs(HCAD$ptcoords))

highways_distance <- st_distance(HCAD$ptcoords,roads[which(roads$MTFCC=="S1100"),]$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
h_distance <- as.data.table(highways_distance)
HCAD$min_h_distance <- apply(h_distance[,1:90], 1, min) #minimum distance in meters
#secondary streets, including what Houstonians call Freeways and throughways of various sorts - some as large as highways
freeways_distance <- st_distance(HCAD$ptcoords,roads[which(roads$MTFCC=="S1200"),]$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
f_distance <- as.data.table(freeways_distance)
HCAD$min_f_distance <- apply(f_distance[,1:325], 1, min) #minimum distance in meters

#DO 1710 and 1820 for walkways/bikepaths?? 
walk_bike_path_distance <- st_distance(HCAD$ptcoords,roads[which(roads$MTFCC=="S1710" | roads$MTFCC=="S1820"),]$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
wb_distance <- as.data.table(walk_bike_path_distance)
HCAD$min_walk_bike_path_distance <- apply(wb_distance[,1:4], 1, min) #minimum distance in meters - need to look at why there are only 4
#go ahead and do Brownfields, too?

saveRDS(HCAD,file = paste0(housingdir, vintage, "/temp/HCAD_HISD_roads_1_5_20.RDS")) #and everything before

traffic_counts <- st_read(paste0(maindir, "Traffic/2018/TxDOT_AADT_Annuals/TxDOT_AADT_Annuals.shp"))
traffic_counts <- st_transform(traffic_counts, st_crs(HCAD))
#need distance to count and number for top five??


#Not done below here: need to decide on how to make it add-on, since not done at beginning and to think through what you want to represent...
HPD_crime <- st_read(paste0(houstondatadir, "HPD_NIBRS_CRIME/HPD_NIBRS_CRIME.shp"))
HPD_crime <- st_transform(HPD_crime, st_crs(HCAD))
HPD_crime_distance <- st_distance(HCAD$ptcoords,HPD_crime,by_element = FALSE)

brownfields_site <- st_read(paste0(houstondatadir, "COH_PWE_BROWNSFIELDS_SITES/COH_PWE_BROWNSFIELDS_SITES.shp"))
brownfields <- st_read(paste0(houstondatadir, "COH_PWE_BROWNSFIELDS_POINTS/COH_PWE_BROWNSFIELDS_POINTS.shp"))
brownfields_dt <- as.data.table(brownfields)
brownfields_distance <- st_distance(HCAD$ptcoords,brownfields_dt[,]$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
b_distance <- as.data.table(brownfields_distance)
HCAD$min_b_distance <- apply(h_distance[,1:90], 1, min) #minimum distance in meters


#could do sldl and sldu (state legislative units) / and all the stuff from the city???

#civic_clubs - much more detailed than the superneighborhoods; some don't have anything
civic_clubs <- st_read(paste0(houstondatadir, "COH_CIVIC_CLUBS/COH_CIVIC_CLUBS.shp"))
civic_clubs <- st_transform(civic_clubs, st_crs(HCAD)) #HCAD is renamed from sf_HCAD in this run - can change
civic_within <- st_within(HCAD, civic_clubs)
civic_within_unlist <- rapply(civic_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
civic_within_unlist <- unlist(civic_within_unlist)
HCAD$civic_club=civic_clubs$NAME[civic_within_unlist] 

#for download new bus info
#Andrew's primary key: df3a89bdc17449608f370d24a59bb911
library(httr)
library(jsonlite)
metro_resp_stops <- GET("https://api.ridemetro.org/data/Stops?subscription-key=df3a89bdc17449608f370d24a59bb911")
metro_bus_stops_txt <- content(metro_resp_stops,as="text")
metro_stops <- fromJSON(metro_bus_stops_txt,flatten = TRUE)
busstops_dt <- data.table(BSID = c(as.numeric(metro_stops$value$StopCode)),
                       stop_name = c(metro_stops$value$Name),
                       longitude = c(metro_stops$value$Lon),
                       latitude = c(metro_stops$value$Lat))
ridershipcsv <- read.csv2(paste0(houstondatadir,vintage,"/Metro_stops/Riders_4_22.csv"), stringsAsFactors = TRUE,sep = ",")
bus_riders_stops <-  busstops_dt[ridershipcsv,on = .(BSID)]
bus_riders_stops <- bus_riders_stops[!is.na(longitude)]
busstops_sf <- st_as_sf(bus_riders_stops,coords = c("longitude", "latitude"),crs=4326)
busstops <- st_transform(busstops_sf,st_crs(censustracts))
bus_within <- st_within(busstops,censusblocks)
bus_within_unlist <- rapply(bus_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
bus_within_unlist <- unlist(bus_within_unlist)
busstops$geoid=censusblocks$GEOID[bus_within_unlist]
st_write(busstops,paste0(houstondatadir,vintage,"/Metro_stops/bus_riders_april_stops_bg.geojson"),driver = "GeoJSON")
saveRDS(busstops,paste0(houstondatadir,vintage,"/Metro_stops/busstops_blocks.RDS"))
busstops <- readRDS(paste0(houstondatadir,vintage,"/Metro_stops/busstops_blocks.RDS"))

#distance to busstops for each block group
block_bus_distance <- st_distance(censusblocks$geoid_centroid,busstops$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
bus_distance <- as.data.table(block_bus_distance)
censusblocks$min_bus_distance <- apply(bus_distance[,1:length(colnames(bus_distance))], 1, min) #minimum distance in meters

#need to figure out how to get the index number and the BSID for top 20 or so, to make calculation from front doors in groups by censusblock
#censusblocks$min_bus_ID <- busstops[apply(bus_distance[,1:length(colnames(bus_distance))], 1, min),]$BSID

#I think this is from 2017 - could do the within for extra - or add busstops to the HCAD??
bus <- st_read(paste0(houstondatadir, "Bus_Stops/Bus_Stops.shp"))
#bus$geometry <- st_transform(bus$geometry,st_crs(HCAD$ptcoords))
#find buses within tracts, and then calculate distance??
#bus_distance_1 <- st_distance(HCAD$ptcoords,bus[1:1000,]$geometry,by_element = FALSE) 
#calculate distances to stops?
#doing buses within censustracts for side project - buses from below
bus <- st_transform(bus,st_crs(censustracts)) 
bus_within <- st_within(bus,censustracts)
bus_within_unlist <- rapply(bus_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
bus_within_unlist <- unlist(bus_within_unlist)
bus$tract=censustracts$GEOID[bus_within_unlist]
#zip
zip <- st_read(paste0(houstondatadir, "zipcode/ZIPCODE.shp"))
zip <- st_transform(zip,st_crs(bus)) 
zbus_within <- st_within(bus,zip)
zbus_within_unlist <- rapply(zbus_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
zbus_within_unlist <- unlist(zbus_within_unlist)
bus$zip=zip$ZIPCODE[zbus_within_unlist]

superneighborhoods <- st_read(paste0(houstondatadir, "COH_SUPER_NEIGHBORHOODS/COH_SUPER_NEIGHBORHOODS.shp"))
superneighborhoods <- st_transform(superneighborhoods, st_crs(bus)) #HCAD is renamed from sf_HCAD in this run - can change
superb_within <- st_within(bus, superneighborhoods)
superb_within_unlist <- rapply(superb_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
superb_within_unlist <- unlist(superb_within_unlist)
bus$superneighborhood=superneighborhoods$SNBNAME[superb_within_unlist]

#saveRDS(bus,file = paste0(censusdir, vintage, "/temp/bus.RDS"))

#parks - 
parks <- st_read(paste0(houstondatadir, "COH_PARK_SECTOR/COH_PARK_SECTOR.shp"))
#calculate distances to parks?
pst
libraries <- st_read(paste0(houstondatadir, "COH_LIBRARIES/COH_LIBRARIES.shp"))
#calculate distances to libraries?

#traffic - in Traffic/2018/TxDOT_AADT_Annuals - http://gis-txdot.opendata.arcgis.com/datasets/txdot-aadt-annuals/geoservice


#Do distance to highways? or use all roads? tl_2017_48201_roads, or compare with products from TXDot in ../Traffic
#do city within, school (HISD and census), see what is in place and puma
#do centroids

#for distance to grocery stores, pantries, etc.
#and to pollution: https://www.tceq.texas.gov/compliance/enforcement/compliance-history/get_list.html


#TCEQ in general: https://www.tceq.texas.gov/agency/data/lookup-data/download-data.html
#air compliance in TCQQ - CN_OUT2.txt is compliance history database - would be hard to get assigned to sites
#2013thru2017statesum is point source emissions inventory. https://www.tceq.texas.gov/airquality/air-emissions
#brownfields is bsadb, but also available from COH - need to compare - https://www.tceq.texas.gov/remediation/bsa/bsa.html
#drycleaners is DC_Registration_All.xls: https://www.tceq.texas.gov/agency/data/lookup-data/drycleaners-data-records.html
#these are good class examples - 14523 pages of text available as a page of text??
#above ground storage facilities: https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_ast.txt
#and with facility info: https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_fac.txt
#could do annual waste summaries, too: https://www.tceq.texas.gov/agency/data/lookup-data/ihw-datasets.html / https://www.tceq.texas.gov/tires/tires 
#municipal solid waste https://www.tceq.texas.gov/permitting/waste_permits/msw_permits/msw-data 

#enviromapper: https://geopub.epa.gov/myem/efmap/index.html?ve=13,29.760588,-95.369680&pText=77203,%20Houston,%20Texas
#TRI: https://www.epa.gov/toxics-release-inventory-tri-program/find-understand-and-use-tri
#https://svi.cdc.gov/data-and-tools-download.html (social vulnerability index)

#might be interesting to look at pulse surveys around covid, too: https://www.census.gov/programs-surveys/household-pulse-survey/data.html#phase3.1


east_end_tracts <- c("324200","311400","311300","311500","311600","311000","311100","310900","320200","320100")

busstops <- readRDS(paste0(houstondatadir,vintage,"/Metro_stops/busstops_blocks.RDS"))


#already have the within geoid marked by the fact that theses are in HCAD
fast_food_HCAD <- HCAD[improv_typ_real%in%c("4323","4325"),] #food stands (130) and fast food (1,627)
super_market_HCAD <- HCAD[improv_typ_real=="4347",]
convenience_stores_HCAD <- HCAD[improv_typ_real%in%c("4324","4348","4335"),] #and conv. attached to gas pump and truck stop
shop_center_HCAD <- HCAD[improv_typ_real%in%c("4341","4342","4343","4344","4345"),] #includes things like Target in 4345
food_sources_HCAD <- HCAD[HCAD$improv_typ_real %in% c("4325","4323","4324","4335","4347","4348"),] %>% 
  distinct(account, .keep_all = TRUE)

#using block groups centroids because calculation is hanging for each household
censusblocks <- censusblocks[which(censusblocks$COUNTYFP=="201"),]

#for busstops
busstops <- st_transform(busstops,crs = st_crs(censusblocks))
metro_stop_distance <- st_distance(censusblocks$geoid_centroid,busstops$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
censusblocks$metro_stop_distance_min <- as.matrix(apply(metro_stop_distance,1,min,na.rm=TRUE)) 
censusblocks$metro_stop_BSID <- busstops$BSID[as.matrix(apply(metro_stop_distance,1,which.min))]

#for convenience stores
food_source_distance <- st_distance(censusblocks$geoid_centroid,food_sources_HCAD$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
censusblocks$food_source_distance_min <- as.matrix(apply(food_source_distance,1,min,na.rm=TRUE)) 
#censusblocks$closest_food_distance_acct <- food_source_distance[as.matrix(apply(food_source_distance,1,which.min))] #need to confirm how which.min deals with NA
censusblocks$closest_food_account <- food_sources_HCAD$account[as.matrix(apply(food_source_distance,1,which.min))]
#should check to see if it's inside block group??

fast_food_distance <- st_distance(censusblocks$geoid_centroid,fast_food_HCAD$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
censusblocks$fast_food_distance_min <- as.matrix(apply(fast_food_distance,1,min,na.rm=TRUE))
#censusblocks$fast_food_distance_acct <- fast_food_distance[as.matrix(apply(fast_food_distance,1,which.min))] #need to confirm how it deals with NA
censusblocks$fast_food_account <- fast_food_HCAD$account[as.matrix(apply(fast_food_distance,1,which.min))]

super_market_distance <- st_distance(censusblocks$geoid_centroid,super_market_HCAD$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
censusblocks$super_market_distance <- as.matrix(apply(super_market_distance,1,min,na.rm=TRUE))
censusblocks$super_market_account <- super_market_HCAD$account[as.matrix(apply(super_market_distance,1,which.min))]

convenience_stores_distance <- st_distance(censusblocks$geoid_centroid,convenience_stores_HCAD$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
censusblocks$convenience_stores_distance <- as.matrix(apply(convenience_stores_distance,1,min,na.rm=TRUE))
censusblocks$convenience_stores_account <- convenience_stores_HCAD$account[as.matrix(apply(convenience_stores_distance,1,which.min))]

shop_center_distance <- st_distance(censusblocks$geoid_centroid,shop_center_HCAD$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
censusblocks$shop_center_distance <- as.matrix(apply(shop_center_distance,1,min,na.rm=TRUE))
censusblocks$shop_center_account <- shop_center_HCAD$account[as.matrix(apply(shop_center_distance,1,which.min))]

#could do - closest 10 for each block_group, then do the calculation by block group with those ten for each property
#food_source_distance <- st_distance(censusblocks$geoid_centroid,food_sources_HCAD$geometry,by_element = FALSE) #by element picks a vector and not a dense matrix - from closest to furthest - could choose a bunch of them
#the pain in the butt part is how often it seems to require doing a sort and somehow tracking the index after the sort

