library(sf)
library(data.table)


#clean up, rename and join - get HCAD_parcels from HCAD_merge.R, before expanding so these don't take as long.
HCAD_parcels <- HCAD_parcels %>% 
  rename(account=HCAD_NUM) %>%
  select(account,LocAddr,city,zip,geometry) %>%
  mutate(valid = st_is_valid(geometry)) %>% #90 False, 1380490 true
  filter(valid)

#should have reduced to only single accounts here, instead of later.
saveRDS(HCAD_parcels,file = paste0(censusdir, vintage, "/temp/HCAD_parcels_valid.RDS"))
HCAD_parcels <- readRDS(paste0(censusdir, vintage, "/temp/HCAD_parcels_valid.RDS"))

#add geo information from U.S. census: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2017.html
#my download is 12/27/2019
censustracts <- st_read(paste0(censusdir, vintage, "/geo_census/cb_", vintage, "_", state, "_tract_500k/cb_", vintage, "_", state, "_tract_500k.shp"))
# put them in same CRS as Parcels
censustracts <- st_transform(censustracts, st_crs(HCAD_parcels))



#https://catalog.data.gov/dataset/tiger-line-shapefile-2017-county-harris-county-tx-topological-faces-polygons-with-all-geocodes-
census_tl_face <- st_read(paste0(censusdir, vintage, "/geo_census/tl_", vintage, "_", state, county, "_faces/tl_",vintage, "_", state, county, "_faces.shp"))

# match parcels with tracts
CensusTractforHCADParcels <- st_within(HCAD_parcels, censustracts)
#unlist into vector
CensusTractforHCADParcelsunlisted <- rapply(CensusTractforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
CensusTractforHCADParcelsunlisted <- unlist(CensusTractforHCADParcelsunlisted)

# add census tract information to each parcel
HCAD_parcels$tract=censustracts$TRACTCE[CensusTractforHCADParcelsunlisted] 
#get rid of 11337 NAs? - not sure why they died? perhaps overlapping with two? or not residential building? or should try on centroids?
#not run: HCAD_parcels <- HCAD_parcels[which(!is.na(HCAD_parcels$tract)),]
#plot(HCAD_parcels[which(is.na(HCAD_parcels$tract)),])
saveRDS(HCAD_parcels,file = paste0(censusdir, vintage, "/temp/HCAD_parcels_tract.RDS"))
HCAD_parcels <- readRDS(paste0(censusdir, vintage, "/temp/HCAD_parcels_tract.RDS"))

#add centroids for each lot (need to make sure it's not also houses?)
HCAD_parcels_centroids <- st_as_sf(HCAD_parcels, crs=3674)
sf_HCAD_parcels <- HCAD_parcels_centroids %>%
  ungroup() %>%
  mutate(
    NADcentroids = st_centroid(geometry),
    ptcoords = st_transform(NADcentroids,crs=4326),
  )
saveRDS(sf_HCAD_parcels,file = paste0(housingdir, vintage, "/temp/HCAD_parcels_tract_coords.RDS"))
sf_HCAD_parcels <- readRDS(paste0(housingdir, vintage, "/temp/HCAD_parcels_tract_coords.RDS"))

#superneighborhoods
superneighborhoods <- st_read(paste0(houstondatadir, "COH_SUPER_NEIGHBORHOODS/COH_SUPER_NEIGHBORHOODS.shp"))
superneighborhoods <- st_transform(superneighborhoods, st_crs(HCAD)) #HCAD is renamed from sf_HCAD in this run - can change
super_within <- st_within(HCAD, superneighborhoods)
super_within_unlist <- rapply(super_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
super_within_unlist <- unlist(super_within_unlist)
HCAD$superneighborhood=superneighborhoods$SNBNAME[super_within_unlist]


city_council <- st_read(paste0(houstondatadir, "COH_CITY_COUNCIL_DISTRICTS/COH_CITY_COUNCIL_DISTRICTS.shp"))
city_council <- st_transform(city_council, st_crs(HCAD))
council_within <- st_within(HCAD,city_council)
council_within_unlist <- rapply(council_within,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
council_within_unlist <- unlist(council_within_unlist)
HCAD$council_district=city_council$DISTRICT[council_within_unlist]

saveRDS(HCAD,file = paste0(housingdir, vintage, "/temp/HCAD_parcels_tract_sn_coords.RDS"))

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
hisdes_within <- st_within(HCAD, hisdms)
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

saveRDS(bus,file = paste0(censusdir, vintage, "/temp/bus.RDS"))

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
