library(sf)
library(data.table)
library(stringr)
#censusdir for me is from workflow so you just need to set the directions right

#this depends on where your census dir is, but _tract_500k.shp and _faces and _bg, etc. are all downloaded from census, by year: 
#https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html (I got these on 7/10/21 - 2020 was most recent; got 2021 on 7/18/2022)
geo_vintage <- "2021"
censustracts <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_tract_500k/cb_", geo_vintage, "_", state, "_tract_500k.shp"))
tractsDT <- as.data.table(censustracts)
tractsDT[,("centroid"):=st_centroid(geometry)] 
tractsDT[,("longitude"):=unlist(map(centroid,1))]
tractsDT[,("latitude"):=unlist(map(centroid,2))]

#the default is for the whole state, you can get down to Harris county with 201
#8 county region: 201 Harris; 157 Fort Bend; 167 Galveston; 039 Brazoria; 071 Chambers; 291 Liberty; 339 Montgomery; 473 Waller 
FIPS_vector <- c("201","157","167","039","071","291","339","473")
tracts_8county <- tractsDT[COUNTYFP%in%FIPS_vector]

censusblocks <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_bg_500k/cb_", geo_vintage, "_", state, "_bg_500k.shp"))
blocksDT <- as.data.table(censusblocks)
blocks_8county <- blocksDT[COUNTYFP%in%FIPS_vector]
blocksDT[,("TRACTGEOID"):=substr(GEOID,1,11)]
blocksDT[,("centroid"):=st_centroid(geometry)] 
blocksDT[,("longitude"):=unlist(map(centroid,1))]
blocksDT[,("latitude"):=unlist(map(centroid,2))]

#this is an example - it gets you whether the tract is in the city
censusplace <- st_read(paste0(censusdir, geo_vintage, "/geo_census/cb_", geo_vintage, "_", state, "_place_500k/cb_", geo_vintage, "_", state, "_place_500k.shp"))
placeDT <- as.data.table(censusplace) #just cities

#assign to tractsDT
placeDT <- st_as_sf(placeDT)
tracts4places <- st_within(tractsDT$centroid, placeDT)
#unlist into vector
tracts4placesunlisted <- rapply(tracts4places,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
tracts4placesunlisted <- unlist(tracts4placesunlisted)
tractsDT$placename=placeDT$NAME[tracts4placesunlisted]
#or
placeDT <- st_transform(placeDT,st_crs(censusblocks))
blocks4places <- st_within(censusblocks$centroid, placeDT)
blocks4placesunlisted <- rapply(blocks4places,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
blocks4placesunlisted <- unlist(blocks4placesunlisted)
censusblocks$placename=placeDT$NAME[blocks4placesunlisted]