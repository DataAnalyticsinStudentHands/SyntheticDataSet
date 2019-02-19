library(maptools)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(cleangeo)
#hcad_parcels <- readShapePoly("Parcels/Parcels", delete_null_obj=TRUE, force_ring=TRUE,verbose=TRUE, repair=TRUE, proj4string=CRS("+proj=longlat +datum=WGS84"))
#hcad_parcels <- rgdal::readOGR("Parcels")
#the HCAD data is easier to load as R data as soon as you've done it once
morefeatures=read.csv('building_res.csv',header=FALSE,quote="")

TexasCensusTracts <- rgdal::readOGR("TexasCensusTractShapefiles")

TXCensusTracts=spTransform(TexasCensusTracts,proj4string(hcad_parcels))
#hcad2=spTransform(hcad_parcels,proj4string(TexasCensusTracts))

#TXCensusTracts=st_make_valid(TXCensusTracts)
#hcad_parcels=st_make_valid(hcad_parcels)
#proj4string(hcad_parcels) <- TexasCensusTracts@proj4string
over(TXCensusTracts,hcad_parcels)

report <- clgeo_CollectionReport(hcad_parcels)
report2 <- clgeo_CollectionReport(TXCensusTracts)
#ssplot()


#####
library(maptools)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(cleangeo)
#hcad_parcels <- readShapePoly("Parcels/Parcels", delete_null_obj=TRUE, force_ring=TRUE,verbose=TRUE, repair=TRUE, proj4string=CRS("+proj=longlat +datum=WGS84"))

#I used this one the other gave me memory errors
hcad_parcels <- rgdal::readOGR("Parcels")
#the HCAD data is easier to load as R data as soon as you've done it once
morefeatures=read.csv('building_res.csv',header=FALSE,quote="")

TexasCensusTracts <- rgdal::readOGR("TexasCensusTractShapefiles")

TXCensusTracts=spTransform(TexasCensusTracts,proj4string(hcad_parcels))
#hcad2=spTransform(hcad_parcels,proj4string(TexasCensusTracts))


#proj4string(hcad_parcels) <- TexasCensusTracts@proj4string
over(TXCensusTracts,hcad_parcels)


library(sf)
#parcels <- st_read("Parcels/Parcels.shp")
#parcels$valid=st_is_valid(parcels, reason = TRUE)
#validparcels=subset(parcels,parcels$valid=="Valid Geometry")

#st_write(validparcels,"validparcels.shp")

parcels <- st_read("Parcels/Parcels.shp")
