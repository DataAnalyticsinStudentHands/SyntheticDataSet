library(maptools)
library(rgeos)
library(rgdal)
library(sp)
#
#proj: http://www.prj2epsg.org/search
#blocks_houston <- readShapePoly("blocks_houston/blocks_houston", delete_null_obj=TRUE,
 #           verbose=TRUE, proj4string=CRS("+proj=aea +ellps=WGS84"))
hcad_parcels <- readShapePoly("Parcels/Parcels", delete_null_obj=TRUE, force_ring=TRUE,
        verbose=TRUE, repair=TRUE, proj4string=CRS("+proj=longlat +datum=WGS84"));
#      verbose=TRUE, repair=TRUE, proj4string=CRS("+proj=lcc +ellps=GRS80")); 
#took all steps on blocks; doing it again on parcels
#blocks are from HCAT - Josh?
#took about 4 hours; error on next step was Error in TopologyFunc(spgeom, id, byid, "rgeos_getcentroid") : 
#IllegalArgumentException: Invalid number of points in LinearRing found 3 - must be 0 or >= 4
#In addition: Warning message:
#  In .Map2PolyDF(Map, IDs = IDvar, proj4string = proj4string, force_ring = force_ring,  :
#                   Null objects with the following indices deleted: 924063

#polys <- lapply(unique(hcad_parcels$HCAD_NUM), function(i) {
 # Polygons(list(Polygon(coords[coords$ID==i, 1:2])), ID=i)
#})
#spa_polys <- SpatialPolygons(polys)
#writeOGR(blocks_houston, ".", "blocks_houston/blocks_houston_csv", driver = "CSV");
#write.csv(data.frame(hcad_parcels), 'Parcels/Parcels.csv')
centr_parcels <- gCentroid(hcad_parcels, byid = FALSE) #false means no subgeometries
#centr_blocks <- gCentroid(blocks_houston, byid = TRUE)

centr_parcels <- SpatialPointsDataFrame(centr_parcels, data= hcad_parcels@data) 
#centr_blocks <- SpatialPointsDataFrame(centr_blocks, data= blocks_houston@data) 

writeOGR(centr_parcels, ".", "Parcels/parcel_centroids", driver = "ESRI Shapefile")
write.csv(data.frame(centr_parcels), 'Parcels/centr_parcels.csv')
#writeOGR(centr_blocks, ".", "blocks_houston/blocks_centroids_WGS84", driver = "ESRI Shapefile")
#writeOGR(centr_blocks, ".", "blocks_houston/blocks_centroids_csv",layer="blocks_center_houston", driver="CSV")
