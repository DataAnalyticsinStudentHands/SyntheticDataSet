library(leaflet)
library(rgdal)

#I will put everything in the tracts map projection
Harris_tract_data <- readRDS("Harris_tract_data.rds")
Harris_tract_data@data[,17:1878]=NULL

#I'm only interested in looking at Houston so
TxDOT_cities <- readOGR("TxDOT_City_Boundaries","TxDOT_City_Boundaries")
Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]
Houston_bounds <- spTransform(Houston_bounds, CRSobj = CRS(proj4string(Harris_tract_data)))

#Starting with FEMA 2015
FEMA2015floodplains=readOGR(dsn="FEMA_Floodplains_NFHL_2015.gdb")
FEMA2015floodplains <- spTransform(FEMA2015floodplains, CRSobj = CRS(proj4string(Harris_tract_data)))
FEMA2015floodplains=FEMA2015floodplains[Houston_bounds,]
#look at data
View(FEMA2015floodplains@data)

pal <- colorFactor(
  palette = 'Dark2',
  domain = FEMA2015floodplains$ZONE_SUBTY
)

FEMA2015map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = FEMA2015floodplains, 
              fillColor = ~pal(FEMA2015floodplains@data$ZONE_SUBTY), 
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2)%>%
  addLegend(pal = pal, 
            values = unlist(FEMA2015floodplains@data$ZONE_SUBTY), 
            position = "bottomleft", 
            title = "FEMA 2015 Flood Zones")
FEMA2015map

#FEMA 2010
FEMA2010floodplains=readOGR(dsn="FEMA_Floodplains_DFIRM_Q3_2010.gdb")
FEMA2010floodplains <- spTransform(FEMA2010floodplains, CRSobj = CRS(proj4string(Harris_tract_data)))
FEMA2010floodplains=FEMA2010floodplains[Houston_bounds,]
#look at data
View(FEMA2010floodplains@data)

pal <- colorFactor(
  palette = 'Dark2',
  domain = FEMA2010floodplains$Zone
)

FEMA2010map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = FEMA2010floodplains, 
              fillColor = ~pal(FEMA2010floodplains@data$Zone), 
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2)%>%
  addLegend(pal = pal, 
            values = unlist(FEMA2010floodplains@data$Zone), 
            position = "bottomleft", 
            title = "FEMA 2010 Flood Zones")
FEMA2010map

#S1 8-29
S1_8_29floodplains=readOGR(dsn="S1_8_29")
S1_8_29floodplains <- spTransform(S1_8_29floodplains, CRSobj = CRS(proj4string(Harris_tract_data)))
S1_8_29floodplains=S1_8_29floodplains[Houston_bounds,]

S1_8_29map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = S1_8_29floodplains, 
              fillColor = "blue", 
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2)

S1_8_29map

#S1 8-31
S1_8_31floodplains=readOGR(dsn="S1_8_31",layer="2017083120170807S1a_region")
#This was empty for me

#Two Week MODIS
TwoWeekMODISfloodplains=readOGR(dsn="TwoWeekModis")
MODISfloodplains <- spTransform(TwoWeekMODISfloodplains, CRSobj = CRS(proj4string(Harris_tract_data)))
MODISfloodplains=MODISfloodplains[Houston_bounds,]

MODISmap<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = MODISfloodplains, 
              fillColor = "blue", 
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2)

MODISmap

EMSRfloodplains=readOGR(dsn="EMSR")
EMSRfloodplains <- spTransform(EMSRfloodplains, CRSobj = CRS(proj4string(Harris_tract_data)))
EMSRfloodplains=EMSRfloodplains[Houston_bounds,]
  