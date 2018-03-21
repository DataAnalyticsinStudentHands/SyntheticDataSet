# Define server logic required to draw a histogram ----

library(leaflet)

server <- function(input, output) {
  
  output$NicksMap<-renderLeaflet({
    #library(rgdal)
    
    #Read in Tract Data
    #Harris_tract_data <- readRDS("other data/Harris_tract_data.rds")
    #Remove Unnecessary Values
    #Harris_tract_data@data[,6:1779]=NULL
    #Add FIPS code
    #Harris_tract_data$FIPS<-substr(Harris_tract_data$GEOID, 1,11)
    
    #Read in Data For Map
    #data_for_map=read.csv("data_for_map.csv")
    
    #Merge polygons with values of interest
    #library(tigris)
    #data_for_map=geo_join(Harris_tract_data,data_for_map,by="FIPS")
    
    data_for_map=readRDS("data_for_map.RDS")
    #Get Road Data
    #roadsgdb <- "Traffic/Major_Roads/Major_Roads.gdb"
    #major_roads <- readOGR(dsn=roadsgdb,layer="Major_Roads")
    
    #TxDOT_cities <- readOGR("Traffic/TxDOT_City_Boundaries","TxDOT_City_Boundaries")
    #Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]
    
    #maj_roads_proj <- spTransform(major_roads, CRSobj = CRS(proj4string(Houston_bounds)))
    
    #maj_roads_proj1 = spTransform(major_roads, CRSobj = CRS(proj4string(data_for_map)))
    #Houston_bounds1 = spTransform(Houston_bounds, CRSobj = CRS(proj4string(data_for_map)))
    maj_roads_proj1=readRDS("maj_roads_proj1.RDS")
    Houston_bounds1=readRDS("Houston_bounds1.RDS")
    #Make Maps
    library(leaflet)
    
    palCoronary <- colorNumeric(
      palette = "Blues",
      domain = c(data_for_map$X..Coronary..2017)
    )
    
    palStroke <- colorNumeric(
      palette = "Blues",
      domain = c(data_for_map$X..Stroke..2017)
    )
    
    popup=paste0("GEOID: ",data_for_map$FIPS,"<br>","# Stroke: ",data_for_map$X..Stroke..2017,"<br>","# Coronary Heart Disease: ",data_for_map$X..Coronary..2017)
    
    map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                          dragging = FALSE)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = data_for_map, 
                  fillColor = ~palStroke(data_for_map$X..Stroke..2017), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.5, 
                  weight = 1,
                  group="Stroke",
                  smoothFactor = 0.2,
                  popup=popup
      )%>%
      addPolygons(data = data_for_map, 
                  fillColor = ~palCoronary(data_for_map$X..Coronary..2017), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.5, 
                  weight = 1,
                  group="Coronary Heart Disease",
                  smoothFactor = 0.2,
                  popup=popup
      )%>%
      addLayersControl(
        baseGroups=c("Stroke","Coronary Heart Disease"),
        options=layersControlOptions(collapsed=FALSE)
      )%>%
      addPolylines(data=maj_roads_proj1[Houston_bounds1,], 
                   color= "red",
                   fillColor = NULL,
                   weight = 3)
       map
    })
  
  
   
}