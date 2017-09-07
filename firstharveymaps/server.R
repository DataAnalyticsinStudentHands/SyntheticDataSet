library(shiny)

#On second thought let's only do this all at once
#syntheticdataset=read.csv("mergedsampleset.csv")
##get number of children
#numberofchildren=ftable(table(syntheticdataset$tract,syntheticdataset$member))
#number_of_children=as.data.frame.matrix(numberofchildren)

#colnames(number_of_children)=unlist(attr(numberofchildren, "col.vars"))
#number_of_children$tract=unlist(attr(numberofchildren, "row.vars"))
#get number of adults with asthma
#asthma=ftable(table(syntheticdataset$tract,syntheticdataset$adult.asthma))
#adult_asthma=as.data.frame.matrix(asthma)

#colnames(adult_asthma)=unlist(attr(asthma, "col.vars"))
#adult_asthma$tract=unlist(attr(asthma, "row.vars"))

#dataformap=merge(number_of_children,adult_asthma)
dataformap=read.csv("dataformap.csv")

#get number of children with asthma
#asthma=ftable(table(syntheticdataset$tract,syntheticdataset$childhood.asthma))
#child_asthma=as.data.frame.matrix(asthma)

#colnames(child_asthma)=paste0("child",unlist(attr(asthma, "col.vars")))
#child_asthma$tract=unlist(attr(asthma, "row.vars"))

#dataformap=merge(dataformap,child_asthma)


#Read in Tract outline
Harris_tract_data <- readRDS("Harris_tract_data.rds")
Harris_tract_data@data[,17:1878]=NULL

#join with data to map
library(tigris)

dataformap$TRACTCE <- dataformap$tract
data_for_map <- geo_join(Harris_tract_data,dataformap, 'TRACTCE', 'TRACTCE', by = NULL, how = "left")


#On second thought this is also slow and better to be only done once
#read in outline of Houston
#library(rgdal)

#TxDOT_cities <- readOGR("TxDOT_City_Boundaries","TxDOT_City_Boundaries")
#Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]
#Houston_bounds <- spTransform(Houston_bounds, CRSobj = CRS(proj4string(data_for_map)))

#read in flood plain outline
#from http://www.h-gac.com/rds/gis-data/gis-datasets.aspx
#floodplain=readOGR(dsn="FEMA_Floodplains_NFHL_2015.gdb")
#floodplains <- spTransform(floodplain, CRSobj = CRS(proj4string(data_for_map)))
#only use floodplain data related to Houston
#floodplains2=floodplains[Houston_bounds,]

floodplains2 <- readRDS("floodplains2.rds")


library(leaflet)

# Define server logic required to draw maps
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$Children <- renderLeaflet({
    palChild <- colorNumeric(
      palette = "YlOrRd",
      domain = data_for_map@data["Child"]
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = data_for_map, 
                  fillColor = ~palChild(data_for_map@data$Child), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2)%>%
      addPolylines(data = floodplains2,
                   weight=1
      )%>%
      addLegend(pal = palChild, 
                values = unlist(data_for_map@data$Child), 
                position = "bottomleft", 
                title = "Number of Children per Tract")
  })
  
  output$AdultswuthAsthma <- renderLeaflet({
    palAsthma <- colorNumeric(
      palette = "YlOrRd",
      domain = data_for_map@data["yes"]
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(data = data_for_map, 
                  fillColor = ~palAsthma(data_for_map@data$yes), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2)%>%
      addPolylines(data = floodplains2,
                   weight=1
      )%>%
      addLegend(pal = palAsthma, 
                values = unlist(data_for_map@data$yes), 
                position = "bottomleft", 
                title = "Simulated Adult Asthma")
  })
  output$ChildrenwithAsthma <- renderLeaflet({
    palChildAsthma <- colorNumeric(
      palette = "YlOrRd",
      domain = data_for_map@data["childyes"]
    )
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addPolygons(data = data_for_map, 
                  fillColor = ~palChildAsthma(data_for_map@data$childyes), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2)%>%
      addPolylines(data = floodplains2,
                  weight=1
      )%>%
      addLegend(pal = palChildAsthma, 
                values = unlist(data_for_map@data$childyes), 
                position = "bottomleft", 
                title = "Simulated Child Asthma")
  })
})