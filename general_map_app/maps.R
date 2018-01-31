

Harris_tract_data <- readRDS("../maps/Harris_tract_data.rds")

realfrequencypertract=read.csv("../maps/realfrequenciespertract.csv")

syntheticfrequencypertract=read.csv("../maps/syntheticdatasetfrequenciespertract.csv")
#to add right length location field to Harris_tract_data
Harris_tract_data$Location10 <- paste0(Harris_tract_data$STATEFP,Harris_tract_data$COUNTYFP,Harris_tract_data$TRACTCE)
realfrequencypertract$Location10 <- paste0(realfrequencypertract$state,realfrequencypertract$county,realfrequencypertract$tract)
library(tigris)
Houston_real <- geo_join(Harris_tract_data, realfrequencypertract, 'Location10', 'Location10', by = NULL, how = "left")

syntheticfrequencypertract$TRACTCE <- syntheticfrequencypertract$tract

Houston_prime <- geo_join(Harris_tract_data,syntheticfrequencypertract, 'TRACTCE', 'TRACTCE', by = NULL, how = "left")



library(leaflet)
library(htmlwidgets)
library(webshot)

variables=colnames(syntheticfrequencypertract)
variables <- variables[5:(length(variables)-1)]

for (var in variables){
  m=var
  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = Houston_prime@data[m]
  )
  
  map2<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = Houston_prime, 
                fillColor = ~pal(Houston_prime@data[m]), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2)%>%
    addLegend(pal = pal, 
              values = unlist(Houston_prime@data[m]), 
              position = "bottomright", 
              title = m)
  map2
  
  filename=paste0(gsub("[.]", "-", m),"-","prime.png")
  
  saveWidget(map2, "nohealthinsuranceprime.html", selfcontained = FALSE)
  webshot("nohealthinsuranceprime.html", file = filename,
          cliprect = "viewport")
  
}


variables=colnames(realfrequencypertract)
variables <- variables[5:(length(variables)-1)]

for (var in variables){
  m=var
  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = Houston_real@data[m]
  )
  
  map2<-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = Houston_real, 
                fillColor = ~pal(Houston_real@data[m]), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2)%>%
    addLegend(pal = pal, 
              values = unlist(Houston_real@data[m]), 
              position = "bottomright", 
              title = m)
  map2
  
  filename=paste0(gsub("[.]", "-", m),"-","real.png")
  
  saveWidget(map2, "nohealthinsuranceprime.html", selfcontained = FALSE)
  webshot("nohealthinsuranceprime.html", file = filename,
          cliprect = "viewport")
  
}
