#General Script for merging data and making map

#Read in Data
adults1=read.csv("adults_who_had_routine_checkup.csv")
adults2=read.csv("adults_without_health_insurance.csv")

#Sort to relevant data
adults=merge(adults1,adults2,by="Location")
adults=data.frame(Location=adults$Location,adults_with_routine_checkups=adults$Indicator.Value.x,
                  adults_without_routine_checkups=100-adults$Indicator.Value.x,
                  adults_without_insurance=adults$Indicator.Value.y)

#Merge with Shape files
census_tracts=readRDS("texas_tracts_data.rds")
census_tracts@data[,6:1779]=NULL
census_tracts$Location=substr(census_tracts$GEOID,1,11)

library(tigris)
adults=geo_join(census_tracts,adults,by="Location",how="inner")


#Make 2 maps
library(leaflet)
popup <- paste0("GEOID:",adults$Location,"<br>",
                "% Adults without insurance",adults$adults_without_insurance,"<br>",
                "% Adults without regular check ups",adults$adults_without_routine_checkups)

pal_Insurance <- colorNumeric(
  palette = "YlGnBu",
  domain = adults$adults_without_insurance
)

pal_Checkup <- colorNumeric(
  palette = "YlGnBu",
  domain = adults$adults_without_routine_checkups
)
Insurance_map=leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = adults, 
              fillColor = ~pal_Insurance(adults$adults_without_insurance), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal_Insurance, 
            values = adults$adults_without_insurance, 
            position = "bottomright", 
            title = "Percent of<br>Adults without Insurance",
            labFormat = labelFormat(suffix = "%"))
Insurance_map

Checkup_map=leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = adults, 
              fillColor = ~pal_Checkup(adults$adults_without_routine_checkups), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal_Checkup, 
            values = adults$adults_without_routine_checkups, 
            position = "bottomright", 
            title = "Percent of<br>Adults without Routine Checkups",
            labFormat = labelFormat(suffix = "%"))
Checkup_map

saveRDS(adults,"data_for_map.rds")
