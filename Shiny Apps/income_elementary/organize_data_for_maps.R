
#Load map of tract add variable to merge with income file
tract_map=readRDS("Harris_tract_data.rds")
tract_map$FIPS=paste0(tract_map$STATEFP,tract_map$COUNTYFP,tract_map$TRACTCE)

#load income file
income_data=read.csv("better_income_maybe.csv")
income_data$Population.in.Poverty.2017=100-income_data$Population.Not.in.Poverty..Total..2017

#join
library(tigris)
tract_map=geo_join(tract_map,income_data,by="FIPS",how="inner")


#load names of elementary schools of interest
schools_of_interest=read.csv("elementary_schools_dan_wanted.csv")
schools_of_interest$Elementary=sub("Elementary","",schools_of_interest$Elementary)
schools_of_interest$Elementary=as.character(schools_of_interest$Elementary)
schools_of_interest$Elementary=trimws(schools_of_interest$Elementary)

#load school zones
library(sf)

elementary_school_zones=readRDS("elementary_school_zones.RDS")
elementary_school_zones=st_transform(elementary_school_zones,crs='+proj=longlat +datum=WGS84')
elementary_school_zones$Elementary=as.character(elementary_school_zones$Elementary)

#join to remove rows
elementary_school_zones_of_interest=geo_join(elementary_school_zones,schools_of_interest,by="Elementary",how="inner")

#missing rows
schools_of_interest$Elementary[!(schools_of_interest$Elementary %in% elementary_school_zones_of_interest$Elementary)]
#I changed rows manually that didn't match
#I believe Lewis and Bellfort serve the same area
#as well as fonwood and Hilliard
#Halpin and Tinsley
#Ashford and Shadowbriar
#Mistral is not an HISD school

saveRDS(tract_map,"tract_map.RDS")
saveRDS(elementary_school_zones_of_interest,"elementary_school_zones_of_interest.RDS")

#draft map
tract_map=readRDS("tract_map.RDS")
elementary_school_zones_of_interest=readRDS("elementary_school_zones_of_interest.RDS")

pal <- colorNumeric(
  palette = "YlGnBu",
  domain=c(0,200000)
  #domain = c(tract_map$Family.Income.Median..2017,tract_map$Family.Income.Average2017,tract_map$Average.Household.Income..2017,tract_map$Median.Household.Income..2017)
)

popup=paste0("Elementary School:",elementary_school_zones_of_interest$Elementary)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = tract_map, 
              fillColor = ~pal(unlist(tract_map$Median.Household.Income..2017)), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Median Household Income",
              smoothFactor = 0.2)%>%
  addPolygons(data = tract_map, 
              fillColor = ~pal(unlist(tract_map$Average.Household.Income..2017)), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Average Household Income",
              smoothFactor = 0.2)%>%
  addPolygons(data = tract_map, 
              fillColor = ~pal(unlist(tract_map$Family.Income.Median..2017)), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Median Family Income",
              smoothFactor = 0.2)%>%
  addPolygons(data = tract_map, 
              fillColor = ~pal(unlist(tract_map$Average.Household.Income..2017)), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Average Family Income",
              smoothFactor = 0.2)%>%
  addPolylines(data=elementary_school_zones_of_interest,
               popup=popup
  )%>%
  addLayersControl(
    baseGroups=c("Median Household Income","Average Household Income","Median Family Income","Average Family Income"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = pal, 
            values = c(tract_map$Family.Income.Median..2017,tract_map$Family.Income.Average2017,tract_map$Average.Household.Income..2017,tract_map$Median.Household.Income..2017), 
            position = "bottomright", 
            title = paste("Income"))

