#harris_pop = 4589928
#residents_affected = .052 * harris_pop #Harris county population multiplied by 5.2% living near highways
#round(residents, 0)

#num_schools = 1070
#total_students = 877593
#average_students = total_students / num_schools
#affected_schools = .047 * num_schools #4.7% of schools located near highways
#students_affected = average_students * affected_schools
#round(students_affected, 0)

#ratio between ug/m3 and number concentration per m3 is 76419420 Y=ratio*X
#ratio = 76421376.72
#99.6 - 100% increase in risk = 1000 ug/m3 total short term, ratio_risk = {0.0004 - 0.001}

#start of plotting map code
Harris_tract_data <- readRDS("other data/Harris_tract_data.rds")
View(Harris_tract_data)
library(rgdal)
#from http://www.houstonstateofhealth.com


roadsgdb <- "Traffic/Major_Roads/Major_Roads.gdb"
major_roads <- readOGR(dsn=roadsgdb,layer="Major_Roads")

TxDOT_cities <- readOGR("Traffic/TxDOT_City_Boundaries","TxDOT_City_Boundaries")
Houston_bounds <- TxDOT_cities[TxDOT_cities$CITY_NM=='Houston',]

#read in data for households
households = read.csv("households.csv")
#get total number of households per tract
households$total = households$X..Households..HHs...2017
households$Location20 = households$FIPS #combine the different columns into one column
View(households)

maj_roads_proj <- spTransform(major_roads, CRSobj = CRS(proj4string(Houston_bounds)))
plot(maj_roads_proj[Houston_bounds,])
#reading in data
stroke_data <- read.csv("500_Cities__Local_Data_for_Better_Health.csv",sep = ',',stringsAsFactors = FALSE)
View(stroke_data)
coronary_heart_disease_data = read.csv("Coronary Heart Disease.csv", sep = ',',stringsAsFactors = FALSE) 
#abbreviated as CHD
View(coronary_heart_disease_data)
asthma = read.csv("Asthma.csv", sep = ',',stringsAsFactors = FALSE)
View(asthma)
DiagnosedDiabetes = read.csv("Diagnosed Diabetes.csv", sep = ',',stringsAsFactors = FALSE)
View(DiagnosedDiabetes)
Noleisure = read.csv("No Leisure.csv", sep = ',',stringsAsFactors = FALSE)
View(Noleisure)
COPD = read.csv("COPD.csv", sep = ',',stringsAsFactors = FALSE)
View(COPD)
Smoke = read.csv("Smoking.csv", sep = ',',stringsAsFactors = FALSE)
View(Smoke)

#to add right length location field to Harris_tract_data
Harris_tract_data$Location10 <- substr(Harris_tract_data$GEOID,1,11)
View(Harris_tract_data)
stroke_data$Location10 <- substr(stroke_data$UniqueID,9,19)
library(tigris)
Stroke_Visual <- geo_join(Harris_tract_data, stroke_data, 'Location10', 'Location10', by = NULL, how = "left")

asthma$Location10 = substr(asthma$UniqueID, 9,19)
Asthma_Visual = geo_join(Harris_tract_data, asthma, 'Location10','Location10', by = NULL, how = "left")

DiagnosedDiabetes$Location10 = substr(DiagnosedDiabetes$UniqueID, 9,19)
Diabetes_Visual = geo_join(Harris_tract_data, DiagnosedDiabetes, 'Location10','Location10', by = NULL, how = "left")

Noleisure$Location10 = substr(Noleisure$UniqueID, 9,19)
Leisure_Visual = geo_join(Harris_tract_data, Noleisure, 'Location10','Location10', by = NULL, how = "left")

coronary_heart_disease_data$Location10 = substr(coronary_heart_disease_data$UniqueID, 9,19)
CHD_Visual = geo_join(Harris_tract_data, coronary_heart_disease_data, 'Location10','Location10', by = NULL, how = "left")

COPD$Location10 = substr(COPD$UniqueID, 9,19)
COPD_Visual = geo_join(Harris_tract_data, COPD, 'Location10','Location10', by = NULL, how = "left")

Smoke$Location10 = substr(COPD$UniqueID, 9,19)
Smoke_Visual = geo_join(Harris_tract_data, Smoke, 'Location10','Location10', by = NULL, how = "left")


Households_Visual = geo_join(Harris_tract_data, households, 'Location10','Location20', by = NULL, how = "left")

library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
sf <- ggplot() + 
  scale_fill_manual(values=cbPalette) +
  geom_polygon(data = Stroke_Visual, #[!is.na(vis_unresolved_2015$mean_not_resolved),], 
               aes(x=long, y = lat, colour = "tomato", fill = "Indicator.Value", 
                   group = group)) +
  #geom_text(aes(
  #label = STSElemIV$Facility,
  #label = vis_unresolved_2015$mean_not_resolved, 
  #x=STSElemIV$centroids[,1],  
  #y=STSElemIV$centroids[,2]),
  #check_overlap = TRUE,size=2.7) + 
  ggtitle('Prevalence of Stroke in Houston Area') +
  #theme_light() +
  # geom_label() +
  coord_fixed(1.3)

#plot(sf) #unhashtag to run ggplot

maj_roads_proj <- spTransform(major_roads, CRSobj = CRS(proj4string(Stroke_Visual)))
Houston_bounds <- spTransform(Houston_bounds, CRSobj = CRS(proj4string(Stroke_Visual)))

maj_roads_proj1 = spTransform(major_roads, CRSobj = CRS(proj4string(CHD_Visual)))
Houston_bounds1 = spTransform(Houston_bounds, CRSobj = CRS(proj4string(CHD_Visual)))

maj_roads_proj2 = spTransform(major_roads, CRSobj = CRS(proj4string(Households_Visual)))
Houston_bounds2 = spTransform(Houston_bounds, CRSobj = CRS(proj4string(Households_Visual)))

library(leaflet)
leaflet(data = Stroke_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Stroke_Visual$GEOID, "<br>", "Reporting of Stroke in Houston: ", Stroke_Visual$Data_Value)
pal <- colorNumeric(
  palette = "Blues",
  domain = Stroke_Visual$Data_Value
)

mapstroke<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Stroke_Visual, 
              fillColor = ~pal(Data_Value), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Stroke_Visual$Data_Value, 
            position = "bottomright",
            
            title = "Percent of Adults<br> in 
            Census Tract with<br> incidences of Stroke",
            labFormat = labelFormat(suffix = "%")) %>%
  addPolylines(data=maj_roads_proj[Houston_bounds,], 
              color= "red",
              fillColor = NULL)

mapstroke

#\/ \/ mapping CHD \/ \/
leaflet(data = CHD_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", CHD_Visual$GEOID, "<br>", "Reporting of Coronary Heart Disease in Houston: ", CHD_Visual$Data_Value)
pal <- colorNumeric(
  palette = "Blues",
  domain = CHD_Visual$Data_Value
)

mapCHD<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = CHD_Visual, 
              fillColor = ~pal(Data_Value), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = CHD_Visual$Data_Value, 
            position = "bottomright",
            
            title = "Percent of Adults<br> in 
            Census Tract with<br> Coronary Heart Disease",
            labFormat = labelFormat(suffix = "%")) %>%
  addPolylines(data=maj_roads_proj1[Houston_bounds1,], 
               color= "red",
               fillColor = NULL,
               weight = 5)
mapCHD

#\/ \/ mapping asthma \/ \/
leaflet(data = Asthma_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Asthma_Visual$GEOID, "<br>", "Reporting of Asthma Adults: ", Asthma_Visual$Data_Value)
pal <- colorNumeric(
  palette = "Blues",
  domain = Asthma_Visual$Data_Value
)

mapasthma<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Asthma_Visual, 
              fillColor = ~pal(Data_Value), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Asthma_Visual$Data_Value, 
            position = "bottomright",
            
            title = "Percent of Adults<br> in 
            Census Tract with<br> Asthma",
            labFormat = labelFormat(suffix = "%")) %>%
  addPolylines(data=maj_roads_proj1[Houston_bounds1,], 
               color= "red",
               fillColor = NULL,
               weight = 5)
mapasthma

#\/ \/ mapping diabetes \/ \/
leaflet(data = Diabetes_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Diabetes_Visual$GEOID, "<br>", "Reporting of Diabetes in Houston: ", Diabetes_Visual$Data_Value)
pal <- colorNumeric(
  palette = "Blues",
  domain = Diabetes_Visual$Data_Value
)

mapdiabetes<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Diabetes_Visual, 
              fillColor = ~pal(Data_Value), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Diabetes_Visual$Data_Value, 
            position = "bottomright",
            
            title = "Percent of Adults<br> in 
            Census Tract with<br> Diabetes",
            labFormat = labelFormat(suffix = "%")) %>%
  addPolylines(data=maj_roads_proj1[Houston_bounds1,], 
               color= "red",
               fillColor = NULL,
               weight = 5)
mapdiabetes

#\/ \/ mapping leisure \/ \/
leaflet(data = Leisure_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Leisure_Visual$GEOID, "<br>", "Reporting of No Leisure Activity in Houston: ", Leisure_Visual$Data_Value)
pal <- colorNumeric(
  palette = "Blues",
  domain = Leisure_Visual$Data_Value
)

mapleisure<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Leisure_Visual, 
              fillColor = ~pal(Data_Value), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Leisure_Visual$Data_Value, 
            position = "bottomright",
            
            title = "Percent of Adults<br> in 
            Census Tract with<br> No Leisure Activity",
            labFormat = labelFormat(suffix = "%")) %>%
  addPolylines(data=maj_roads_proj1[Houston_bounds1,], 
               color= "red",
               fillColor = NULL,
               weight = 5)
mapleisure

#\/ \/ mapping COPD \/ \/
leaflet(data = COPD_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", COPD_Visual$GEOID, "<br>", "Reporting of COPD: ", COPD_Visual$Data_Value)
pal <- colorNumeric(
  palette = "Blues",
  domain = COPD_Visual$Data_Value
)

mapCOPD<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = COPD_Visual, 
              fillColor = ~pal(Data_Value), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = COPD_Visual$Data_Value, 
            position = "bottomright",
            
            title = "Percent of Adults<br> in 
            Census Tract with<br> COPD",
            labFormat = labelFormat(suffix = "%")) %>%
  addPolylines(data=maj_roads_proj1[Houston_bounds1,], 
               color= "red",
               fillColor = NULL,
               weight = 5)
mapCOPD

#\/ \/ mapping Smoke\/ \/
leaflet(data = Smoke_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Smoke_Visual$GEOID, "<br>", "Reporting of Smoking: ", Smoke_Visual$Data_Value)
pal <- colorNumeric(
  palette = "Blues",
  domain = Smoke_Visual$Data_Value
)

mapsmoke<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Smoke_Visual, 
              fillColor = ~pal(Data_Value), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Smoke_Visual$Data_Value, 
            position = "bottomright",
            
            title = "Percent of Adults<br> in 
            Census Tract who<br> Smoke",
            labFormat = labelFormat(suffix = "%")) %>%
  addPolylines(data=maj_roads_proj1[Houston_bounds1,], 
               color= "red",
               fillColor = NULL,
               weight = 5)
mapsmoke


#\/ \/ mapping households in each census tract \/ \/
leaflet(data = Households_Visual) %>%
  addTiles() %>% addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% 
  setView(lng = -95.42, lat = 29.75, zoom = 10) # set centre and extent of map

popup <- paste0("GEOID: ", Households_Visual$GEOID, "<br>", "Households in Census Tract: ", Households_Visual$total)
pal <- colorNumeric(
  palette = "Greens",
  domain = Households_Visual$total
)

mapHouseholds<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = Households_Visual, 
              fillColor = ~pal(total), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = Households_Visual$total, 
            position = "bottomright",
            
            title = "Households in<br> 
            Census Tract") %>%
  addPolylines(data=maj_roads_proj1[Houston_bounds2,], 
               color= "red",
               fillColor = NULL,
               weight = 5)
mapHouseholds

#add spatialdatavalue, gcontains, buffer 100 meters up 100 meters down and all around 

#characteristics of individuals (socioeconomic indicators/determinants of health)
#change road width (to mirror the areas pollution affects)
#ratio for other causes (contact Emerald for macrovascular/microvascular)
#find out the data value information (what does the data mean) - probably percentage of people within census tract with ___ disease

#form variable with ethnicity, denominator is population and numerator is ethnicity

#insert name $ brings up variable list
#leaflet is for interactive map, install package first
library(rgeos)
