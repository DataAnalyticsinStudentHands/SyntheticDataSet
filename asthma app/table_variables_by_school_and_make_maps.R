#Read in school kids
elementary_school_kids=readRDS("elementary_school_kids.RDS")
middle_school_kids=readRDS("middle_school_kids.RDS")
high_school_kids=readRDS("high_school_kids.RDS")

#Read in Spatial files for school zones
library(sf)
#Read in elementary school zones
elementary_school_zones <- readRDS("elementary_school_zones.RDS")
#Read in middle school zones
middle_school_zones <- readRDS("middle_school_zones.RDS")
#Read in high school zones
high_school_zones <- readRDS("high_school_zones.RDS")

#use table command to get variables of interest by school zone in elementary schools
Frequency_Elementary_School_Table=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$EPIS_12M))
Frequency_Elementary_School_Table=subset(Frequency_Elementary_School_Table,Frequency_Elementary_School_Table$Var2==1)
Frequency_Elementary_School_Table$Var2=NULL
colnames(Frequency_Elementary_School_Table)<-c("Elementary","Children with Asthma Attack or Episode in past 12 months")

ER_Visit_Elementary_Schools=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$ER_VISIT))
ER_Visit_Elementary_Schools=subset(ER_Visit_Elementary_Schools,ER_Visit_Elementary_Schools$Var2==1)
ER_Visit_Elementary_Schools$Var2=NULL
colnames(ER_Visit_Elementary_Schools)<-c("Elementary","Children who have visited the Er because of asthma symptoms")

Frequency_Elementary_School_Table=merge(Frequency_Elementary_School_Table,ER_Visit_Elementary_Schools)

Another_Variable_Elementary_Schools=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$COOK_GAS))
Another_Variable_Elementary_Schools=subset(Another_Variable_Elementary_Schools,Another_Variable_Elementary_Schools$Var2==1)
Another_Variable_Elementary_Schools$Var2=NULL
colnames(Another_Variable_Elementary_Schools)<-c("Elementary","Gas Used in Cooking in Asthmatic Child home")

Frequency_Elementary_School_Table=merge(Frequency_Elementary_School_Table,Another_Variable_Elementary_Schools)

Another_Variable_Elementary_Schools=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$ENV_MOLD))
Another_Variable_Elementary_Schools=subset(Another_Variable_Elementary_Schools,Another_Variable_Elementary_Schools$Var2==1)
Another_Variable_Elementary_Schools$Var2=NULL
colnames(Another_Variable_Elementary_Schools)<-c("Elementary","Seen or Smelt Mold or Musty in Asthmatic Child home")

Frequency_Elementary_School_Table=merge(Frequency_Elementary_School_Table,Another_Variable_Elementary_Schools)

Another_Variable_Elementary_Schools=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$C_ROACH))
Another_Variable_Elementary_Schools=subset(Another_Variable_Elementary_Schools,Another_Variable_Elementary_Schools$Var2==1)
Another_Variable_Elementary_Schools$Var2=NULL
colnames(Another_Variable_Elementary_Schools)<-c("Elementary","Seen Cockroach in Asthmatic Child home")

Frequency_Elementary_School_Table=merge(Frequency_Elementary_School_Table,Another_Variable_Elementary_Schools)

Another_Variable_Elementary_Schools=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$C_RODENT))
Another_Variable_Elementary_Schools=subset(Another_Variable_Elementary_Schools,Another_Variable_Elementary_Schools$Var2==1)
Another_Variable_Elementary_Schools$Var2=NULL
colnames(Another_Variable_Elementary_Schools)<-c("Elementary","Seen Rodent in Asthmatic Child home")

Frequency_Elementary_School_Table=merge(Frequency_Elementary_School_Table,Another_Variable_Elementary_Schools)

Another_Variable_Elementary_Schools=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$S_INSIDE))
Another_Variable_Elementary_Schools=subset(Another_Variable_Elementary_Schools,Another_Variable_Elementary_Schools$Var2==1)
Another_Variable_Elementary_Schools$Var2=NULL
colnames(Another_Variable_Elementary_Schools)<-c("Elementary","Smoked inside Asthmatic Child home")

Frequency_Elementary_School_Table=merge(Frequency_Elementary_School_Table,Another_Variable_Elementary_Schools)

#Merge the table of variables by school zone to map of school zones and transform projection for maps
elementary_school_zones=merge(elementary_school_zones,Frequency_Elementary_School_Table)
elementary_school_zones=st_transform(elementary_school_zones,crs='+proj=longlat +datum=WGS84')

#create map for elementary schools with asthma symptom variables
library(leaflet)
palAsthmaElementary <- colorNumeric(
  palette = "YlOrRd",
  domain = c(elementary_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,elementary_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms)
)

popup<-paste0("School: ",elementary_school_zones$Elementary,"<br>",
              "Children who had an asthma attack or episode in past year: ",
              elementary_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,"<br>",
              "Children who went to the ER for asthma symptoms in past year: ",
              elementary_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms)

map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                      dragging = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children with Asthma Attack or Episode in past 12 months",
              smoothFactor = 0.2,
              popup=popup)%>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children who have visited the ER because of asthma symptoms",
              smoothFactor = 0.2,
              popup=popup)%>%
  addLayersControl(
    baseGroups=c("Children with Asthma Attack or Episode in past 12 months","Children who have visited the ER because of asthma symptoms"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaElementary, 
            values = c(elementary_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,elementary_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms),
            position = "bottomleft")
map

saveRDS(map,"Elementary_School_Severity_Map.rds")

#table variables by middle schools
Frequency_Middle_School_Table=as.data.frame(table(middle_school_kids$School,middle_school_kids$EPIS_12M))
Frequency_Middle_School_Table=subset(Frequency_Middle_School_Table,Frequency_Middle_School_Table$Var2==1)
Frequency_Middle_School_Table$Var2=NULL
colnames(Frequency_Middle_School_Table)<-c("Middle_Sch","Children with Asthma Attack or Episode in past 12 months")

ER_Visit_Middle_Schools=as.data.frame(table(middle_school_kids$School,middle_school_kids$ER_VISIT))
ER_Visit_Middle_Schools=subset(ER_Visit_Middle_Schools,ER_Visit_Middle_Schools$Var2==1)
ER_Visit_Middle_Schools$Var2=NULL
colnames(ER_Visit_Middle_Schools)<-c("Middle_Sch","Children who have visited the Er because of asthma symptoms")

Frequency_Middle_School_Table=merge(Frequency_Middle_School_Table,ER_Visit_Middle_Schools)

Middle_school_kids=middle_school_kids
Another_Variable_Middle_Schools=as.data.frame(table(Middle_school_kids$School,Middle_school_kids$COOK_GAS))
Another_Variable_Middle_Schools=subset(Another_Variable_Middle_Schools,Another_Variable_Middle_Schools$Var2==1)
Another_Variable_Middle_Schools$Var2=NULL
colnames(Another_Variable_Middle_Schools)<-c("Middle_Sch","Gas Used in Cooking in Asthmatic Child home")

Frequency_Middle_School_Table=merge(Frequency_Middle_School_Table,Another_Variable_Middle_Schools)

Another_Variable_Middle_Schools=as.data.frame(table(Middle_school_kids$School,Middle_school_kids$ENV_MOLD))
Another_Variable_Middle_Schools=subset(Another_Variable_Middle_Schools,Another_Variable_Middle_Schools$Var2==1)
Another_Variable_Middle_Schools$Var2=NULL
colnames(Another_Variable_Middle_Schools)<-c("Middle_Sch","Seen or Smelt Mold or Musty in Asthmatic Child home")

Frequency_Middle_School_Table=merge(Frequency_Middle_School_Table,Another_Variable_Middle_Schools)

Another_Variable_Middle_Schools=as.data.frame(table(Middle_school_kids$School,Middle_school_kids$C_ROACH))
Another_Variable_Middle_Schools=subset(Another_Variable_Middle_Schools,Another_Variable_Middle_Schools$Var2==1)
Another_Variable_Middle_Schools$Var2=NULL
colnames(Another_Variable_Middle_Schools)<-c("Middle_Sch","Seen Cockroach in Asthmatic Child home")

Frequency_Middle_School_Table=merge(Frequency_Middle_School_Table,Another_Variable_Middle_Schools)

Another_Variable_Middle_Schools=as.data.frame(table(Middle_school_kids$School,Middle_school_kids$C_RODENT))
Another_Variable_Middle_Schools=subset(Another_Variable_Middle_Schools,Another_Variable_Middle_Schools$Var2==1)
Another_Variable_Middle_Schools$Var2=NULL
colnames(Another_Variable_Middle_Schools)<-c("Middle_Sch","Seen Rodent in Asthmatic Child home")

Frequency_Middle_School_Table=merge(Frequency_Middle_School_Table,Another_Variable_Middle_Schools)

Another_Variable_Middle_Schools=as.data.frame(table(Middle_school_kids$School,Middle_school_kids$S_INSIDE))
Another_Variable_Middle_Schools=subset(Another_Variable_Middle_Schools,Another_Variable_Middle_Schools$Var2==1)
Another_Variable_Middle_Schools$Var2=NULL
colnames(Another_Variable_Middle_Schools)<-c("Middle_Sch","Smoked inside Asthmatic Child home")

Frequency_Middle_School_Table=merge(Frequency_Middle_School_Table,Another_Variable_Middle_Schools)

middle_school_zones=merge(middle_school_zones,Frequency_Middle_School_Table)
middle_school_zones=st_transform(middle_school_zones,crs='+proj=longlat +datum=WGS84')

#create middle school map of symptoms
palAsthmaMiddle <- colorNumeric(
  palette = "YlOrRd",
  domain = c(middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,middle_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms)
)

popup<-paste0("School: ",middle_school_zones$Middle_Sch,"<br>",
              "Children who had an asthma attack or episode in past year: ",
              middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,"<br>",
              "Children who went to the ER for asthma symptoms in past year: ",
              middle_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms)


map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                      dragging = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children with Asthma Attack or Episode in past 12 months",
              smoothFactor = 0.2,
              popup=popup)%>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children who have visited the ER because of asthma symptoms",
              smoothFactor = 0.2,
              popup=popup)%>%
  addLayersControl(
    baseGroups=c("Children with Asthma Attack or Episode in past 12 months","Children who have visited the ER because of asthma symptoms"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaMiddle, 
            values = c(middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,middle_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms),
            position = "bottomleft")
map

saveRDS(map,"Middle_School_Severity_Map.rds")

#table high school variables
Frequency_High_School_Table=as.data.frame(table(high_school_kids$School,high_school_kids$EPIS_12M))
Frequency_High_School_Table=subset(Frequency_High_School_Table,Frequency_High_School_Table$Var2==1)
Frequency_High_School_Table$Var2=NULL
colnames(Frequency_High_School_Table)<-c("High_Schoo","Children with Asthma Attack or Episode in past 12 months")

ER_Visit_High_Schools=as.data.frame(table(high_school_kids$School,high_school_kids$ER_VISIT))
ER_Visit_High_Schools=subset(ER_Visit_High_Schools,ER_Visit_High_Schools$Var2==1)
ER_Visit_High_Schools$Var2=NULL
colnames(ER_Visit_High_Schools)<-c("High_Schoo","Children who have visited the Er because of asthma symptoms")

Frequency_High_School_Table=merge(Frequency_High_School_Table,ER_Visit_High_Schools)

High_school_kids=high_school_kids
Another_Variable_High_Schools=as.data.frame(table(High_school_kids$School,High_school_kids$COOK_GAS))
Another_Variable_High_Schools=subset(Another_Variable_High_Schools,Another_Variable_High_Schools$Var2==1)
Another_Variable_High_Schools$Var2=NULL
colnames(Another_Variable_High_Schools)<-c("High_Schoo","Gas Used in Cooking in Asthmatic Child home")

Frequency_High_School_Table=merge(Frequency_High_School_Table,Another_Variable_High_Schools)

Another_Variable_High_Schools=as.data.frame(table(High_school_kids$School,High_school_kids$ENV_MOLD))
Another_Variable_High_Schools=subset(Another_Variable_High_Schools,Another_Variable_High_Schools$Var2==1)
Another_Variable_High_Schools$Var2=NULL
colnames(Another_Variable_High_Schools)<-c("High_Schoo","Seen or Smelt Mold or Musty in Asthmatic Child home")

Frequency_High_School_Table=merge(Frequency_High_School_Table,Another_Variable_High_Schools)

Another_Variable_High_Schools=as.data.frame(table(High_school_kids$School,High_school_kids$C_ROACH))
Another_Variable_High_Schools=subset(Another_Variable_High_Schools,Another_Variable_High_Schools$Var2==1)
Another_Variable_High_Schools$Var2=NULL
colnames(Another_Variable_High_Schools)<-c("High_Schoo","Seen Cockroach in Asthmatic Child home")

Frequency_High_School_Table=merge(Frequency_High_School_Table,Another_Variable_High_Schools)

Another_Variable_High_Schools=as.data.frame(table(High_school_kids$School,High_school_kids$C_RODENT))
Another_Variable_High_Schools=subset(Another_Variable_High_Schools,Another_Variable_High_Schools$Var2==1)
Another_Variable_High_Schools$Var2=NULL
colnames(Another_Variable_High_Schools)<-c("High_Schoo","Seen Rodent in Asthmatic Child home")

Frequency_High_School_Table=merge(Frequency_High_School_Table,Another_Variable_High_Schools)

Another_Variable_High_Schools=as.data.frame(table(High_school_kids$School,High_school_kids$S_INSIDE))
Another_Variable_High_Schools=subset(Another_Variable_High_Schools,Another_Variable_High_Schools$Var2==1)
Another_Variable_High_Schools$Var2=NULL
colnames(Another_Variable_High_Schools)<-c("High_Schoo","Smoked inside Asthmatic Child home")

Frequency_High_School_Table=merge(Frequency_High_School_Table,Another_Variable_High_Schools)

High_school_zones=merge(high_school_zones,Frequency_High_School_Table)
High_school_zones=st_transform(High_school_zones,crs='+proj=longlat +datum=WGS84')

high_school_zones=merge(high_school_zones,Frequency_High_School_Table)
high_school_zones=st_transform(high_school_zones,crs='+proj=longlat +datum=WGS84')

#Create High School map of symptoms
palAsthmaHigh <- colorNumeric(
  palette = "YlOrRd",
  domain = c(high_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,high_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms)
)
popup<-paste0("School: ",high_school_zones$High_Schoo,"<br>",
              "Children who had an asthma attack or episode in past year: ",
              high_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,"<br>",
              "Children who went to the ER for asthma symptoms in past year: ",
              high_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms)


map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                      dragging = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = high_school_zones, 
              fillColor = ~palAsthmaHigh(high_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children with Asthma Attack or Episode in past 12 months",
              smoothFactor = 0.2,
              popup=popup)%>%
  addPolygons(data = high_school_zones, 
              fillColor = ~palAsthmaHigh(high_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children who have visited the ER because of asthma symptoms",
              smoothFactor = 0.2,
              popup=popup)%>%
  addLayersControl(
    baseGroups=c("Children with Asthma Attack or Episode in past 12 months","Children who have visited the ER because of asthma symptoms"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaHigh, 
            values = c(high_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,high_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms),
            position = "bottomleft")
map

saveRDS(map,"High_School_Severity_Map.rds")

#Make maps by environmental hazards and school zones

palAsthmaElementary <- colorNumeric(
  palette = "YlOrRd",
  domain = c(elementary_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home,elementary_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home,elementary_school_zones$Seen.Cockroach.in.Asthmatic.Child.home,elementary_school_zones$Seen.Rodent.in.Asthmatic.Child.home,elementary_school_zones$Smoked.inside.Asthmatic.Child.home)
)

map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                      dragging = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Gas Stove in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Mold in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Seen.Cockroach.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Cockroach in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Seen.Rodent.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Rodent in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Smoked.inside.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Smoked in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addLayersControl(
    baseGroups=c("Gas Stove in Asthmatic Child's Home","Mold in Asthmatic Child's Home","Cockroach in Asthmatic Child's Home","Rodent in Asthmatic Child's Home","Smoked in Asthmatic Child's Home"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaElementary, 
            values = c(elementary_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home,elementary_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home,elementary_school_zones$Seen.Cockroach.in.Asthmatic.Child.home,elementary_school_zones$Seen.Rodent.in.Asthmatic.Child.home,elementary_school_zones$Smoked.inside.Asthmatic.Child.home),
            position = "bottomleft")
map

saveRDS(map,"Elementary_School_Hazards_Map.rds")

palAsthmaMiddle <- colorNumeric(
  palette = "YlOrRd",
  domain = c(middle_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home,middle_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home,middle_school_zones$Seen.Cockroach.in.Asthmatic.Child.home,middle_school_zones$Seen.Rodent.in.Asthmatic.Child.home,middle_school_zones$Smoked.inside.Asthmatic.Child.home)
)

map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                      dragging = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Gas Stove in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Mold in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Seen.Cockroach.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Cockroach in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Seen.Rodent.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Rodent in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Smoked.inside.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Smoked in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addLayersControl(
    baseGroups=c("Gas Stove in Asthmatic Child's Home","Mold in Asthmatic Child's Home","Cockroach in Asthmatic Child's Home","Rodent in Asthmatic Child's Home","Smoked in Asthmatic Child's Home"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaMiddle, 
            values = c(middle_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home,middle_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home,middle_school_zones$Seen.Cockroach.in.Asthmatic.Child.home,middle_school_zones$Seen.Rodent.in.Asthmatic.Child.home,middle_school_zones$Smoked.inside.Asthmatic.Child.home),
            position = "bottomleft")
map

saveRDS(map,"Middle_School_Hazards_Map.rds")

palAsthmaHigh <- colorNumeric(
  palette = "YlOrRd",
  domain = c(High_school_zones$Seen.Cockroach.in.Asthmatic.Child.home,High_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home,High_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home,High_school_zones$Seen.Rodent.in.Asthmatic.Child.home,High_school_zones$Smoked.inside.Asthmatic.Child.home)
)

map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                      dragging = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = High_school_zones, 
              fillColor = ~palAsthmaHigh(High_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Gas Stove in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = High_school_zones, 
              fillColor = ~palAsthmaHigh(High_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Mold in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = High_school_zones, 
              fillColor = ~palAsthmaHigh(High_school_zones$Seen.Cockroach.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Cockroach in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = High_school_zones, 
              fillColor = ~palAsthmaHigh(High_school_zones$Seen.Rodent.in.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Rodent in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addPolygons(data = High_school_zones, 
              fillColor = ~palAsthmaHigh(High_school_zones$Smoked.inside.Asthmatic.Child.home), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Smoked in Asthmatic Child's Home",
              smoothFactor = 0.2)%>%
  addLayersControl(
    baseGroups=c("Gas Stove in Asthmatic Child's Home","Mold in Asthmatic Child's Home","Cockroach in Asthmatic Child's Home","Rodent in Asthmatic Child's Home","Smoked in Asthmatic Child's Home"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaHigh, 
            values = c(High_school_zones$Seen.Cockroach.in.Asthmatic.Child.home,High_school_zones$Gas.Used.in.Cooking.in.Asthmatic.Child.home,High_school_zones$Seen.or.Smelt.Mold.or.Musty.in.Asthmatic.Child.home,High_school_zones$Seen.Rodent.in.Asthmatic.Child.home,High_school_zones$Smoked.inside.Asthmatic.Child.home),
            position = "bottomleft")
map

saveRDS(map,"High_School_Hazards_Map.rds")

saveRDS(Frequency_Elementary_School_Table,"Frequency_Elementary_School_Table.RDS")
saveRDS(Frequency_Middle_School_Table,"Frequency_Middle_School_Table.RDS")
saveRDS(Frequency_High_School_Table,"Frequency_High_School_Table.RDS")

