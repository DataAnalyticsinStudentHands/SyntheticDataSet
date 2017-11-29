syntheticdataset=readRDS("complete_sample_set.RDS")
syntheticdataset$X.1=NULL
syntheticdataset$X1=NULL
syntheticdataset=syntheticdataset[,1:65]#I don't ned HCAD variables except geometry which can't save properly
syntheticdataset=syntheticdataset[!is.na(syntheticdataset$householdID),]

#Only really need the children
syntheticdataset=subset(syntheticdataset,syntheticdataset$member=="Child")

#Copy straight from dansdiabetesandprediabetes.R Script
#data from csv accessed at:
#https://www.cdc.gov/asthma/most_recent_data.htm
#used 8.4 % raw chance from prevalence adjusted for sex, age and race by changing the chance by the difference
#between the prevalence in that group and the overall national range
childhood.asthma <- function(member,sex,age,race,seedy){
  if(member!="Child"){
    childhood.asthma=NA
    return(childhood.asthma)
  }
  else{
    chance=0.084
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance+0.015,chance-0.015)
    #Change Chance by Age
    if(age=="Under 5"){
      chance=chance-0.037
    }
    if(age %in% c("5 to 9","10 to 14","15 to 17")){
      chance=chance+0.014
    }
    #Change chance by Race
    if (race=="White"){chance=chance-0.01}
    if (race=="Black or African American"){chance=chance+0.05}
    if (race=="Hispanic or Latino"){chance=chance-0.0004}
    
    #And finally sample
    set.seed(seedy)
    childhood.asthma=sample(c("yes","no"),1,prob=c(chance,1-chance))
    return(childhood.asthma)
  }
}

syntheticdataset$childhood.asthma=mapply(childhood.asthma,syntheticdataset$member,syntheticdataset$sex,syntheticdataset$age,syntheticdataset$race,syntheticdataset$X)

#Get Health information from National Housing Survey
#Table downloaded from
#https://www.census.gov/programs-surveys/ahs/data/interactive/ahstablecreator.html#?s_areas=a26420&s_year=m2015&s_tableName=TableS01&s_byGroup1=a7&s_byGroup2=a7&s_filterGroup1=t1&s_filterGroup2=g1
#In November

#health_problems_in_house=read.csv("healthsafetynumbers.csv")
#I can't find information that I want on the risk reduction for individual problems so while
#I think this data is cool I haven't quite found a use for it

#Instead I will rely on averages from these two academic papers
#https://www.ncbi.nlm.nih.gov/pubmed/?term=Effectiveness+of+Home-Based%2C+Multi-Trigger%2C+Multicomponent+Interventions+with+an+Environmental+Focus+for+Reducing+Asthma+Morbidity%3A+A+Community+Guide+Systematic+Review
#https://www.ncbi.nlm.nih.gov/pubmed/?term=Economic+Value+of+Home-Based%2C+Multi-Trigger%2C+Multicomponent+Interventions+with+an+Environmental+Focus+for+Reducing+Asthma+Morbidity%3A+A+Community+Guide+Systematic+Review

#Something else I started on and decided not to use
#participation_rate=0.35

#participating_households<-function(participation_rate,householdID){
#  set.seed(sub('.*\\.', '', householdID))
#  would_participate=sample(c("yes","no"),size=1,prob=c(participation_rate,1-participation_rate))
#}

#kids_with_asthma$would_participate=mapply(participating_households,participation_rate,kids_with_asthma$householdID)







#Chose instead to simulate asthma from BRFSS data and Asthma call back survey
#Scripts for this with accompanying data are in the file BRSSR
syntheticdataset=readRDS("asthma_simulation.RDS")


library(sf)
parcels <- st_read("../hcadparcelstuff/Parcels/Parcels.shp")

parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=subset(parcels,parcels$valid=="Valid Geometry")


#Read in data
buildingfeatures=read.table('../hcadparcelstuff/building_res.txt',sep="\t", header=FALSE, fill=TRUE,colClasses = "character",strip.white = TRUE,quote = "")#, ,colClasses = "character",stringsAsFactors = FALSE
#buildingfeatures=buildingfeatures[-(1:371770),]
colnames(buildingfeatures)=c("ACCOUNT", "USE_CODE", "BUILDING_NUMBER","IMPRV_TYPE","BUILDING_STYLE_CODE","CLASS_STRUCTURE","CLASS_STRUC_DESCRIPTION","DEPRECIATION_VALUE","CAMA_REPLACEMENT_COST","ACCRUED_DEPR_PCT","QUALITY","QUALITY_DESCRIPTION","DATE_ERECTED","EFFECTIVE_DATE","YR_REMODEL","YR_ROLL","APPRAISED_BY","APPRAISED_DATE","NOTE","IMPR_SQ_FT","ACTUAL_AREA","HEAT_AREA","GROSS_AREA","EFFECTIVE_AREA","BASE_AREA","PERIMETER","PERCENT_COMPLETE","NBHD_FACTOR","RCNLD","SIZE_INDEX","LUMP_SUM_ADJ")

#validparceldataframe=as.data.frame(validparcels)

#validparceldataframe2=merge(buildingfeatures,validparceldataframe,by.x="ACCOUNT",by.y="HCAD_NUM")

#Maybe this is better than that
library(tigris)
buildingfeatures$"HCAD_NUM"=buildingfeatures$ACCOUNT

validparcel2=geo_join(validparcels,buildingfeatures,by="HCAD_NUM",how="inner")

#Read in elementary school zones
elementary_school_zones <- st_read("HISD_Elementary_Boundary.shp")
#Put them in same CRS as Parcels
elementary_school_zones <- st_transform(elementary_school_zones,st_crs(validparcel2))

#Get elementary school for each parcel
ElementarySchoolforHCADParcels=st_within(validparcel2,elementary_school_zones)
ElementarySchoolforHCADParcelsunlisted=rapply(ElementarySchoolforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
ElementarySchoolforHCADParcelsunlisted=unlist(ElementarySchoolforHCADParcelsunlisted)

validparcel2$Elementary_School=elementary_school_zones$Elementary[ElementarySchoolforHCADParcelsunlisted]
#validparcels$TRACT=TXCensusTracts$TRACT[CensusTractforHCADParcelsunlisted]

#Read in middle school zones
middle_school_zones <- st_read("HISD_Middle_School_Boundary.shp")
#Put them in same CRS as Parcels
middle_school_zones <- st_transform(middle_school_zones,st_crs(validparcel2))

#Get elementary school for each parcel
MiddleSchoolforHCADParcels=st_within(validparcel2,middle_school_zones)
MiddleSchoolforHCADParcelsunlisted=rapply(MiddleSchoolforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
MiddleSchoolforHCADParcelsunlisted=unlist(MiddleSchoolforHCADParcelsunlisted)

validparcel2$Middle_School=middle_school_zones$Middle_Sch[MiddleSchoolforHCADParcelsunlisted]

#Read in high school zones
high_school_zones <- st_read("HISD_High_School_Boundary.shp")
#Put them in same CRS as Parcels
high_school_zones <- st_transform(high_school_zones,st_crs(validparcel2))

#Get elementary school for each parcel
HighSchoolforHCADParcels=st_within(validparcel2,high_school_zones)
HighSchoolforHCADParcelsunlisted=rapply(HighSchoolforHCADParcels,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
HighSchoolforHCADParcelsunlisted=unlist(HighSchoolforHCADParcelsunlisted)

validparcel2$High_School=high_school_zones$High_Schoo[HighSchoolforHCADParcelsunlisted]

rm(validparcels)
#rm(elementary_school_zones,middle_school_zones,high_school_zones)
rm(buildingfeatures,parcels,ElementarySchoolforHCADParcels,ElementarySchoolforHCADParcelsunlisted,HighSchoolforHCADParcels,HighSchoolforHCADParcelsunlisted,MiddleSchoolforHCADParcels,MiddleSchoolforHCADParcelsunlisted)

#Let's start by putting children in public schools
#Only interested in public school kids
syntheticdataset=subset(syntheticdataset,syntheticdataset$school.enrollment=="Public School")

elementary_school_kids=subset(syntheticdataset,syntheticdataset$age=="5 to 9")
elementary_schools_for_merge=data.frame(ACCOUNT=validparcel2$HCAD_NUM,School=validparcel2$Elementary_School)
elementary_school_kids=merge(elementary_school_kids,elementary_schools_for_merge,all.x = TRUE)

middle_school_kids=subset(syntheticdataset,syntheticdataset$age=="10 to 14")
middle_schools_for_merge=data.frame(ACCOUNT=validparcel2$HCAD_NUM,School=validparcel2$Middle_School)
middle_school_kids=merge(middle_school_kids,middle_schools_for_merge,all.x = TRUE)

high_school_kids=subset(syntheticdataset,syntheticdataset$age=="15 to 17")
high_schools_for_merge=data.frame(ACCOUNT=validparcel2$HCAD_NUM,School=validparcel2$High_School)
high_school_kids=merge(high_school_kids,high_schools_for_merge,all.x = TRUE)

kids_with_asthma=rbind(elementary_school_kids,middle_school_kids,high_school_kids)
kids_with_asthma=subset(kids_with_asthma,kids_with_asthma$childhood.asthma=="yes")
saveRDS(kids_with_asthma,"kids_with_asthma.RDS")

saveRDS(elementary_school_kids,"elementary_school_kids.RDS")
saveRDS(middle_school_kids,"middle_school_kids.RDS")
saveRDS(high_school_kids,"high_school_kids.RDS")

Frequency_Elementary_School_Table=as.data.frame(table(elementary_school_kids$School,elementary_school_kids$EPIS_12M))
#Frequency_Elementary_School_Table=as.data.frame.matrix(table(elementary_school_kids$School,elementary_school_kids$EPIS_12M))
#Only interested in kids who have had an asthma attack which is variable 1
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

elementary_school_zones=merge(elementary_school_zones,Frequency_Elementary_School_Table)
elementary_school_zones=st_transform(elementary_school_zones,crs='+proj=longlat +datum=WGS84')

library(leaflet)
palAsthmaElementary <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0,max(elementary_school_zones))
)
map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children with Asthma Attack or Episode in past 12 months",
              smoothFactor = 0.2)%>%
  addPolygons(data = elementary_school_zones, 
              fillColor = ~palAsthmaElementary(elementary_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children who have visited the ER because of asthma symptoms",
              smoothFactor = 0.2)%>%
  addLayersControl(
    baseGroups=c("Children with Asthma Attack or Episode in past 12 months","Children who have visited the ER because of asthma symptoms"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaElementary, 
            values = elementary_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,
            position = "bottomleft")
map

saveRDS(map,"Elementary_School_Severity_Map.rds")

Frequency_Middle_School_Table=as.data.frame(table(middle_school_kids$School,middle_school_kids$EPIS_12M))
#Frequency_Elementary_School_Table=as.data.frame.matrix(table(elementary_school_kids$School,elementary_school_kids$EPIS_12M))
#Only interested in kids who have had an asthma attack which is variable 1
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

palAsthmaMiddle <- colorNumeric(
  palette = "YlOrRd",
  domain = middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months
)
map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children with Asthma Attack or Episode in past 12 months",
              smoothFactor = 0.2)%>%
  addPolygons(data = middle_school_zones, 
              fillColor = ~palAsthmaMiddle(middle_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children who have visited the ER because of asthma symptoms",
              smoothFactor = 0.2)%>%
  addLayersControl(
    baseGroups=c("Children with Asthma Attack or Episode in past 12 months","Children who have visited the ER because of asthma symptoms"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaMiddle, 
            values = middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,
            position = "bottomleft")
map

saveRDS(map,"Middle_School_Severity_Map.rds")

Frequency_High_School_Table=as.data.frame(table(high_school_kids$School,high_school_kids$EPIS_12M))
#Frequency_Elementary_School_Table=as.data.frame.matrix(table(elementary_school_kids$School,elementary_school_kids$EPIS_12M))
#Only interested in kids who have had an asthma attack which is variable 1
Frequency_High_School_Table=subset(Frequency_High_School_Table,Frequency_High_School_Table$Var2==1)
Frequency_High_School_Table$Var2=NULL
colnames(Frequency_High_School_Table)<-c("High_Schoo","Children with Asthma Attack or Episode in past 12 months")

ER_Visit_High_Schools=as.data.frame(table(high_school_kids$School,high_school_kids$ER_VISIT))
ER_Visit_High_Schools=subset(ER_Visit_High_Schools,ER_Visit_High_Schools$Var2==1)
ER_Visit_High_Schools$Var2=NULL
colnames(ER_Visit_High_Schools)<-c("High_Schoo","Children who have visited the Er because of asthma symptoms")

Frequency_High_School_Table=merge(Frequency_High_School_Table,ER_Visit_High_Schools)

high_school_zones=merge(high_school_zones,Frequency_High_School_Table)
high_school_zones=st_transform(high_school_zones,crs='+proj=longlat +datum=WGS84')

palAsthmaHigh <- colorNumeric(
  palette = "YlOrRd",
  domain = high_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months
)
map<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = high_school_zones, 
              fillColor = ~palAsthmaHigh(high_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children with Asthma Attack or Episode in past 12 months",
              smoothFactor = 0.2)%>%
  addPolygons(data = high_school_zones, 
              fillColor = ~palAsthmaHigh(high_school_zones$Children.who.have.visited.the.Er.because.of.asthma.symptoms), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Children who have visited the ER because of asthma symptoms",
              smoothFactor = 0.2)%>%
  addLayersControl(
    baseGroups=c("Children with Asthma Attack or Episode in past 12 months","Children who have visited the ER because of asthma symptoms"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palAsthmaHigh, 
            values = high_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,
            position = "bottomleft")
map

saveRDS(map,"High_School_Severity_Map.rds")

map<-leaflet() %>%
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
            values = elementary_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,
            position = "bottomleft")
map

saveRDS(map,"Elementary_School_Hazards_Map.rds")

palAsthmaMiddle <- colorNumeric(
  palette = "YlOrRd",
  domain = middle_school_zones$Seen.Cockroach.in.Asthmatic.Child.home
)

map<-leaflet() %>%
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
            values = middle_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,
            position = "bottomleft")
map

saveRDS(map,"Middle_School_Hazards_Map.rds")

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

palAsthmaHigh <- colorNumeric(
  palette = "YlOrRd",
  domain = High_school_zones$Seen.Cockroach.in.Asthmatic.Child.home
)

map<-leaflet() %>%
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
            values = High_school_zones$Children.with.Asthma.Attack.or.Episode.in.past.12.months,
            position = "bottomleft")
map

saveRDS(map,"High_School_Hazards_Map.rds")
