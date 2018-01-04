#First get the super neighborhood for each house

library(sf)
#Read in Super Neighborhoods
#data from:
#http://cohgis-mycity.opendata.arcgis.com/datasets/super-neighborhoods
super_neighborhoods=st_read("Super_Neighborhoods.shp")

#Read in cleaned HCAD parcels
validparcels=readRDS("validparcels.RDS")

#Get Super Neighborhood for each parcel
#first place in same CRS
super_neighborhoods <- st_transform(super_neighborhoods,st_crs(validparcels))
#find neighborhood for each parcel replace with placeholder if none exists and add as column to validparcel
Super_Neighborhood_for_parcel=st_within(validparcels,super_neighborhoods)
Super_Neighborhood_for_parcel_unlisted=rapply(Super_Neighborhood_for_parcel,function(x) ifelse(length(x)==0,9999999999999999999,x), how = "replace")
Super_Neighborhood_for_parcel_unlisted=unlist(Super_Neighborhood_for_parcel_unlisted)
validparcels$Super_Neighborhood=super_neighborhoods$SNBNAME[Super_Neighborhood_for_parcel_unlisted]
#I really only need the Acoount number and neighboorhood from validparcels
validparcels=validparcels[c("HCAD_NUM","Super_Neighborhood")]

#Now add Super Neighboorhood to simulated households
#load simulated households
syntheticdataset=readRDS("prediabetes_simulation_for_app_not_matching_insurance.RDS")
#merge to get superneighborhoods
syntheticdataset=merge(syntheticdataset,validparcels)

#Start tabling for map
Frequency_Table=as.data.frame(table(syntheticdataset$Super_Neighborhood,syntheticdataset$pre_diabetes))
Frequency_Table$Var2=NULL
colnames(Frequency_Table)<-c("Super_Neighborhood","pre-diabetes")

Another_Variable=as.data.frame(table(syntheticdataset$Super_Neighborhood,syntheticdataset$would_participate))
Another_Variable=subset(Another_Variable,Another_Variable$Var2=="yes")
Another_Variable$Var2=NULL
colnames(Another_Variable)<-c("Super_Neighborhood","would_participate")
Frequency_Table=merge(Frequency_Table,Another_Variable)

get_number_of_cases_and_costs=function(develop_diabetes,cost_for_year_of_diagnosis=6424,cost_every_year_after=3900){
  cases_that_year=sum(develop_diabetes=="yes"|develop_diabetes=="already developed")
  costs_that_year=cost_for_year_of_diagnosis*sum(develop_diabetes=="yes")+cost_every_year_after*sum(develop_diabetes=="already developed")
  return(c(cases_that_year,costs_that_year))
}



get_number_of_cases_and_costs_for_10_years=function(part_of_set_of_interest,cost_for_year_of_diagnosis=6424,cost_every_year_after=3900,cost_per_participant=417){
  #without intervention
  #first find correct column numbers
  correct_column_number=grep("without_intervention_develop_diabetes_year10", colnames(part_of_set_of_interest))
  without_intervention_cases_costs=sapply(part_of_set_of_interest[(correct_column_number-9):correct_column_number],get_number_of_cases_and_costs,cost_for_year_of_diagnosis,cost_every_year_after)
  row.names(without_intervention_cases_costs)<-c("cumulative_cases_of_diabetes_without_intervention","cumlative_costs_without_intervention")
  without_intervention_cases_costs=as.data.frame(without_intervention_cases_costs)
  colnames(without_intervention_cases_costs)=paste0("year",1:10)
  #And with intervention
  correct_column_number=grep("with_intervention_develop_diabetes_year10", colnames(part_of_set_of_interest))
  with_intervention_cases_costs=sapply(part_of_set_of_interest[(correct_column_number-9):correct_column_number],get_number_of_cases_and_costs,cost_for_year_of_diagnosis,cost_every_year_after)
  row.names(with_intervention_cases_costs)<-c("cumulative_cases_of_diabetes_with_intervention","cumlative_costs_with_intervention")
  with_intervention_cases_costs=as.data.frame(with_intervention_cases_costs)
  colnames(with_intervention_cases_costs)=paste0("year",1:10)
  with_intervention_cases_costs["cumulative_costs_with_intervention","year1"]<-with_intervention_cases_costs["cumulative_costs_with_intervention","year1"]+sum(part_of_set_of_interest$would_participate=="yes")*cost_per_participant
  
  cases_costs=rbind(without_intervention_cases_costs,with_intervention_cases_costs)
  net_costs=with_intervention_cases_costs[2,]-without_intervention_cases_costs[2,]
  QALYgained=0.05*(without_intervention_cases_costs[1,]-with_intervention_cases_costs[1,])
  for(year in 2:10){
    QALYgained[year]=QALYgained[year]+QALYgained[year-1]
  }
  net_costs=cbind.data.frame(split(unlist(net_costs),1:10))
  colnames(net_costs)=paste0("net_costs_year",1:10)
  
  QALYgained=cbind.data.frame(split(unlist(QALYgained),1:10))
  colnames(QALYgained)<-paste0("QALY_gained_year",1:10)
  
  ICERS=net_costs/QALYgained
  ICERS=cbind.data.frame(split(unlist(ICERS),1:10))
  colnames(ICERS)=paste0("ICERS_year",1:10)
  
  final_frame=cbind(net_costs,ICERS,QALYgained)
  return(final_frame)
}

More_Variables_by_Neighborhood=data.frame()
for(Neighborhoods in super_neighborhoods$SNBNAME){
  part_of_set_of_interest<-subset(syntheticdataset,syntheticdataset$Super_Neighborhood==Neighborhoods)
  variables_to_graph=get_number_of_cases_and_costs_for_10_years(part_of_set_of_interest)
  More_Variables_by_Neighborhood=rbind(More_Variables_by_Neighborhood,variables_to_graph)
}

row.names(More_Variables_by_Neighborhood)<-NULL
More_Variables_by_Neighborhood$Super_Neighborhood=super_neighborhoods$SNBNAME

Frequency_Table=merge(Frequency_Table,More_Variables_by_Neighborhood)
Frequency_Table$SNBNAME=Frequency_Table$Super_Neighborhood
#Merge Frequency table with map
super_neighborhoods<-merge(super_neighborhoods,Frequency_Table)
super_neighborhoods=st_transform(super_neighborhoods,crs='+proj=longlat +datum=WGS84')

#Make choropleth for prediabetes and number of participants

library(leaflet)
#Color Scale
palPrediabetes <- colorNumeric(
  palette = "YlOrRd",
  domain = super_neighborhoods$pre.diabetes
)

map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                      dragging = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = super_neighborhoods, 
              fillColor = ~palPrediabetes(super_neighborhoods$pre.diabetes), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Prediabetes",
              smoothFactor = 0.2)%>%
  addPolygons(data = super_neighborhoods, 
              fillColor = ~palPrediabetes(super_neighborhoods$would_participate), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1,
              group="Participants",
              smoothFactor = 0.2)%>%
  addLayersControl(
    baseGroups=c("Prediabetes","Participants"),
    options=layersControlOptions(collapsed=FALSE)
  )%>%
  addLegend(pal = palPrediabetes, 
            values = super_neighborhoods$pre.diabetes,
            position = "bottomleft")
map

saveRDS(map,"super_neighborhoods_prediabetes.rds")

palPrediabetes <- colorNumeric(
  palette = "YlOrRd",
  domain = c(-500000,500000)
)

easier_to_refer_to=as.data.frame(super_neighborhoods)
for(year in 1:10){
  net_costs=paste0("net_costs_year",year)
  ICERS=paste0("ICERS_year",year)
  QALYgained=paste0("QALY_gained_year",year)
  map<-leaflet(options = leafletOptions(zoomControl = FALSE,
                                        dragging = FALSE)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = super_neighborhoods, 
                fillColor = ~palPrediabetes(easier_to_refer_to[net_costs]), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1,
                group="Net Costs",
                smoothFactor = 0.2)%>%
    addPolygons(data = super_neighborhoods, 
                fillColor = ~palPrediabetes(easier_to_refer_to[QALYgained]), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1,
                group="Quality of Life Years Gained",
                smoothFactor = 0.2)%>%
    addPolygons(data = super_neighborhoods, 
                fillColor = ~palPrediabetes(easier_to_refer_to[ICERS]), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1,
                group="ICERS",
                smoothFactor = 0.2)%>%
    addLayersControl(
      baseGroups=c("Net Costs","Quality of Life Years Gained","ICERS"),
      options=layersControlOptions(collapsed=FALSE)
    )%>%
    addLegend(pal = palPrediabetes, 
              values = c(-50000:50000),
              position = "bottomleft")
  map
  
  filename=paste0("super_neighborhood_DPP_projection_year",year,".RDS")
  saveRDS(map,filename)
}
