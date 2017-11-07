#read in data set
syntheticdataset=readRDS("fake_complete_set.rds")

syntheticdataset$X.1=NULL
syntheticdataset$X=NULL
syntheticdataset$X1=NULL
syntheticdataset=syntheticdataset[,1:65]#I don't ned HCAD variables except geometry which can't save properly
syntheticdataset=syntheticdataset[!is.na(syntheticdataset$householdID),]
#Need to simulate diabetes and prediabetes
#copied directly from script dansdiabetesandprediabetes.R
dans_diabetes <- function(member,sex,education.attainment,race,seedy){
  #from table 1a in the appendix of the National Diabetes Statistics Report 2017
  if(member=="Child"){
    dans_diabetes=NA
    return(dans_diabetes)
  }
  else{
    chance=0.115
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance+0.008,chance-0.007)
    #Change Chance by Education
    if(education.attainment %in% c("Less than 9th grade","9th to 12th grade, no diploma")){
      chance=chance+0.04
    }
    if(education.attainment=="High School Graduate"){
      chance=chance+0.02
    }
    if(education.attainment %in% c("Associate's degree","Some College, no degree","Graduate or Professional Degree","Bachelor's Degree")){
      chance=chance-0.019
    }
    #Change chance by Race
    if (race=="White"){chance=chance-0.022}
    if (race=="Black or African American"){chance=chance+0.062}
    if (race=="Hispanic or Latino"){chance=chance+0.049}
    if (race=="Asian"){chance=chance+0.045}
    #And finally sample
    set.seed(seedy)
    dans_diabetes=sample(c("yes","no"),1,prob=c(chance,1-chance))
    return(dans_diabetes)
  }
}

syntheticdataset$dans_diabetes=mapply(dans_diabetes,syntheticdataset$member,syntheticdataset$sex,syntheticdataset$education.attainment,syntheticdataset$race,1:nrow(syntheticdataset))

dans_prediabetes <- function(member,dans_diabetes,sex,education.attainment,race,seedy){
  #from table 3a in the appendix of the National Diabetes Statistics Report 2017
  if(member=="Child"|dans_diabetes=="yes"){
    dans_diabetes=NA
    return(dans_diabetes)
  }
  else{
    chance=33
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance+3.6,chance-3.7)
    #Change Chance by Education
    if(education.attainment %in% c("Less than 9th grade","9th to 12th grade, no diploma")){
      chance=chance+4.6
    }
    if(education.attainment=="High School Graduate"){
      chance=chance+4
    }
    if(education.attainment %in% c("Associate's degree","Some College, no degree","Graduate or Professional Degree","Bachelor's Degree")){
      chance=chance-2.6
    }
    #Change chance by Race
    if (race=="White"){chance=chance-1.5}
    if (race=="Black or African American"){chance=chance+3.3}
    if (race=="Hispanic or Latino"){chance=chance-1.3}
    if (race=="Asian"){chance=chance+2.7}
    #And finally sample
    set.seed(seedy)
    dans_diabetes=sample(c("yes","no"),1,prob=c(chance/(100),1-(chance/(100))))
    return(dans_diabetes)
  }
}

syntheticdataset$dans_prediabetes=mapply(dans_prediabetes,syntheticdataset$member,syntheticdataset$dans_diabetes,syntheticdataset$sex,syntheticdataset$education.attainment,syntheticdataset$race,1:nrow(syntheticdataset))

#Ok now let's start looking at the app the CDC gave us

#First we need to simulate who with prediabetes is going to get diabetes
#the CDC Impact app allows the user to establish and incidence rate and suggests to keep it between 1-6%

incidence_rate_of_diabetes=3

#Ask Dan if he wants to include the cost of a screening

#We haven't simulated weight so I don't want to simulate weight loss, we could do this by matching with either
#the BRSSR data or NHANES may have something too if we wanted to go that route

#They separate out people previously screened and screening again, but for starters we will just start with a participation
#rate and ask more questions later

participation_rate=35 #start at 35

#the reduction of risk in diabetes was related to the weight loss, but
#as I chose not to simulate that we will use averages from table 14

year1_risk_reduction=35.4
year2_risk_reduction=19.3
year3_risk_reduction=15.3
year4_and_on=0

#we will assume a program cost of 417 but will allow the user to change that

cost_per_participant=417

#Medical Costs
#refer to medical costs from impact methodology
#will use simpler model for now

cost_for_year_of_diagnosis=6424
cost_every_year_after=3900

#Days lost

days_lost=3.3

#Time to start simulating
#first subset people with prediabetes

part_of_set_of_interest=subset(syntheticdataset,syntheticdataset$dans_prediabetes=="yes")

diabetes_without_intervention <- function(incidence_rate_of_diabetes,diabetes,seed){
  if(diabetes=="yes"|diabetes=="already developed"){
    develop_diabetes_this_year="already developed"
    return(develop_diabetes_this_year)
  }
  else
  set.seed(seed)
  develop_diabetes_this_year=sample(c("yes","no"),prob=c(incidence_rate_of_diabetes/100,(100-incidence_rate_of_diabetes)/100),size=1)
  return(develop_diabetes_this_year)
}

#Now simulate diabetes without intervention
incidence_rate_of_diabetes_vector=rep(incidence_rate_of_diabetes,nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year1=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,rep("no",nrow(part_of_set_of_interest)),1:nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year2=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year1,1:nrow(part_of_set_of_interest)+2*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year3=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year2,1:nrow(part_of_set_of_interest)+3*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year4=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year3,1:nrow(part_of_set_of_interest)+4*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year5=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year4,1:nrow(part_of_set_of_interest)+5*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year6=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year5,1:nrow(part_of_set_of_interest)+6*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year7=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year6,1:nrow(part_of_set_of_interest)+7*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year8=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year7,1:nrow(part_of_set_of_interest)+8*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year9=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year8,1:nrow(part_of_set_of_interest)+9*nrow(part_of_set_of_interest))
part_of_set_of_interest$without_intervention_develop_diabetes_year10=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$without_intervention_develop_diabetes_year9,1:nrow(part_of_set_of_interest)+10*nrow(part_of_set_of_interest))


#With intervention

#first we have to see who will be able to get to the classes
library(sf)
library(dplyr)

miles_from_location=1

#So read in all parcels to find address of input locations
parcels <- st_read("../hcadparcelstuff/Parcels/Parcels.shp")

parcels$valid=st_is_valid(parcels, reason = TRUE)
validparcels=filter(parcels,parcels$valid=="Valid Geometry")

#test an address
address=filter(validparcels,validparcels$LocAddr==toupper("14060 Dublin St"))

#is_house_close_enough <- function(address_of_class,address_of_possible_prticipant){
#  close_enough=ifelse(st_distance(address_of_class,address_of_possible_prticipant)<=1,"yes","no")
#  return(close_enough)
#}
#st_distance

validparcels$close_enough_to_participate=st_distance(validparcels,address)
#Convert to miles
validparcels$close_enough_to_participate2=validparcels$close_enough_to_participate/5280
validparcels$close_enough_to_participate3=as.numeric(as.character(validparcels$close_enough_to_participate2))
#If it's less than one it's good
validparcels$close_enough_to_participate_final=validparcels$close_enough_to_participate3
validparcels$close_enough_to_participate_final[validparcels$close_enough_to_participate_final<=miles_from_location]="yes"
validparcels$close_enough_to_participate_final[validparcels$close_enough_to_participate_final>=miles_from_location]="no"

for_merge_with_synthetic_data_set=data.frame(ACCOUNT=validparcels$HCAD_NUM,close_enough_to_participate=validparcels$close_enough_to_participate)
part_of_set_of_interest=merge(part_of_set_of_interest,for_merge_with_synthetic_data_set,by="ACCOUNT",all.x=TRUE)

will_participate=function(close_enough_to_participate,participation_rate,seed){
  if(close_enough_to_participate=="no"){
    will_participate="no"
    return(will_participate)
  }
  else
    set.seed(seed)
    will_participate=sample(c("yes","no"),size=1,prob=c(participation_rate/100,(1-participation_rate/100)))
    return(will_participate)
}

part_of_set_of_interest$will_participate=mapply(will_participate,part_of_set_of_interest$close_enough_to_participate,rep(participation_rate,
                                                              nrow(part_of_set_of_interest)),1:nrow(part_of_set_of_interest))

diabetes_with_intervention <- function(incidence_rate_of_diabetes,diabetes,will_participate,risk_reduction,seed){
  if(diabetes=="yes"|diabetes=="already developed"){
    develop_diabetes_this_year="already developed"
    return(develop_diabetes_this_year)
  }
  else
  if(will_participate=="no"){
    set.seed(seed)
    develop_diabetes_this_year=sample(c("yes","no"),prob=c(incidence_rate_of_diabetes/100,(100-incidence_rate_of_diabetes)/100),size=1)
    return(develop_diabetes_this_year)
  }
  else
    new_incidence_rate_of_diabetes=(incidence_rate_of_diabetes/100*risk_reduction/100)*100
    set.seed(seed)
  develop_diabetes_this_year=sample(c("yes","no"),prob=c(new_incidence_rate_of_diabetes/100,(100-new_incidence_rate_of_diabetes)/100),size=1)
  return(develop_diabetes_this_year)
}

part_of_set_of_interest$with_intervention_develop_diabetes_year1=mapply(diabetes_with_intervention,incidence_rate_of_diabetes_vector,rep("no",nrow(part_of_set_of_interest)),
                                                                      part_of_set_of_interest$will_participate,rep(year1_risk_reduction,nrow(part_of_set_of_interest)),
                                                                        1:nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year2=mapply(diabetes_with_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year1,
                                                                        part_of_set_of_interest$will_participate,rep(year2_risk_reduction,nrow(part_of_set_of_interest)),
                                                                        1:nrow(part_of_set_of_interest)+nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year3=mapply(diabetes_with_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year2,
                                                                        part_of_set_of_interest$will_participate,rep(year3_risk_reduction,nrow(part_of_set_of_interest)),
                                                                        1:nrow(part_of_set_of_interest)+2*nrow(part_of_set_of_interest))
#rest can be simulated with function without intervention as there is no longer a risk reduction
part_of_set_of_interest$with_intervention_develop_diabetes_year4=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year3,1:nrow(part_of_set_of_interest)+4*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year5=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year4,1:nrow(part_of_set_of_interest)+5*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year6=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year5,1:nrow(part_of_set_of_interest)+6*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year7=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year6,1:nrow(part_of_set_of_interest)+7*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year8=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year7,1:nrow(part_of_set_of_interest)+8*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year9=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year8,1:nrow(part_of_set_of_interest)+9*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year10=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year9,1:nrow(part_of_set_of_interest)+10*nrow(part_of_set_of_interest))

#Output participation
projected_participants=sum(part_of_set_of_interest$will_participate=="yes")

#Output cumalative number of cases of diabetes and costs

get_number_of_cases_and_costs=function(develop_diabetes,cost_for_year_of_diagnosis,cost_every_year_after,cost_per_participant){
  cases_that_year=sum(develop_diabetes=="yes"|develop_diabetes=="already developed")
  costs_that_year=cost_for_year_of_diagnosis*sum(develop_diabetes=="yes")+cost_every_year_after*sum(develop_diabetes=="no")+cost_per_participant*length(develop_diabetes)
  return(c(cases_that_year,costs_that_year))
}

#without intervention
#first find correct column numbers
correct_column_number=grep("without_intervention_develop_diabetes_year10", colnames(part_of_set_of_interest))
without_intervention_cases_costs=sapply(part_of_set_of_interest[(correct_column_number-9):correct_column_number],get_number_of_cases_and_costs,cost_for_year_of_diagnosis,cost_every_year_after,0)
row.names(without_intervention_cases_costs)<-c("cumulative_cases_of_diabetes_without_intervention","cumlative_costs_without_intervention")
without_intervention_cases_costs=as.data.frame(without_intervention_cases_costs)
colnames(without_intervention_cases_costs)=paste0("year",1:10)
#And with intervention
correct_column_number=grep("with_intervention_develop_diabetes_year10", colnames(part_of_set_of_interest))
with_intervention_cases_costs=sapply(part_of_set_of_interest[(correct_column_number-9):correct_column_number],get_number_of_cases_and_costs,cost_for_year_of_diagnosis,cost_every_year_after,cost_per_participant)
row.names(with_intervention_cases_costs)<-c("cumulative_cases_of_diabetes_with_intervention","cumlative_costs_with_intervention")
with_intervention_cases_costs=as.data.frame(with_intervention_cases_costs)
colnames(with_intervention_cases_costs)=paste0("year",1:10)

cases_costs=rbind(without_intervention_cases_costs,with_intervention_cases_costs)
net_costs=with_intervention_cases_costs[2,]-without_intervention_cases_costs[2,]


