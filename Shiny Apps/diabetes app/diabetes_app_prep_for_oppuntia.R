#We will do all simulations on oppuntia and then just use subsets in the app
syntheticdataset=readRDS("complete_sample_set.RDS")

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

#Now for simulating development of diabetes with and without intervention using numbers from
#CDC's IMPACT app for the Diabetes Prevention Program

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
#Assuming everyone could make it to the class we will simulate who would choose to participate or not

would_participate=function(participation_rate,seed){
  set.seed(seed)
  would_participate=sample(c("yes","no"),size=1,prob=c(participation_rate/100,(1-participation_rate/100)))
  return(would_participate)
}

part_of_set_of_interest$would_participate=mapply(would_participate,rep(participation_rate,nrow(part_of_set_of_interest)),1:nrow(part_of_set_of_interest))

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
                                                                        part_of_set_of_interest$would_participate,rep(year1_risk_reduction,nrow(part_of_set_of_interest)),
                                                                        1:nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year2=mapply(diabetes_with_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year1,
                                                                        part_of_set_of_interest$would_participate,rep(year2_risk_reduction,nrow(part_of_set_of_interest)),
                                                                        1:nrow(part_of_set_of_interest)+nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year3=mapply(diabetes_with_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year2,
                                                                        part_of_set_of_interest$would_participate,rep(year3_risk_reduction,nrow(part_of_set_of_interest)),
                                                                        1:nrow(part_of_set_of_interest)+2*nrow(part_of_set_of_interest))
#rest can be simulated with function without intervention as there is no longer a risk reduction
part_of_set_of_interest$with_intervention_develop_diabetes_year4=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year3,1:nrow(part_of_set_of_interest)+4*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year5=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year4,1:nrow(part_of_set_of_interest)+5*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year6=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year5,1:nrow(part_of_set_of_interest)+6*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year7=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year6,1:nrow(part_of_set_of_interest)+7*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year8=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year7,1:nrow(part_of_set_of_interest)+8*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year9=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year8,1:nrow(part_of_set_of_interest)+9*nrow(part_of_set_of_interest))
part_of_set_of_interest$with_intervention_develop_diabetes_year10=mapply(diabetes_without_intervention,incidence_rate_of_diabetes_vector,part_of_set_of_interest$with_intervention_develop_diabetes_year9,1:nrow(part_of_set_of_interest)+10*nrow(part_of_set_of_interest))

saveRDS(part_of_set_of_interest,file="prediabetes_simulation_for_app.RDS")