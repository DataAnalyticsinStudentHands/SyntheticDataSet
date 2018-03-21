
getBRSSRdata_not_matching_with_health_insurance <- function(syntheticdataset,seed){
  set.seed(seed)
  
  BRSSR=readRDS('BRFSS_for_diabetes.rds')
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiaan or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
  education=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$RACE
  brace=c("2","3","4","5","6","7","1","8")
  #BRSSR$EDUCA
  beducation=c("2 and 3","3","4","5","5","6","6")
  #BRSSR$INCOME2
  bincome=c("1","2","3","4","5","5","6","6","6","7","7","8","8","8","8")
  
  finaldataset=data.frame()
  for (indexsex in 1:2){
    for (indexrace in 1:8){
      for (indexedu in 1:7){
        for (indexincome in 1:16){
          
          subsyntheticdataset=syntheticdataset[(syntheticdataset$sex==sex[indexsex])&(syntheticdataset$race==race[indexrace])&(syntheticdataset$education.attainment==education[indexedu])&(syntheticdataset$household.income==income[indexincome]),]
          
          subBRSSR=BRSSR[(BRSSR$SEX==bsex[indexsex])&(BRSSR$RACE==brace[indexrace])&(BRSSR$EDUCA==beducation[indexedu])&(BRSSR$INCOME2==bincome[indexincome])& !is.na(BRSSR$SEX)& !is.na(BRSSR$RACE)& !is.na(BRSSR$EDUCA)& !is.na(BRSSR$INCOME2),]
          if (nrow(subBRSSR)>0){
            subBRSSR$row.id.for.merge=1:nrow(subBRSSR)
            if (nrow(subsyntheticdataset)>0){
              subsyntheticdataset$row.id.for.merge=sample(1:nrow(subBRSSR),nrow(subsyntheticdataset),TRUE)
              subsyntheticdataset=merge(subBRSSR,subsyntheticdataset,by.x="row.id.for.merge",by.y="row.id.for.merge",all=FALSE,ll.x=FALSE,all.y=TRUE)
              subsyntheticdataset=subset(subsyntheticdataset,!is.na(subsyntheticdataset$household.type))
              finaldataset=rbind(finaldataset,subsyntheticdataset)
            }
          }
          
          
        }
      }
    }
  }
  finaldataset$SEX=NULL
  finaldataset$RACE=NULL
  finaldataset$EDUCA=NULL
  finaldataset$INCOME2=NULL
  finaldataset$row.id.for.merge=NULL
  return(finaldataset)
}

#with insurance
getBRSSRdata_with_health_insurance <- function(syntheticdataset,seed){
  set.seed(seed)
  
  BRSSR=readRDS('BRFSS_for_diabetes.rds')
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiaan or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
  education=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  insurance=c("public insurance","private insurance","no insurance")
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$RACE
  brace=c("2","3","4","5","6","7","1","8")
  #BRSSR$EDUCA
  beducation=c("2 and 3","3","4","5","5","6","6")
  #BRSSR$INCOME2
  bincome=c("1","2","3","4","5","5","6","6","6","7","7","8","8","8","8")
  #BRSSR$HLTHPLN
  binsurance=c("1","1","2")
  
  finaldataset=data.frame()
  for (indexsex in 1:2){
    for (indexrace in 1:8){
      for (indexedu in 1:7){
        for (indexincome in 1:16){
          for(indexinsurance in 1:2){
            subsyntheticdataset=syntheticdataset[(syntheticdataset$sex==sex[indexsex])&(syntheticdataset$race==race[indexrace])&(syntheticdataset$education.attainment==education[indexedu])&(syntheticdataset$household.income==income[indexincome])&&(syntheticdataset$health.insurance==insurance[indexinsurance]),]
            
            subBRSSR=BRSSR[(BRSSR$SEX==bsex[indexsex])&(BRSSR$health_insurance==binsurance[indexinsurance])&(BRSSR$RACE==brace[indexrace])&(BRSSR$EDUCA==beducation[indexedu])&(BRSSR$INCOME2==bincome[indexincome])& !is.na(BRSSR$health_insurance)& !is.na(BRSSR$SEX)& !is.na(BRSSR$RACE)& !is.na(BRSSR$EDUCA)& !is.na(BRSSR$INCOME2),]
            if (nrow(subBRSSR)>0){
              subBRSSR$row.id.for.merge=1:nrow(subBRSSR)
              if (nrow(subsyntheticdataset)>0){
                subsyntheticdataset$row.id.for.merge=sample(1:nrow(subBRSSR),nrow(subsyntheticdataset),TRUE)
                subsyntheticdataset=merge(subBRSSR,subsyntheticdataset,by.x="row.id.for.merge",by.y="row.id.for.merge",all=FALSE,ll.x=FALSE,all.y=TRUE)
                subsyntheticdataset=subset(subsyntheticdataset,!is.na(subsyntheticdataset$household.type))
                finaldataset=rbind(finaldataset,subsyntheticdataset)
              }
            } 
          }
          
          
        }
      }
    }
  }
  finaldataset$SEX=NULL
  finaldataset$RACE=NULL
  finaldataset$EDUCA=NULL
  finaldataset$INCOME2=NULL
  finaldataset$row.id.for.merge=NULL
  return(finaldataset)
}

#syntheticdataset=readRDS('complete_sample_set.RDS')

#remove unnecessary variables for now
#syntheticdataset=syntheticdataset[,1:65]
#syntheticdataset$diagnosed.diabetes=NULL
#No kids for now
#syntheticdataset=subset(syntheticdataset,syntheticdataset$member!="Child")

#match to get prediabetes, diabetes, and BMI
#syntheticdataset_not_matching_with_health_insurance=getBRSSRdata_not_matching_with_health_insurance(syntheticdataset,1)
#saveRDS(syntheticdataset_not_matching_with_health_insurance,"prediabetes_simulation_not_matching_insurance.RDS")
#rm(syntheticdataset_not_matching_with_health_insurance)

#syntheticdataset_matching_with_health_insurance=getBRSSRdata_with_health_insurance(syntheticdataset,1)
#saveRDS(syntheticdataset_matching_with_health_insurance,"prediabetes_simulation_matching_insurance.RDS")

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
syntheticdataset=readRDS('prediabetes_simulation_not_matching_insurance.RDS')
part_of_set_of_interest=subset(syntheticdataset,syntheticdataset$pre_diabetes=="1")

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

saveRDS(part_of_set_of_interest,file="prediabetes_simulation_for_app_not_matching_insurance.RDS")

#Time to start simulating
#first subset people with prediabetes
syntheticdataset=readRDS('prediabetes_simulation_matching_insurance.RDS')
part_of_set_of_interest=subset(syntheticdataset,syntheticdataset$pre_diabetes=="1")

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

saveRDS(part_of_set_of_interest,file="prediabetes_simulation_for_app_matching_insurance.RDS")