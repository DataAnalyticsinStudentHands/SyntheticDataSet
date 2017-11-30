##function to simulate diagnosis and active status of asthma
getAsthmaDiagnosisandStatus <- function(syntheticdataset,seed){
  set.seed(seed)
  
  BRSSR=readRDS("BRSSR_for_children.rds")
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiaan or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$RACE
  brace=c("2","3","4","5","6","7","1","8")
  #BRSSR$INCOME2
  bincome=c("1","2","3","4","5","5","6","6","6","7","7","8","8","8","8","8")
  
  
  #subset synthetic data set by race income and gender, subset the BRFSS data by the same variables and match randomly
  finaldataset=data.frame()
  for (indexsex in 1:2){
    for (indexrace in 1:8){
      for (indexincome in 1:16){
        
        #subset synthetic data set
        subsyntheticdataset=syntheticdataset[(syntheticdataset$sex==sex[indexsex])&(syntheticdataset$race==race[indexrace])&(syntheticdataset$household.income==income[indexincome]),]
        
        #subset BRFSS data
        subBRSSR=BRSSR[(BRSSR$SEX==bsex[indexsex])&(BRSSR$RACE==brace[indexrace])&(BRSSR$INCOME2==bincome[indexincome])& !is.na(BRSSR$SEX)& !is.na(BRSSR$RACE)& !is.na(BRSSR$INCOME2),]
        if (nrow(subBRSSR)>0){
          subBRSSR$row.id.for.merge=1:nrow(subBRSSR)
          if (nrow(subsyntheticdataset)>0){
            #randomly sample a row id and then merge
            subsyntheticdataset$row.id.for.merge=sample(1:nrow(subBRSSR),nrow(subsyntheticdataset),TRUE)
            subsyntheticdataset=merge(subBRSSR,subsyntheticdataset,by.x="row.id.for.merge",by.y="row.id.for.merge",all=FALSE,ll.x=FALSE,all.y=TRUE)
            subsyntheticdataset=subset(subsyntheticdataset,!is.na(subsyntheticdataset$household.type))
            finaldataset=rbind(finaldataset,subsyntheticdataset)
          }
        }
        
        
      }
    }
  }
  #remove variables used for merge and return final data set
  finaldataset$SEX=NULL
  finaldataset$RACE=NULL
  finaldataset$EDUCA=NULL
  finaldataset$INCOME2=NULL
  finaldataset$row.id.for.merge=NULL
  return(finaldataset)
}

#function for Asthma Call Back Survey

getASTHMA_BRSSRdata <- function(syntheticdataset,seed){
  set.seed(seed)
  
  BRSSR=readRDS('Child_Asthma_call_back_survey.rds')
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$INCOME2
  bincome=c("1","2","3","4","5","5","6","6","6","7","7","8","8","8","8","8")
  
  finaldataset=data.frame()
  
  #subset syntheticdataset and survey data by sex and income
  #match subsets of each
  for (indexsex in 1:2){
    for (indexincome in 1:16){
      
      subsyntheticdataset=syntheticdataset[(syntheticdataset$sex==sex[indexsex])&(syntheticdataset$household.income==income[indexincome]),]
      
      subBRSSR=BRSSR[(BRSSR$SEX==bsex[indexsex])&(BRSSR$INCOME2==bincome[indexincome])& !is.na(BRSSR$SEX)& !is.na(BRSSR$INCOME2),]
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
  finaldataset$SEX=NULL
  finaldataset$RACE=NULL
  finaldataset$EDUCA=NULL
  finaldataset$INCOME2=NULL
  finaldataset$row.id.for.merge=NULL
  return(finaldataset)
}

#Code to run simulation
#read in base synthetic dataset
syntheticdataset=readRDS("complete_sample_set.RDS")
# remove uneeded variables and subset Children
syntheticdataset$X.1=NULL
syntheticdataset$X1=NULL
syntheticdataset=syntheticdataset[,1:65]#I don't ned HCAD variables except geometry which can't save properly
syntheticdataset=syntheticdataset[!is.na(syntheticdataset$householdID),]
syntheticdataset=subset(syntheticdataset,syntheticdataset$member=="Child")

#run function and save to get asthma diagnosis and status
syntheticdataset=getAsthmaDiagnosisandStatus(syntheticdataset,1)
saveRDS(syntheticdataset,"synthetic_children_with_Asthma_diagnosis_status.rds")

#subset children with active asthma to simulate symptoms and environmental hazards
syntheticdataset=subset(syntheticdataset,syntheticdataset$Active_Asthma==1)
syntheticdatasetasthma=getASTHMA_BRSSRdata(syntheticdataset,1)
saveRDS(syntheticdatasetasthma,"asthma_simulation.RDS")
