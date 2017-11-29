#Organize BRFSS and Child Call Back asthma data

#Read in BRFSS data
library(SASxport)
library(gtools)
library(car)

#Read SAS files in from BRSSR survey
BRSSR2014=read.xport('LLCP2014.XPT')
BRSSR2014=subset(BRSSR2014,BRSSR2014$X.STATE==48 & BRSSR2014$DISPCODE==1100)#Filters by state and completed interviews
BRSSR2014$SCNTMONY=BRSSR2014$SCNTMNY1
BRSSR2014$SCNTMEAL=BRSSR2014$SCNTMEL1

BRSSR2013=read.xport('LLCP2013.XPT')
BRSSR2013=subset(BRSSR2013,BRSSR2013$X.STATE==48 & BRSSR2013$DISPCODE==1100)

BRSSR2012=read.xport('LLCP2012.XPT')
BRSSR2012=subset(BRSSR2012,BRSSR2012$X.STATE==48 & BRSSR2012$DISPCODE==1100)
BRSSR2012$SLEPTIM1=BRSSR2012$SLEPTIME
BRSSR2012$FLUSHOT6=BRSSR2012$FLUSHOT5

BRSSR2011=read.xport('LLCP2011.XPT')
BRSSR2011=subset(BRSSR2011,BRSSR2011$X.STATE==48 & BRSSR2011$DISPCODE==110)
BRSSR2011$SLEPTIM1=BRSSR2011$SLEPTIME
BRSSR2011$FLUSHOT6=BRSSR2011$FLUSHOT5

BRSSR2010=read.xport('CDBRFS10.XPT')
BRSSR2010=subset(BRSSR2010,BRSSR2010$X.STATE==48 & BRSSR2010$DISPCODE==110)
BRSSR2010$SLEPTIM1=BRSSR2010$SLEPTIME
BRSSR2010$ASTHMA3=BRSSR2010$ASTHMA2
#skin cancer isn't a question
#other types of cancer isn't a question
#chronic obstructive pulmonary disease emphysema chronic bronchitis
BRSSR2010$HAVARTH3=BRSSR2010$HAVARTH2
BRSSR2010$ADDEPEV2=BRSSR2010$ADDEPEV
#ever told you had a kidney disease
BRSSR2010$DIABETE3=BRSSR2010$DIABETE2
BRSSR2010$LASTSMK2=BRSSR2010$LASTSMK1
BRSSR2010$ALCDAY5=BRSSR2010$ALCDAY4
BRSSR2010$X.BMI5=BRSSR2010$X.BMI4
BRSSR2010$FLUSHOT6=BRSSR2010$FLUSHOT4
BRSSR2010$HIVTST6=BRSSR2010$HIVTST5

BRSSR2009=read.xport('CDBRFS09.XPT')
BRSSR2009=subset(BRSSR2009,BRSSR2009$X.STATE==48 & BRSSR2009$DISPCODE==110)
BRSSR2009$SLEPTIM1=BRSSR2009$SLEPTIME
BRSSR2009$ASTHMA3=BRSSR2009$ASTHMA2
#skin cancer isn't a question
#other types of cancer isn't a question
#chronic obstructive pulmonary disease emphysema chronic bronchitis
BRSSR2009$HAVARTH3=BRSSR2009$HAVARTH2
#Ever told you had a depressive order isn't a question
#ever told you had kidney disease isn't a question
BRSSR2009$DIABETE3=BRSSR2009$DIABETE2
BRSSR2009$LASTSMK2=BRSSR2009$LASTSMK1
BRSSR2009$ALCDAY5=BRSSR2009$ALCDAY4
BRSSR2009$X.BMI5=BRSSR2009$X.BMI4
BRSSR2009$FLUSHOT6=BRSSR2009$FLUSHOT3
BRSSR2009$HIVTST6=BRSSR2009$HIVTST5

#Bind them all together
roughBRSSR=smartbind(BRSSR2009,BRSSR2010,BRSSR2011,BRSSR2012,BRSSR2013,BRSSR2014)

#Original BRSSR data will be used first to match for asthma diagnosis and status
BRSSR=data.frame(SEX=roughBRSSR$RCSGENDR,RACE=roughBRSSR$RACE,EDUCA=roughBRSSR$EDUCA,INCOME2=roughBRSSR$INCOME2,year=roughBRSSR$IYEAR,CHILDREN=roughBRSSR$CHILDREN,Diagnosed_Asthma=roughBRSSR$CASTHDX2,Active_Asthma=roughBRSSR$CASTHNO2,stringsAsFactors=FALSE)
#Subset to Children
BRSSR=subset(BRSSR,(BRSSR$CHILDREN!=88 & BRSSR$CHILDREN!=99 & is.na(BRSSR$CHILDREN)==FALSE))
#double check all years are represented no years were lost
table(BRSSR$year,BRSSR$CHILDREN)
table(BRSSR$year,BRSSR$Diagnosed_Asthma)
table(BRSSR$year,BRSSR$Active_Asthma)
#Why is 2015 in here
subset(BRSSR,BRSSR$year==2015)
#Shouldn't be in here remove
BRSSR=subset(BRSSR,BRSSR$year!=2015)
BRSSR2=within(BRSSR,Active_Asthma[Diagnosed_Asthma!=1]<-"2")
BRSSR2=BRSSR2[complete.cases(BRSSR2),]

saveRDS(BRSSR2,"BRSSR_for_children.rds")


#Organize data from Child Asthma Call Back survey
library(rio)
ChildASTHMA2014=import('ACBS_2014_CHILD_PUBLIC_LLCP.sas7bdat')
ChildASTHMA2013=import('acbs_2013_child_public_llcp.sas7bdat')
ChildASTHMA2012=import('acbs_2012_child_public_llcp.sas7bdat')
ChildASTHMA2011=import('acbs_2011_child_public.sas7bdat')
ChildASTHMA2010=import('acbs_2010_child_public.sas7bdat')
ChildASTHMA2009=import('acbs_2009_child_public.sas7bdat')

common_cols=intersect(colnames(ChildASTHMA2014),colnames(ChildASTHMA2013))
ChildASTHMA=rbind(
  subset(ChildASTHMA2014, select = common_cols), 
  subset(ChildASTHMA2013, select = common_cols)
)
common_cols=intersect(common_cols,colnames(ChildASTHMA2012))
ChildASTHMA=rbind(
  subset(ChildASTHMA, select = common_cols), 
  subset(ChildASTHMA2012, select = common_cols)
)
common_cols=intersect(common_cols,colnames(ChildASTHMA2011))
ChildASTHMA=rbind(
  subset(ChildASTHMA, select = common_cols), 
  subset(ChildASTHMA2011, select = common_cols)
)
common_cols=intersect(common_cols,colnames(ChildASTHMA2010))
ChildASTHMA=rbind(
  subset(ChildASTHMA, select = common_cols), 
  subset(ChildASTHMA2010, select = common_cols)
)
common_cols=intersect(common_cols,colnames(ChildASTHMA2009))
ChildASTHMA=rbind(
  subset(ChildASTHMA, select = common_cols), 
  subset(ChildASTHMA2009, select = common_cols)
)
ChildASTHMA2=ChildASTHMA[,c(36,104,152:241)]
ChildASTHMA3=ChildASTHMA2[complete.cases(ChildASTHMA2),]
ChildASTHMA3$SEX=ChildASTHMA3$RCSGENDR
saveRDS(ChildASTHMA3,"Child_Asthma_call_back_survey.rds")

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
  
  finaldataset=data.frame()
  for (indexsex in 1:2){
    for (indexrace in 1:8){
      for (indexincome in 1:16){
        
        subsyntheticdataset=syntheticdataset[(syntheticdataset$sex==sex[indexsex])&(syntheticdataset$race==race[indexrace])&(syntheticdataset$household.income==income[indexincome]),]
        
        subBRSSR=BRSSR[(BRSSR$SEX==bsex[indexsex])&(BRSSR$RACE==brace[indexrace])&(BRSSR$INCOME2==bincome[indexincome])& !is.na(BRSSR$SEX)& !is.na(BRSSR$RACE)& !is.na(BRSSR$INCOME2),]
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
  
  BRSSR=readRDS('BRSSR.rds')
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$INCOME2
  bincome=c("1","2","3","4","5","5","6","6","6","7","7","8","8","8","8","8")
  
  finaldataset=data.frame()
  for (indexsex in 1:2){
    #for (indexrace in 1:8){
    #for (indexedu in 1:7){
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

syntheticdataset=readRDS("complete_sample_set.RDS")
#subset Children
syntheticdataset$X.1=NULL
syntheticdataset$X1=NULL
syntheticdataset=syntheticdataset[,1:65]#I don't ned HCAD variables except geometry which can't save properly
syntheticdataset=syntheticdataset[!is.na(syntheticdataset$householdID),]
syntheticdataset=subset(syntheticdataset,syntheticdataset$member=="Child")
syntheticdataset=getAsthmaDiagnosisandStatus(syntheticdataset,1)
saveRDS(syntheticdataset,"synthetic_children_with_Asthma_diagnosis_status.rds")

syntheticdataset=subset(syntheticdataset,syntheticdataset$Active_Asthma==1)
syntheticdatasetasthma=getASTHMA_BRSSRdata(syntheticdataset,1)
saveRDS(syntheticdatasetasthma,"asthma_simulation.RDS")
