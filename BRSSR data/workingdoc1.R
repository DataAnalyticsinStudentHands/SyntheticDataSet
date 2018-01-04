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

#Take the columns I want and rename them
BRSSR=data.frame(SEX=roughBRSSR$SEX,RACE=roughBRSSR$RACE,EDUCA=roughBRSSR$EDUCA,INCOME2=roughBRSSR$INCOME2,year=roughBRSSR$IYEAR,general.health=roughBRSSR$GENHLTH,flu.shot=roughBRSSR$FLUSHOT6,exercised.past.30.days=roughBRSSR$EXERANY2,ever.test.for.HIV=roughBRSSR$HIVTST6,diagnosed.with.heart.attack=roughBRSSR$CVDINFR4,diagnosed.with.angina.or.coronary.heart.disease=roughBRSSR$CVDCRHD4,diagnosed.with.stroke=roughBRSSR$CVDSTRK3,diagnosed.with.asthma=roughBRSSR$ASTHMA3,diagnosed.arthritis=roughBRSSR$HAVARTH3,diagnosed.diabetes=roughBRSSR$DIABETE3,limited.activies.because.of.problems=roughBRSSR$QLACTLM2,smoked.at.least.100.cigs=roughBRSSR$SMOKE100,use.of.smokeless.tobacco=roughBRSSR$USENOW3,days.in.last.30.drank.alcohol=roughBRSSR$ALCDAY5,couldnt.see.a.doctor.because.of.cost=roughBRSSR$MEDCOST,last.dr.checkup=roughBRSSR$CHECKUP1,BMI=roughBRSSR$X.BMI5,stringsAsFactors=FALSE)
BRSSR=BRSSR[complete.cases(BRSSR),]
#Variables I wanted but no one answered
#asthma.attacking.last.year=roughBRSSR$ASATTACK,days.needed.ER.asthma.care=roughBRSSR$ASERVIST,race.related.physical.stressors=roughBRSSR$RRPHYSM2,race.related.emotional.stressors=roughBRSSR$RREMTSM2,
#worried.about.paying.rent.mortgage=roughBRSSR$SCNTMONY,worried.about.food.money=roughBRSSR$SCNTMEAL,hours worked,

#Variables with less than 75% measurements
#sleep.hours=roughBRSSR$SLEPTIM1,still.have.asthma=roughBRSSR$ASTHNOW,skin.cancer=roughBRSSR$CHCSCNCR,other.cancer=roughBRSSR$CHCOCNCR,
#chronic.obstructive.pulmonary.disease.emphysema.chronic.bronchitis=roughBRSSR$CHCCOPD1,diagnosed.depression=roughBRSSR$ADDEPEV2,diagnosed.kidney.disease=roughBRSSR$CHCKIDNY,
#taking.insulin=roughBRSSR$INSULIN,check.blood.glucose=roughBRSSR$BLDSUGAR,diabetes.education=roughBRSSR$DIABEDU,how.often.have.needed.social.emotional.support=roughBRSSR$EMTSUPRT,satisfied.with.life=roughBRSSR$LSATISFY,
#avg.drinks.per.day.last.30=roughBRSSR$AVEDRNK2,binge.drank.many.times=roughBRSSR$DRNK3GE5,interval.since.last.smoked=roughBRSSR$LASTSMK2,
#alcohol=subset(BRSSR,!days.in.last.30.drank.alcohol=="Yes"&!is.na(days.in.last.30.drank.alcohol))
#smoke=subset(BRSSR,smoked.at.least.100.cigs=="Yes"),had.mammogram=roughBRSSR$HADMAM,
#diabetessubset=subset(BRSSR,diagnosed.diabetes=="Yes"),pap.test=roughBRSSR$HADPAP2,smoking.frequency=roughBRSSR$SMOKDAY2,

#Variables not available for every year
#Falls

#Write to csv
write.csv(BRSSR,"BRSSR.csv")

#New problem trying to get extra modules

#BRSSR2014v1=read.xport("LLCP14V1.XPT")
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
saveRDS(ChildASTHMA,"ChildASTHMA.rds")

#library("dplyr")
#ChildASTHMA=bind_rows(ChildASTHMA2009,ChildASTHMA2010)
#this variable messes up the merge and I don't really need it
#ChildASTHMA$RCVFVCH4=NULL
#ChildASTHMA=bind_rows(ChildASTHMA,ChildASTHMA2011)
#ChildASTHMA=bind_rows(ChildASTHMA,ChildASTHMA2012,ChildASTHMA2013,ChildASTHMA2014)

#Children_BRSSR=data.frame(IYEAR=roughBRSSR$IYEAR,RCSGENDR=roughBRSSR$RCSGENDR,RACE=roughBRSSR$RACE,EDUCA=roughBRSSR$EDUCA,INCOME2=roughBRSSR$INCOME2,CHILDREN=roughBRSSR$CHILDREN,CASTHDX2=roughBRSSR$CASTHDX2,CASTHNO2=roughBRSSR$CASTHNO2,stringsAsFactors=FALSE)
#Children_BRSSR=subset(roughBRSSR,(roughBRSSR$CHILDREN!=88 & roughBRSSR$CHILDREN!=99 & is.na(roughBRSSR$CHILDREN)==FALSE))

#rm(ChildASTHMA2009,ChildASTHMA2010,ChildASTHMA2011,ChildASTHMA2012,ChildASTHMA2013,ChildASTHMA2014)
#rm(BRSSR2009,BRSSR2010,BRSSR2011,BRSSR2012,BRSSR2013,BRSSR2014,BRSSR)
#rm(roughBRSSR)
#Children_BRSSR=merge(Children_BRSSR,ChildASTHMA,all.x=TRUE)

#first many variables should be the same between asthma call back and rough 
#common_cols=intersect(colnames(Children_BRSSR),colnames(ChildASTHMA))
#combine_children=rbind(
#  subset(Children_BRSSR, select = common_cols), 
#  subset(ChildASTHMA, select = common_cols)
#)

#children_in_BRSSR_not_in_asthma_call_back_survey=combine_children[!duplicated(combine_children), ]

#this didn't work so now I'm just going to match for the diagnosis and active asthma then match for variables afterwards

#Ok first match for asthma diagnosis and status
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

saveRDS(BRSSR2,"BRSSR.rds")
source("BRSSRfunction.R")

#Ok finally let's load the synthetic data set subset kids and match
#But I need to not include education because it's the householders education not the kids
getBRSSRdata <- function(syntheticdataset,seed){
  set.seed(seed)
  
  BRSSR=readRDS('BRSSR.rds')
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiaan or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
  #education=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$RACE
  brace=c("2","3","4","5","6","7","1","8")
  #BRSSR$EDUCA
  #beducation=c("2 and 3","3","4","5","5","6","6")
  #BRSSR$INCOME2
  bincome=c("1","2","3","4","5","5","6","6","6","7","7","8","8","8","8","8")
  
  finaldataset=data.frame()
  for (indexsex in 1:2){
    for (indexrace in 1:8){
      #for (indexedu in 1:7){
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
      #}
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
syntheticdataset=getBRSSRdata(syntheticdataset,1)

ChildASTHMA=readRDS("ChildASTHMA.rds")
#ChildASTHMA=subset(ChildASTHMA,ChildASTHMA$`_state`==48 & ChildASTHMA$DISPCODE==110)

#ChildASTHMA$RACE=ChildASTHMA$`_RACE` #I can't find race kept consistent
#ChildASTHMA$INCOME2=ChildASTHMA$INCOME2 #36

ChildASTHMA2=ChildASTHMA[,c(36,104,152:241)]
ChildASTHMA3=ChildASTHMA2[complete.cases(ChildASTHMA2),]
ChildASTHMA3$SEX=ChildASTHMA3$RCSGENDR
saveRDS(ChildASTHMA3,"BRSSR.rds")

#Ok finally let's load the synthetic data set subset kids and match
#But I need to not include education because it's the householders education not the kids
syntheticdataset=readRDS("notdoneyet.rds")
syntheticdataset=subset(syntheticdataset,syntheticdataset$Active_Asthma==1)
getBRSSRdata <- function(syntheticdataset,seed){
  set.seed(seed)
  
  BRSSR=readRDS('BRSSR.rds')
  
  #how the synthetic data is subdivided
  sex=c("Male","Female")
  race=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiaan or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
  #education=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
  income=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  #codes for how that corresponds in BRSSR data
  #BRSSR$SEX
  bsex=c("1","2")
  #BRSSR$RACE
  #brace=c("2","3","4","5","6","7","1","8")
  #BRSSR$EDUCA
  #beducation=c("2 and 3","3","4","5","5","6","6")
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
      #}
    #}
  }
  finaldataset$SEX=NULL
  finaldataset$RACE=NULL
  finaldataset$EDUCA=NULL
  finaldataset$INCOME2=NULL
  finaldataset$row.id.for.merge=NULL
  return(finaldataset)
}
syntheticdatasetasthma=getBRSSRdata(syntheticdataset,1)
saveRDS(syntheticdatasetasthma,"asthma_simulation.RDS")
