#Using the 500 Cities Project Data for better spacial resolution
inputdir = "../Inputs/"
houstondata=read.csv(paste0(inputdir,'houstondata.csv'))
#This provides us with 28 variables all in percentages

variables=unique(houstondata$Measure)


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

houstondata$UniqueID=as.character(houstondata$UniqueID)
houstondata$tract=substrRight(houstondata$UniqueID,6)
houstondata$county=substr(houstondata$TractFIPS, 3, 5)
houstondata$Measure=as.character(houstondata$Measure)

#Not Doing Health Insurance because we have that through Census Data

#Arthritis among adults aged >=18 Years
getarthritis <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  arthritis=houstondata[(houstondata$Measure=="Arthritis among adults aged >=18 Years"),]
  arthritis=arthritis[(arthritis$tract==tract)&(arthritis$county==county),]
  
  if (nrow(arthritis)==0||is.na(arthritis$Data_Value)){
    diagnosed.arthritis=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else {
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    diagnosed.arthritis=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(arthritis$Data_Value/100,(1-arthritis$Data_Value/100))),NA)
    
  }
  syntheticdataset$diagnosed.arthritis=diagnosed.arthritis
  return(syntheticdataset)
}

#Binge drinking among adults aged >=18 Years 
getbingedrinking <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  binge=houstondata[(houstondata$Measure=="Binge drinking among adults aged >=18 Years"),]
  binge=binge[(binge$tract==tract)&(binge$county==county),]
  
  if (nrow(binge)==0||is.na(binge$Data_Value)){
    binge.drinker=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    binge.drinker=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(binge$Data_Value/100,(1-binge$Data_Value/100))),NA)
  }
  syntheticdataset$binge.drinker=binge.drinker
  return(syntheticdataset)
}

#High blood pressure among adults aged >=18 Years
gethighbloodpressure <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  hbp=houstondata[(houstondata$Measure=="High blood pressure among adults aged >=18 Years"),]
  hbp=hbp[(hbp$tract==tract)&(hbp$county==county),]
  
  if (nrow(hbp)==0||is.na(hbp$Data_Value)){
    high.blood.pressure=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    high.blood.pressure=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(hbp$Data_Value/100,(1-hbp$Data_Value/100))),NA)
  }
   syntheticdataset$high.blood.pressure=high.blood.pressure
  return(syntheticdataset)
}

#Mammography use among women aged 50-74 Years 
#AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS AGE PROBLEMS!!!!!!!!!!!!
getmammographyuse <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  mam=houstondata[(houstondata$Measure=="Mammography use among women aged 50-74 Years"),]
  mam=mam[(mam$tract==tract)&(mam$county==county),]
  
  if (nrow(mam)==0||is.na(mam$Data_Value)){
    mammography.use=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    ages50to74=c("45 to 54","55 to 64","65 to 74")
    mammography.use=ifelse((syntheticdataset$age %in% ages50to74)&(syntheticdataset$sex="Female"),sample(c("yes","no"),1,prob=c(mam$Data_Value/100,(1-mam$Data_Value/100))),NA)
  }
  syntheticdataset$mammography.use=mammography.use
  return(syntheticdataset)
}

#Obesity among adults aged >=18 Years 
getobesity <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  Ob=houstondata[(houstondata$Measure=="Obesity among adults aged >=18 Years"),]
  Ob=Ob[(Ob$tract==tract)&(Ob$county==county),]
  
  if (nrow(Ob)==0||is.na(Ob$Data_Value)){
    Obesity=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    Obesity=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(Ob$Data_Value/100,(1-Ob$Data_Value/100))),NA)
  }
  syntheticdataset$Obesity=Obesity
  return(syntheticdataset)
}

#Taking medicine for high blood pressure control among adults aged >=18 Years with high blood pressure  
gethbpmedications <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  hbpmeds=houstondata[(houstondata$Measure=="Taking medicine for high blood pressure control among adults aged >=18 Years with high blood pressure"),]
  hbpmeds=hbpmeds[(hbpmeds$tract==tract)&(hbpmeds$county==county),]
  
  if (nrow(hbpmeds)==0||is.na(hbpmeds$Data_Value)){
    Blood.pressure.medications=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    Blood.pressure.medications=ifelse((syntheticdataset$age %in% over18ages)&(syntheticdataset$high.blood.pressure=="yes"),sample(c("yes","no"),1,prob=c(hbpmeds$Data_Value/100,(1-hbpmeds$Data_Value/100))),NA)
  }
  syntheticdataset$blood.pressure.medications=Blood.pressure.medications
  return(syntheticdataset)
}

#Cancer (excluding skin cancer) among adults aged >=18 Years 
getcancer <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  cancer=houstondata[(houstondata$Measure=="Cancer (excluding skin cancer) among adults aged >=18 Years"),]
  cancer=cancer[(cancer$tract==tract)&(cancer$county==county),]
  if (nrow(cancer)==0||is.na(cancer$Data_Value)){
    cancer.not.including.skin.cancer=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    cancer.not.including.skin.cancer=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(cancer$Data_Value/100,(1-cancer$Data_Value/100))),NA)
  }
  syntheticdataset$cancer.not.including.skin.cancer=cancer.not.including.skin.cancer
  return(syntheticdataset)
}

#Current asthma among adults aged >=18 Years 
getadultasthma <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  adultasthma=houstondata[(houstondata$Measure=="Current asthma among adults aged >=18 Years"),]
  adultasthma=adultasthma[(adultasthma$tract==tract)&(adultasthma$county==county),]
  
  if (nrow(adultasthma)==0||is.na(adultasthma$Data_Value)){
    adult.asthma=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    adult.asthma=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(adultasthma$Data_Value/100,(1-adultasthma$Data_Value/100))),NA)
  }
  syntheticdataset$adult.asthma=adult.asthma
  return(syntheticdataset)
}

#Coronary heart disease among adults aged >=18 Years 
getcoronaryheartdisease <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  corhd=houstondata[(houstondata$Measure=="Coronary heart disease among adults aged >=18 Years"),]
  corhd=corhd[(corhd$tract==tract)&(corhd$county==county),]
  
  if (nrow(corhd)==0||is.na(corhd$Data_Value)){
    coronary.heart.disease=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    coronary.heart.disease=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(corhd$Data_Value/100,(1-corhd$Data_Value/100))),NA)
  }
  syntheticdataset$coronary.heart.disease=coronary.heart.disease
  return(syntheticdataset)
}

#Visits to doctor for routine checkup within the past Year among adults aged >=18 Years 
getroutinecheckups <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  rtcu=houstondata[(houstondata$Measure=="Visits to doctor for routine checkup within the past Year among adults aged >=18 Years"),]
  rtcu=rtcu[(rtcu$tract==tract)&(rtcu$county==county),]
  
  if (nrow(rtcu)==0||is.na(rtcu$Data_Value)){
    routine.checkups=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    routine.checkups=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(rtcu$Data_Value/100,(1-rtcu$Data_Value/100))),NA)
  }
  
  syntheticdataset$routine.checkups=routine.checkups
  return(syntheticdataset)
}

#Cholesterol screening among adults aged >=18 Years
getcholesterolscreening <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  cholsc=houstondata[(houstondata$Measure=="Cholesterol screening among adults aged >=18 Years"),]
  cholsc=cholsc[(cholsc$tract==tract)&(cholsc$county==county),]
  
  if (nrow(cholsc)==0||is.na(cholsc$Data_Value)){
    cholesterol.screening=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    cholesterol.screening=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(cholsc$Data_Value/100,(1-cholsc$Data_Value/100))),NA)
  }
  syntheticdataset$cholesterol.screening=cholesterol.screening
  return(syntheticdataset)
}

#Fecal occult blood test, sigmoidoscopy, or colonoscopy among adults aged 50-75 Years
getcolonoscopy <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  colon=houstondata[(houstondata$Measure=="Fecal occult blood test, sigmoidoscopy, or colonoscopy among adults aged 50-75 Years"),]
  colon=colon[(colon$tract==tract)&(colon$county==county),]
  if (nrow(colon)==0||is.na(colon$Data_Value)){
    colonoscopy=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    ages50to75=c("45 to 54","55 to 64","65 to 74")
    colonoscopy=ifelse((syntheticdataset$age %in% ages50to75),sample(c("yes","no"),1,prob=c(colon$Data_Value/100,(1-colon$Data_Value/100))),NA)
  }
  syntheticdataset$colonoscopy=colonoscopy
  return(syntheticdataset)
}

#Chronic obstructive pulmonary disease among adults aged >=18 Years
getchronicobspulmonarydisease <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  copd=houstondata[(houstondata$Measure=="Chronic obstructive pulmonary disease among adults aged >=18 Years"),]
  copd=copd[(copd$tract==tract)&(copd$county==county),]
  if (nrow(copd)==0||is.na(copd$Data_Value)){
    chronic.obstructive.pulmonary.disease=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    chronic.obstructive.pulmonary.disease=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(copd$Data_Value/100,(1-copd$Data_Value/100))),NA)
  }
  syntheticdataset$chronic.obstructive.pulmonary.disease=chronic.obstructive.pulmonary.disease
  return(syntheticdataset)
}

#Older adult men aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening
get65menuptodate <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  oldmen=houstondata[(houstondata$Measure=="Older adult men aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening"),]
  oldmen=oldmen[(oldmen$tract==tract)&(oldmen$county==county),]
  if (nrow(oldmen)==0||is.na(oldmen$Data_Value)){
    older.men.up.to.date.on.preventative.services=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over65ages=c("65 to 74","75 to 84","Over 85")
    older.men.up.to.date.on.preventative.services=ifelse((syntheticdataset$age %in% over65ages)&syntheticdataset$sex=="Male",sample(c("yes","no"),1,prob=c(oldmen$Data_Value/100,(1-oldmen$Data_Value/100))),NA)
  }
  syntheticdataset$older.men.up.to.date.on.preventative.services=older.men.up.to.date.on.preventative.services
  return(syntheticdataset)
}

#Older adult women aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 Years
get65womenuptodate <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  oldwomen=houstondata[(houstondata$Measure=="Older adult women aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 Years"),]
  oldwomen=oldwomen[(oldwomen$tract==tract)&(oldwomen$county==county),]
  if (nrow(oldwomen)==0||is.na(oldwomen$Data_Value)){
    older.women.up.to.date.on.preventitive.services=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over65ages=c("65 to 74","75 to 84","Over 85")
    older.women.up.to.date.on.preventitive.services=ifelse((syntheticdataset$age %in% over65ages)&syntheticdataset$sex=="Female",sample(c("yes","no"),1,prob=c(oldwomen$Data_Value/100,(1-oldwomen$Data_Value/100))),NA)
  }
  
  syntheticdataset$older.women.up.to.date.on.preventative.services=older.women.up.to.date.on.preventitive.services
  return(syntheticdataset)
}

#Current smoking among adults aged >=18 Years
getsmokers <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  smokers=houstondata[(houstondata$Measure=="Current smoking among adults aged >=18 Years"),]
  smokers=smokers[(smokers$tract==tract)&(smokers$county==county),]
  if (nrow(smokers)==0||is.na(smokers$Data_Value)){
    smoker=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    smoker=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(smokers$Data_Value/100,(1-smokers$Data_Value/100))),NA)
  }
  syntheticdataset$smoker=smoker
  return(syntheticdataset)
}

#Diagnosed diabetes among adults aged >=18 Years
getdiabetes <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  diabetes=houstondata[(houstondata$Measure=="Diagnosed diabetes among adults aged >=18 Years"),]
  diabetes=diabetes[(diabetes$tract==tract)&(diabetes$county==county),]
  
  if (nrow(diabetes)==0||is.na(diabetes$Data_Value)){
    diagnosed.diabetes=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    diagnosed.diabetes=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(diabetes$Data_Value/100,(1-diabetes$Data_Value/100))),NA)
  }
  syntheticdataset$diagnosed.diabetes=diagnosed.diabetes
  return(syntheticdataset)
}

#High cholesterol among adults aged >=18 Years who have been screened in the past 5 Years
gethighcholesterol <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  highchol=houstondata[(houstondata$Measure=="High cholesterol among adults aged >=18 Years who have been screened in the past 5 Years"),]
  highchol=highchol[(highchol$tract==tract)&(highchol$county==county),]
  
  if (nrow(highchol)==0||is.na(highchol$Data_Value)){
    high.cholesterol=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    high.cholesterol=ifelse((syntheticdataset$age %in% over18ages)&(syntheticdataset$cholesterol.screening=="yes"),sample(c("yes","no"),1,prob=c(highchol$Data_Value/100,(1-highchol$Data_Value/100))),NA)
  }
  syntheticdataset$high.cholesterol=high.cholesterol
  return(syntheticdataset)
}

#Chronic kidney disease among adults aged >=18 Years
getkidneydisease <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  kidneyds=houstondata[(houstondata$Measure=="Chronic kidney disease among adults aged >=18 Years"),]
  kidneyds=kidneyds[(kidneyds$tract==tract)&(kidneyds$county==county),]
  
  if (nrow(kidneyds)==0||is.na(kidneyds$Data_Value)){
    chronic.kidney.disease=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    chronic.kidney.disease=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(kidneyds$Data_Value/100,(1-kidneyds$Data_Value/100))),NA)
  }
  syntheticdataset$chronic.kidney.disease=chronic.kidney.disease
  return(syntheticdataset)
}

#No leisure-time physical activity among adults aged >=18 Years
getnoleisuretime <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  leisure=houstondata[(houstondata$Measure=="No leisure-time physical activity among adults aged >=18 Years"),]
  leisure=leisure[(leisure$tract==tract)&(leisure$county==county),]
  
  if (nrow(leisure)==0||is.na(leisure$Data_Value)){
    no.leisure.time.physical.activity=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    no.leisure.time.physical.activity=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(leisure$Data_Value/100,(1-leisure$Data_Value/100))),NA)
  }
  syntheticdataset$no.leisure.time.physical.activity=no.leisure.time.physical.activity
  return(syntheticdataset)
}

#Mental health not good for >=14 days among adults aged >=18 Years
getmentalhealth <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  mental=houstondata[(houstondata$Measure=="Mental health not good for >=14 days among adults aged >=18 Years"),]
  mental=mental[(mental$tract==tract)&(mental$county==county),]
  
  if (nrow(mental)==0||is.na(mental$Data_Value)){
    mental.health.not.good.for.more.than.14.days=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    mental.health.not.good.for.more.than.14.days=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(mental$Data_Value/100,(1-mental$Data_Value/100))),NA)
  }
  syntheticdataset$mental.health.not.good.for.more.than.14.days=mental.health.not.good.for.more.than.14.days
  return(syntheticdataset)
}

#Stroke among adults aged >=18 Years
getstroke <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  stroke=houstondata[(houstondata$Measure=="Stroke among adults aged >=18 Years"),]
  stroke=stroke[(stroke$tract==tract)&(stroke$county==county),]
  
  if (nrow(stroke)==0||is.na(stroke$Data_Value)){
    had.a.stroke=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    had.a.stroke=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(stroke$Data_Value/100,(1-stroke$Data_Value/100))),NA)
  }
  syntheticdataset$had.a.stroke=had.a.stroke
  return(syntheticdataset)
}

#Papanicolaou smear use among adult women aged 21-65 Years
getpapsmear <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  pap=houstondata[(houstondata$Measure=="Papanicolaou smear use among adult women aged 21-65 Years"),]
  pap=pap[(pap$tract==tract)&(pap$county==county),]
  
  if (nrow(pap)==0||is.na(pap$Data_Value)){
    pap.smear=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    pap.smear=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(pap$Data_Value/100,(1-pap$Data_Value/100))),NA)
  }
  syntheticdataset$pap.smear=pap.smear
  return(syntheticdataset)
}

#Physical health not good for >=14 days among adults aged >=18 Years
getphysicalhealth <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  phy=houstondata[(houstondata$Measure=="Physical health not good for >=14 days among adults aged >=18 Years"),]
  phy=phy[(phy$tract==tract)&(phy$county==county),]
  
  if (nrow(phy)==0||is.na(phy$Data_Value)){
    physical.health.not.good.for.14.days=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    physical.health.not.good.for.14.days=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(phy$Data_Value/100,(1-phy$Data_Value/100))),NA)
  }
  syntheticdataset$physical.health.not.good.for.14.days=physical.health.not.good.for.14.days
  return(syntheticdataset)
}

#Sleeping less than 7 hours among adults aged >=18 Years
getsleep <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  sleep=houstondata[(houstondata$Measure=="Sleeping less than 7 hours among adults aged >=18 Years"),]
  sleep=sleep[(sleep$tract==tract)&(sleep$county==county),]
  
  if (nrow(sleep)==0||is.na(sleep$Data_Value)){
    sleeping.less.than.7.hours=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    sleeping.less.than.7.hours=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(sleep$Data_Value/100,(1-sleep$Data_Value/100))),NA)
  }
  syntheticdataset$sleeping.less.than.7.hours=sleeping.less.than.7.hours
  return(syntheticdataset)
}

#All teeth lost among adults aged >=65 Years
getteeth <- function(county, tract, syntheticdataset,seed){
  set.seed(seed)
  teeth=houstondata[(houstondata$Measure=="All teeth lost among adults aged >=65 Years"),]
  teeth=teeth[(teeth$tract==tract)&(teeth$county==county),]
  
  if (nrow(teeth)==0||is.na(teeth$Data_Value)){
    lost.all.their.teeth=rep("Not Available for this Census Tract",nrow(syntheticdataset))
  }
  else{
    over18ages=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
    lost.all.their.teeth=ifelse((syntheticdataset$age %in% over18ages),sample(c("yes","no"),1,prob=c(teeth$Data_Value/100,(1-teeth$Data_Value/100))),NA)
  }
  syntheticdataset$lost.all.their.teeth=lost.all.their.teeth
  return(syntheticdataset)
}