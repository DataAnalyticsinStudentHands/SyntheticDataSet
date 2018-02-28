
getlangandnativity <- function(county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  languageblack=Census_data[c("black.native.only.english","black.native.other.language.english.well","black.native.other.language.english.bad","black.foreign.only.english","black.foreign.other.language.english.well","black.foreign.other.language.english.bad")]
  languageAIAN=Census_data[c("amer.indian.alaskan.native.only.english","amer.indian.alaskan.native.other.language.english.well","amer.indian.alaskan.native.other.language.english.bad","amer.indian.alaskan.foreign.only.english","amer.indian.alaskan.foreign.other.language.english.well","amer.indian.alaskan.foreign.other.language.english.bad")]
  languageasian=Census_data[c("asian.native.only.english","asian.native.other.language.english.well","asian.native.other.language.english.bad","asian.foreign.only.english","asian.foreign.other.language.english.well","asian.foreign.other.language.english.bad")]
  languageNHPI=Census_data[c("islander.native.only.english","islander.native.other.language.english.well","islander.native.other.language.english.bad","islander.foreign.only.english","islander.foreign.other.language.english.well","islander.foreign.other.language.english.bad")]
  languageother=Census_data[c("other.native.only.english","other.native.other.language.english.well","other.native.other.language.english.bad","other.foreign.only.english","other.foreign.other.language.english.well","other.foreign.other.language.english.bad")]
  language2=Census_data[c("multiracial.native.only.english","multiracial.native.other.language.english.well","multiracial.native.other.language.english.bad","multiracial.foreign.only.english","multiracial.foreign.other.language.english.well","multiracial.foreign.other.language.english.bad")]
  languagewhite=Census_data[c("white.native.only.english","white.native.other.language.english.well","white.native.other.language.english.bad","white.foreign.only.english","white.foreign.other.language.english.well","white.foreign.other.language.english.bad")]
  languagehispanic=Census_data[c("hispanic.native.only.english","hispanic.native.other.language.english.well","hispanic.native.other.language.english.bad","hispanic.foreign.only.english","hispanic.foreign.other.language.english.well","hispanic.foreign.other.language.english.bad")]
  
  codelangnat=ifelse(syntheticdataset$race=="Black or African American"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageblack/sum(languageblack)),
         ifelse(syntheticdataset$race=="American Indian or Alaskan Native"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageAIAN/sum(languageAIAN)),
                ifelse(syntheticdataset$race=="Asian"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageasian/sum(languageasian)),
                       ifelse(syntheticdataset$race=="Native Hawaiian or Other Pacific Islander"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageNHPI/sum(languageNHPI)),
                              ifelse(syntheticdataset$race=="Some Other Race"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageother/sum(languageother)),
                                     ifelse(syntheticdataset$race=="Two or More Races"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=language2/sum(language2)),
                                            ifelse(syntheticdataset$race=="White"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languagewhite/sum(languagewhite)),
                                                   ifelse(syntheticdataset$race=="Hispanic or Latino"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languagehispanic/sum(languagehispanic)),
                                                          NA))))))))
  nativity=ifelse(codelangnat %in% c("black.native.only.english","black.native.other.language.english.well","black.native.other.language.english.bad","amer.indian.alaskan.native.only.english","amer.indian.alaskan.native.other.language.english.well","amer.indian.alaskan.native.other.language.english.bad","asian.native.only.english","asian.native.other.language.english.well","asian.native.other.language.english.bad",
                                     "islander.native.only.english","islander.native.other.language.english.well","islander.native.other.language.english.bad","other.native.only.english","other.native.other.language.english.well","other.native.other.language.english.bad","multiracial.native.only.english","multiracial.native.other.language.english.well","multiracial.native.other.language.english.bad","white.native.only.english","white.native.other.language.english.well","white.native.other.language.english.bad",
                                     "hispanic.native.only.english","hispanic.native.other.language.english.well","hispanic.native.other.language.english.bad"),"Native",
                                      ifelse(codelangnat %in% c("black.foreign.only.english","black.foreign.other.language.english.well","black.foreign.other.language.english.bad","amer.indian.alaskan.foreign.only.english","amer.indian.alaskan.foreign.other.language.english.well","amer.indian.alaskan.foreign.other.language.english.bad","asian.foreign.only.english","asian.foreign.other.language.english.well","asian.foreign.other.language.english.bad",
                                                                "islander.foreign.only.english","islander.foreign.other.language.english.well","islander.foreign.other.language.english.bad","other.foreign.only.english","other.foreign.other.language.english.well","other.foreign.other.language.english.bad",
                                                                "multiracial.foreign.only.english","multiracial.foreign.other.language.english.well","multiracial.foreign.other.language.english.bad","white.foreign.only.english","white.foreign.other.language.english.well","white.foreign.other.language.english.bad","hispanic.foreign.only.english","hispanic.foreign.other.language.english.well","hispanic.foreign.other.language.english.bad"),"Foreign",NA))
  
  English=ifelse(codelangnat %in% c("hispanic.native.only.english","other.native.only.english","white.native.only.english","multiracial.native.only.english","islander.native.only.english","asian.native.only.english","amer.indian.alaskan.native.only.english","black.native.only.english","hispanic.foreign.only.english","white.foreign.only.english","multiracial.foreign.only.english","islander.foreign.only.english","asian.foreign.only.english","amer.indian.alaskan.foreign.only.english","black.foreign.only.english","other.foreign.only.english"),"Speaks Only English",
                 ifelse(codelangnat %in% c("other.native.other.language.english.well","other.foreign.other.language.english.well","hispanic.foreign.other.language.english.well","white.foreign.other.language.english.well","multiracial.foreign.other.language.english.well","islander.foreign.other.language.english.well","asian.foreign.other.language.english.well","amer.indian.alaskan.foreign.other.language.english.well","black.foreign.other.language.english.well"),"Speaks English Very Well",
                        ifelse(codelangnat %in% c("other.native.other.language.english.bad","other.foreign.other.language.english.bad","hispanic.foreign.other.language.english.bad","white.foreign.other.language.english.bad","multiracial.foreign.other.language.english.bad","islander.foreign.other.language.english.bad","asian.foreign.other.language.english.bad","amer.indian.alaskan.foreign.other.language.english.bad","black.foreign.other.language.english.bad"),"Speaks English less than well",NA)))
  syntheticdataset$nativity=nativity
  syntheticdataset$English.speaking.skills=English
  return(syntheticdataset)
}


getcitizenandlang <- function(county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  native5.17goodenglish=Census_data[c("spanish.native.5.17.english.well","other.lang.native.5.17.english.well")]
  native5.17badenglish=Census_data[c("spanish.native.5.17.english.bad","other.lang.native.5.17.english.bad")]
  native18goodenglish=Census_data[c("spanish.native.over18.english.well","other.lang.native.over18.english.well")]
  native18badenglish=Census_data[c("spanish.native.over18.english.bad","other.lang.native.over18.english.bad")]
  foreign5.17onlyenglish=Census_data[c("english.foreign.5.17.naturalized","english.foreign.5.17.not.citizen")]
  foreign5.17goodenglish=Census_data[c("spanish.foreign.5.17.naturalized.english.well","other.lang.foreign.5.17.naturalized.english.well","spanish.foreign.5.17.not.citizen.english.well","other.lang.foreign.5.17.not.citizen.english.well")]
  foreign5.17badenglish=Census_data[c("spanish.foreign.5.17.naturalized.english.bad","other.lang.foreign.5.17.naturalized.english.bad","spanish.foreign.5.17.not.citizen.english.bad","other.lang.foreign.5.17.not.citizen.english.bad")]
  foreign18onlyenglish=Census_data[c("english.foreign.over18.naturalized","english.foreign.over18.not.citizen")]
  foreign18goodenglish=Census_data[c("spanish.foreign.18.naturalized.english.well","other.lang.foreign.18.naturalized.english.well","spanish.foreign.18.not.citizen.english.well","other.lang.foreign.18.not.citizen.english.well")]
  foreign18badenglish=Census_data[c("spanish.foreign.18.naturalized.english.bad","other.lang.foreign.18.naturalized.english.bad","spanish.foreign.18.not.citizen.english.bad","other.lang.foreign.18.not.citizen.english.bad")]
  
  syntheticdataset$acode=ifelse(syntheticdataset$nativity=="Native"&syntheticdataset$English.speaking.skills=="Speaks English Very Well",sample(colnames(cbind(native5.17goodenglish,native18goodenglish)),1,prob=cbind(native5.17goodenglish/sum(native5.17goodenglish,native18goodenglish),native18goodenglish/sum(native5.17goodenglish,native18goodenglish))),
                                ifelse(syntheticdataset$nativity=="Native"&syntheticdataset$English.speaking.skills=="Speaks English Not Very Well",sample(colnames(cbind(native5.17badenglish,native18badenglish)),1,prob=cbind(native5.17badenglish/sum(native5.17badenglish,native18badenglish),native18badenglish/sum(native18badenglish,native5.17badenglish))),
                                                     ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks Only English",sample(colnames(cbind(foreign5.17onlyenglish,foreign18onlyenglish)),1,prob=cbind(foreign5.17onlyenglish/sum(foreign5.17onlyenglish,foreign18onlyenglish),foreign18onlyenglish/sum(foreign18onlyenglish,foreign5.17onlyenglish))),
                                                     ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks English Very Well",sample(colnames(cbind(foreign5.17goodenglish,foreign18goodenglish)),1,prob=cbind(foreign5.17goodenglish/sum(foreign5.17goodenglish,foreign18goodenglish),foreign18goodenglish/sum(foreign18goodenglish,foreign5.17goodenglish))),
                                                            ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks English Not Very Well",sample(colnames(cbind(foreign5.17badenglish,foreign18badenglish)),1,prob=cbind(foreign5.17badenglish/sum(foreign5.17badenglish,foreign18badenglish),foreign18badenglish/sum(foreign18badenglish,foreign5.17badenglish))),NA)))))
  syntheticdataset$citizenship=ifelse(syntheticdataset$nativity=="Native","Citizen",
                                      ifelse(syntheticdataset$acode %in% c("english.foreign.5.17.naturalized","spanish.foreign.5.17.naturalized.english.well","other.lang.foreign.5.17.naturalized.english.well","spanish.foreign.5.17.naturalized.english.bad","other.lang.foreign.5.17.naturalized.english.bad","english.foreign.18.naturalized","spanish.foreign.18.naturalized.english.well","other.lang.foreign.18.naturalized.english.well","spanish.foreign.18.naturalized.english.bad","other.lang.foreign.18.naturalized.english.bad"),"Naturalized Citizen",
                                             ifelse(grepl("not.citizen",syntheticdataset$acode),"Not a U.S. Citizen",NA)))
  syntheticdataset$Language.at.home=ifelse(syntheticdataset$English.speaking.skills=="Speaks Only English","English",
                                      ifelse(grepl("spanish",syntheticdataset$acode),"Speaks Spanish",
                                             ifelse(grepl("other.lang",syntheticdataset$acode),"Speaks Other Languages",NA)))
  syntheticdataset$acode=NULL
  return(syntheticdataset)
}



getvets <- function(county,tract,syntheticdataset,seed,Census_data_List){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  men18to34=Census_data[c("veteran.men.18.34","nonveteran.men.18.34")]
  men35to54=Census_data[c("veteran.men.35.54","nonveteran.men.35.54")]
  men55to64=Census_data[c("veteran.men.55.64","nonveteran.men.55.64")]
  men65to74=Census_data[c("veteran.men.65.74","nonveteran.men.65.74")]
  men75up=Census_data[c("veteran.men.over75","nonveteran.men.over75")]

  women18to34=Census_data[c("veteran.women.18.34","nonveteran.women.18.34")]
  women35to54=Census_data[c("veteran.women.35.54","nonveteran.women.35.54")]
  women55to64=Census_data[c("veteran.women.55.64","nonveteran.women.55.64")]
  women65to74=Census_data[c("veteran.women.65.74","nonveteran.women.65.74")]
  women75up=Census_data[c("veteran.women.over75","nonveteran.women.over75")]
  
  veteran.status=ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24"|syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men18to34/sum(men18to34)),
                        ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men35to54/sum(men35to54)),
                               ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="55 to 64")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men55to64/sum(men55to64)),
                                      ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="65 to 74")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men65to74/sum(men65to74)),
                                             ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=men75up/sum(men75up)),
                                                    ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24"|syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women18to34/sum(women18to34)),
                                                           ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women35to54/sum(women35to54)),
                                                                  ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="55 to 64")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women55to64/sum(women55to64)),
                                                                         ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="65 to 74")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women65to74/sum(women65to74)),
                                                                                ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85")&!(syntheticdataset$employment=="In Armed Forces"),sample(c("Veteran","Nonveteran"),1,prob=women75up/sum(women75up)),"Nonveteran"))))))))))
  syntheticdataset$veteran.status=veteran.status
  return(syntheticdataset)
}


gettransport <- function(county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  male=Census_data[c("drove.alone.men","carpooled.men","public.transport.men","bike.men","walk.men","other.transport.men","work.at.home.men")]
  female=Census_data[c("drove.alone.women","carpooled.women","public.transport.women","bike.women","walk.women","other.transport.women","work.at.home.women")]

    code=c("drove alone","carpooled","public transportation","bicycle","walked","motorcycle taxicab or other","worked at home")

  transport=ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$sex=="Male",sample(code,1,prob=male/sum(male)),
                       ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$sex=="Female",sample(code,1,prob=female/sum(female)),NA))
  
  syntheticdataset$means.of.transportation.to.work=transport
  return(syntheticdataset)
}



gettraveltime=function(county,tract,syntheticdataset,seed,Census_data_List){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  drovealone=Census_data[c("drove.alone.less.than.10.minutes","drove.alone.10.14.minutes","drove.alone.15.19.minutes","drove.alone.20.24.minutes","drove.alone.30.34.minutes","drove.alone.35.44.minutes","drove.alone.45.59.minutes","drove.alone.over60.minutes")]
  carpooled=Census_data[c("carpooled.less.than.10.minutes","carpooled.10.14.minutes","carpooled.15.19.minutes","carpooled.20.24.minutes","carpooled.30.34.minutes","carpooled.35.44.minutes","carpooled.45.59.minutes","carpooled.over60.minutes")]
  publictransport==Census_data[c("public.transport.less.than.10.minutes","public.transport.10.14.minutes","public.transport.15.19.minutes","public.transport.20.24.minutes","public.transport.30.34.minutes","public.transport.35.44.minutes","public.transport.45.59.minutes","public.transport.over60.minutes")]
  walked=Census_data[c("walked.less.than.10.minutes","walked.10.14.minutes","walked.15.19.minutes","walked.20.24.minutes","walked.30.34.minutes","walked.35.44.minutes","walked.45.59.minutes","walked.over60.minutes")]
  other=Census_data[c("other.less.than.10.minutes","other.10.14.minutes","other.15.19.minutes","other.20.24.minutes","other.30.34.minutes","other.35.44.minutes","other.45.59.minutes","other.over60.minutes")]

  code=c("less than 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes","25 to 29 minutes","30 to 34 minutes","35 to 44 minutes","45 to 59 minutes","60 minutes or more")
  
  travel.time.to.work=ifelse(syntheticdataset$means.of.transportation.to.work=="drove alone",sample(code,1,prob=drovealone/sum(drovealone)),
                             ifelse(syntheticdataset$means.of.transportation.to.work=="carpooled",sample(code,1,prob=carpooled/sum(carpooled)),
                                    ifelse(syntheticdataset$means.of.transportation.to.work=="public transport",sample(code,1,prob=publictransport/sum(publictransport)),
                                           ifelse(syntheticdataset$means.of.transportation.to.work=="walked",sample(code,1,prob=walked/sum(walked)),
                                                  ifelse(syntheticdataset$means.of.transportation.to.work=="taxi, motorcycle, bike or other",sample(code,1,prob=other/sum(other)),NA)))))
  syntheticdataset$travel.time.to.work=travel.time.to.work
  return(syntheticdataset)
}


getincome <- function(county,tract,syntheticdataset,seed,Census_data_List){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  income=Census_data[c("income.less.10000","income.10000.14999","income.15000.19999","income.20000.24999","income.25000.29999",
                       "income.30000.34999","income.35000.39999","income.40000.44999","income.45000.49999","income.50000.59999",
                       "income.60000.74999","income.75000.99999","income.100000.124999","income.125000.149999","income.150000.199999","income.over.200000")]
  
  code=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  
  household.income=sample(code,size=1,prob=c(income/sum(income)))
  household.income=rep(household.income,nrow(syntheticdataset))
  syntheticdataset$household.income=household.income
  

  return(syntheticdataset)#=finalsyntheticdataset)
  
}


getinsurance <- function(county,tract,syntheticdataset,seed,inputdir){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  #Organize Data Set by row
  under25000=Census_data[c("private.insurance.under25000","public.insurance.under25000","no.insurance.under25000")]
  between25to49=Census_data[c("private.insurance.25.49","public.insurance.25.49","no.insurance.25.49")]
  between50to75=Census_data[c("private.insurance.50.75","public.insurance.50.75","no.insurance.50.75")]
  between75to100=Census_data[c("private.insurance.75.100","public.insurance.75.100","no.insurance.75.100")]
  over100=Census_data[c("private.insurance.over100","public.insurance.over100","no.insurance.over100")]

  
  code=c("private insurance","public insurance","no insurance")
  
  health.insurance=ifelse(syntheticdataset$household.income=="less than 10,000"|syntheticdataset$household.income=="10,000 to 14,999"|syntheticdataset$household.income=="15,000 to 19,999"|syntheticdataset$household.income=="20,000 to 24,999",sample(code,1,prob=under25000/sum(under25000)),
                          ifelse(syntheticdataset$household.income=="20,000 to 24,999"|syntheticdataset$household.income=="25,000 to 29,999"|syntheticdataset$household.income=="30,000 to 34,999"|syntheticdataset$household.income=="35,000 to 39,999"|syntheticdataset$household.income=="40,000 to 44,999"|syntheticdataset$household.income=="45,000 to 49,999",sample(code,1,prob=between25to49/sum(between25to49)),
                                 ifelse(syntheticdataset$household.income=="50,000 to 59,999"|syntheticdataset$household.income=="60,000 to 74,999",sample(code,1,prob=between50to75/sum(between50to75)),
                                        ifelse(syntheticdataset$household.income=="75,000 to 99,999",sample(code,1,prob=between75to100/sum(between75to100)),
                                               ifelse(syntheticdataset$household.income=="100,000 to 124,999"|syntheticdataset$household.income=="125,000 to 149,999"|syntheticdataset$household.income=="150,000 to 199,999"|syntheticdataset$household.income=="200,000 or more",sample(code,1,prob=over100/sum(over100)),
                            NA)))))
  syntheticdataset$health.insurance=health.insurance
  return(syntheticdataset)
}