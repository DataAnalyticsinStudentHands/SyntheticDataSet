######ASK ABOUT AGE PROBLEM!!!!!


getdegree <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  field=read.csv("field_Bachelor_degree.csv")
  field=field[(field$tract==tract)&(field$county==county),]
  
  degreeblack=field[c("C15010B_002E","C15010B_003E","C15010B_004E","C15010B_005E","C15010B_006E")]
  degreeAIAN=field[c("C15010C_002E","C15010C_003E","C15010C_004E","C15010C_005E","C15010C_006E")]
  degreeasian=field[c("C15010D_002E","C15010D_003E","C15010D_004E","C15010D_005E","C15010D_006E")]
  degreeNHPI=field[c("C15010E_002E","C15010E_003E","C15010E_004E","C15010E_005E","C15010E_006E")]
  degreeother=field[c("C15010F_002E","C15010F_003E","C15010F_004E","C15010F_005E","C15010F_006E")]
  degree2=field[c("C15010G_002E","C15010G_003E","C15010G_004E","C15010G_005E","C15010G_006E")]
  degreewhite=field[c("C15010H_002E","C15010H_003E","C15010H_004E","C15010H_005E","C15010H_006E")]
  degreehispanic=field[c("C15010I_002E","C15010I_003E","C15010I_004E","C15010I_005E","C15010I_006E")]
  
  code=c("Science and Engineering","Science and Engineering Related Fields","Business","Education","Arts, Humanities and Other")
  
  bachelors.degree=ifelse(syntheticdataset$race=="Black or African American"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degreeblack/sum(degreeblack)),
         ifelse(syntheticdataset$race=="American Indian or Alaskan Native"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degreeAIAN/sum(degreeAIAN)),
                ifelse(syntheticdataset$race=="Asian"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degreeasian/sum(degreeasian)),
                       ifelse(syntheticdataset$race=="Native Hawaiian or Other Pacific Islander"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degreeNHPI/sum(degreeNHPI)),
                              ifelse(syntheticdataset$race=="Some Other Race"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degreeother/sum(degreeother)),
                                     ifelse(syntheticdataset$race=="Two or More Races"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degree2/sum(degree2)),
                                            ifelse(syntheticdataset$race=="White"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degreewhite/sum(degreewhite)),
                                                   ifelse(syntheticdataset$race=="Hispanic or Latino"&(syntheticdataset$education.attainment=="Bachelor's Degree"|syntheticdataset$education.attainment=="Graduate or Professional Degree"),sample(code,1,prob=degreehispanic/sum(degreehispanic)),
                                                          NA))))))))
  syntheticdataset$bachelors.degree=bachelors.degree
  return(syntheticdataset)
}



getlangandnativity <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  language=read.csv("nativity_language.csv")
  language=language[(language$tract==tract)&(language$county==county),]
  
  languageblack=language[c("B16005B_003E","B16005B_005E","B16005B_006E","B16005B_008E","B16005B_010E","B16005B_011E")]
  languageAIAN=language[c("B16005C_003E","B16005C_005E","B16005C_006E","B16005C_008E","B16005C_010E","B16005C_011E")]
  languageasian=language[c("B16005D_003E","B16005D_005E","B16005D_006E","B16005D_008E","B16005D_010E","B16005D_011E")]
  languageNHPI=language[c("B16005E_003E","B16005E_005E","B16005E_006E","B16005E_008E","B16005E_010E","B16005E_011E")]
  languageother=language[c("B16005F_003E","B16005F_005E","B16005F_006E","B16005F_008E","B16005F_010E","B16005F_011E")]
  language2=language[c("B16005G_003E","B16005G_005E","B16005G_006E","B16005G_008E","B16005G_010E","B16005G_011E")]
  languagewhite=language[c("B16005H_003E","B16005H_005E","B16005H_006E","B16005H_008E","B16005H_010E","B16005H_011E")]
  languagehispanic=language[c("B16005I_003E","B16005I_005E","B16005I_006E","B16005I_008E","B16005I_010E","B16005I_011E")]
  codelangnat=ifelse(syntheticdataset$race=="Black or African American"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageblack/sum(languageblack)),
         ifelse(syntheticdataset$race=="American Indian or Alaskan Native"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageAIAN/sum(languageAIAN)),
                ifelse(syntheticdataset$race=="Asian"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageasian/sum(languageasian)),
                       ifelse(syntheticdataset$race=="Native Hawaiian or Other Pacific Islander"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageNHPI/sum(languageNHPI)),
                              ifelse(syntheticdataset$race=="Some Other Race"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languageother/sum(languageother)),
                                     ifelse(syntheticdataset$race=="Two or More Races"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=language2/sum(language2)),
                                            ifelse(syntheticdataset$race=="White"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languagewhite/sum(languagewhite)),
                                                   ifelse(syntheticdataset$race=="Hispanic or Latino"&!syntheticdataset$age=="Under 5",sample(colnames(languageblack),1,prob=languagehispanic/sum(languagehispanic)),
                                                          NA))))))))
  nativity=ifelse(codelangnat=="B16005B_003E"|codelangnat=="B16005B_005E"|codelangnat=="B16005B_006E","Native",
                                      ifelse(codelangnat=="B16005B_008E"|codelangnat=="B16005B_010E"|codelangnat=="B16005B_011E","Foreign",NA))
  English=ifelse(codelangnat=="B16005B_003E"|codelangnat=="B16005B_008E","Speaks Only English",
                 ifelse(codelangnat=="B16005B_005E"|codelangnat=="B16005B_010E","Speaks English Very Well",
                        ifelse(codelangnat=="B16005B_006E"|codelangnat=="B16005B_011E","Speaks English Not Very Well",NA)))
  syntheticdataset$nativity=nativity
  syntheticdataset$English.speaking.skills=English
  return(syntheticdataset)
}


getcitizenandlang <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  citizen=read.csv("citizenship_language.csv")
  citizen=citizen[(citizen$tract==tract)&(citizen$county==county),]
  
  native5.17goodenglish=citizen[c("B16008_006E","B16008_009E")]
  native5.17badenglish=citizen[c("B16008_007E","B16008_010E")]
  native18goodenglish=citizen[c("B16008_014E","B16008_017E")]
  native18badenglish=citizen[c("B16008_015E","B16008_018E")]
  foreign5.17onlyenglish=citizen[c("B16008_022E","B16008_039E")]
  foreign5.17goodenglish=citizen[c("B16008_024E","B16008_027E","B16008_041E","B16008_044E")]
  foreign5.17badenglish=citizen[c("B16008_025E","B16008_028E","B16008_042E","B16008_045E")]
  foreign18onlyenglish=citizen[c("B16008_030E","B16008_047E")]
  foreign18goodenglish=citizen[c("B16008_032E","B16008_035E","B16008_049E","B16008_052E")]
  foreign18badenglish=citizen[c("B16008_033E","B16008_036E","B16008_050E","B16008_053E")]
  
  syntheticdataset$acode=ifelse(syntheticdataset$nativity=="Native"&syntheticdataset$English.speaking.skills=="Speaks English Very Well",sample(colnames(cbind(native5.17goodenglish,native18goodenglish)),1,prob=cbind(native5.17goodenglish/sum(native5.17goodenglish,native18goodenglish),native18goodenglish/sum(native5.17goodenglish,native18goodenglish))),
                                ifelse(syntheticdataset$nativity=="Native"&syntheticdataset$English.speaking.skills=="Speaks English Not Very Well",sample(colnames(cbind(native5.17badenglish,native18badenglish)),1,prob=cbind(native5.17badenglish/sum(native5.17badenglish,native18badenglish),native18badenglish/sum(native18badenglish,native5.17badenglish))),
                                                     ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks Only English",sample(colnames(cbind(foreign5.17onlyenglish,foreign18onlyenglish)),1,prob=cbind(foreign5.17onlyenglish/sum(foreign5.17onlyenglish,foreign18onlyenglish),foreign18onlyenglish/sum(foreign18onlyenglish,foreign5.17onlyenglish))),
                                                     ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks English Very Well",sample(colnames(cbind(foreign5.17goodenglish,foreign18goodenglish)),1,prob=cbind(foreign5.17goodenglish/sum(foreign5.17goodenglish,foreign18goodenglish),foreign18goodenglish/sum(foreign18goodenglish,foreign5.17goodenglish))),
                                                            ifelse(syntheticdataset$nativity=="Foreign"&syntheticdataset$English.speaking.skills=="Speaks English Not Very Well",sample(colnames(cbind(foreign5.17badenglish,foreign18badenglish)),1,prob=cbind(foreign5.17badenglish/sum(foreign5.17badenglish,foreign18badenglish),foreign18badenglish/sum(foreign18badenglish,foreign5.17badenglish))),NA)))))
  syntheticdataset$citizenship=ifelse(syntheticdataset$nativity=="Native","Citizen",
                                      ifelse(syntheticdataset$acode=="B16008_022E"|syntheticdataset$acode=="B16008_024E"|syntheticdataset$acode=="B16008_025E"|syntheticdataset$acode=="B16008_027E"|syntheticdataset$acode=="B16008_028E"|syntheticdataset$acode=="B16008_030E"|syntheticdataset$acode=="B16008_032E"|syntheticdataset$acode=="B16008_033E"|syntheticdataset$acode=="B16008_035E"|syntheticdataset$acode=="B16008_037E","Naturalized Citizen",
                                             ifelse(syntheticdataset$acode=="B16008_039E"|syntheticdataset$acode=="B16008_041E"|syntheticdataset$acode=="B16008_042E"|syntheticdataset$acode=="B16008_044E"|syntheticdataset$acode=="B16008_045E"|syntheticdataset$acode=="B16008_047E"|syntheticdataset$acode=="B16008_049E"|syntheticdataset$acode=="B16008_050E"|syntheticdataset$acode=="B16008_052E"|syntheticdataset$acode=="B16008_053E","Not a U.S. Citizen",NA)))
  syntheticdataset$Language.at.home=ifelse(syntheticdataset$English.speaking.skills=="Speaks Only English","English",
                                      ifelse(syntheticdataset$acode=="B16008_006E"|syntheticdataset$acode=="B16008_007E"|syntheticdataset$acode=="B16008_014E"|syntheticdataset$acode=="B16008_015E"|syntheticdataset$acode=="B16008_024E"|syntheticdataset$acode=="B16008_025E"|syntheticdataset$acode=="B16008_032E"|syntheticdataset$acode=="B16008_033E"|syntheticdataset$acode=="B16008_041E"|syntheticdataset$acode=="B16008_042E"|syntheticdataset$acode=="B16008_049E"|syntheticdataset$acode=="B16008_050E","Speaks Spanish",
                                             ifelse(syntheticdataset$acode=="B16008_009E"|syntheticdataset$acode=="B16008_010E"|syntheticdataset$acode=="B16008_017E"|syntheticdataset$acode=="B16008_018E"|syntheticdataset$acode=="B16008_027E"|syntheticdataset$acode=="B16008_028E"|syntheticdataset$acode=="B16008_035E"|syntheticdataset$acode=="B16008_036E"|syntheticdataset$acode=="B16008_044E"|syntheticdataset$acode=="B16008_045E"|syntheticdataset$acode=="B16008_052E"|syntheticdataset$acode=="B16008_053E","Speaks Other Languages",NA)))
  syntheticdataset$acode=NULL
  return(syntheticdataset)
}



getvets <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  veterans=read.csv("veteran_status.csv")
  veterans=veterans[(veterans$tract==tract)&(veterans$county==county),]
  
  men18to34=c(veterans$B21001_008E,veterans$B21001_009E)
  men35to54=c(veterans$B21001_011E,veterans$B21001_012E)
  men55to64=c(veterans$B21001_014E,veterans$B21001_015E)
  men65to74=c(veterans$B21001_017E,veterans$B21001_018E)
  men75up=c(veterans$B21001_020E,veterans$B21001_021E)
  women18to34=c(veterans$B21001_026E,veterans$B21001_027E)
  women35to54=c(veterans$B21001_029E,veterans$B21001_030E)
  women55to64=c(veterans$B21001_032E,veterans$B21001_033E)
  women65to74=c(veterans$B21001_035E,veterans$B21001_036E)
  women75up=c(veterans$B21001_038E,veterans$B21001_039E)
  
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


gettransport <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  transport=read.csv("work_transportation.csv")
  transport=transport[(transport$tract==tract)&(transport$county==county),]
  
  male=c(transport$B08006_020E,transport$B08006_021E,transport$B08006_025E,transport$B08006_031E,transport$B08006_032E,transport$B08006_033E,transport$B08006_034E)
  female=c(transport$B08006_037E,transport$B08006_038E,transport$B08006_042E,transport$B08006_048E,transport$B08006_049E,transport$B08006_050E,transport$B08006_051E)
  
  #novehicleavailable=c(transport$B08141_007E,transport$B08141_012E,transport$B08141_017E,transport$B08141_022E,transport$B08141_027E,transport$B08141_032E)
  #onevehicleavailable=c(transport$B08141_008E,transport$B08141_013E,transport$B08141_018E,transport$B08141_023E,transport$B08141_028E,transport$B08141_033E)
  #twovehiclesavailable=c(transport$B08141_009E,transport$B08141_014E,transport$B08141_019E,transport$B08141_024E,transport$B08141_029E,transport$B08141_034E)
  #threevehiclesavailable=c(transport$B08141_010E,transport$B08141_015E,transport$B08141_020E,transport$B08141_025E,transport$B08141_030E,transport$B08141_035E)
  code=c("drove alone","carpooled","public transportation","bicycle","walked","motorcycle taxicab or other","worked at home")
  
  #transport=ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$number.of.vehicles==0,sample(code,1,prob=novehicleavailable/sum(novehicleavailable)),
   #                ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$number.of.vehicles==1,sample(code,1,prob=onevehicleavailable/sum(onevehicleavailable)),
    #                      ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$number.of.vehicles==2,sample(code,1,prob=twovehiclesavailable/sum(twovehiclesavailable)),
     #                            ifelse(syntheticdataset$employment=="Employed"&(syntheticdataset$number.of.vehicles==3|syntheticdataset$number.of.vehicles==4),sample(code,1,prob=threevehiclesavailable/sum(threevehiclesavailable)),NA))))
  
  transport=ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$sex=="Male",sample(code,1,prob=male/sum(male)),
                       ifelse(syntheticdataset$employment=="Employed"&syntheticdataset$sex=="Female",sample(code,1,prob=female/sum(female)),NA))
  
  syntheticdataset$means.of.transportation.to.work=transport
  return(syntheticdataset)
}



gettraveltime=function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  traveltime=read.csv("travel_time.csv")
  traveltime=traveltime[(traveltime$tract==tract)&(traveltime$county==county),]
  
  drovealone=c(traveltime$B08134_022E,traveltime$B08134_023E,traveltime$B08134_024E,traveltime$B08134_025E,traveltime$B08134_026E,traveltime$B08134_027E,traveltime$B08134_028E,traveltime$B08134_029E,traveltime$B08134_030E)
  carpooled=c(traveltime$B08134_032E,traveltime$B08134_033E,traveltime$B08134_034E,traveltime$B08134_035E,traveltime$B08134_036E,traveltime$B08134_037E,traveltime$B08134_038E,traveltime$B08134_039E,traveltime$B08134_040E)
  publictransport=c(traveltime$B08134_062E,traveltime$B08134_063E,traveltime$B08134_064E,traveltime$B08134_065E,traveltime$B08134_066E,traveltime$B08134_067E,traveltime$B08134_068E,traveltime$B08134_069E,traveltime$B08134_070E)
  walked=c(traveltime$B08134_102E,traveltime$B08134_103E,traveltime$B08134_104E,traveltime$B08134_105E,traveltime$B08134_106E,traveltime$B08134_107E,traveltime$B08134_108E,traveltime$B08134_109E,traveltime$B08134_110E)
  other=c(traveltime$B08134_112E,traveltime$B08134_113E,traveltime$B08134_114E,traveltime$B08134_115E,traveltime$B08134_116E,traveltime$B08134_117E,traveltime$B08134_118E,traveltime$B08134_119E,traveltime$B08134_120E)
  
  code=c("less than 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes","25 to 29 minutes","30 to 34 minutes","35 to 44 minutes","45 to 59 minutes","60 minutes or more")
  
  travel.time.to.work=ifelse(syntheticdataset$means.of.transportation.to.work=="drove alone",sample(code,1,prob=drovealone/sum(drovealone)),
                             ifelse(syntheticdataset$means.of.transportation.to.work=="carpooled",sample(code,1,prob=carpooled/sum(carpooled)),
                                    ifelse(syntheticdataset$means.of.transportation.to.work=="public transport",sample(code,1,prob=publictransport/sum(publictransport)),
                                           ifelse(syntheticdataset$means.of.transportation.to.work=="walked",sample(code,1,prob=walked/sum(walked)),
                                                  ifelse(syntheticdataset$means.of.transportation.to.work=="taxi, motorcycle, bike or other",sample(code,1,prob=other/sum(other)),NA)))))
  syntheticdataset$travel.time.to.work=travel.time.to.work
  return(syntheticdataset)
}


getincome <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  income=read.csv("household_income")
  income=income[(income$tract==tract)&(income$county==county),]
  income$county=NULL
  income$tract=NULL
  income$state=NULL
  
  #This is from when income was cross tabulated with race
  #blackincome=income[c("B19001B_002E","B19001B_003E","B19001B_004E","B19001B_005E","B19001B_006E","B19001B_007E","B19001B_008E","B19001B_009E","B19001B_010E","B19001B_011E","B19001B_012E","B19001B_013E","B19001B_014E","B19001B_015E","B19001B_016E","B19001B_017E")]
  #AIANincome=income[c("B19001C_002E","B19001C_003E","B19001C_004E","B19001C_005E","B19001C_006E","B19001C_007E","B19001C_008E","B19001C_009E","B19001C_010E","B19001C_011E","B19001C_012E","B19001C_013E","B19001C_014E","B19001C_015E","B19001C_016E","B19001C_017E")]
  #asianincome=income[c("B19001D_002E","B19001D_003E","B19001D_004E","B19001D_005E","B19001D_006E","B19001D_007E","B19001D_008E","B19001D_009E","B19001D_010E","B19001D_011E","B19001D_012E","B19001D_013E","B19001D_014E","B19001D_015E","B19001D_016E","B19001D_017E")]
  #NHPIincome=income[c("B19001E_002E","B19001E_003E","B19001E_004E","B19001E_005E","B19001E_006E","B19001E_007E","B19001E_008E","B19001E_009E","B19001E_010E","B19001E_011E","B19001E_012E","B19001E_013E","B19001E_014E","B19001E_015E","B19001E_016E","B19001E_017E")]
  #otherincome=income[c("B19001F_002E","B19001F_003E","B19001F_004E","B19001F_005E","B19001F_006E","B19001F_007E","B19001F_008E","B19001F_009E","B19001F_010E","B19001F_011E","B19001F_012E","B19001F_013E","B19001F_014E","B19001F_015E","B19001F_016E","B19001F_017E")]
  #morethan1income=income[c("B19001G_002E","B19001G_003E","B19001G_004E","B19001G_005E","B19001G_006E","B19001G_007E","B19001G_008E","B19001G_009E","B19001G_010E","B19001G_011E","B19001G_012E","B19001G_013E","B19001G_014E","B19001G_015E","B19001G_016E","B19001G_017E")]
  #whiteincome=income[c("B19001H_002E","B19001H_003E","B19001H_004E","B19001H_005E","B19001H_006E","B19001H_007E","B19001H_008E","B19001H_009E","B19001H_010E","B19001H_011E","B19001H_012E","B19001H_013E","B19001H_014E","B19001H_015E","B19001H_016E","B19001H_017E")]
  #hispanicincome=income[c("B19001I_002E","B19001I_003E","B19001I_004E","B19001I_005E","B19001I_006E","B19001I_007E","B19001I_008E","B19001I_009E","B19001I_010E","B19001I_011E","B19001I_012E","B19001I_013E","B19001I_014E","B19001I_015E","B19001I_016E","B19001I_017E")]
  code=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")
  
  finalsyntheticdataset=data.frame()
  
  samplehouses=unique(syntheticdataset$householdID)
  for(sampleID in samplehouses){
    sampledhouse=subset(syntheticdataset,syntheticdataset$householdID==sampleID)
    
    household.income=sample(code,size=1,prob=c(income/sum(income)))
    household.income=rep(household.income,nrow(sampledhouse))
    sampledhouse$household.income=household.income

    finalsyntheticdataset=rbind(finalsyntheticdataset,sampledhouse)
    
    
  }
  return(syntheticdataset=finalsyntheticdataset)
  #household.income=ifelse(syntheticdataset$race=="Black or African American",sample(code,1,prob=blackincome/sum(blackincome)),
  #                        ifelse(syntheticdataset$race=="American Indian or Alaskan Native",sample(code,1,prob=AIANincome/sum(AIANincome)),
   #                              ifelse(syntheticdataset$race=="Asian",sample(code,1,prob=asianincome/sum(asianincome)),
    #                                    ifelse(syntheticdataset$race=="Native Hawaiian or Other Pacific Islander",sample(code,1,prob=NHPIincome/sum(NHPIincome)),
     #                                          ifelse(syntheticdataset$race=="Some Other Race",sample(code,1,prob=otherincome/sum(otherincome)),
      #                                                ifelse(syntheticdataset$race=="Two or More Races",sample(code,1,prob=morethan1income/sum(morethan1income)),
       #                                                      ifelse(syntheticdataset$race=="White",sample(code,1,prob=whiteincome/sum(whiteincome)),
        #                                                            ifelse(syntheticdataset$race=="Hispanic or Latino",sample(code,1,prob=hispanicincome/sum(hispanicincome)),
         #                 NA))))))))
  
  
  syntheticdataset$household.income=household.income
  return(syntheticdataset)
}


getinsurance <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  insurance=read.csv("health_insurance.csv")
  insurance=insurance[(insurance$tract==tract)&(insurance$county==county),]
  
  #Organize Data Set by row
  under25000=insurance[c("B27015_004E","B27015_005E","B27015_006E")]
  between25to49=insurance[c("B27015_009E","B27015_010E","B27015_011E")]
  between50to75=insurance[c("B27015_014E","B27015_015E","B27015_016E")]
  between75to100=insurance[c("B27015_019E","B27015_020E","B27015_021E")]
  over100=insurance[c("B27015_024E","B27015_025E","B27015_026E")]
  
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