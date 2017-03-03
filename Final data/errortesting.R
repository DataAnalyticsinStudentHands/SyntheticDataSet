#Error testing
source('masterfunction.R')
library("plyr", lib.loc="~/R/win-library/3.3")

sampleset=read.csv('sample_set5.csv')
#sample.set.error=read.csv('error_for_sample_set5.csv')

#for (seed in 1:10){
  #source.of.tracts=read.csv("veteran_status.csv") #choice of file was arbitrary all files have counties and regions
  #a=subset(source.of.tracts,source.of.tracts$county==201)
  #tracts=unique(a$tract)
#  sample.set=master(201,tracts,100,seed)
#  filename <- paste("Harris_sample_set",seed, ".csv", sep="") 
#  sample.set=read.csv(filename)
  
#  error.for.sample.set=run.me.for.errors(sample.set)
#  filename2 <- paste("error_for_Harris.sample_set",seed,".csv",sep="")
#  write.csv(error.for.sample.set,file=filename2)
  
#}


#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
run.me.for.errors=function(sampleset){
  
  sampleseterror=data.frame()
  for (county in unique(sampleset$county)){
    b=county
    getmytracts=subset(sampleset,sampleset$county==b)
    tracts=unique(getmytracts$tract)
    
    for (tract in tracts){
      subsample <- sampleset[(sampleset$tract==tract) & (sampleset$county==b),]
      tractsampleseterror=data.frame(tract)
      #for (col in c(21:23,27:43)){
      #  variables=unique(subsample[,col])
      #  
      #  for (var in variables){
      #    percentage=data.frame(sum(subsample[,col]==var)/length(subsample[,col]))
      #    q=colnames(subsample)
      #    colnames(percentage)=paste(q[col],var,sep=".")
      #    tractsampleseterror=cbind(percentage,tractsampleseterror)
          
        #}
      sexbyagebyrace=read.csv("sex_by_age_by_race.csv")
      
      sexbyagebyrace <- sexbyagebyrace[(sexbyagebyrace$tract==tract) & (sexbyagebyrace$county==b),]
      
      Blackboys=sexbyagebyrace[c("B01001B_003E","B01001B_004E","B01001B_005E","B01001B_006E")]
      BlackMen=sexbyagebyrace[c("B01001B_007E","B01001B_008E","B01001B_009E","B01001B_010E","B01001B_011E","B01001B_012E","B01001B_013E","B01001B_014E","B01001B_015E","B01001B_016E")]
      Blackgirls=sexbyagebyrace[c("B01001B_018E","B01001B_019E","B01001B_020E","B01001B_021E")]
      BlackWomen=sexbyagebyrace[c("B01001B_022E","B01001B_023E","B01001B_024E","B01001B_025E","B01001B_026E","B01001B_027E","B01001B_028E","B01001B_029E","B01001B_030E","B01001B_031E")]
      
      totalBlack=sum(BlackWomen,Blackgirls,BlackMen,Blackboys)
      
      AmerIndianAlaskanboys=sexbyagebyrace[c("B01001C_003E","B01001C_004E","B01001C_005E","B01001C_006E")]
      AmerIndianAlaskanMen=sexbyagebyrace[c("B01001C_007E","B01001C_008E","B01001C_009E","B01001C_010E","B01001C_011E","B01001C_012E","B01001C_013E","B01001C_014E","B01001C_015E","B01001C_016E")]
      AmerIndianAlaskangirls=sexbyagebyrace[c("B01001C_018E","B01001C_019E","B01001C_020E","B01001C_021E")]
      AmerIndianAlaskanWomen=sexbyagebyrace[c("B01001C_022E","B01001C_023E","B01001C_024E","B01001C_025E","B01001C_026E","B01001C_027E","B01001C_028E","B01001C_029E","B01001C_030E","B01001C_031E")]
      
      totalIndianAlaskan=sum(AmerIndianAlaskanMen,AmerIndianAlaskanWomen,AmerIndianAlaskanboys,AmerIndianAlaskangirls)
      
      Asianboys=sexbyagebyrace[c("B01001D_003E","B01001D_004E","B01001D_005E","B01001D_006E")]
      AsianMen=sexbyagebyrace[c("B01001D_007E","B01001D_008E","B01001D_009E","B01001D_010E","B01001D_011E","B01001D_012E","B01001D_013E","B01001D_014E","B01001D_015E","B01001D_016E")]
      Asiangirls=sexbyagebyrace[c("B01001D_018E","B01001D_019E","B01001D_020E","B01001D_021E")]
      AsianWomen=sexbyagebyrace[c("B01001D_022E","B01001D_023E","B01001D_024E","B01001D_025E","B01001D_026E","B01001D_027E","B01001D_028E","B01001D_029E","B01001D_030E","B01001D_031E")]
      
      totalAsian=sum(AsianWomen,Asiangirls,AsianMen,Asianboys)
      
      
      Islanderboys=sexbyagebyrace[c("B01001E_003E","B01001E_004E","B01001E_005E","B01001E_006E")]
      Islandermen=sexbyagebyrace[c("B01001E_007E","B01001E_008E","B01001E_009E","B01001E_010E","B01001E_011E","B01001E_012E","B01001E_013E","B01001E_014E","B01001E_015E","B01001E_016E")]
      Islandergirls=sexbyagebyrace[c("B01001E_018E","B01001E_019E","B01001E_020E","B01001E_021E")]
      Islanderwomen=sexbyagebyrace[c("B01001E_022E","B01001E_023E","B01001E_024E","B01001E_025E","B01001E_026E","B01001E_027E","B01001E_028E","B01001E_029E","B01001E_030E","B01001E_031E")]
      
      totalIslanders=sum(Islandermen,Islanderboys,Islanderwomen,Islandergirls)
      
      OtherRaceboys=sexbyagebyrace[c("B01001F_003E","B01001F_004E","B01001F_005E","B01001F_006E")]
      OtherRaceMen=sexbyagebyrace[c("B01001F_007E","B01001F_008E","B01001F_009E","B01001F_010E","B01001F_011E","B01001F_012E","B01001F_013E","B01001F_014E","B01001F_015E","B01001F_016E")]
      OtherRacegirls=sexbyagebyrace[c("B01001F_018E","B01001F_019E","B01001F_020E","B01001F_021E")]
      OtherRaceWomen=sexbyagebyrace[c("B01001F_022E","B01001F_023E","B01001F_024E","B01001F_025E","B01001F_026E","B01001F_027E","B01001F_028E","B01001F_029E","B01001F_030E","B01001F_031E")]
      
      totalOtherRace=sum(OtherRaceWomen,OtherRacegirls,OtherRaceMen,OtherRaceboys)
      
      MultiRaceboys=sexbyagebyrace[c("B01001G_003E","B01001G_004E","B01001G_005E","B01001G_006E")]
      MultiRaceMen=sexbyagebyrace[c("B01001G_007E","B01001G_008E","B01001G_009E","B01001G_010E","B01001G_011E","B01001G_012E","B01001G_013E","B01001G_014E","B01001G_015E","B01001G_016E")]
      MultiRacegirls=sexbyagebyrace[c("B01001G_018E","B01001G_019E","B01001G_020E","B01001G_021E")]
      MultiRaceWomen=sexbyagebyrace[c("B01001G_022E","B01001G_023E","B01001G_024E","B01001G_025E","B01001G_026E","B01001G_027E","B01001G_028E","B01001G_029E","B01001G_030E","B01001G_031E")]
      
      totalMultiRace=sum(MultiRaceWomen,MultiRaceMen,MultiRaceboys,MultiRacegirls)
      
      Whiteboys=sexbyagebyrace[c("B01001H_003E","B01001H_004E","B01001H_005E","B01001H_006E")]
      WhiteMen=sexbyagebyrace[c("B01001H_007E","B01001H_008E","B01001H_009E","B01001H_010E","B01001H_011E","B01001H_012E","B01001H_013E","B01001H_014E","B01001H_015E","B01001H_016E")]
      Whitegirls=sexbyagebyrace[c("B01001H_018E","B01001H_019E","B01001H_020E","B01001H_021E")]
      WhiteWomen=sexbyagebyrace[c("B01001H_022E","B01001H_023E","B01001H_024E","B01001H_025E","B01001H_026E","B01001H_027E","B01001H_028E","B01001H_029E","B01001H_030E","B01001H_031E")]
      
      totalWhite=sum(WhiteWomen,Whitegirls,WhiteMen,Whiteboys)
      
      Hispanicboys=sexbyagebyrace[c("B01001I_003E","B01001I_004E","B01001I_005E","B01001I_006E")]
      HispanicMen=sexbyagebyrace[c("B01001I_007E","B01001I_008E","B01001I_009E","B01001I_010E","B01001I_011E","B01001I_012E","B01001I_013E","B01001I_014E","B01001I_015E","B01001I_016E")]
      Hispanicgirls=sexbyagebyrace[c("B01001I_018E","B01001I_019E","B01001I_020E","B01001I_021E")]
      HispanicWomen=sexbyagebyrace[c("B01001I_022E","B01001I_023E","B01001I_024E","B01001I_025E","B01001I_026E","B01001I_027E","B01001I_028E","B01001I_029E","B01001I_030E","B01001I_031E")]
      
      totalHispanic=sum(HispanicWomen,HispanicMen,Hispanicboys,Hispanicgirls)
      
      all.the.kids=cbind(Blackboys,Blackgirls,AmerIndianAlaskanboys,AmerIndianAlaskangirls,Asianboys,Asiangirls,Islanderboys,Islandergirls,OtherRaceboys,OtherRacegirls,MultiRaceboys,MultiRacegirls,Whiteboys,Whitegirls,Hispanicboys,Hispanicgirls)
      all.the.adult.men=cbind(BlackMen,AmerIndianAlaskanMen,AsianMen,Islandermen,OtherRaceMen,MultiRaceMen,WhiteMen,HispanicMen)
      all.the.adult.women=cbind(BlackWomen,AmerIndianAlaskanWomen,AsianWomen,Islanderwomen,OtherRaceWomen,MultiRaceWomen,WhiteWomen,HispanicWomen)
      
      raceofsampleset=table(subsample$race)
      raceofcensus=data.frame(totalBlack,totalIndianAlaskan,totalAsian,totalIslanders,totalOtherRace,totalMultiRace,totalWhite,totalHispanic)
      colnames(raceofcensus)=c("Black or African American","American Indian or Alaskan Native","Asian","Native Hawaiian or Other Pacific Islander","Some Other Race","Two or More Races","White","Hispanic or Latino")
      
      for (race in colnames(raceofcensus)){
        successes=c(ifelse(race %in% names(raceofsampleset),raceofsampleset[race],0),raceofcensus[[race]])
        trials=c(sum(raceofsampleset),sum(raceofcensus[1,]))

        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(race,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      Female=cbind(BlackWomen,Blackgirls,AmerIndianAlaskanWomen,AmerIndianAlaskangirls,AsianWomen,Asiangirls,Islanderwomen,Islandergirls,OtherRaceWomen,OtherRacegirls,MultiRaceWomen,MultiRacegirls,WhiteWomen,Whitegirls,HispanicWomen,Hispanicgirls)
      Male=cbind(all.the.adult.men,Blackboys,AmerIndianAlaskanboys,Asianboys,Islanderboys,OtherRaceboys,MultiRaceboys,Whiteboys,Hispanicboys)
      
      males=(c(nrow(subset(subsample,subsample$sex=="Male")),sum(Male)))
      females=c(nrow(subset(subsample,subsample$sex=="Female")),sum(Female))
      totalpeople=c(nrow(subsample),sum(Male,Female))
      
      male.p=prop.test(males,totalpeople)
      female.p=prop.test(females,totalpeople)
      sex.p=data.frame(male.p$p.value,female.p$p.value)
      colnames(sex.p)=c("Male","Female")
      tractsampleseterror=cbind(sex.p,tractsampleseterror)
      
      ageofsampleset=table(subsample$age)
      CensusAges=cbind((AmerIndianAlaskanboys+AmerIndianAlaskangirls+Asianboys+Asiangirls+Blackboys+Blackgirls+Hispanicboys+Hispanicgirls+Islanderboys+Islandergirls+MultiRaceboys+MultiRacegirls+OtherRaceboys+OtherRacegirls+Whiteboys+Whitegirls),(WhiteMen+WhiteWomen+OtherRaceMen+OtherRaceWomen+MultiRaceMen+MultiRaceWomen+Islandermen+Islanderwomen+HispanicMen+HispanicWomen+BlackMen+BlackWomen+AsianMen+AsianWomen+AmerIndianAlaskanMen+AmerIndianAlaskanWomen))
      colnames(CensusAges)<-c("Under 5","5 to 9","10 to 14","15 to 17","18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
      
      for (age in colnames(CensusAges)){
        successes=c(ifelse(age %in% names(ageofsampleset),ageofsampleset[age],0),CensusAges[[age]])
        trials=c(sum(ageofsampleset),sum(CensusAges[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(age,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      enrollschool=read.csv("school_enrollment_by_sex_by_age.csv")
      enrollschool=enrollschool[(enrollschool$tract==tract)&(enrollschool$county==county),]
      
      Women5.9=enrollschool[c("B14003_033E","B14003_042E","B14003_051E")]
      Women10.14=enrollschool[c("B14003_034E","B14003_043E","B14003_052E")]
      Women15.17=enrollschool[c("B14003_035E","B14003_044E","B14003_053E")]
      Women18.19=enrollschool[c("B14003_036E","B14003_045E","B14003_054E")]
      Women20.24=enrollschool[c("B14003_037E","B14003_046E","B14003_055E")]
      Women25.34=enrollschool[c("B14003_038E","B14003_047E","B14003_056E")]
      Women35=enrollschool[c("B14003_039E","B14003_048E","B14003_057E")]
      
      Men5.9=enrollschool[c("B14003_005E","B14003_014E","B14003_023E")]
      Men10.14=enrollschool[c("B14003_006E","B14003_015E","B14003_024E")]
      Men15.17=enrollschool[c("B14003_007E","B14003_016E","B14003_025E")]
      Men18.19=enrollschool[c("B14003_008E","B14003_017E","B14003_026E")]
      Men20.24=enrollschool[c("B14003_009E","B14003_018E","B14003_027E")]
      Men25.34=enrollschool[c("B14003_010E","B14003_019E","B14003_028E")]
      Men35=enrollschool[c("B14003_011E","B14003_020E","B14003_029E")]
      
      school.enrollment.sampleset=table(subsample$school.enrollment)
      census.school.enrollment=(Women35+Women25.34+Women20.24+Women18.19+Women15.17+Women10.14+Women5.9+Men5.9+Men10.14+Men15.17+Men18.19+Men20.24+Men25.34+Men35)
      colnames(census.school.enrollment)=c("Public School","Private School","Not Enrolled in School")
      
      for (school.enrollment in colnames(census.school.enrollment)){
        successes=c(ifelse(school.enrollment %in% names(school.enrollment.sampleset),school.enrollment.sampleset[school.enrollment],0),census.school.enrollment[[school.enrollment]])
        trials=c(sum(school.enrollment.sampleset),sum(census.school.enrollment[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(school.enrollment,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      eduattain=read.csv("education_attainment_by_sex_by_age.csv")
      eduattain=eduattain[(eduattain$tract==tract)&(eduattain$county==county),]
      
      
      men18.24=eduattain[c("B15001_004E","B15001_005E","B15001_006E","B15001_007E","B15001_008E","B15001_009E","B15001_010E")]
      men25.34=eduattain[c("B15001_012E","B15001_013E","B15001_014E","B15001_015E","B15001_016E","B15001_017E","B15001_018E")]
      men35.44=eduattain[c("B15001_020E","B15001_021E","B15001_022E","B15001_023E","B15001_024E","B15001_025E","B15001_026E")]
      men45.64=eduattain[c("B15001_028E","B15001_029E","B15001_030E","B15001_031E","B15001_032E","B15001_033E","B15001_034E")]
      men65=eduattain[c("B15001_036E","B15001_037E","B15001_038E","B15001_039E","B15001_040E","B15001_041E","B15001_042E")]
      
      women18.24=eduattain[c("B15001_045E","B15001_046E","B15001_047E","B15001_048E","B15001_049E","B15001_050E","B15001_051E")]
      women25.34=eduattain[c("B15001_053E","B15001_054E","B15001_055E","B15001_056E","B15001_057E","B15001_058E","B15001_059E")]
      women35.44=eduattain[c("B15001_061E","B15001_062E","B15001_063E","B15001_064E","B15001_065E","B15001_066E","B15001_067E")]
      women45.64=eduattain[c("B15001_069E","B15001_070E","B15001_071E","B15001_072E","B15001_073E","B15001_074E","B15001_075E")]
      women65=eduattain[c("B15001_077E","B15001_078E","B15001_079E","B15001_080E","B15001_081E","B15001_082E","B15001_083E")]
      
      edu.attain.sampleset=table(subsample$education.attainment)
      edu.attain.census=(men65+men45.64+men35.44+men25.34+men18.24+women65+women45.64+women35.44+women25.34+women18.24)
      colnames(edu.attain.census)=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
      
      for (edu.attain in colnames(edu.attain.census)){
        successes=c(ifelse(edu.attain %in% names(edu.attain.sampleset),edu.attain.sampleset[edu.attain],0),edu.attain.census[[edu.attain]])
        trials=c(sum(edu.attain.sampleset),sum(edu.attain.census[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(edu.attain,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      employment=read.csv("employment.csv")
      employment=employment[(employment$tract==tract)&(employment$county==county),]
      
      eWomen16.19=employment[c("B23001_091E","B23001_093E","B23001_094E","B23001_095E")]
      Women20.21=employment[c("B23001_098E","B23001_100E","B23001_101E","B23001_102E")]
      Women22.24=employment[c("B23001_105E","B23001_107E","B23001_108E","B23001_109E")]
      eWomen20.24=c(Women20.21$B23001_098E+Women22.24$B23001_105E,Women20.21$B23001_100E+Women22.24$B23001_107E,Women20.21$B23001_101E+Women22.24$B23001_108E,Women20.21$B23001_102E+Women22.24$B23001_109E)
      eWomen25.29=employment[c("B23001_112E","B23001_114E","B23001_115E","B23001_116E")]
      eWomen30.34=employment[c("B23001_119E","B23001_121E","B23001_122E","B23001_123E")]
      eWomen35.44=employment[c("B23001_126E","B23001_128E","B23001_129E","B23001_130E")]
      eWomen45.54=employment[c("B23001_133E","B23001_135E","B23001_136E","B23001_137E")]
      Women55.59=employment[c("B23001_140E","B23001_142E","B23001_143E","B23001_144E")]
      Women60.61=employment[c("B23001_147E","B23001_149E","B23001_150E","B23001_151E")]
      Women62.64=employment[c("B23001_154E","B23001_156E","B23001_157E","B23001_158E")]
      eWomen55.64=c(Women55.59$B23001_140E+Women60.61$B23001_147E+Women62.64$B23001_154E,Women55.59$B23001_142E+Women60.61$B23001_149E+Women62.64$B23001_156E,Women55.59$B23001_143E+Women60.61$B23001_150E+Women62.64$B23001_157E,Women55.59$B23001_144E+Women60.61$B23001_151E+Women62.64$B23001_158E)
      Women65.69=employment[c("B23001_161E","B23001_162E","B23001_163E")]
      Women70.74=employment[c("B23001_166E","B23001_167E","B23001_168E")]
      eWomen65.74=c(0,Women65.69$B23001_161E+Women70.74$B23001_166E,Women65.69$B23001_162E+Women70.74$B23001_167E,Women65.69$B23001_163E+Women70.74$B23001_168E)
      eWomen75=c(0,as.numeric(employment[c("B23001_171E","B23001_172E","B23001_173E")]))
      eMen16.19=employment[c("B23001_005E","B23001_007E","B23001_008E","B23001_009E")]
      Men20.21=employment[c("B23001_012E","B23001_014E","B23001_015E","B23001_016E")]
      Men22.24=employment[c("B23001_019E","B23001_021E","B23001_022E","B23001_023E")]
      eMen20.24=c(Men20.21$B23001_012E+Men22.24$B23001_019E,Men20.21$B23001_014E+Men22.24$B23001_021E,Men20.21$B23001_015E+Men22.24$B23001_022E,Men20.21$B23001_016E+Men22.24$B23001_023E)
      eMen25.29=employment[c("B23001_026E","B23001_028E","B23001_029E","B23001_030E")]
      eMen30.34=employment[c("B23001_033E","B23001_035E","B23001_036E","B23001_037E")]
      eMen35.44=employment[c("B23001_040E","B23001_042E","B23001_043E","B23001_044E")]
      eMen45.54=employment[c("B23001_047E","B23001_049E","B23001_050E","B23001_051E")]
      Men55.59=employment[c("B23001_054E","B23001_056E","B23001_057E","B23001_058E")]
      Men60.61=employment[c("B23001_061E","B23001_063E","B23001_064E","B23001_065E")]
      Men62.64=employment[c("B23001_068E","B23001_070E","B23001_071E","B23001_072E")]
      eMen55.64=c(Men55.59$B23001_054E+Men60.61$B23001_061E+Men62.64$B23001_068E,Men55.59$B23001_056E+Men60.61$B23001_063E+Men62.64$B23001_070E,Men55.59$B23001_057E+Men60.61$B23001_064E+Men62.64$B23001_071E,Men55.59$B23001_058E+Men60.61$B23001_065E+Men62.64$B23001_072E)
      Men65.69=employment[c("B23001_075E","B23001_076E","B23001_077E")]
      Men70.74=employment[c("B23001_080E","B23001_081E","B23001_082E")]
      eMen65.74=c(0,Men65.69$B23001_075E+Men70.74$B23001_080E,Men65.69$B23001_076E+Men70.74$B23001_081E,Men65.69$B23001_077E+Men70.74$B23001_082E)
      eMen75=c(0,as.numeric(employment[c("B23001_085E","B23001_086E","B23001_087E")]))
      
      census.employment=(eWomen75+eWomen65.74+eWomen55.64+eWomen45.54+eWomen35.44+eWomen30.34+eWomen25.29+eWomen20.24+eWomen16.19+eMen75+eMen65.74+eMen55.64+eMen45.54+eMen35.44+eMen30.34+eMen25.29+eMen20.24+eMen16.19)
      colnames(census.employment)=c("In Armed Forces","Employed","Unemployed","Not in labor force")
      employment.sampleset=table(subsample$employment)
      
      for (employment.status in colnames(census.employment)){
        successes=c(ifelse(employment.status %in% names(employment.sampleset),employment.sampleset[employment.status],0),census.employment[[employment.status]])
        trials=c(sum(employment.sampleset),sum(census.employment[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(employment.status,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      disability=read.csv("disability_status.csv")
      disability=disability[(disability$tract==tract)&(disability$county==county),]
      
      Under18=disability[c("C18108_003E","C18108_004E","C18108_005E")]
      From18.64=disability[c("C18108_007E","C18108_008E","C18108_009E")]
      Over65=disability[c("C18108_011E","C18108_012E","C18108_013E")]
      
      disability.census=(Under18+From18.64+Over65)
      colnames(disability.census)=c("With One Type of Disability","With Two or More Types of Disabilities","No Disabilities")
      disability.sampleset=table(subsample$disability)
      
      for (disable in colnames(disability.census)){
        successes=c(ifelse(disable %in% names(disability.sampleset),disability.sampleset[disable],0),disability.census[[disable]])
        trials=c(sum(disability.sampleset),sum(disability.census[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(disable,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
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
      
      censusvets=(men18to34+men35to54+men55to64+men65to74+men75up+women18to34+women35to54+women55to64+women65to74+women75up)
      censusvets=data.frame(Veteran=censusvets[1],Nonveteran=censusvets[2])
      vetsampleset=table(subsample$veteran.status)
      
      for (vet in colnames(censusvets)){
       successes=c(ifelse(vet %in% names(vetsampleset),vetsampleset[vet],0),censusvets[[vet]])
       trials=c(sum(vetsampleset),sum(censusvets[1,]))
      
      p=prop.test(successes,trials, correct=FALSE)
      p=data.frame(p$p.value)
      colnames(p)=paste(vet,sep=".")
      tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
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
      
      lang=(languagehispanic+languagewhite+language2+languageother+languageNHPI+languageasian+languageAIAN+languageblack)
      nativity.census=data.frame(Native=sum(lang[1],lang[2],lang[3]),Foreign=sum(lang[4],lang[5],lang[6]))
      nativity.sampleset=table(subsample$nativity)
      
      for (nat in colnames(nativity.census)){
        successes=c(ifelse(nat %in% names(nativity.sampleset),nativity.sampleset[nat],0),nativity.census[[nat]])
        trials=c(sum(nativity.sampleset),sum(nativity.census[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(nat,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      english.census=data.frame("Speaks Only English"=sum(lang[1],lang[4]),"Speaks English Very Well"=sum(lang[2],lang[5]),"Speaks English Not Very Well"=sum(lang[3],lang[6]))
      english.sampleset=table(subsample$English.speaking.skills)
      
      for (Eng in colnames(english.census)){
        successes=c(ifelse(Eng %in% names(english.sampleset),english.sampleset[Eng],0),english.census[[Eng]])
        trials=c(sum(english.sampleset),sum(english.census[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(Eng,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      transport=read.csv("work_transportation.csv")
      transport=transport[(transport$tract==tract)&(transport$county==county),]
      
      tmale=c(transport$B08006_020E,transport$B08006_021E,transport$B08006_025E,transport$B08006_031E,transport$B08006_032E,transport$B08006_033E,transport$B08006_034E)
      tfemale=c(transport$B08006_037E,transport$B08006_038E,transport$B08006_042E,transport$B08006_048E,transport$B08006_049E,transport$B08006_050E,transport$B08006_051E)
      transport.census=(tmale+tfemale)
      transport.census<-data.frame("drove alone"=transport.census[1],"carpooled"=transport.census[2],"public transportation"=transport.census[3],"bicycle"=transport.census[4],"walked"=transport.census[5],"motorcycle taxicab or other"=transport.census[6],"worked at home"=transport.census[7])
      transport.sampleset=table(subsample$means.of.transportation.to.work)
      
      for (t in colnames(transport.census)){
        successes=c(ifelse(t %in% names(transport.sampleset),transport.sampleset[t],0),transport.census[[t]])
        trials=c(sum(transport.sampleset),sum(transport.census[1,]))
        
        if(!(0 %in% trials)){
          p=prop.test(successes,trials, correct=FALSE)
          p=data.frame(p$p.value)
        }
        
        if((0 %in% trials)){
          p=data.frame("No one is working :(")
        }
        
        colnames(p)=paste(t,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      traveltime=read.csv("travel_time.csv")
      traveltime=traveltime[(traveltime$tract==tract)&(traveltime$county==county),]
      
      drovealone=c(traveltime$B08134_022E,traveltime$B08134_023E,traveltime$B08134_024E,traveltime$B08134_025E,traveltime$B08134_026E,traveltime$B08134_027E,traveltime$B08134_028E,traveltime$B08134_029E,traveltime$B08134_030E)
      carpooled=c(traveltime$B08134_032E,traveltime$B08134_033E,traveltime$B08134_034E,traveltime$B08134_035E,traveltime$B08134_036E,traveltime$B08134_037E,traveltime$B08134_038E,traveltime$B08134_039E,traveltime$B08134_040E)
      publictransport=c(traveltime$B08134_062E,traveltime$B08134_063E,traveltime$B08134_064E,traveltime$B08134_065E,traveltime$B08134_066E,traveltime$B08134_067E,traveltime$B08134_068E,traveltime$B08134_069E,traveltime$B08134_070E)
      walked=c(traveltime$B08134_102E,traveltime$B08134_103E,traveltime$B08134_104E,traveltime$B08134_105E,traveltime$B08134_106E,traveltime$B08134_107E,traveltime$B08134_108E,traveltime$B08134_109E,traveltime$B08134_110E)
      other=c(traveltime$B08134_112E,traveltime$B08134_113E,traveltime$B08134_114E,traveltime$B08134_115E,traveltime$B08134_116E,traveltime$B08134_117E,traveltime$B08134_118E,traveltime$B08134_119E,traveltime$B08134_120E)
      
      time.census=(drovealone+carpooled+publictransport+walked+other)
      colnames(time.census)=c("less than 10 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes","25 to 29 minutes","30 to 34 minutes","35 to 44 minutes","45 to 59 minutes","60 minutes or more")
      
      time.sampleset=table(subsample$travel.time.to.work)
      
      for (time in colnames(time.census)){
        successes=c(ifelse(time %in% names(time.sampleset),time.sampleset[t],0),time.census[[t]])
        trials=c(sum(time.sampleset),sum(time.census[1,]))
        
        if(!(0 %in% trials)){
          p=prop.test(successes,trials, correct=FALSE)
          p=data.frame(p$p.value)
        }
        
        if((0 %in% trials)){
          p=data.frame("No one is working :(")
        }
        
        colnames(p)=paste(time(),sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      
      citizenship.and.language=read.csv('citizenship_language_for_error.csv')
      #because I had not included natives who only speak English when mining for the U.S. Census data in the first place, 
      #as there language and citizenship are already known, I re-mined this data to get it un-cross-tabulated
      citizenship.and.language=citizenship.and.language[(citizenship.and.language$tract==tract)&(citizenship.and.language$county==county),]
      
      citizenship.census=citizenship.and.language[,c("Citizen","Naturalized Citizen","Not a U.S. Citizen")]
      citizenship.sampleset=table(subsample$citizenship)
      
      for (c in colnames(citizenship.census)){
        successes=c(ifelse(c %in% names(citizenship.sampleset),citizenship.sampleset[c],0),citizenship.census[[c]])
        trials=c(sum(citizenship.sampleset),sum(citizenship.census[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(c,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      
      at.home.lang.census=citizenship.and.language[,c("English","Speaks Spanish","Speaks Other Language")]
      at.home.lang.sampleset=table(subsample$Language.at.home)
      
      for (lang in colnames(at.home.lang.census)){
        successes=c(ifelse(lang %in% names(at.home.lang.sampleset),at.home.lang.sampleset[c],0),at.home.lang.census[[c]])
        trials=c(sum(at.home.lang.sampleset),sum(at.home.lang.census[1,]))
        
        p=prop.test(successes,trials, correct=FALSE)
        p=data.frame(p$p.value)
        colnames(p)=paste(lang,sep=".")
        tractsampleseterror=cbind(p,tractsampleseterror)
      }
      #insurance=read.csv("health_insurance.csv")
      #insurance=insurance[(insurance$tract==tract)&(insurance$county==county),]
      
      #Organize Data Set by row
      #under25000=insurance[c("B27015_004E","B27015_005E","B27015_006E")]
      #between25to49=insurance[c("B27015_009E","B27015_010E","B27015_011E")]
      #between50to75=insurance[c("B27015_014E","B27015_015E","B27015_016E")]
      #between75to100=insurance[c("B27015_019E","B27015_020E","B27015_021E")]
      #over100=insurance[c("B27015_024E","B27015_025E","B27015_026E")]
      
      #insurance.census=(under25000+between75to100+between50to75+between25to49+over100)
      #colnames(insurance.census)=c("private insurance","public insurance","no insurance")
      
      #insurance.sampleset=table(subsample$health.insurance)
      
      #for (health.insure in colnames(insurance.census)){
       # successes=c(ifelse(health.insure %in% names(insurance.sampleset),insurance.sampleset[health.insure],0),insurance.census[[health.insure]])
        #trials=c(sum(insurance.sampleset),sum(insurance.census[1,]))
        
        #p=prop.test(successes,trials, correct=FALSE)
        #p=data.frame(p$p.value)
        #colnames(p)=paste(health.insure,sep=".")
        #tractsampleseterror=cbind(p,tractsampleseterror)
      #}
      
      sampleseterror=rbind.fill(sampleseterror,tractsampleseterror)
      
    }
    

  }
  return(sampleseterror)
}