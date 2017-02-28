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
      
      sampleseterror=rbind.fill(sampleseterror,tractsampleseterror)
      
      }
    

  }
  return(sampleseterror)
}