#syntheticdataset=read.csv('mergedsampleset.csv')


childhood.asthma <- function(member,sex,age,race,seedy){
  if(member!="Child"){
    childhood.asthma=NA
    return(childhood.asthma)
  }
  else{
    chance=0.084
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance+0.015,chance-0.015)
    #Change Chance by Age
    if(age=="Under 5"){
      chance=chance-0.037
    }
    if(age %in% c("5 to 9","10 to 14","15 to 17")){
      chance=chance+0.014
    }
    #Change chance by Race
    if (race=="White"){chance=chance-0.01}
    if (race=="Black or African American"){chance=chance+0.05}
    if (race=="Hispanic or Latino"){chance=chance-0.0004}
    
    #And finally sample
    set.seed(seedy)
    childhood.asthma=sample(c("yes","no"),1,prob=c(chance,1-chance))
    return(childhood.asthma)
  }
}

syntheticdataset$childhood.asthma=mapply(childhood.asthma,syntheticdataset$member,syntheticdataset$sex,syntheticdataset$age,syntheticdataset$race,syntheticdataset$X)





adults.diagnosed.with.depression.in.lifetime <- function(member,sex,age,race,seedy){
  if(member=="Child"){
    depression=NA
    return(depression)
  }
  else{
    chance=0.161
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance-0.049,chance+0.046)
    #Change Chance by Age
    if(age %in% c("18 to 19","20 to 24")){
      chance=chance-0.013
    }
    if(age %in% c("25 to 29","30 to 34")){
      chance=chance-0.017
    }
    if(age=="35 to 44"){
      chance=chance+0.006
    }
    if(age=="45 to 54"){
      chance=chance+0.038
    }
    if(age %in% c("55 to 64","65 to 74","75 to 84","Over 85")){
      chance=chance-0.012
    }
    
    #Change chance by Race
    if (race=="White"){chance=chance+0.012}
    if (race=="Black or African American"){chance=chance-0.038}
    if (race=="Hispanic or Latino"){chance=chance-0.033}
    if (!(race %in% c("White","Black or African American","Hispanic or Latino")))
      {chance=chance-0.039}
    
    #And finally sample
    set.seed(seedy)
    depression=sample(c("yes","no"),1,prob=c(chance,1-chance))
    return(depression)
  }
}

syntheticdataset$diagnosed.depression.adults=mapply(adults.diagnosed.with.depression.in.lifetime,syntheticdataset$member,syntheticdataset$sex,syntheticdataset$age,syntheticdataset$race,syntheticdataset$X)


adults.diagnosed.with.anxiety.in.lifetime <- function(member,sex,age,race,seedy){
  if(member=="Child"){
    anxiety=NA
    return(anxiety)
  }
  else{
    chance=0.123
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance-0.032,chance+0.029)
    #Change Chance by Age
    if(age %in% c("18 to 19","20 to 24")){
      chance=chance+0.007
    }
    if(age %in% c("25 to 29","30 to 34")){
      chance=chance-0.004
    }
    if(age=="35 to 44"){
      chance=chance+0.013
    }
    if(age=="45 to 54"){
      chance=chance+0.018
    }
    if(age %in% c("55 to 64","65 to 74","75 to 84","Over 85")){
      chance=chance-0.02
    }
    
    #Change chance by Race
    if (race=="White"){chance=chance+0.006}
    if (race=="Black or African American"){chance=chance-0.03}
    if (race=="Hispanic or Latino"){chance=chance-0.026}
    if (!(race %in% c("White","Black or African American","Hispanic or Latino")))
    {chance=chance-0.005}
    
    #And finally sample
    set.seed(seedy)
    anxiety=sample(c("yes","no"),1,prob=c(chance,1-chance))
    return(anxiety)
  }
}

syntheticdataset$diagnosed.anxiety.adults=mapply(adults.diagnosed.with.anxiety.in.lifetime,syntheticdataset$member,syntheticdataset$sex,syntheticdataset$age,syntheticdataset$race,syntheticdataset$X)

mentally.unhealthy.days.in.month=function(syntheticdataset,seedy){
  sex=c("Male","Female")
  sex.mean.days=c(2.9,4.0)
  sex.respondents=c(161046,263172)
  sex.upperconfidence.boundary=c(3.1,4.1)
  
  age=c("18 to 19","20 to 24","25 to 29","30 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","Over 85")
  age.mean.days=c(3.9,3.9,3.8,3.8,3.7,3.9,2.8,2.8,2.8,2.8)
  age.respondents=c(13032,13032,36755,36755,59253,85446,226168,226168,226168,226168)
  age.upperconfidence.boundary=c(4.2,4.2,4.0,4.0,3.9,4.1,2.9,2.9,2.9,2.9)
  
  race=c("White","Black or African American","Hispanic or Latino","Asian","Some Other Race","Two or More Races","American Indian or Alaskan Native","Native Hawaiian or Other Pacific Islander")
  race.mean.days=c(3.3,4.1,3.8,3.6,3.6,3.6,3.6,3.6)
  race.respondents=c(333119,33741,29361,23549,23549,23549,23549,23549)
  race.upperconfidence.boundary=c(3.4,4.4,4.1,3.3,3.3,3.3,3.3,3.3)
  
  for (s in 1:2){
    for (a in 1:10){
      for (r in 1:8){
        updatedmean=(sex.mean.days[s]*sex.respondents[s]+age.mean.days[a]*age.respondents[a]+race.mean.days[r]*race.respondents[r])/(sex.respondents[s]+age.respondents[a]+race.respondents[r])
        

      }
    }
  }
}
