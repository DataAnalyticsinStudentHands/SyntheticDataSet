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

dans_diabetes <- function(member,sex,education.attainment,race,seedy){
  #from table 1a in the appendix of the National Diabetes Statistics Report 2017
  if(member=="Child"){
    dans_diabetes=NA
    return(dans_diabetes)
  }
  else{
    chance=0.115
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance+0.008,chance-0.007)
    #Change Chance by Education
    if(education.attainment %in% c("Less than 9th grade","9th to 12th grade, no diploma")){
      chance=chance+0.04
    }
    if(education.attainment=="High School Graduate"){
      chance=chance+0.02
    }
    if(education.attainment %in% c("Associate's degree","Some College, no degree","Graduate or Professional Degree","Bachelor's Degree")){
      chance=chance-0.019
    }
    #Change chance by Race
    if (race=="White"){chance=chance-0.022}
    if (race=="Black or African American"){chance=chance+0.062}
    if (race=="Hispanic or Latino"){chance=chance+0.049}
    if (race=="Asian"){chance=chance+0.045}
    #And finally sample
    set.seed(seedy)
    dans_diabetes=sample(c("yes","no"),1,prob=c(chance,1-chance))
    return(dans_diabetes)
  }
}

syntheticdataset$dans_diabetes=mapply(dans_diabetes,syntheticdataset$member,syntheticdataset$sex,syntheticdataset$education.attainment,syntheticdataset$race,syntheticdataset$X)

dans_prediabetes <- function(member,dans_diabetes,sex,education.attainment,race,seedy){
  #from table 3a in the appendix of the National Diabetes Statistics Report 2017
  if(member=="Child"|dans_diabetes=="yes"){
    dans_diabetes=NA
    return(dans_diabetes)
  }
  else{
    chance=33
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance+3.6,chance-3.7)
    #Change Chance by Education
    if(education.attainment %in% c("Less than 9th grade","9th to 12th grade, no diploma")){
      chance=chance+4.6
    }
    if(education.attainment=="High School Graduate"){
      chance=chance+4
    }
    if(education.attainment %in% c("Associate's degree","Some College, no degree","Graduate or Professional Degree","Bachelor's Degree")){
      chance=chance-2.6
    }
    #Change chance by Race
    if (race=="White"){chance=chance-1.5}
    if (race=="Black or African American"){chance=chance+3.3}
    if (race=="Hispanic or Latino"){chance=chance-1.3}
    if (race=="Asian"){chance=chance+2.7}
    #And finally sample
    set.seed(seedy)
    dans_diabetes=sample(c("yes","no"),1,prob=c(chance/(100),1-(chance/(100))))
    return(dans_diabetes)
  }
}

syntheticdataset$dans_prediabetes=mapply(dans_prediabetes,syntheticdataset$member,syntheticdataset$dans_diabetes,syntheticdataset$sex,syntheticdataset$education.attainment,syntheticdataset$race,syntheticdataset$X)

dans_diabetes_diagnosed <- function(dans_diabetes,sex,education.attainment,race,seedy){
  #from table 1a in the appendix of the National Diabetes Statistics Report 2017
  if(dans_diabetes=="no"|is.na(dans_diabetes)){
    dans_diabetes_diagnosed=NA
    return(dans_diabetes_diagnosed)
  }
  else{
    chance=0.756
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance-0.016,chance+0.031)
    #Change Chance by Education
    if(education.attainment %in% c("Less than 9th grade","9th to 12th grade, no diploma")){
      chance=chance-0.021
    }
    if(education.attainment=="High School Graduate"){
      chance=chance+0.007
    }
    if(education.attainment %in% c("Associate's degree","Some College, no degree","Graduate or Professional Degree","Bachelor's Degree")){
      chance=chance+0.015
    }
    #Change chance by Race
    if (race=="White"){chance=chance+0.029}
    if (race=="Black or African American"){chance=chance+0.001}
    if (race=="Hispanic or Latino"){chance=chance-0.030}
    if (race=="Asian"){chance=chance-0.112}
    #And finally sample
    set.seed(seedy)
    dans_diabetes_diagnosed=sample(c("yes","no"),1,prob=c(chance,1-chance))
    return(dans_diabetes_diagnosed)
  }
}

syntheticdataset$dans_diabetes_diagnosed=mapply(dans_diabetes_diagnosed,syntheticdataset$dans_diabetes,syntheticdataset$sex,syntheticdataset$education.attainment,syntheticdataset$race,syntheticdataset$X)

dans_prediabetes_reported <- function(dans_prediabetes,sex,education.attainment,race,seedy){
  #from table 3a in the appendix of the National Diabetes Statistics Report 2017
  if(dans_prediabetes=="no"|is.na(dans_prediabetes)){
    dans_prediabetes_reported=NA
    return(dans_prediabetes_reported)
  }
  else{
    chance=0.321
    #Change Chance by Sex
    chance=ifelse(sex=="Male",chance-0.078,chance+0.132)
    #Change Chance by Education
    if(education.attainment %in% c("Less than 9th grade","9th to 12th grade, no diploma")){
      chance=chance-0.074
    }
    if(education.attainment=="High School Graduate"){
      chance=chance+0.014
    }
    if(education.attainment %in% c("Associate's degree","Some College, no degree","Graduate or Professional Degree","Bachelor's Degree")){
      chance=chance+0.021
    }
    #Change chance by Race
    if (race=="White"){chance=chance+0.038}
    if (race=="Black or African American"){chance=chance-0.032}
    if (race=="Hispanic or Latino"){chance=chance-0.084}
    if (race=="Asian"){chance=chance-0.069}
    #And finally sample
    set.seed(seedy)
    dans_prediabetes_reported=sample(c("yes","no"),1,prob=c(chance,1-(chance)))
    return(dans_prediabetes_reported)
  }
}

syntheticdataset$dans_prediabetes_reported=mapply(dans_prediabetes_reported,syntheticdataset$dans_prediabetes,syntheticdataset$sex,syntheticdataset$education.attainment,syntheticdataset$race,syntheticdataset$X)
