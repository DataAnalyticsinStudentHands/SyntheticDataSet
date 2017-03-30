#set.seed(2)
#tractvar=550402

gethouseholdtypeandrace <- function(county,tract,number.of.households,seed){
  set.seed(seed)
  householdtypeandrace=read.csv("householdtypeandrace.csv")
  
    house <- householdtypeandrace[(householdtypeandrace$tract==tract) & (householdtypeandrace$county==county),]
    total1=sum(house[1,4:43])
    prob1=(house[1,4:43])/total1
    
    finalsyntheticdataset=data.frame()
    if( sum(is.na(prob1))!=0 ) stop(tract,county)
      samples=sample(colnames(prob1),size=number.of.households,replace=TRUE,prob=prob1)
      
      #Guess Size assuming Type
      householdsizebytype=read.csv("household_size.csv")
      
      family=householdsizebytype[c("tract","B11016_003E","B11016_004E","B11016_005E","B11016_006E","B11016_007E","B11016_008E")]
      nonfamily=householdsizebytype[c("tract","B11016_011E","B11016_012E","B11016_013E","B11016_014E","B11016_015E","B11016_016E")]
      
      index=1
      for (a in samples){
        #Guess Size Knowing it's a family household
        if (a=="B11001B_003E"|a=="B11001B_005E"|a=="B11001B_006E"|a=="B11001C_003E"|a=="B11001C_005E"|a=="B11001C_006E"|a=="B11001D_003E"|a=="B11001D_005E"|a=="B11001D_006E"|a=="B11001E_003E"|a=="B11001E_005E"|a=="B11001E_006E"|a=="B11001F_003E"|a=="B11001F_005E"|a=="B11001F_006E"|a=="B11001G_003E"|a=="B11001G_005E"|a=="B11001G_006E"|a=="B11001H_003E"|a=="B11001H_005E"|a=="B11001H_006E"|a=="B11001I_003E"|a=="B11001I_005E"|a=="B11001I_006E"){#if it's a family household
          house=family[family$tract==tract,]
          total=sum(house[1,2:6])
          prob=(house[1,2:6])/total
          b=sample(colnames(prob),size=1,prob=prob)
        }
        
        #Guess Size Knowing i's a Non-family Household
        if (a=="B11001I_009E"|a=="B11001H_009E"|a=="B11001G_009E"|a=="B11001F_009E"|a=="B11001E_009E"|a=="B11001D_009E"|a=="B11001C_009E"|a=="B11001B_009E"){#if it's a non-family household
          house=nonfamily[nonfamily$tract==tract,]
          total=sum(house[1,2:6])
          prob=(house[1,2:6])/total
          b=sample(colnames(prob),size=1,prob=prob)
        }
        
        #Alone means Alone
        if (a=="B11001I_008E"|a=="B11001H_008E"|a=="B11001G_008E"|a=="B11001F_008E"|a=="B11001E_008E"|a=="B11001D_008E"|a=="B11001C_008E"|a=="B11001B_008E"){
          b="Alone"
        }
        
        ##Start Making Dataframe for household!
        
        if (b=="Alone"){
          syntheticdataset=data.frame(household.type="Alone",member="Householder",race=a,size=1)
        }
        
        if (b=="B11016_003E"){ #two member family household
          if (a=="B11001I_003E"|a=="B11001H_003E"|a=="B11001G_003E"|a=="B11001F_003E"|a=="B11001E_003E"|a=="B11001D_003E"|a=="B11001C_003E"|a=="B11001B_003E"){
            syntheticdataset=data.frame(household.type=c("Married Couple","Married Couple"),member=c("Husband","Wife"),race=c(a,a),size=rep(2,2))
          }
          if (a=="B11001I_005E"|a=="B11001H_005E"|a=="B11001G_005E"|a=="B11001F_005E"|a=="B11001E_005E"|a=="B11001D_005E"|a=="B11001C_005E"|a=="B11001B_005E"){
            syntheticdataset=data.frame(household.type=c("Male Householder family no wife present","Male Householder family no wife present"),member=c("Male Householder","NA"),race=c(a,a),size=rep(2,2))
          }
          if (a=="B11001I_006E"|a=="B11001H_006E"|a=="B11001G_006E"|a=="B11001F_006E"|a=="B11001E_006E"|a=="B11001D_006E"|a=="B11001C_006E"|a=="B11001B_006E"){
            syntheticdataset=data.frame(household.type=c("Female Householder family no husband present","Female Householder family no husband present"),member=c("Female Householder","NA"),race=c(a,a),size=rep(2,2))
          }
        }
        
        if (b=="B11016_004E"){ #three member family household
          if (a=="B11001I_003E"|a=="B11001H_003E"|a=="B11001G_003E"|a=="B11001F_003E"|a=="B11001E_003E"|a=="B11001D_003E"|a=="B11001C_003E"|a=="B11001B_003E"){
            syntheticdataset=data.frame(household.type=c("Married Couple","Married Couple","Married Couple"),member=c("Husband","Wife","NA"),race=c(a,a,a),size=rep(3,3))
          }
          if (a=="B11001I_005E"|a=="B11001H_005E"|a=="B11001G_005E"|a=="B11001F_005E"|a=="B11001E_005E"|a=="B11001D_005E"|a=="B11001C_005E"|a=="B11001B_005E"){
            syntheticdataset=data.frame(household.type=c("Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present"),member=c("Male Householder","NA","NA"),race=c(a,a,a),size=rep(3,3))
          }
          if (a=="B11001I_006E"|a=="B11001H_006E"|a=="B11001G_006E"|a=="B11001F_006E"|a=="B11001E_006E"|a=="B11001D_006E"|a=="B11001C_006E"|a=="B11001B_006E"){
            syntheticdataset=data.frame(household.type=c("Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present"),member=c("Female Householder","NA","NA"),race=c(a,a,a),size=rep(3,3))
          }
        }
        
        if (b=="B11016_005E"){ #four member family household
          if (a=="B11001I_003E"|a=="B11001H_003E"|a=="B11001G_003E"|a=="B11001F_003E"|a=="B11001E_003E"|a=="B11001D_003E"|a=="B11001C_003E"|a=="B11001B_003E"){
            syntheticdataset=data.frame(household.type=c("Married Couple","Married Couple","Married Couple","Married Couple"),member=c("Husband","Wife","NA","NA"),race=c(a,a,a,a),size=rep(4,4))
          }
          if (a=="B11001I_005E"|a=="B11001H_005E"|a=="B11001G_005E"|a=="B11001F_005E"|a=="B11001E_005E"|a=="B11001D_005E"|a=="B11001C_005E"|a=="B11001B_005E"){
            syntheticdataset=data.frame(household.type=c("Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present"),member=c("Male Householder","NA","NA","NA"),race=c(a,a,a,a),size=rep(4,4))
          }
          if (a=="B11001I_006E"|a=="B11001H_006E"|a=="B11001G_006E"|a=="B11001F_006E"|a=="B11001E_006E"|a=="B11001D_006E"|a=="B11001C_006E"|a=="B11001B_006E"){
            syntheticdataset=data.frame(household.type=c("Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present"),member=c("Female Householder","NA","NA","NA"),race=c(a,a,a,a),size=rep(4,4))
          }
        }  
        
        if (b=="B11016_006E"){ #five member family household
          if (a=="B11001I_003E"|a=="B11001H_003E"|a=="B11001G_003E"|a=="B11001F_003E"|a=="B11001E_003E"|a=="B11001D_003E"|a=="B11001C_003E"|a=="B11001B_003E"){
            syntheticdataset=data.frame(household.type=c("Married Couple","Married Couple","Married Couple","Married Couple","Married Couple"),member=c("Husband","Wife","NA","NA","NA"),race=c(a,a,a,a,a),size=rep(5,5))
          }
          if (a=="B11001I_005E"|a=="B11001H_005E"|a=="B11001G_005E"|a=="B11001F_005E"|a=="B11001E_005E"|a=="B11001D_005E"|a=="B11001C_005E"|a=="B11001B_005E"){
            syntheticdataset=data.frame(household.type=c("Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present"),member=c("Male Householder","NA","NA","NA","NA"),race=c(a,a,a,a,a),size=rep(5,5))
          }
          if (a=="B11001I_006E"|a=="B11001H_006E"|a=="B11001G_006E"|a=="B11001F_006E"|a=="B11001E_006E"|a=="B11001D_006E"|a=="B11001C_006E"|a=="B11001B_006E"){
            syntheticdataset=data.frame(household.type=c("Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present"),member=c("Female Householder","NA","NA","NA","NA"),race=c(a,a,a,a,a),size=rep(5,5))
          }
        }
        
        if (b=="B11016_007E"){ #six member family household
          if (a=="B11001I_003E"|a=="B11001H_003E"|a=="B11001G_003E"|a=="B11001F_003E"|a=="B11001E_003E"|a=="B11001D_003E"|a=="B11001C_003E"|a=="B11001B_003E"){
            syntheticdataset=data.frame(household.type=c("Married Couple","Married Couple","Married Couple","Married Couple","Married Couple","Married Couple"),member=c("Husband","Wife","NA","NA","NA","NA"),race=c(a,a,a,a,a,a),size=rep(6,6))
          }
          if (a=="B11001I_005E"|a=="B11001H_005E"|a=="B11001G_005E"|a=="B11001F_005E"|a=="B11001E_005E"|a=="B11001D_005E"|a=="B11001C_005E"|a=="B11001B_005E"){
            syntheticdataset=data.frame(household.type=c("Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present"),member=c("Male Householder","NA","NA","NA","NA","NA"),race=c(a,a,a,a,a,a),size=rep(6,6))
          }
          if (a=="B11001I_006E"|a=="B11001H_006E"|a=="B11001G_006E"|a=="B11001F_006E"|a=="B11001E_006E"|a=="B11001D_006E"|a=="B11001C_006E"|a=="B11001B_006E"){
            syntheticdataset=data.frame(household.type=c("Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present"),member=c("Female Householder","NA","NA","NA","NA","NA"),race=c(a,a,a,a,a,a),size=rep(6,6))
          }
        }
        
        if (b=="B11016_008E"){ #seven member family household
          if (a=="B11001I_003E"|a=="B11001H_003E"|a=="B11001G_003E"|a=="B11001F_003E"|a=="B11001E_003E"|a=="B11001D_003E"|a=="B11001C_003E"|a=="B11001B_003E"){
            syntheticdataset=data.frame(household.type=c("Married Couple","Married Couple","Married Couple","Married Couple","Married Couple","Married Couple"),member=c("Husband","Wife","NA","NA","NA","NA","NA"),race=c(a,a,a,a,a,a,a),size=rep(7,7))
          }
          if (a=="B11001I_005E"|a=="B11001H_005E"|a=="B11001G_005E"|a=="B11001F_005E"|a=="B11001E_005E"|a=="B11001D_005E"|a=="B11001C_005E"|a=="B11001B_005E"){
            syntheticdataset=data.frame(household.type=c("Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present","Male Householder family no wife present"),member=c("Male Householder","NA","NA","NA","NA","NA","NA"),race=c(a,a,a,a,a,a,a),size=rep(7,7))
          }
          if (a=="B11001I_006E"|a=="B11001H_006E"|a=="B11001G_006E"|a=="B11001F_006E"|a=="B11001E_006E"|a=="B11001D_006E"|a=="B11001C_006E"|a=="B11001B_006E"){
            syntheticdataset=data.frame(household.type=c("Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present","Female Householder family no husband present"),member=c("Female Householder","NA","NA","NA","NA","NA","NA"),race=c(a,a,a,a,a,a,a),size=rep(7,7))
          }
        }
        
        if (b=="B11016_011E"){ #two member non-family household
          syntheticdataset=data.frame(household.type=c("Non-family","Non-family"),member=c("Householder","NA"),race=c(a,a),size=rep(2,2))
        }
        
        if (b=="B11016_012E"){ #3 member non-family household
          syntheticdataset=data.frame(household.type=c("Non-family","Non-family","Non-family"),member=c("Householder","NA","NA"),race=c(a,a,a),size=rep(3,3))
        }
        
        if (b=="B11016_013E"){ #4 member non-family household
          syntheticdataset=data.frame(household.type=c("Non-family","Non-family","Non-family","Non-family"),member=c("Householder","NA","NA","NA"),race=c(a,a,a,a),size=rep(4,4))
        }
        
        if (b=="B11016_014E"){ #5 member non-family household
          syntheticdataset=data.frame(household.type=c("Non-family","Non-family","Non-family","Non-family","Non-family"),member=c("Householder","NA","NA","NA","NA"),race=c(a,a,a,a,a),size=rep(5,5))
        }
        
        if (b=="B11016_015E"){ #6 member non-family household
          syntheticdataset=data.frame(household.type=c("Non-family","Non-family","Non-family","Non-family","Non-family","Non-family"),member=c("Householder","NA","NA","NA","NA","NA"),race=c(a,a,a,a,a,a),size=rep(6,6))
        }
        
        if (b=="B11016_016E"){ #7 member non-family household
          syntheticdataset=data.frame(household.type=c("Non-family","Non-family","Non-family","Non-family","Non-family","Non-family","Non-family"),member=c("Householder","NA","NA","NA","NA","NA","NA"),race=c(a,a,a,a,a,a,a),size=rep(7,7))
        }
        
        if( exists("syntheticdataset")==FALSE ) stop(a,b)
        #Replace A codes with Race Names
        syntheticdataset$race <- paste (syntheticdataset$race)
        syntheticdataset$race[syntheticdataset$race=="B11001B_003E"]<-"Black or African American"
        syntheticdataset$race[syntheticdataset$race=="B11001B_005E"]<-"Black or African American"
        syntheticdataset$race[syntheticdataset$race=="B11001B_006E"]<-"Black or African American"
        syntheticdataset$race[syntheticdataset$race=="B11001B_008E"]<-"Black or African American"
        syntheticdataset$race[syntheticdataset$race=="B11001B_009E"]<-"Black or African American"
        
        syntheticdataset$race[syntheticdataset$race=="B11001C_003E"]<-"American Indian or Alaskan Native"
        syntheticdataset$race[syntheticdataset$race=="B11001C_005E"]<-"American Indian or Alaskan Native"
        syntheticdataset$race[syntheticdataset$race=="B11001C_006E"]<-"American Indian or Alaskan Native"
        syntheticdataset$race[syntheticdataset$race=="B11001C_008E"]<-"American Indian or Alaskan Native"
        syntheticdataset$race[syntheticdataset$race=="B11001C_009E"]<-"American Indian or Alaskan Native"
        
        syntheticdataset$race[syntheticdataset$race=="B11001D_003E"]<-"Asian"
        syntheticdataset$race[syntheticdataset$race=="B11001D_005E"]<-"Asian"
        syntheticdataset$race[syntheticdataset$race=="B11001D_006E"]<-"Asian"
        syntheticdataset$race[syntheticdataset$race=="B11001D_008E"]<-"Asian"
        syntheticdataset$race[syntheticdataset$race=="B11001D_009E"]<-"Asian"
        
        syntheticdataset$race[syntheticdataset$race=="B11001E_003E"]<-"Native Hawaiian or Other Pacific Islander"
        syntheticdataset$race[syntheticdataset$race=="B11001E_005E"]<-"Native Hawaiian or Other Pacific Islander"
        syntheticdataset$race[syntheticdataset$race=="B11001E_006E"]<-"Native Hawaiian or Other Pacific Islander"
        syntheticdataset$race[syntheticdataset$race=="B11001E_008E"]<-"Native Hawaiian or Other Pacific Islander"
        syntheticdataset$race[syntheticdataset$race=="B11001E_009E"]<-"Native Hawaiian or Other Pacific Islander"
        
        syntheticdataset$race[syntheticdataset$race=="B11001F_003E"]<-"Some Other Race"
        syntheticdataset$race[syntheticdataset$race=="B11001F_005E"]<-"Some Other Race"
        syntheticdataset$race[syntheticdataset$race=="B11001F_006E"]<-"Some Other Race"
        syntheticdataset$race[syntheticdataset$race=="B11001F_008E"]<-"Some Other Race"
        syntheticdataset$race[syntheticdataset$race=="B11001F_009E"]<-"Some Other Race"
        
        syntheticdataset$race[syntheticdataset$race=="B11001G_003E"]<-"Two or More Races"
        syntheticdataset$race[syntheticdataset$race=="B11001G_005E"]<-"Two or More Races"
        syntheticdataset$race[syntheticdataset$race=="B11001G_006E"]<-"Two or More Races"
        syntheticdataset$race[syntheticdataset$race=="B11001G_008E"]<-"Two or More Races"
        syntheticdataset$race[syntheticdataset$race=="B11001G_009E"]<-"Two or More Races"
        
        syntheticdataset$race[syntheticdataset$race=="B11001H_003E"]<-"White"
        syntheticdataset$race[syntheticdataset$race=="B11001H_005E"]<-"White"
        syntheticdataset$race[syntheticdataset$race=="B11001H_006E"]<-"White"
        syntheticdataset$race[syntheticdataset$race=="B11001H_008E"]<-"White"
        syntheticdataset$race[syntheticdataset$race=="B11001H_009E"]<-"White"
        
        syntheticdataset$race[syntheticdataset$race=="B11001I_003E"]<-"Hispanic or Latino"
        syntheticdataset$race[syntheticdataset$race=="B11001I_005E"]<-"Hispanic or Latino"
        syntheticdataset$race[syntheticdataset$race=="B11001I_006E"]<-"Hispanic or Latino"
        syntheticdataset$race[syntheticdataset$race=="B11001I_008E"]<-"Hispanic or Latino"
        syntheticdataset$race[syntheticdataset$race=="B11001I_009E"]<-"Hispanic or Latino"
        
        syntheticdataset$householdID=rep(paste(county,tract,index,sep=".",collapse="."))
        syntheticdataset$county=rep(county,nrow(syntheticdataset))
        syntheticdataset$tract=rep(tract,nrow(syntheticdataset))
        finalsyntheticdataset <- rbind(finalsyntheticdataset,syntheticdataset)
        index=index+1
      }
      
    
  
  return(syntheticdataset=finalsyntheticdataset)
}


#Get number of vehicles available

getnumberofvehicles <- function(county, tract,syntheticdataset,seed){
  set.seed(seed)
  householdsizebyvehicles=read.csv("household_size_by_vehicles_available.csv")
  
  finalsyntheticdataset=data.frame()
  house <- householdsizebyvehicles[(householdsizebyvehicles$tract==tract) & (householdsizebyvehicles$county==county),]
    
    samplehouses=unique(syntheticdataset$householdID)
    for(sampleID in samplehouses){
      sampledhouse=subset(syntheticdataset,syntheticdataset$householdID==sampleID)
      
      if (nrow(sampledhouse)==1){
        houseofone=house[c("B08201_008E","B08201_009E","B08201_010E","B08201_011E","B08201_012E")]
        total=sum(houseofone[1,1:5])
        prob1=(houseofone[1,1:5])/total
        colnames(prob1)=c(0,1,2,3,4)
        a=sample(colnames(prob1),size=1,prob=prob1)
        number.of.vehicles <- rep(a,nrow(sampledhouse))
        sampledhouse$number.of.vehicles=number.of.vehicles
      }
      if (nrow(sampledhouse)==2){
        houseofone=house[c("B08201_014E","B08201_015E","B08201_016E","B08201_017E","B08201_018E")]
        total=sum(houseofone[1,1:5])
        prob2=(houseofone[1,1:5])/total
        colnames(prob2)=c(0,1,2,3,4)
        a=sample(colnames(prob2),size=1,prob=prob2)
        number.of.vehicles <- rep(a,nrow(sampledhouse))
        sampledhouse$number.of.vehicles=number.of.vehicles
      }
      if (nrow(sampledhouse)>=3){
        houseofone=house[c("B08201_020E","B08201_021E","B08201_022E","B08201_023E","B08201_024E","B08201_026E","B08201_027E","B08201_028E","B08201_029E","B08201_030E")]
        total=sum(houseofone[1,1:10])
        prob3=(houseofone[1,1:10])/total
        colnames(prob3)=c(0,1,2,3,4,0,1,2,3,4)
        a=sample(colnames(prob3),size=1,prob=prob3)
        number.of.vehicles <- rep(a,nrow(sampledhouse))
        sampledhouse$number.of.vehicles=number.of.vehicles
      }
      finalsyntheticdataset=rbind(finalsyntheticdataset,sampledhouse)
      
      
    }
  return(syntheticdataset=finalsyntheticdataset)
}



