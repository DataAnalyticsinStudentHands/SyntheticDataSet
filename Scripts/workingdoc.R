

gethouseholdtypeandrace <- function(county,tract,number.of.households,seed,inputdir){
  #Generates family structure and race characteristics for household
  
  #Set seed so sampling is random but repeatable
  set.seed(seed)
  #load data
  householdtypeandrace=read.csv(paste0(inputdir,"householdtypeandrace.csv"))
  
    #Subset data for approrpriate county and tract and create probability distribution
    house <- householdtypeandrace[(householdtypeandrace$tract==tract) & (householdtypeandrace$county==county),]
    total1=sum(house[1,4:43])
    prob1=(house[1,4:43])/total1
    
    #Initialize data.frame
    finalsyntheticdataset=data.frame()
    
    #If it's an empty tract stop
    if( sum(is.na(prob1))!=0 ) stop(tract,county)
    
    #Sample for households' type and race from Census data
      samples=sample(colnames(prob1),size=number.of.households,replace=TRUE,prob=prob1)
      
      
      #Assuming previously sampled type sample size
      #load size data
      householdsizebytype=read.csv(paste0(inputdir,"household_size.csv"))
      
      #separate types by family and non family, as their probability for sizes is different
      family=householdsizebytype[c("tract","B11016_003E","B11016_004E","B11016_005E","B11016_006E","B11016_007E","B11016_008E")]
      nonfamily=householdsizebytype[c("tract","B11016_011E","B11016_012E","B11016_013E","B11016_014E","B11016_015E","B11016_016E")]
      
      
      index=1
      for (a in samples){
        #Sample Size Knowing it's a family household
        if (a=="B11001B_003E"|a=="B11001B_005E"|a=="B11001B_006E"|a=="B11001C_003E"|a=="B11001C_005E"|a=="B11001C_006E"|a=="B11001D_003E"|a=="B11001D_005E"|a=="B11001D_006E"|a=="B11001E_003E"|a=="B11001E_005E"|a=="B11001E_006E"|a=="B11001F_003E"|a=="B11001F_005E"|a=="B11001F_006E"|a=="B11001G_003E"|a=="B11001G_005E"|a=="B11001G_006E"|a=="B11001H_003E"|a=="B11001H_005E"|a=="B11001H_006E"|a=="B11001I_003E"|a=="B11001I_005E"|a=="B11001I_006E"){#if it's a family household
          house=family[family$tract==tract,]
          total=sum(house[1,2:6])
          prob=(house[1,2:6])/total
          b=sample(colnames(prob),size=1,prob=prob)
        }
        
        #Sample Size Knowing i's a Non-family Household
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
        
        ##Build Dataframe for each household
        
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
        
        #Because of sampling problems, we've decided to sample race with age and gender and not with household, 
        #so we are not using this function to determine race anymore
        syntheticdataset$race=NULL
        
        #Create Unique household ID for each household
        syntheticdataset$householdID=rep(paste(county,tract,index,sep=".",collapse="."))
        #Make columns for county and tract
        syntheticdataset$county=rep(county,nrow(syntheticdataset))
        syntheticdataset$tract=rep(tract,nrow(syntheticdataset))
        #Join new households with old households
        finalsyntheticdataset <- rbind(finalsyntheticdataset,syntheticdataset)
        #increase index
        index=index+1
      }
      
    
  
  return(syntheticdataset=finalsyntheticdataset)
}


#Get number of vehicles available

getnumberofvehicles <- function(county, tract,syntheticdataset,seed,inputdir){
  #Generates number of vehicles per household
  
  #Set seed so sampling is random but repeatable
  set.seed(seed)
  #Read in data for sampling distribution
  householdsizebyvehicles=read.csv(paste0(inputdir,"household_size_by_vehicles_available.csv"))
  
  #initialize empty data.frame
  finalsyntheticdataset=data.frame()
  #Subset data for approrpriate county and tract
  house <- householdsizebyvehicles[(householdsizebyvehicles$tract==tract) & (householdsizebyvehicles$county==county),]
    
  
  #For each unique household ID sample for number of cars based on size of household
    samplehouses=unique(syntheticdataset$householdID)
    for(sampleID in samplehouses){
      sampledhouse=subset(syntheticdataset,syntheticdataset$householdID==sampleID)
      
      #Sample for one person household
      if (nrow(sampledhouse)==1){
        #Use appropriate Census Subheadings
        houseofone=house[c("B08201_008E","B08201_009E","B08201_010E","B08201_011E","B08201_012E")]
        #Create probability Distribution
        total=sum(houseofone[1,1:5])
        prob1=(houseofone[1,1:5])/total
        colnames(prob1)=c(0,1,2,3,4)
        #Samples
        a=sample(colnames(prob1),size=1,prob=prob1)
        #add number of cars to each person in household in data frame
        number.of.vehicles <- rep(a,nrow(sampledhouse))
        sampledhouse$number.of.vehicles=number.of.vehicles
      }
      #Sample for 2 person households
      if (nrow(sampledhouse)==2){
        #Use apppropriate Census subheadings for 2 people households
        houseofone=house[c("B08201_014E","B08201_015E","B08201_016E","B08201_017E","B08201_018E")]
        #Create probability distribution
        total=sum(houseofone[1,1:5])
        prob2=(houseofone[1,1:5])/total
        colnames(prob2)=c(0,1,2,3,4)
        #Sample for number of cars
        a=sample(colnames(prob2),size=1,prob=prob2)
        #add number of cars to each person in household in data frame
        number.of.vehicles <- rep(a,nrow(sampledhouse))
        sampledhouse$number.of.vehicles=number.of.vehicles
      }
      #Sample for 3 or more person households
      if (nrow(sampledhouse)>=3){
        #Use appropriate Census Subheadings for 3 or more person households
        houseofone=house[c("B08201_020E","B08201_021E","B08201_022E","B08201_023E","B08201_024E","B08201_026E","B08201_027E","B08201_028E","B08201_029E","B08201_030E")]
        #Generate probability distribution
        total=sum(houseofone[1,1:10])
        prob3=(houseofone[1,1:10])/total
        colnames(prob3)=c(0,1,2,3,4,0,1,2,3,4)
        #Sample for number of cars
        a=sample(colnames(prob3),size=1,prob=prob3)
        #add number of cars to each person in household in data frame
        number.of.vehicles <- rep(a,nrow(sampledhouse))
        sampledhouse$number.of.vehicles=number.of.vehicles
      }
      
      #Add new sampled house to old
      finalsyntheticdataset=rbind(finalsyntheticdataset,sampledhouse)
      
      
    }
  return(syntheticdataset=finalsyntheticdataset)
}



