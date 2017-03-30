#Master Function
#This is the function that will be responsible for putting the dataset together using all of the functions in the working docs

source('workingdoc.R')
source('workingdoc2.R')
source('workingdoc3.R')
source('workingdoc4.R')
#source('BRSSRfunction.R')
source('citiesproject.R')



#site link to list of variables for U.S. Census
#http://api.census.gov/data/2014/acs5/variables.html

master<-function(county,tracts,number.of.households,seed){
  library(doParallel)
  cl <- makeCluster(detectCores(), type='PSOCK')
  registerDoParallel(cl)
  
  houstondata=read.csv('houstondata.csv')
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  houstondata$UniqueID=as.character(houstondata$UniqueID)
  houstondata$tract=substrRight(houstondata$UniqueID,6)
  houstondata$county=substr(houstondata$TractFIPS, 3, 5)
  houstondata$Measure=as.character(houstondata$Measure)

  set.seed(seed)
  fullset=data.frame()
  foreach (tract=tracts)%dopar%{ 
    seeds=sample(1:100000000, number.of.households, replace=FALSE)
    foreach (seedy=seeds)%dopar%{ 
      #I found out some tracts don't actually have people in them :( hence the next 4 lines
      householdtypeandrace=read.csv("householdtypeandrace.csv")
      house <- householdtypeandrace[(householdtypeandrace$tract==tract) & (householdtypeandrace$county==county),]
      total1=sum(house[1,4:43])
      if (total1>0){
        partofset=gethouseholdtypeandrace(county,tract,1,seedy)#not dependent on anything gets type and race
        
        #Debugging Stuff
        #householdsizebyvehicles=read.csv("household_size_by_vehicles_available.csv")
        #house <- householdsizebyvehicles[(householdsizebyvehicles$tract==tract) & (householdsizebyvehicles$county==county),]
        #samplehouses=unique(partofset$householdID)
        #for(sampleID in samplehouses){
        #  sampledhouse=subset(partofset,partofset$householdID==sampleID)
        #  houseofone=house[c("B08201_020E","B08201_021E","B08201_022E","B08201_023E","B08201_024E","B08201_026E","B08201_027E","B08201_028E","B08201_029E","B08201_030E")]
        #  total=sum(houseofone[1,1:10])
        #  if(total==0 & nrow(sampledhouse)>=3)stop(partofset)
        #}
        
        partofset=getnumberofvehicles(county,tract,partofset,seedy)#only dependent on size
        
        #More Debugging Stuff
        #sexbyagebyrace=read.csv("sex_by_age_by_race.csv")
        #sexbyagebyrace <- sexbyagebyrace[(sexbyagebyrace$tract==tract) & (sexbyagebyrace$county==county),]
        #AmerIndianAlaskanMen=sexbyagebyrace[c("B01001C_007E","B01001C_008E","B01001C_009E","B01001C_010E","B01001C_011E","B01001C_012E","B01001C_013E","B01001C_014E","B01001C_015E","B01001C_016E")]
        #if(sum(AmerIndianAlaskanMen)==0 & ("American Indian or Alaskan Native" %in% partofset$race))stop(tract)
        
        
        partofset=getsexandage(county,tract,partofset,seedy)#only dependent on race
        partofset=getschoolenrollment(county,tract,partofset,seedy)#dependent on sex and age which is fine because those two were cross tabulated together
        partofset=geteducationattainment(county,tract,partofset,seedy)#dependent on sex and age which is fine because those two are cross tabulated together
        
        ##Debugging Stuff don't pay attention
        #employment=read.csv("employment.csv")
        #employment=employment[(employment$tract==tract)&(employment$county==county),]
        #Men16.19=employment[c("B23001_005E","B23001_007E","B23001_008E","B23001_009E")]
        #Men75=employment[c("B23001_085E","B23001_086E","B23001_087E")]
        #if(sum(Men16.19)==0&()) stop(partofset)
        
        
        partofset=getemployment(county,tract,partofset,seedy)#dependent on sex and age which is fine because those two are tabulated together
        partofset=getdisability(county,tract,partofset,seedy)#dependent on age
        #partofset=getdegree(county,tract,partofset,seed)#dependent on race but it's also linked to educattion attainment which wasn't done by race so it's out for now
        partofset=getlangandnativity(county,tract,partofset,seedy)#dependent on race
        partofset=getcitizenandlang(county,tract,partofset,seedy)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
        partofset=getvets(county,tract,partofset,seedy)#dependent on sex and age which are cross tabulated
        partofset=gettransport(county,tract,partofset,seedy)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
        partofset=gettraveltime(county,tract,partofset,seedy)#dependent on travel method
        partofset=getincome(county,tract,partofset,seedy)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
        partofset=getinsurance(county,tract,partofset,seedy)#dependent on income
        
        partofset=get65menuptodate(county,tract,partofset,seedy)
        partofset=get65womenuptodate(county,tract,partofset,seedy)
        partofset=getadultasthma(county,tract,partofset,seedy)
        partofset=getarthritis(county,tract,partofset,seedy)
        partofset=getbingedrinking(county,tract,partofset,seedy)
        partofset=getcancer(county,tract,partofset,seedy)
        partofset=getcholesterolscreening(county,tract,partofset,seedy)
        partofset=getchronicobspulmonarydisease(county,tract,partofset,seedy)
        partofset=getcolonoscopy(county,tract,partofset,seedy)
        partofset=getcoronaryheartdisease(county,tract,partofset,seedy)
        partofset=getdiabetes(county,tract,partofset,seedy)
        partofset=gethbpmedications(county,tract,partofset,seedy)
        partofset=gethighbloodpressure(county,tract,partofset,seedy)
        partofset=gethighcholesterol(county,tract,partofset,seedy)
        partofset=getkidneydisease(county,tract,partofset,seedy)
        partofset=getmammographyuse(county,tract,partofset,seedy)
        partofset=getmentalhealth(county,tract,partofset,seedy)
        partofset=getnoleisuretime(county,tract,partofset,seedy)
        partofset=getobesity(county,tract,partofset,seedy)
        partofset=getpapsmear(county,tract,partofset,seedy)
        partofset=getphysicalhealth(county,tract,partofset,seedy)
        partofset=getroutinecheckups(county,tract,partofset,seedy)
        partofset=getsleep(county,tract,partofset,seedy)
        partofset=getsmokers(county,tract,partofset,seedy)
        partofset=getstroke(county,tract,partofset,seedy)
        partofset=getteeth(county,tract,partofset,seedy)
        
        fullset=rbind(fullset,partofset)
      
    }
    
    
      
    }
    
   
  }
  #fullset=getBRSSRdata(fullset,seed)
  return(fullset)
}



#list of counties in Houston
#https://www.houston.org/business/regionalProfile.html
#looked up conty number on site below
#https://www.census.gov/2010census/partners/pdf/FIPS_StateCounty_Code.pdf
countycodesinhouston=c(201,071,167,039,157,473,015,339,471,407,291)
#there are 1086 total tracts in this area

run.me.for.houston=function(countycodesinhouston,number.of.households,seed){
  
  source.of.tracts=read.csv("veteran_status.csv") #choice of file was arbitrary all files have counties and regions
  syntheticdataset=data.frame()
  for (county in 1:length(countycodesinhouston)){
    b=countycodesinhouston[county]
    a=subset(source.of.tracts,source.of.tracts$county==b)
    tracts=unique(a$tract)
    part.of.syntheticdataset=master(b,tracts,number.of.households,seed)
    
    syntheticdataset=rbind(syntheticdataset,part.of.syntheticdataset)
  }
  return(syntheticdataset)
}

#sample.set=run.me.for.houston(countycodesinhouston,50,2)
#write.csv(sample.set,"sample_set2.csv")
