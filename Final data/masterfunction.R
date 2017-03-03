#Master Function
#This is the function that will be responsible for putting the dataset together using all of the functions in the working docs

source('workingdoc.R')
source('workingdoc2.R')
source('workingdoc3.R')
source('workingdoc4.R')
source('BRSSRfunction.R')


#site link to list of variables for U.S. Census
#http://api.census.gov/data/2014/acs5/variables.html

master<-function(county,tracts,number.of.households,seed){
  set.seed(seed)
  fullset=data.frame()
  for (tract in tracts){
    
    for(seed in 1:sample(1:100, number.of.households, replace=TRUE)){
      #I found out some tracts don't actually have people in them :( hence the next 4 lines
      householdtypeandrace=read.csv("householdtypeandrace.csv")
      house <- householdtypeandrace[(householdtypeandrace$tract==tract) & (householdtypeandrace$county==county),]
      total1=sum(house[1,4:43])
      if (total1>0){
        partofset=gethouseholdtypeandrace(county,tract,1,seed)#not dependent on anything gets type and race
        
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
        
        partofset=getnumberofvehicles(county,tract,partofset,seed)#only dependent on size
        
        #More Debugging Stuff
        #sexbyagebyrace=read.csv("sex_by_age_by_race.csv")
        #sexbyagebyrace <- sexbyagebyrace[(sexbyagebyrace$tract==tract) & (sexbyagebyrace$county==county),]
        #AmerIndianAlaskanMen=sexbyagebyrace[c("B01001C_007E","B01001C_008E","B01001C_009E","B01001C_010E","B01001C_011E","B01001C_012E","B01001C_013E","B01001C_014E","B01001C_015E","B01001C_016E")]
        #if(sum(AmerIndianAlaskanMen)==0 & ("American Indian or Alaskan Native" %in% partofset$race))stop(tract)
        
        
        partofset=getsexandage(county,tract,partofset,seed)#only dependent on race
        partofset=getschoolenrollment(county,tract,partofset,seed)#dependent on sex and age which is fine because those two were cross tabulated together
        partofset=geteducationattainment(county,tract,partofset,seed)#dependent on sex and age which is fine because those two are cross tabulated together
        
        ##Debugging Stuff don't pay attention
        #employment=read.csv("employment.csv")
        #employment=employment[(employment$tract==tract)&(employment$county==county),]
        #Men16.19=employment[c("B23001_005E","B23001_007E","B23001_008E","B23001_009E")]
        #Men75=employment[c("B23001_085E","B23001_086E","B23001_087E")]
        #if(sum(Men16.19)==0&()) stop(partofset)
        
        
        partofset=getemployment(county,tract,partofset,seed)#dependent on sex and age which is fine because those two are tabulated together
        partofset=getdisability(county,tract,partofset,seed)#dependent on age
        #partofset=getdegree(county,tract,partofset,seed)#dependent on race but it's also linked to educattion attainment which wasn't done by race so it's out for now
        partofset=getlangandnativity(county,tract,partofset,seed)#dependent on race
        partofset=getcitizenandlang(county,tract,partofset,seed)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
        partofset=getvets(county,tract,partofset,seed)#dependent on sex and age which are cross tabulated
        partofset=gettransport(county,tract,partofset,seed)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
        partofset=gettraveltime(county,tract,partofset,seed)#dependent on travel method
        partofset=getincome(county,tract,partofset,seed)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
        partofset=getinsurance(county,tract,partofset,seed)#dependent on income
        
        fullset=rbind(fullset,partofset)
      
    }
    
    
      
    }
    
   
  }
  fullset=getBRSSRdata(fullset,seed)
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
