#Master Function
#This is the function that will be responsible for putting households together using all of the functions in the working docs


#site link to list of variables for U.S. Census
#http://api.census.gov/data/2014/acs5/variables.html


#The master function needs the county and tract number to build it's probability distributions for
#the number.of.households to make
#a seed so the sampling is random but repeatable
#the input directory for where it will access the Census and 500 Cities Project data

#It builds using the functions stored in the following scripts:
source('workingdoc.R')
source('workingdoc2.R')
source('workingdoc3.R')
source('workingdoc4.R')
source('citiesproject.R')

household_generator<-function(county,tract,seed,inputdir = "../Inputs/",Census_data_List){  
  
  #initialize data frame
  fullset=data.frame()
  
  #Get the number of each size of family households to make
  family=Census_data_List[["family"]]
  familyhouseholds=family[(family$tract==tract & family$county==county),]
  
  #Get a probability vector for their type
  familyHHtypes=Census_data_List[["familyhouseholdtypes"]]
  familyHHtypes=familyHHtypes[(familyHHtypes$county==county & familyHHtypes$tract==tract),]
  familyHHtypes=familyHHtypes[4:6]
  colnames(familyHHtypes)<-c("Married-couple family", "Male householder- no wife present","Female householder- no husband present")
  familyHHtypes=familyHHtypes/rowSums(familyHHtypes)
  
  #load and organize 500 Cities project data
  houstondata=read.csv(paste0(inputdir,'houstondata.csv'))
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  houstondata$UniqueID=as.character(houstondata$UniqueID)
  houstondata$tract=substrRight(houstondata$UniqueID,6)
  houstondata$county=substr(houstondata$TractFIPS, 3, 5)
  houstondata$Measure=as.character(houstondata$Measure)
  
  
  #Create 2 family households
  #Make Sure there are 2 family households
  if(familyhouseholds$B11016_003E>0){
    #make a seed for each household
    family_HH_sz2_seeds=sample(1:100000000,familyhouseholds$B11016_003E,replace = FALSE)
    
    for(seedy in family_HH_sz2_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)
      
      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,2),member=c("Husband","Wife"),size=rep(2,2))
      }
      
      if(HHtype=="Male householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,2),member=c("Male Householder","NA"),size=rep(2,2))
      }
      
      if(HHtype=="Female householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,2),member=c("Female Householder","NA"),size=rep(2,2))
      }

        #Begin sampling characteristics of household (functions stored in other scripts)
        #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
        
        
        #Build using Census Data
        #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
        partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
        partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)#only dependent on race
        partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
        partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
        partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
        partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
        partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
        partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
        partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
        partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
        partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
        partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
        partofset=getinsurance(county,tract,partofset,seedy,Census_data_List)#dependent on income
        
        #Build Using 500 Cities Project Data
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
        partofset=gethighbloodpressure(county,tract,partofset,seedy)
        partofset=gethbpmedications(county,tract,partofset,seedy)
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
        
        partofset$householdID=rep(paste(county,tract,seedy,"B11016_003E",sep=".",collapse="."),nrow(partofset))
        
        #Save new household with any previous households
        fullset=rbind(fullset,partofset)
        }
  }
  
  #Create 3 family households
  #Make Sure there are 3 family households
  if(familyhouseholds$B11016_004E>0){
    #make a seed for each household
    family_HH_sz3_seeds=sample(1:100000000,familyhouseholds$B11016_004E,replace = FALSE)
    
    for(seedy in family_HH_sz3_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)
      
      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,3),member=c("Husband","Wife","NA"),size=rep(3,3))
      }
      
      if(HHtype=="Male householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,3),member=c("Male Householder",rep("NA",2)),size=rep(3,3))
      }
      
      if(HHtype=="Female householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,3),member=c("Female Householder",rep("NA",2)),size=rep(3,3))
      }
      
      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
      
      
      #Build using Census Data
      #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
      partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
      partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)#only dependent on race
      partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
      partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
      partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
      partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
      partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=getinsurance(county,tract,partofset,seedy,Census_data_List)#dependent on income
      
      #Build Using 500 Cities Project Data
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
      partofset=gethighbloodpressure(county,tract,partofset,seedy)
      partofset=gethbpmedications(county,tract,partofset,seedy)
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
      
      partofset$householdID=rep(paste(county,tract,seedy,"B11016_004E",sep=".",collapse="."),nrow(partofset))
      
      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }  
  
  #Create 4 family households
  #Make Sure there are 4 family households
  if(familyhouseholds$B11016_005E>0){
    #make a seed for each household
    family_HH_sz4_seeds=sample(1:100000000,familyhouseholds$B11016_005E,replace = FALSE)
    
    for(seedy in family_HH_sz4_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)
      
      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,4),member=c("Husband","Wife",rep("NA",2)),size=rep(4,4))
      }
      
      if(HHtype=="Male householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,4),member=c("Male Householder",rep("NA",3)),size=rep(4,4))
      }
      
      if(HHtype=="Female householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,4),member=c("Female Householder",rep("NA",3)),size=rep(4,4))
      }
      
      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
      
      
      #Build using Census Data
      #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
      partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
      partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)#only dependent on race
      partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
      partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
      partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
      partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
      partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=getinsurance(county,tract,partofset,seedy,Census_data_List)#dependent on income
      
      #Build Using 500 Cities Project Data
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
      partofset=gethighbloodpressure(county,tract,partofset,seedy)
      partofset=gethbpmedications(county,tract,partofset,seedy)
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
      
      partofset$householdID=rep(paste(county,tract,seedy,"B11016_005E",sep=".",collapse="."),nrow(partofset))
      
      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  } 
  
  #Create 5 family households
  #Make Sure there are 5 family households
  if(familyhouseholds$B11016_006E>0){
    #make a seed for each household
    family_HH_sz5_seeds=sample(1:100000000,familyhouseholds$B11016_006E,replace = FALSE)
    
    for(seedy in family_HH_sz5_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)
      
      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,5),member=c("Husband","Wife",rep("NA",3)),size=rep(5,5))
      }
      
      if(HHtype=="Male householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,5),member=c("Male Householder",rep("NA",4)),size=rep(5,5))
      }
      
      if(HHtype=="Female householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,5),member=c("Female Householder",rep("NA",4)),size=rep(5,5))
      }
      
      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
      
      
      #Build using Census Data
      #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
      partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
      partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)#only dependent on race
      partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
      partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
      partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
      partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
      partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=getinsurance(county,tract,partofset,seedy,Census_data_List)#dependent on income
      
      #Build Using 500 Cities Project Data
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
      partofset=gethighbloodpressure(county,tract,partofset,seedy)
      partofset=gethbpmedications(county,tract,partofset,seedy)
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
      
      partofset$householdID=rep(paste(county,tract,seedy,"B11016_006E",sep=".",collapse="."),nrow(partofset))
      
      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }
  
  #Create 6 family households
  #Make Sure there are 6 family households
  if(familyhouseholds$B11016_007E>0){
    #make a seed for each household
    family_HH_sz6_seeds=sample(1:100000000,familyhouseholds$B11016_007E,replace = FALSE)
    
    for(seedy in family_HH_sz6_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)
      
      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,6),member=c("Husband","Wife",rep("NA",4)),size=rep(6,6))
      }
      
      if(HHtype=="Male householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,6),member=c("Male Householder",rep("NA",5)),size=rep(6,6))
      }
      
      if(HHtype=="Female householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,6),member=c("Female Householder",rep("NA",5)),size=rep(6,6))
      }
      
      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
      
      
      #Build using Census Data
      #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
      partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
      partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)#only dependent on race
      partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
      partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
      partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
      partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
      partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=getinsurance(county,tract,partofset,seedy,Census_data_List)#dependent on income
      
      #Build Using 500 Cities Project Data
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
      partofset=gethighbloodpressure(county,tract,partofset,seedy)
      partofset=gethbpmedications(county,tract,partofset,seedy)
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
      
      partofset$householdID=rep(paste(county,tract,seedy,"B11016_007E",sep=".",collapse="."),nrow(partofset))
      
      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }
  
  #Create 7 family households
  #Make Sure there are 7 family households
  if(familyhouseholds$B11016_008E>0){
    #make a seed for each household
    family_HH_sz7_seeds=sample(1:100000000,familyhouseholds$B11016_008E,replace = FALSE)
    
    for(seedy in family_HH_sz7_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)
      
      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,7),member=c("Husband","Wife",rep("NA",5)),size=rep(7,7))
      }
      
      if(HHtype=="Male householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,7),member=c("Male Householder",rep("NA",6)),size=rep(7,7))
      }
      
      if(HHtype=="Female householder, no wife present"){
        partofset=data.frame(household.type=rep(HHtype,7),member=c("Female Householder",rep("NA",6)),size=rep(7,7))
      }
      
      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
      
      
      #Build using Census Data
      #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
      partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
      partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)#only dependent on race
      partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
      partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
      partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
      partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
      partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=getinsurance(county,tract,partofset,seedy,Census_data_List)#dependent on income
      
      #Build Using 500 Cities Project Data
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
      partofset=gethighbloodpressure(county,tract,partofset,seedy)
      partofset=gethbpmedications(county,tract,partofset,seedy)
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
      
      partofset$householdID=rep(paste(county,tract,seedy,"B11016_008E",sep=".",collapse="."),nrow(partofset))
      
      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }
  
  #Create Non family households
  nonfamily=Census_data_List[["nonfamily"]]
  nonfamilyHHs=nonfamily[(nonfamily$county==county & nonfamily$tract==tract),]

  for(x in 1:7){
    
    if(nonfamilyHHs[2+x]>0){
      #make a seed for each household
      nonfamily_seeds=sample(1:100000000,nonfamilyHHs[2+x],replace = FALSE)
      
      for(seedy in nonfamily_seeds){ #for each seed create a household
        #set seed
        set.seed(seedy)
        
        #Create initial data frame
        if(x==1){
          partofset=data.frame(household.type="Alone",member="Householder",size=1)
        }
        if(x>1){
          partofset=data.frame(household.type=rep("Non-family",x),member=c("Householder",rep("NA",(x-1))),size=rep(x,x))
        }
        
        #Begin sampling characteristics of household (functions stored in other scripts)
        #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
        
        
        #Build using Census Data
        #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
        partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
        partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)#only dependent on race
        partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
        partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
        partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
        partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
        partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
        partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
        partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
        partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
        partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
        partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
        partofset=getinsurance(county,tract,partofset,seedy,Census_data_List)#dependent on income
        
        #Build Using 500 Cities Project Data
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
        partofset=gethighbloodpressure(county,tract,partofset,seedy)
        partofset=gethbpmedications(county,tract,partofset,seedy)
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
        
        partofset$householdID=rep(paste(county,tract,seedy,"B11016_008E",sep=".",collapse="."),nrow(partofset))
        
        #Save new household with any previous households
        fullset=rbind(fullset,partofset)
      }
    }
    
  }
  
  
  #return data.frame with all households built
  return(fullset)
  
}



#list of counties in Houston
#https://www.houston.org/business/regionalProfile.html
#looked up conty number on site below
#https://www.census.gov/2010census/partners/pdf/FIPS_StateCounty_Code.pdf
#countycodesinhouston=c(201,071,167,039,157,473,015,339,471,407,291)
#there are 1086 total tracts in this area

#sample.set=run.me.for.houston(countycodesinhouston,50,2)
#write.csv(sample.set,"sample_set2.csv")