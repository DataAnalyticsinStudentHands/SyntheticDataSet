

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
source('workingdoc5.R')
source('citiesproject.R')

group_quarters_simulater<-function(county,tract,number.of.people,seed,inputdir = "../Inputs/",Census_data){  
  #Set seed so sampling will be repeatable
  set.seed(seed)
  fullset=data.frame()
  
  if(number.of.people>0){
    #Create a seed for each household from original seed
    seeds=sample(1:100000000, number.of.people, replace=FALSE)
    #create empty data set to build from
    
    for(seedy in seeds){ #for each seed create a group quarter person
      
      
      #load and organize 500 Cities project data
      houstondata=read.csv(paste0(inputdir,'houstondata.csv'))
      substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
      }
      
      houstondata$UniqueID=as.character(houstondata$UniqueID)
      houstondata$tract=substrRight(houstondata$UniqueID,6)
      houstondata$county=substr(houstondata$TractFIPS, 3, 5)
      houstondata$Measure=as.character(houstondata$Measure)
      
      #Make Sure tract picked actually has people in it
      group_quarters=read.csv(paste0(inputdir,"group_quarters.csv"))
      group_quarters_people <- group_quarters[(group_quarters$tract==tract) & (group_quarters$county==county),]
      total1=group_quarters[1,3]
      if (total1>0){
        
        #Begin sampling characteristics of household (functions stored in other scripts)
        #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
        
        
        #Build using Census Data
        #partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type
        partofset=data.frame(household.type="Group Quarters",member="NA",size="Group Quarters")
        #partofset=getnumberofvehicles(county,tract,partofset,seedy,Census_data_List)#only dependent on size
        #partofset$number.of.vehicles=rep(NA,nrow(partofset))#leave as NA for now
        partofset=getsexandage(county,tract,partofset,seedy,Census_data_List)
        partofset=getnumberofvehiclesforgroupquarters(county,tract,partofset,seedy,Census_data_List)#by sex
        partofset=getschoolenrollment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two were cross tabulated together
        partofset=geteducationattainment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are cross tabulated together
        partofset=getemployment(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which is fine because those two are tabulated together
        partofset=getdisability(county,tract,partofset,seedy,Census_data_List)#dependent on age
        partofset=getlangandnativity(county,tract,partofset,seedy,Census_data_List)#dependent on race
        partofset=getcitizenandlang(county,tract,partofset,seedy,Census_data_List)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
        partofset=getvets(county,tract,partofset,seedy,Census_data_List)#dependent on sex and age which are cross tabulated
        partofset=gettransport(county,tract,partofset,seedy,Census_data_List)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
        partofset=gettraveltime(county,tract,partofset,seedy,Census_data_List)#dependent on travel method
        #partofset=getincome(county,tract,partofset,seedy,Census_data_List)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
        #partofset$household.income=rep(NA,nrow(partofset))#leave as NA for now
        partofset=getincomeforgroupquarters(county,tract,partofset,seedy,Census_data_List)
        partofset=gethealthinsuranceforgroupquarters(county,tract,partofset,seedy,Census_data_List)#dependent on income
        #partofset$health.insurance=rep(NA,nrow(partofset))#leave as NA for now
        
        
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
        
        partofset$householdID=rep(paste("group_quarters",county,tract,seedy,sep=".",collapse="."),nrow(partofset))
        
        #Save new household with any previous households
        fullset=rbind(fullset,partofset)
        
        
      } 
      
      
    }
    
  }
  #return data.frame with all households built
  return(fullset)
  
}