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
  
household_generator<-function(county,tract,number.of.households,seed,inputdir = "../Inputs/",Census_data_List){  
  #Set seed so sampling will be repeatable
  set.seed(seed)
    #Create a seed for each household from original seed
    seeds=sample(1:100000000, number.of.households, replace=FALSE)
    #create empty data set to build from
    fullset=data.frame()
    
    for(seedy in seeds){ #for each seed create a household
      
      
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
      householdtypeandrace=read.csv(paste0(inputdir,"householdtypeandrace.csv"))
      house <- householdtypeandrace[(householdtypeandrace$tract==tract) & (householdtypeandrace$county==county),]
      total1=sum(house[1,4:43])
      if (total1>0){
        
        #Begin sampling characteristics of household (functions stored in other scripts)
        #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics
        
        
        #Build using Census Data
        partofset=gethouseholdtypeandrace(county,tract,seedy,Census_data_List)#not dependent on anything gets type and race
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
        
        partofset$householdID=rep(paste(county,tract,seedy,sep=".",collapse="."),nrow(partofset))
        
        #Save new household with any previous households
        fullset=rbind(fullset,partofset)
    
    
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
