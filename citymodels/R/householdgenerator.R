#' Household Generator
#'
#' This function simulates people living in households.
#'
#' It calls other functions in the citymodels package in order to simulate characteristics for households and for each individual
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param number.of.people The number of people to simulate
#' @param seed The seed to use for sampling.
#' @param inputdir The input directory for other data
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset A dataframe of simulated people living in households.

household_generator<-function(state,county,tract,seed,inputdir = "../Inputs/",Census_data){

  #initialize data frame
  fullset=data.frame()

  #set seed
  set.seed(seed)

  #subset data for correct Census tract
  Census_data=Census_data[(Census_data$state==state)&(Census_data$tract==tract)&(Census_data$county==county),]

  #Get a probability vector for their type
  familyHHtypes=Census_data[c("married.couple.families","male.householders.no.wife","female.householders.no.husband")]

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
  if(Census_data$family.2.person.household>0){
    #make a seed for each household
    family_HH_sz2_seeds=sample(1:100000000,Census_data$family.2.person.household,replace = FALSE)

    for(seedy in family_HH_sz2_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)

      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,2),member=c("Husband","Wife"),size=rep(2,2))
      }

      if(HHtype=="Male householder- no wife present"){
        partofset=data.frame(household.type=rep(HHtype,2),member=c("Male Householder","NA"),size=rep(2,2))
      }

      if(HHtype=="Female householder- no husband present"){
        partofset=data.frame(household.type=rep(HHtype,2),member=c("Female Householder","NA"),size=rep(2,2))
      }

      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics


      #Build using Census Data
      #partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
      partofset=getnumberofvehiclesforhouseholds(state,county,tract,partofset,seedy,Census_data)#only dependent on size
      partofset=getsexraceandage(state,county,tract,partofset,seedy,Census_data)#only dependent on race
      partofset=getschoolenrollment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(state,county,tract,partofset,seedy,Census_data)#dependent on age
      partofset=getlangandnativity(state,county,tract,partofset,seedy,Census_data)#dependent on race
      partofset=getcitizenandlang(state,county,tract,partofset,seedy,Census_data)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which are cross tabulated
      partofset=gettransport(state,county,tract,partofset,seedy,Census_data)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(state,county,tract,partofset,seedy,Census_data)#dependent on travel method
      partofset=gethouseholdincome(state,county,tract,partofset,seedy,Census_data)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=gethouseholdhealthinsurance(state,county,tract,partofset,seedy,Census_data)#dependent on income

      #Build Using 500 Cities Project Data
      #partofset=get65menuptodate(state,county,tract,partofset,seedy)
      #partofset=get65womenuptodate(state,county,tract,partofset,seedy)
      #partofset=getadultasthma(state,county,tract,partofset,seedy)
      #partofset=getarthritis(state,county,tract,partofset,seedy)
      #partofset=getbingedrinking(state,county,tract,partofset,seedy)
      #partofset=getcancer(state,county,tract,partofset,seedy)
      #partofset=getcholesterolscreening(state,county,tract,partofset,seedy)
      #partofset=getchronicobspulmonarydisease(state,county,tract,partofset,seedy)
      #partofset=getcolonoscopy(state,county,tract,partofset,seedy)
      #partofset=getcoronaryheartdisease(state,county,tract,partofset,seedy)
      #partofset=getdiabetes(state,county,tract,partofset,seedy)
      #partofset=gethighbloodpressure(state,county,tract,partofset,seedy)
      #partofset=gethbpmedications(state,county,tract,partofset,seedy)
      #partofset=gethighcholesterol(state,county,tract,partofset,seedy)
      #partofset=getkidneydisease(state,county,tract,partofset,seedy)
      #partofset=getmammographyuse(state,county,tract,partofset,seedy)
      #partofset=getmentalhealth(state,county,tract,partofset,seedy)
      #partofset=getnoleisuretime(state,county,tract,partofset,seedy)
      #partofset=getobesity(state,county,tract,partofset,seedy)
      #partofset=getpapsmear(state,county,tract,partofset,seedy)
      #partofset=getphysicalhealth(state,county,tract,partofset,seedy)
      #partofset=getroutinecheckups(state,county,tract,partofset,seedy)
      #partofset=getsleep(state,county,tract,partofset,seedy)
      #partofset=getsmokers(state,county,tract,partofset,seedy)
      #partofset=getstroke(state,county,tract,partofset,seedy)
      #partofset=getteeth(state,county,tract,partofset,seedy)

      partofset$householdID=rep(paste(state,county,tract,seedy,"family.2.person.household",sep=".",collapse="."),nrow(partofset))

      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }

  #Create 3 family households
  #Make Sure there are 3 family households
  if(Census_data$family.3.person.household>0){
    #make a seed for each household
    family_HH_sz3_seeds=sample(1:100000000,Census_data$family.3.person.household,replace = FALSE)

    for(seedy in family_HH_sz3_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)

      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,3),member=c("Husband","Wife","NA"),size=rep(3,3))
      }

      if(HHtype=="Male householder- no wife present"){
        partofset=data.frame(household.type=rep(HHtype,3),member=c("Male Householder",rep("NA",2)),size=rep(3,3))
      }

      if(HHtype=="Female householder- no husband present"){
        partofset=data.frame(household.type=rep(HHtype,3),member=c("Female Householder",rep("NA",2)),size=rep(3,3))
      }

      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics


      #Build using Census Data
      #partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
      partofset=getnumberofvehiclesforhouseholds(state,county,tract,partofset,seedy,Census_data)#only dependent on size
      partofset=getsexraceandage(state,county,tract,partofset,seedy,Census_data)#only dependent on race
      partofset=getschoolenrollment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(state,county,tract,partofset,seedy,Census_data)#dependent on age
      partofset=getlangandnativity(state,county,tract,partofset,seedy,Census_data)#dependent on race
      partofset=getcitizenandlang(state,county,tract,partofset,seedy,Census_data)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which are cross tabulated
      partofset=gettransport(state,county,tract,partofset,seedy,Census_data)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(state,county,tract,partofset,seedy,Census_data)#dependent on travel method
      partofset=gethouseholdincome(state,county,tract,partofset,seedy,Census_data)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=gethouseholdhealthinsurance(state,county,tract,partofset,seedy,Census_data)#dependent on income

      #Build Using 500 Cities Project Data
      #partofset=get65menuptodate(state,county,tract,partofset,seedy)
      #partofset=get65womenuptodate(state,county,tract,partofset,seedy)
      #partofset=getadultasthma(state,county,tract,partofset,seedy)
      #partofset=getarthritis(state,county,tract,partofset,seedy)
      #partofset=getbingedrinking(state,county,tract,partofset,seedy)
      #partofset=getcancer(state,county,tract,partofset,seedy)
      #partofset=getcholesterolscreening(state,county,tract,partofset,seedy)
      #partofset=getchronicobspulmonarydisease(state,county,tract,partofset,seedy)
      #partofset=getcolonoscopy(state,county,tract,partofset,seedy)
      #partofset=getcoronaryheartdisease(state,county,tract,partofset,seedy)
      #partofset=getdiabetes(state,county,tract,partofset,seedy)
      #partofset=gethighbloodpressure(state,county,tract,partofset,seedy)
      #partofset=gethbpmedications(state,county,tract,partofset,seedy)
      #partofset=gethighcholesterol(state,county,tract,partofset,seedy)
      #partofset=getkidneydisease(state,county,tract,partofset,seedy)
      #partofset=getmammographyuse(state,county,tract,partofset,seedy)
      #partofset=getmentalhealth(state,county,tract,partofset,seedy)
      #partofset=getnoleisuretime(state,county,tract,partofset,seedy)
      #partofset=getobesity(state,county,tract,partofset,seedy)
      #partofset=getpapsmear(state,county,tract,partofset,seedy)
      #partofset=getphysicalhealth(state,county,tract,partofset,seedy)
      #partofset=getroutinecheckups(state,county,tract,partofset,seedy)
      #partofset=getsleep(state,county,tract,partofset,seedy)
      #partofset=getsmokers(state,county,tract,partofset,seedy)
      #partofset=getstroke(state,county,tract,partofset,seedy)
      #partofset=getteeth(state,county,tract,partofset,seedy)

      partofset$householdID=rep(paste(state,county,tract,seedy,"family.3.person.household",sep=".",collapse="."),nrow(partofset))

      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }

  #Create 4 family households
  #Make Sure there are 4 family households
  if(Census_data$family.4.person.household>0){
    #make a seed for each household
    family_HH_sz4_seeds=sample(1:100000000,Census_data$family.4.person.household,replace = FALSE)

    for(seedy in family_HH_sz4_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)

      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,4),member=c("Husband","Wife",rep("NA",2)),size=rep(4,4))
      }

      if(HHtype=="Male householder- no wife present"){
        partofset=data.frame(household.type=rep(HHtype,4),member=c("Male Householder",rep("NA",3)),size=rep(4,4))
      }

      if(HHtype=="Female householder- no husband present"){
        partofset=data.frame(household.type=rep(HHtype,4),member=c("Female Householder",rep("NA",3)),size=rep(4,4))
      }

      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics


      #Build using Census Data
      #partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
      partofset=getnumberofvehiclesforhouseholds(state,county,tract,partofset,seedy,Census_data)#only dependent on size
      partofset=getsexraceandage(state,county,tract,partofset,seedy,Census_data)#only dependent on race
      partofset=getschoolenrollment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(state,county,tract,partofset,seedy,Census_data)#dependent on age
      partofset=getlangandnativity(state,county,tract,partofset,seedy,Census_data)#dependent on race
      partofset=getcitizenandlang(state,county,tract,partofset,seedy,Census_data)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which are cross tabulated
      partofset=gettransport(state,county,tract,partofset,seedy,Census_data)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(state,county,tract,partofset,seedy,Census_data)#dependent on travel method
      partofset=gethouseholdincome(state,county,tract,partofset,seedy,Census_data)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=gethouseholdhealthinsurance(state,county,tract,partofset,seedy,Census_data)#dependent on income

      #Build Using 500 Cities Project Data
      #partofset=get65menuptodate(state,county,tract,partofset,seedy)
      #partofset=get65womenuptodate(state,county,tract,partofset,seedy)
      #partofset=getadultasthma(state,county,tract,partofset,seedy)
      #partofset=getarthritis(state,county,tract,partofset,seedy)
      #partofset=getbingedrinking(state,county,tract,partofset,seedy)
      #partofset=getcancer(state,county,tract,partofset,seedy)
      #partofset=getcholesterolscreening(state,county,tract,partofset,seedy)
      #partofset=getchronicobspulmonarydisease(state,county,tract,partofset,seedy)
      #partofset=getcolonoscopy(state,county,tract,partofset,seedy)
      #partofset=getcoronaryheartdisease(state,county,tract,partofset,seedy)
      #partofset=getdiabetes(state,county,tract,partofset,seedy)
      #partofset=gethighbloodpressure(state,county,tract,partofset,seedy)
      #partofset=gethbpmedications(state,county,tract,partofset,seedy)
      #partofset=gethighcholesterol(state,county,tract,partofset,seedy)
      #partofset=getkidneydisease(state,county,tract,partofset,seedy)
      #partofset=getmammographyuse(state,county,tract,partofset,seedy)
      #partofset=getmentalhealth(state,county,tract,partofset,seedy)
      #partofset=getnoleisuretime(state,county,tract,partofset,seedy)
      #partofset=getobesity(state,county,tract,partofset,seedy)
      #partofset=getpapsmear(state,county,tract,partofset,seedy)
      #partofset=getphysicalhealth(state,county,tract,partofset,seedy)
      #partofset=getroutinecheckups(state,county,tract,partofset,seedy)
      #partofset=getsleep(state,county,tract,partofset,seedy)
      #partofset=getsmokers(state,county,tract,partofset,seedy)
      #partofset=getstroke(state,county,tract,partofset,seedy)
      #partofset=getteeth(state,county,tract,partofset,seedy)

      partofset$householdID=rep(paste(state,county,tract,seedy,"family.4.person.household",sep=".",collapse="."),nrow(partofset))

      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }

  #Create 5 family households
  #Make Sure there are 5 family households
  if(Census_data$family.5.person.household>0){
    #make a seed for each household
    family_HH_sz5_seeds=sample(1:100000000,Census_data$family.5.person.household,replace = FALSE)

    for(seedy in family_HH_sz5_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)

      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,5),member=c("Husband","Wife",rep("NA",3)),size=rep(5,5))
      }

      if(HHtype=="Male householder- no wife present"){
        partofset=data.frame(household.type=rep(HHtype,5),member=c("Male Householder",rep("NA",4)),size=rep(5,5))
      }

      if(HHtype=="Female householder- no husband present"){
        partofset=data.frame(household.type=rep(HHtype,5),member=c("Female Householder",rep("NA",4)),size=rep(5,5))
      }

      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics


      #Build using Census Data
      #partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
      partofset=getnumberofvehiclesforhouseholds(state,county,tract,partofset,seedy,Census_data)#only dependent on size
      partofset=getsexraceandage(state,county,tract,partofset,seedy,Census_data)#only dependent on race
      partofset=getschoolenrollment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(state,county,tract,partofset,seedy,Census_data)#dependent on age
      partofset=getlangandnativity(state,county,tract,partofset,seedy,Census_data)#dependent on race
      partofset=getcitizenandlang(state,county,tract,partofset,seedy,Census_data)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which are cross tabulated
      partofset=gettransport(state,county,tract,partofset,seedy,Census_data)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(state,county,tract,partofset,seedy,Census_data)#dependent on travel method
      partofset=gethouseholdincome(state,county,tract,partofset,seedy,Census_data)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=gethouseholdhealthinsurance(state,county,tract,partofset,seedy,Census_data)#dependent on income

      #Build Using 500 Cities Project Data
      #partofset=get65menuptodate(state,county,tract,partofset,seedy)
      #partofset=get65womenuptodate(state,county,tract,partofset,seedy)
      #partofset=getadultasthma(state,county,tract,partofset,seedy)
      #partofset=getarthritis(state,county,tract,partofset,seedy)
      #partofset=getbingedrinking(state,county,tract,partofset,seedy)
      #partofset=getcancer(state,county,tract,partofset,seedy)
      #partofset=getcholesterolscreening(state,county,tract,partofset,seedy)
      #partofset=getchronicobspulmonarydisease(state,county,tract,partofset,seedy)
      #partofset=getcolonoscopy(state,county,tract,partofset,seedy)
      #partofset=getcoronaryheartdisease(state,county,tract,partofset,seedy)
      #partofset=getdiabetes(state,county,tract,partofset,seedy)
      #partofset=gethighbloodpressure(state,county,tract,partofset,seedy)
      #partofset=gethbpmedications(state,county,tract,partofset,seedy)
      #partofset=gethighcholesterol(state,county,tract,partofset,seedy)
      #partofset=getkidneydisease(state,county,tract,partofset,seedy)
      #partofset=getmammographyuse(state,county,tract,partofset,seedy)
      #partofset=getmentalhealth(state,county,tract,partofset,seedy)
      #partofset=getnoleisuretime(state,county,tract,partofset,seedy)
      #partofset=getobesity(state,county,tract,partofset,seedy)
      #partofset=getpapsmear(state,county,tract,partofset,seedy)
      #partofset=getphysicalhealth(state,county,tract,partofset,seedy)
      #partofset=getroutinecheckups(state,county,tract,partofset,seedy)
      #partofset=getsleep(state,county,tract,partofset,seedy)
      #partofset=getsmokers(state,county,tract,partofset,seedy)
      #partofset=getstroke(state,county,tract,partofset,seedy)
      #partofset=getteeth(state,county,tract,partofset,seedy)

      partofset$householdID=rep(paste(state,county,tract,seedy,"family.5.person.household",sep=".",collapse="."),nrow(partofset))

      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }

  #Create 6 family households
  #Make Sure there are 6 family households
  if(Census_data$family.6.person.household>0){
    #make a seed for each household
    family_HH_sz6_seeds=sample(1:100000000,Census_data$family.6.person.household,replace = FALSE)

    for(seedy in family_HH_sz6_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)

      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,6),member=c("Husband","Wife",rep("NA",4)),size=rep(6,6))
      }

      if(HHtype=="Male householder- no wife present"){
        partofset=data.frame(household.type=rep(HHtype,6),member=c("Male Householder",rep("NA",5)),size=rep(6,6))
      }

      if(HHtype=="Female householder- no husband present"){
        partofset=data.frame(household.type=rep(HHtype,6),member=c("Female Householder",rep("NA",5)),size=rep(6,6))
      }

      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics


      #Build using Census Data
      #partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
      partofset=getnumberofvehiclesforhouseholds(state,county,tract,partofset,seedy,Census_data)#only dependent on size
      partofset=getsexraceandage(state,county,tract,partofset,seedy,Census_data)#only dependent on race
      partofset=getschoolenrollment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(state,county,tract,partofset,seedy,Census_data)#dependent on age
      partofset=getlangandnativity(state,county,tract,partofset,seedy,Census_data)#dependent on race
      partofset=getcitizenandlang(state,county,tract,partofset,seedy,Census_data)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which are cross tabulated
      partofset=gettransport(state,county,tract,partofset,seedy,Census_data)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(state,county,tract,partofset,seedy,Census_data)#dependent on travel method
      partofset=gethouseholdincome(state,county,tract,partofset,seedy,Census_data)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=gethouseholdhealthinsurance(state,county,tract,partofset,seedy,Census_data)#dependent on income

      #Build Using 500 Cities Project Data
      #partofset=get65menuptodate(state,county,tract,partofset,seedy)
      #partofset=get65womenuptodate(state,county,tract,partofset,seedy)
      #partofset=getadultasthma(state,county,tract,partofset,seedy)
      #partofset=getarthritis(state,county,tract,partofset,seedy)
      #partofset=getbingedrinking(state,county,tract,partofset,seedy)
      #partofset=getcancer(state,county,tract,partofset,seedy)
      #partofset=getcholesterolscreening(state,county,tract,partofset,seedy)
      #partofset=getchronicobspulmonarydisease(state,county,tract,partofset,seedy)
      #partofset=getcolonoscopy(state,county,tract,partofset,seedy)
      #partofset=getcoronaryheartdisease(state,county,tract,partofset,seedy)
      #partofset=getdiabetes(state,county,tract,partofset,seedy)
      #partofset=gethighbloodpressure(state,county,tract,partofset,seedy)
      #partofset=gethbpmedications(state,county,tract,partofset,seedy)
      #partofset=gethighcholesterol(state,county,tract,partofset,seedy)
      #partofset=getkidneydisease(state,county,tract,partofset,seedy)
      #partofset=getmammographyuse(state,county,tract,partofset,seedy)
      #partofset=getmentalhealth(state,county,tract,partofset,seedy)
      #partofset=getnoleisuretime(state,county,tract,partofset,seedy)
      #partofset=getobesity(state,county,tract,partofset,seedy)
      #partofset=getpapsmear(state,county,tract,partofset,seedy)
      #partofset=getphysicalhealth(state,county,tract,partofset,seedy)
      #partofset=getroutinecheckups(state,county,tract,partofset,seedy)
      #partofset=getsleep(state,county,tract,partofset,seedy)
      #partofset=getsmokers(state,county,tract,partofset,seedy)
      #partofset=getstroke(state,county,tract,partofset,seedy)
      #partofset=getteeth(state,county,tract,partofset,seedy)

      partofset$householdID=rep(paste(state,county,tract,seedy,"family.6.person.household",sep=".",collapse="."),nrow(partofset))

      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }

  #Create 7 family households
  #Make Sure there are 7 family households
  if(Census_data$family.7.person.household>0){
    #make a seed for each household
    family_HH_sz7_seeds=sample(1:100000000,Census_data$family.7.person.household,replace = FALSE)

    for(seedy in family_HH_sz7_seeds){ #for each seed create a household
      #set seed
      set.seed(seedy)
      #sample Household Type
      HHtype=sample(colnames(familyHHtypes),size=1,prob=familyHHtypes)

      #Create initial data frame
      if(HHtype=="Married-couple family"){
        partofset=data.frame(household.type=rep(HHtype,7),member=c("Husband","Wife",rep("NA",5)),size=rep(7,7))
      }

      if(HHtype=="Male householder- no wife present"){
        partofset=data.frame(household.type=rep(HHtype,7),member=c("Male Householder",rep("NA",6)),size=rep(7,7))
      }

      if(HHtype=="Female householder- no husband present"){
        partofset=data.frame(household.type=rep(HHtype,7),member=c("Female Householder",rep("NA",6)),size=rep(7,7))
      }

      #Begin sampling characteristics of household (functions stored in other scripts)
      #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics


      #Build using Census Data
      #partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
      partofset=getnumberofvehiclesforhouseholds(state,county,tract,partofset,seedy,Census_data)#only dependent on size
      partofset=getsexraceandage(state,county,tract,partofset,seedy,Census_data)#only dependent on race
      partofset=getschoolenrollment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two were cross tabulated together
      partofset=geteducationattainment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are cross tabulated together
      partofset=getemployment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are tabulated together
      partofset=getdisability(state,county,tract,partofset,seedy,Census_data)#dependent on age
      partofset=getlangandnativity(state,county,tract,partofset,seedy,Census_data)#dependent on race
      partofset=getcitizenandlang(state,county,tract,partofset,seedy,Census_data)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
      partofset=getvets(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which are cross tabulated
      partofset=gettransport(state,county,tract,partofset,seedy,Census_data)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
      partofset=gettraveltime(state,county,tract,partofset,seedy,Census_data)#dependent on travel method
      partofset=gethouseholdincome(state,county,tract,partofset,seedy,Census_data)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
      partofset=gethouseholdhealthinsurance(state,county,tract,partofset,seedy,Census_data)#dependent on income

      #Build Using 500 Cities Project Data
      #partofset=get65menuptodate(state,county,tract,partofset,seedy)
      #partofset=get65womenuptodate(state,county,tract,partofset,seedy)
      #partofset=getadultasthma(state,county,tract,partofset,seedy)
      #partofset=getarthritis(state,county,tract,partofset,seedy)
      #partofset=getbingedrinking(state,county,tract,partofset,seedy)
      #partofset=getcancer(state,county,tract,partofset,seedy)
      #partofset=getcholesterolscreening(state,county,tract,partofset,seedy)
      #partofset=getchronicobspulmonarydisease(state,county,tract,partofset,seedy)
      #partofset=getcolonoscopy(state,county,tract,partofset,seedy)
      #partofset=getcoronaryheartdisease(state,county,tract,partofset,seedy)
      #partofset=getdiabetes(state,county,tract,partofset,seedy)
      #partofset=gethighbloodpressure(state,county,tract,partofset,seedy)
      #partofset=gethbpmedications(state,county,tract,partofset,seedy)
      #partofset=gethighcholesterol(state,county,tract,partofset,seedy)
      #partofset=getkidneydisease(state,county,tract,partofset,seedy)
      #partofset=getmammographyuse(state,county,tract,partofset,seedy)
      #partofset=getmentalhealth(state,county,tract,partofset,seedy)
      #partofset=getnoleisuretime(state,county,tract,partofset,seedy)
      #partofset=getobesity(state,county,tract,partofset,seedy)
      #partofset=getpapsmear(state,county,tract,partofset,seedy)
      #partofset=getphysicalhealth(state,county,tract,partofset,seedy)
      #partofset=getroutinecheckups(state,county,tract,partofset,seedy)
      #partofset=getsleep(state,county,tract,partofset,seedy)
      #partofset=getsmokers(state,county,tract,partofset,seedy)
      #partofset=getstroke(state,county,tract,partofset,seedy)
      #partofset=getteeth(state,county,tract,partofset,seedy)

      partofset$householdID=rep(paste(state,county,tract,seedy,"family.7.person.household",sep=".",collapse="."),nrow(partofset))

      #Save new household with any previous households
      fullset=rbind(fullset,partofset)
    }
  }

  #Create Non family households
  nonfamilyHHs=Census_data[c("nonfamily.1.person.household","nonfamily.2.person.household","nonfamily.3.person.household","nonfamily.4.person.household","nonfamily.5.person.household","nonfamily.6.person.household","nonfamily.7.person.household")]

  for(x in 1:7){

    if(nonfamilyHHs[x]>0){
      #make a seed for each household
      nonfamily_seeds=sample(1:100000000,as.numeric(nonfamilyHHs[x]),replace = FALSE)

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
        #partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
        partofset=getnumberofvehiclesforhouseholds(state,county,tract,partofset,seedy,Census_data)#only dependent on size
        partofset=getsexraceandage(state,county,tract,partofset,seedy,Census_data)#only dependent on race
        partofset=getschoolenrollment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two were cross tabulated together
        partofset=geteducationattainment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are cross tabulated together
        partofset=getemployment(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which is fine because those two are tabulated together
        partofset=getdisability(state,county,tract,partofset,seedy,Census_data)#dependent on age
        partofset=getlangandnativity(state,county,tract,partofset,seedy,Census_data)#dependent on race
        partofset=getcitizenandlang(state,county,tract,partofset,seedy,Census_data)#dependent on age,english, and nativity, age and nativity are not directly correlated this one needs to go, so this function had to be reworked
        partofset=getvets(state,county,tract,partofset,seedy,Census_data)#dependent on sex and age which are cross tabulated
        partofset=gettransport(state,county,tract,partofset,seedy,Census_data)#dependent on number of vehicles but also is inheritently dependent on employment because it's transportation to work so it has to be changed to dependent on gender instead of vehicles available
        partofset=gettraveltime(state,county,tract,partofset,seedy,Census_data)#dependent on travel method
        partofset=gethouseholdincome(state,county,tract,partofset,seedy,Census_data)#this was previously dependent on a cross tabulation for race, but since race is no longer sampled with household it's no done just by the census tract
        partofset=gethouseholdhealthinsurance(state,county,tract,partofset,seedy,Census_data)#dependent on income

        #Build Using 500 Cities Project Data
        #partofset=get65menuptodate(state,county,tract,partofset,seedy)
        #partofset=get65womenuptodate(state,county,tract,partofset,seedy)
        #partofset=getadultasthma(state,county,tract,partofset,seedy)
        #partofset=getarthritis(state,county,tract,partofset,seedy)
        #partofset=getbingedrinking(state,county,tract,partofset,seedy)
        #partofset=getcancer(state,county,tract,partofset,seedy)
        #partofset=getcholesterolscreening(state,county,tract,partofset,seedy)
        #partofset=getchronicobspulmonarydisease(state,county,tract,partofset,seedy)
        #partofset=getcolonoscopy(state,county,tract,partofset,seedy)
        #partofset=getcoronaryheartdisease(state,county,tract,partofset,seedy)
        #partofset=getdiabetes(state,county,tract,partofset,seedy)
        #partofset=gethighbloodpressure(state,county,tract,partofset,seedy)
        #partofset=gethbpmedications(state,county,tract,partofset,seedy)
        #partofset=gethighcholesterol(state,county,tract,partofset,seedy)
        #partofset=getkidneydisease(state,county,tract,partofset,seedy)
        #partofset=getmammographyuse(state,county,tract,partofset,seedy)
        #partofset=getmentalhealth(state,county,tract,partofset,seedy)
        #partofset=getnoleisuretime(state,county,tract,partofset,seedy)
        #partofset=getobesity(state,county,tract,partofset,seedy)
        #partofset=getpapsmear(state,county,tract,partofset,seedy)
        #partofset=getphysicalhealth(state,county,tract,partofset,seedy)
        #partofset=getroutinecheckups(state,county,tract,partofset,seedy)
        #partofset=getsleep(state,county,tract,partofset,seedy)
        #partofset=getsmokers(state,county,tract,partofset,seedy)
        #partofset=getstroke(state,county,tract,partofset,seedy)
        #partofset=getteeth(state,county,tract,partofset,seedy)

        partofset$householdID=rep(paste(state,county,tract,seedy,"nonfamily",sep=".",collapse="."),nrow(partofset))

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
