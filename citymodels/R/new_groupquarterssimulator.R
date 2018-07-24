#' Group Quarters Simulator
#'
#' This function simulates people living in group quarters.
#'
#' It calls other functions in the citymodels package in order to simulate characteristics for each individual
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param seed The seed to use for sampling.
#' @param inputdir The input directory for other data
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset A dataframe of simulated people living in group quarters.

group_quarters_simulater <- function(state, county, tract, seed, inputdir = "../Inputs/", Census_data){
  #Set seed so sampling will be repeatable
  set.seed(seed)
  fullset = data.frame()

  #subset data for correct Census tract
  Census_data = Census_data[(Census_data$state == state) & (Census_data$tract == tract) & (Census_data$county == county),]
  column_names = colnames(Census_data)

  number.of.people = Census_data$group.quarters.population

  if(number.of.people > 0){
    #Create a seed for each household from original seed
    seeds = sample(1:100000000, number.of.people, replace = FALSE)
    #create empty data set to build from

    fullset = as.data.frame(do.call(rbind, lapply(seeds, function(seedy){
    #Make Sure tract picked actually has people in it
      total1 = number.of.people
      if (total1 > 0){
        #Begin sampling characteristics of household (functions stored in other scripts)
        #The functions must be called in this order as some characteristics have different probability distributions based on other characteristics

        #Build using Census Data
        partofset=data.frame(household.type="Group Quarters",members="NA",size="Group Quarters") #create initial data frame
        partofset = getindividualcharacteristics(partofset, seedy, Census_data) #simulates sex, race, age, school.enrollment, education.attainment, employment, disability, nativity, citizenship, language, veteran.status, transport.method, travel.time
        partofset$number.of.vehicles = getnumberofvehiclesforgroupquarters(Census_data, seedy, partofset, column_names) #depends on sex and employment
        partofset$household.income = getincomeforgroupquarters(Census_data, seedy, partofset) #independent  -- samples are directly from census data
        partofset$health.insurance = gethealthinsuranceforgroupquarters(Census_data, seedy, partofset) #depends on disability and age
        partofset$bracket.age=NULL #this column is no longer needed

        #Build Using 500 Cities Project Data
        #partofset=get65menuptodate(county,tract,partofset,seedy)
        #partofset=get65womenuptodate(county,tract,partofset,seedy)
        #partofset=getadultasthma(county,tract,partofset,seedy)
        #partofset=getarthritis(county,tract,partofset,seedy)
        #partofset=getbingedrinking(county,tract,partofset,seedy)
        #partofset=getcancer(county,tract,partofset,seedy)
        #partofset=getcholesterolscreening(county,tract,partofset,seedy)
        #partofset=getchronicobspulmonarydisease(county,tract,partofset,seedy)
        #partofset=getcolonoscopy(county,tract,partofset,seedy)
        #partofset=getcoronaryheartdisease(county,tract,partofset,seedy)
        #partofset=getdiabetes(county,tract,partofset,seedy)
        #partofset=gethighbloodpressure(county,tract,partofset,seedy)
        #partofset=gethbpmedications(county,tract,partofset,seedy)
        #partofset=gethighcholesterol(county,tract,partofset,seedy)
        #partofset=getkidneydisease(county,tract,partofset,seedy)
        #partofset=getmammographyuse(county,tract,partofset,seedy)
        #partofset=getmentalhealth(county,tract,partofset,seedy)
        #partofset=getnoleisuretime(county,tract,partofset,seedy)
        #partofset=getobesity(county,tract,partofset,seedy)
        #partofset=getpapsmear(county,tract,partofset,seedy)
        #partofset=getphysicalhealth(county,tract,partofset,seedy)
        #partofset=getroutinecheckups(county,tract,partofset,seedy)
        #partofset=getsleep(county,tract,partofset,seedy)
        #partofset=getsmokers(county,tract,partofset,seedy)
        #partofset=getstroke(county,tract,partofset,seedy)
        #partofset=getteeth(county,tract,partofset,seedy)
        partofset$state = rep(state,nrow(partofset))
        partofset$county = rep(county,nrow(partofset))
        partofset$tract = rep(tract,nrow(partofset))

        partofset$householdID = rep(paste("group_quarters", county, tract, seedy, sep=".", collapse="."), nrow(partofset))

        #Save everyone together
        return(partofset)
      }
    })))
  }

  #return data.frame with all households built
  return(fullset)
}
