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

household_generator <- function(state, county, tract, seed, inputdir = "../Inputs/", Census_data){

  # initialize data frame
  fullset = data.frame()

  # set seed
  set.seed(seed)

  # subset data for correct Census tract
  Census_data = Census_data[(Census_data$state == state) & (Census_data$tract == tract) & (Census_data$county == county),]

  # Get a probability vector for their type
  familyHHtypes = Census_data[c("married.couple.families", "male.householders.no.wife", "female.householders.no.husband")]

  colnames(familyHHtypes) <- c("Married-couple family", "Male householder- no wife present", "Female householder- no husband present")
  familyHHtypes = familyHHtypes/rowSums(familyHHtypes)
  if(sum(familyHHtypes) <= 0){
    familyHHtypes = data.frame(household_type_not_available_this_tract_wont_run = 1)
  }

  # load and organize 500 Cities project data

  # Create family and nonfamily households
  HHs = Census_data[c("family.2.person.household","family.3.person.household","family.4.person.household","family.5.person.household","family.6.person.household","family.7.person.household",
                      "nonfamily.1.person.household","nonfamily.2.person.household","nonfamily.3.person.household","nonfamily.4.person.household","nonfamily.5.person.household","nonfamily.6.person.household","nonfamily.7.person.household")]

  fullset = do.call(rbind,sapply(2:14, function(x){
    create_household(state, county, tract, seed, Census_data, HHs[x-1], familyHHtypes, x)
  }))

  # return data.frame with all households built
  return(fullset)
}

create_household <- function(state, county, tract, seed, Census_data, census_col, family_type, family_size){
  if(census_col > 0){
    house_set = data.frame()

    # make a seed for each household
    family_HH_seeds = sample(1:100000000, as.numeric(census_col), replace = FALSE)

    house_set = as.data.frame(do.call(rbind, lapply(family_HH_seeds, function(seedy){ # for each seed create a household
      # set seed
      set.seed(seedy)

      # sample Household Type
      if(family_size < 8){
        HHtype = sample(colnames(family_type), size = 1, prob = family_type)

        # Create initial data frame
        if(HHtype == "Married-couple family"){
          partofset = data.frame(household.type = rep(HHtype, family_size), member = c("Husband","Wife", rep("NA", family_size - 2)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Male householder- no wife present"){
          partofset = data.frame(household.type = rep(HHtype, family_size), member = c("Male Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
        else if(HHtype == "Female householder- no husband present"){
          partofset = data.frame(household.type = rep(HHtype, family_size), member = c("Female Householder", rep("NA", family_size - 1)), size = rep(family_size, family_size))
        }
      }
      else{
        if(family_size == 8){
          partofset = data.frame(household.type = "Alone", member = "Householder", size = 1)
        }
        else if(family_size > 8){
          partofset = data.frame(household.type = rep("Non-family", family_size - 7), member = c("Householder", rep("NA",(family_size - 8))), size = rep(family_size - 7, family_size -7))
        }
      }

      # Begin sampling characteristics of household (functions stored in other scripts)
      # The functions must be called in this order as some characteristics have different probability distributions based on other characteristics

      # Build using Census Data
      # partofset=gethouseholdtypeandrace(state,county,tract,seedy,Census_data)#not dependent on anything gets type and race
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
      partofset$state=rep(state,nrow(partofset))
      partofset$county=rep(county,nrow(partofset))
      partofset$tract=rep(tract,nrow(partofset))

      # Build Using 500 Cities Project Data
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

      if(family_size < 8)
        partofset$householdID=rep(paste(state, county, tract, seedy, paste0("family.", as.character(family_size), ".person.household"), sep=".",collapse="."),nrow(partofset))
      else
        partofset$householdID=rep(paste(state, county, tract, seedy, "nonfamily", sep=".", collapse="."), nrow(partofset))

      # Save new household with any previous households
      return( partofset)
    })))

    return(house_set)
  }
}

#list of counties in Houston
#https://www.houston.org/business/regionalProfile.html
#looked up conty number on site below
#https://www.census.gov/2010census/partners/pdf/FIPS_StateCounty_Code.pdf
#countycodesinhouston=c(201,071,167,039,157,473,015,339,471,407,291)
#there are 1086 total tracts in this area

#sample.set=run.me.for.houston(countycodesinhouston,50,2)
#write.csv(sample.set,"sample_set2.csv")
