#' Simulate number of vehicles for households
#'
#' This function uses data from the U.S. Census to sample the number of vehicles a household would have based on its size.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector from the number of households of the inputed size with either no vehicles, 1 vehicle, 2 vehicles, 3 vehicles or 4 or more vehicles.
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The household simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated household with the added variable for number of vehicles.

getnumberofvehiclesforhouseholds <- function(state, county, tract, syntheticdataset, seed, Census_data){
  #Generates number of vehicles per household

  #Set seed so sampling is random but repeatable
  set.seed(seed)

  #Read in data for sampling distribution
  Census_data = Census_data[(Census_data$tract == tract) & (Census_data$county == county) & (Census_data$state == state),]

  syntheticdataset = getnumberofvehiclessample(syntheticdataset, Census_data)

  return(syntheticdataset)
}

getnumberofvehiclessample <- function(syntheticdataset, Census_data){
  house_size = (nrow(syntheticdataset))

  if(house_size > 4){
    house_size = 4
  }

  # Use appropriate Census Subheadings
  houseof_ = Census_data[c( paste0("households.of.", as.character(house_size), ".no.vehicle"), paste0("households.of.", as.character(house_size), ".1.vehicle"), paste0("households.of.", as.character(house_size), ".2.vehicles"), paste0("households.of.", as.character(house_size), ".3.vehicles"), paste0("households.of.", as.character(house_size), ".4.vehicles"))]

  # Create probability Distribution
  total = sum(houseof_[1,1:5])
  prob1 = (houseof_[1,1:5])/total
  colnames(prob1) = c(0,1,2,3,4)
  if(sum(total) <= 0){
    prob2 = data.frame(assign(paste0("number_of_vehicles_available_for_households_of_", as.character(house_size), "_not_available_for_this_Census_Tract"), 1))
  }

  # Samples
  a = sample(colnames(prob1), size = 1, prob = prob1)
  # add number of cars to each person in household in data frame
  number.of.vehicles <- rep(a, nrow(syntheticdataset))
  syntheticdataset$number.of.vehicles = number.of.vehicles

  return(syntheticdataset = syntheticdataset)
}

#' Simulate Household Income
#'
#' This function uses data from the U.S. Census on the tract level to build a probability vector for different household incomes and sample with the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The household simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated household with the added variable of household income.

gethouseholdincome <- function(state, county, tract, syntheticdataset, seed, Census_data){
  set.seed(seed)

  Census_data = Census_data[(Census_data$state == state) & (Census_data$county == county) & (Census_data$tract == tract),]

  income = Census_data[c("income.less.10000","income.10000.14999","income.15000.19999","income.20000.24999","income.25000.29999",
                       "income.30000.34999","income.35000.39999","income.40000.44999","income.45000.49999","income.50000.59999",
                       "income.60000.74999","income.75000.99999","income.100000.124999","income.125000.149999","income.150000.199999","income.over.200000")]

  code = c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")

  if(sum(income) <= 0){
    income = 1
    code = "household income not available in this Census Tract"
  }

  household.income = sample(code, size = 1, prob = c(income/sum(income)))
  household.income = rep(household.income, nrow(syntheticdataset))
  syntheticdataset$household.income = household.income

  return(syntheticdataset)
}

#' Simulate Household Health Insurance
#'
#' This function uses data from the U.S. Census on the tract level to build a probability vector for health insurance status based on the presimulated household income. It then samples with the user inputed seed. Household income can be simulated with the function gethouseholdincome.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The household simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated household with the added variable of health insurance status.

gethouseholdhealthinsurance <- function(state, county, tract, syntheticdataset, seed, Census_data){
  set.seed(seed)

  Census_data = Census_data[(Census_data$state == state) & (Census_data$county == county) & (Census_data$tract == tract),]

  #Organize Data Set by row
  under25000 = Census_data[c("private.insurance.under25000","public.insurance.under25000","no.insurance.under25000")]
  between25to49 = Census_data[c("private.insurance.25.49","public.insurance.25.49","no.insurance.25.49")]
  between50to75 = Census_data[c("private.insurance.50.75","public.insurance.50.75","no.insurance.50.75")]
  between75to100 = Census_data[c("private.insurance.75.100","public.insurance.75.100","no.insurance.75.100")]
  over100 = Census_data[c("private.insurance.over100","public.insurance.over100","no.insurance.over100")]


  code = c("private insurance","public insurance","no insurance")
  warningMessage = "Health Insurance Status By Income Not Available for this Census Tract"

  health.insurance = sapply(syntheticdataset$household.income, function(val) switch(val,
                            "less than 10,000"=, "10,000 to 14,999"=, "15,000 to 19,999"=, "20,000 to 24,999"= ifelse(sum(under25000) > 0, sample(code, 1, prob = under25000/sum(under25000)), warningMessage),
                            "25,000 to 29,999"=, "30,000 to 34,999"=, "35,000 to 39,999"=, "40,000 to 44,999"=, "45,000 to 49,99"= ifelse(sum(between25to49) > 0, sample(code, 1, prob = between25to49/sum(between25to49)), warningMessage),
                            "50,000 to 59,999"=, "60,000 to 74,999" = ifelse(sum(between50to75) > 0, sample(code, 1, prob = between50to75/sum(between50to75)), warningMessage),
                            "75,000 to 99,999" = ifelse(sum(between75to100) > 0, sample(code, 1, prob = between75to100/sum(between75to100)), warningMessage),
                            "100,000 to 124,999"=, "125,000 to 149,999"=, "150,000 to 199,999"=, "200,000 or more" = ifelse(sum(over100) > 0, sample(code, 1, prob = over100/sum(over100)), warningMessage)
  ))

  syntheticdataset$health.insurance = health.insurance
  return(syntheticdataset)
}
