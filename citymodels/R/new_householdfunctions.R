#' Simulate number of vehicles for households
#'
#' This function uses data from the U.S. Census to sample the number of vehicles a household would have based on its size.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector from the number of households of the inputed size with either no vehicles, 1 vehicle, 2 vehicles, 3 vehicles or 4 or more vehicles.
#' It then samples using the user inputed seed.
#'
#' @param syntheticdataset The household simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated household with the added variable of number of vehicles.

getnumberofvehiclesforhouseholds <- function(syntheticdataset, seed, Census_data){
  set.seed(seed)

  #samples for vehicles
  house_size = nrow(syntheticdataset)

  if(house_size > 4){
    house_size = 4
  }

  # Use appropriate Census Subheadings and create probability distribution
  houseof_ = Census_data[startsWith(names(Census_data),paste0("households.of.", as.character(house_size)))]
  colnames(houseof_) = c(0,1,2,3,4)
  
  syntheticdataset$number.of.vehicles = sample(colnames(houseof_), size = 1, prob = c(houseof_/sum(houseof_)))

  return(syntheticdataset)
}

#' Simulate Household Income
#'
#' This function uses data from the U.S. Census on the tract level to build a probability vector for different household incomes and sample with the user inputed seed.
#'
#' @param syntheticdataset The household simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated household with the added variable of bracket household income and household income.

gethouseholdincome <- function(syntheticdataset, seed, Census_data){
  set.seed(seed)

  #samples for income
  income <- Census_data[startsWith(names(Census_data), "income")]

  # bracket.household.income is a string that gives the income range (Ex: "income.10000.14999"). This is added to the dataset so it will be easier to find health.insurance, and then it will be removed.
  syntheticdataset$bracket.household.income = sample(colnames(income), size = 1, prob = c(income/sum(income)))

  twonumbers <- strsplit(gsub("income.", "", syntheticdataset$bracket.household.income), "\\.")
  income_range <- as.numeric(twonumbers[[1]])

  # household.income is an actual number from the income range stated in bracket.household.income
  syntheticdataset$household.income = sample(c(income_range[1]:income_range[2]),1)

  return(syntheticdataset)
}

#' Simulate Household Health Insurance
#'
#' This function uses data from the U.S. Census on the tract level to build a probability vector for health insurance status based on the presimulated household income. It then samples with the user inputed seed. Household income can be simulated with the function gethouseholdincome.
#'
#' @param syntheticdataset The household simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated household with the added variable of health insurance.

gethouseholdhealthinsurance<-function(syntheticdataset, seed, Census_data){
  set.seed(seed)

  #samples for insurance
  income_group = c("0.25", "25.49", "50.75", "75.100", "100.500")
  sapply(income_group, function(group) assign(paste0("between",group), Census_data[endsWith(names(Census_data), group)], envir = parent.frame(3)))

  insurance_code = c("private insurance","public insurance","no insurance")
  
  syntheticdataset$health.insurance = switch(syntheticdataset[1,]$bracket.household.income,
                            "income.0.10000"=, "income.10000.14999"=, "income.15000.19999"=, "income.20000.24999"= sample(insurance_code, 1, prob = between0.25/sum(between0.25)),
                            "income.25000.29999"=, "income.30000.34999"=, "income.35000.39999"=, "income.40000.44999"=, "income.45000.49999"= sample(insurance_code, 1, prob = between25.49/sum(between25.49)),
                            "income.50000.59999"=, "income.60000.74999" = sample(insurance_code, 1, prob = between50.75/sum(between50.75)),
                            "income.75000.99999" = sample(insurance_code, 1, prob = between75.100/sum(between75.100)),
                            "income.100000.124999"=, "income.125000.149999"=, "income.150000.199999"=, "income.200000.500000" = sample(insurance_code, 1, prob = between100.500/sum(between100.500)))

  return(syntheticdataset)
}
