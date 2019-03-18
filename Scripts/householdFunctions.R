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
  disability_group = c("with.disability.under18", "without.disability.under18", "with.disability.18.64", "without.disability.18.64", "with.disability.over65", "without.disability.over65")
  sapply(disability_group, function(group) assign(group, Census_data[c(paste0(group,".private.insurance"),paste0(group,".public.insurance"),paste0(group,".no.insurance"))], envir = parent.frame(3)))
  
  code = c("private insurance","public insurance","no insurance")
  
  syntheticdataset$health.insurance = switch(syntheticdataset[1,]$disability,
                                             "With One Type of Disability" =, "With Two or More Types of Disabilities" = switch(syntheticdataset[1,]$bracket.age,
                                                                                                                               "0.to.4"=, "5.to.9"=, "10.to.14"=, "15.to.17" = ifelse(sum(with.disability.under18) > 0, sample(code, 1, prob = with.disability.under18/sum(with.disability.under18)), NA),
                                                                                                                               "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34"=, "35.to.44"=, "45.to.54"=, "55.to.64" = ifelse(sum(with.disability.18.64) > 0, sample(code, 1, prob = with.disability.18.64/sum(with.disability.18.64)), NA),
                                                                                                                               "65.to.74"=, "75.to.84"=, "85.to.100" = ifelse(sum(with.disability.over65) > 0, sample(code, 1, prob = with.disability.over65/sum(with.disability.over65)), NA)),
                                             "No Disabilities" = switch(syntheticdataset[1,]$bracket.age,
                                                                        "0.to.4"=, "5.to.9"=, "10.to.14"=, "15.to.17" = ifelse(sum(without.disability.under18) > 0, sample(code, 1, prob = without.disability.under18/sum(without.disability.under18)), NA),
                                                                        "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34"=, "35.to.44"=, "45.to.54"=, "55.to.64" = ifelse(sum(without.disability.18.64) > 0, sample(code, 1, prob = without.disability.18.64/sum(without.disability.18.64)), NA),
                                                                        "65.to.74"=, "75.to.84"=, "85.to.100" = ifelse(sum(without.disability.over65) > 0, sample(code, 1, prob = without.disability.over65/sum(without.disability.over65)), NA)))
 
  return(syntheticdataset)
}
