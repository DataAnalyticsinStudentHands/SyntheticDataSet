# The broman package must be installed/attcahed for the switch function in the health insurance section to work
library(broman)

#' Simulate number of vehicles for people in group quarters
#'
#' This function uses data from the U.S. Census to sample the number of vehicles for people living in group quarters.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for the number of cars based off of sex.
#' Sex can be simulated using the getsexraceandage function.
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for number.of.vehicles.

getnumberofvehiclesforgroupquarters <- function(state, county, tract, syntheticdataset, seed, Census_data){
  #Generates number of vehicles per household

  #Set seed so sampling is random but repeatable
  set.seed(seed)

  Census_data = Census_data[(Census_data$state == state) & (Census_data$county == county) & (Census_data$tract == tract),]

  male_number_of_vehicles = Census_data[c("male0cars","male1car","male2cars","male3cars","male4cars","male5cars")]
  colnames(male_number_of_vehicles) = paste0(0:5)
  female_number_of_vehicles = Census_data[c("female0cars","female1car","female2cars","female3cars","female4cars","female5cars")]
  colnames(female_number_of_vehicles) = paste0(0:5)

  if(sum(male_number_of_vehicles) <= 0){
    male_number_of_vehicles = data.frame(number_of_cars_available_by_gender_not_available_for_this_Census_Tract = 1)
  }
  if(sum(female_number_of_vehicles) <= 0){
    female_number_of_vehicles = data.frame(number_of_cars_available_by_gender_not_available_for_this_Census_Tract = 1)
  }

  number.of.vehicles = ifelse(syntheticdataset$sex == "Male" & syntheticdataset$employment == "Employed", sample(colnames(male_number_of_vehicles), 1, prob = male_number_of_vehicles/sum(male_number_of_vehicles)),
                            ifelse(syntheticdataset$sex == "Female" & syntheticdataset$employment == "Employed", sample(colnames(female_number_of_vehicles), 1, prob = female_number_of_vehicles/sum(female_number_of_vehicles)), NA))

  syntheticdataset$number.of.vehicles = number.of.vehicles

  return(syntheticdataset = syntheticdataset)
}

#' Simulate income for people in group quarters
#'
#' This function uses data from the U.S. Census to sample the income for people living in group quarters.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for income.
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for income.
#'
getincomeforgroupquarters <- function(state, county, tract, syntheticdataset, seed, Census_data){
  #Generates number of vehicles per household

  #Set seed so sampling is random but repeatable
  set.seed(seed)

  Census_data = Census_data[(Census_data$state == state) & (Census_data$county == county) & (Census_data$tract == tract),]

  income = Census_data[c("individual.no.income",
                       "individual.less.10000",
                       "individual.10000.14999",
                       "individual.15000.19999",
                       "individual.20000.24999",
                       "individual.25000.34999",
                       "individual.35000.49999",
                       "individual.50000.64999",
                       "individual.65000.74999",
                       "individual.over75000")]
  #Give variable colnames
  colnames(income) = c("No income","less than 10,000","10,000 to 14,999","15,000 to 24,999","25,000 to 34,999","35,000 to 49,999","50,000 to 64,999","65,000 to 74,999","Over 75,000")

  if(sum(income) >= 0){
    income = data.frame(income_not_available_for_this_Census_Tract = 1)
  }
  #Sample
  household.income = paste0("individual_income:", sample(colnames(income), 1, prob = income/sum(income)))

  #add to data frame
  syntheticdataset$household.income = household.income

  return(syntheticdataset = syntheticdataset)
}

#' Simulate health insurance for people in group quarters
#'
#' This function uses data from the U.S. Census to sample the health insurance status for people living in group quarters.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for health insurance status based off of age and disability status.
#' Age can be simulated with the getsexraceandage function. Disability status cen be simulated using the getdisability function.
#' It then samples using the user inputed seed.
#'
#' @param state The state the user is simulating
#' @param county The county the user is simulating
#' @param tract The tract the user is simulating
#' @param syntheticdataset The individual simulated must include age and disability status.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return syntheticdataset The simulated dataset with the added variable for health insurance.
#'
gethealthinsuranceforgroupquarters <- function(state, county, tract, syntheticdataset, seed, Census_data){
  #Generates number of vehicles per household

  #Set seed so sampling is random but repeatable
  set.seed(seed)

  Census_data = Census_data[(Census_data$state == state) & (Census_data$county == county) & (Census_data$tract == tract),]

  #divide out appropriately
  with_disability_under_18 = Census_data[c("with.disability.under18.private.insurance","with.disability.under18.public.insurance","with.disability.under18.no.insurance")]
  without_disability_under_18 = Census_data[c("without.disability.under18.private.insurance","without.disability.under18.public.insurance","without.disability.under18.no.insurance")]
  with_disability_18_to_64 = Census_data[c("with.disability.18.64.private.insurance","with.disability.18.64.public.insurance","with.disability.18.64.no.insurance")]
  without_disability_18_to_64 = Census_data[c("without.disability.18.64.private.insurance","without.disability.18.64.public.insurance","without.disability.18.64.no.insurance")]
  with_disability_over_65 = Census_data[c("with.disability.over65.private.insurance","with.disability.over65.public.insurance","with.disability.over65.no.insurance")]
  without_disability_over_65 = Census_data[c("without.disability.over65.private.insurance","without.disability.over65.public.insurance","without.disability.over65.no.insurance")]

  code = c("private insurance","public insurance","no insurance")
  warning_message = "health_insurance_not_available_by_age_and_disability_for_this_Census_Tract"

  health.insurance = switchv(syntheticdataset$disability,
                             "With One Type of Disability"=, "With Two or More Types of Disabilities" = switchv(syntheticdataset$age,
                                                                                                                "Under 5"=, "5 to 9"=, "10 to 14"=, "15 to 17" = ifelse(sum(with_disability_under_18) > 0, sample(code, 1, prob = with_disability_under_18/sum(with_disability_under_18)), warning_message),
                                                                                                                "18 to 19"=, "20 to 24"=, "25 to 29"=, "30 to 34"=, "35 to 44"=, "45 to 54"=, "55 to 64" = ifelse(sum(with_disability_18_to_64) > 0, sample(code, 1, prob = with_disability_18_to_64/sum(with_disability_18_to_64)), warning_message),
                                                                                                                "65 to 74"=, "75 to 84"=, "Over 85" = ifelse(sum(with_disability_over_65) > 0, sample(code, 1, prob = with_disability_over_65/sum(with_disability_over_65)), warning_message)),
                              "No Disabilities" = switchv(syntheticdataset$age,
                                                          "Under 5"=, "5 to 9"=, "10 to 14"=, "15 to 17" = ifelse(sum(without_disability_under_18) > 0, sample(code, 1, prob = without_disability_under_18/sum(without_disability_under_18)), warning_message),
                                                          "18 to 19"=, "20 to 24"=, "25 to 29"=, "30 to 34"=, "35 to 44"=, "45 to 54"=, "55 to 64" = ifelse(sum(without_disability_18_to_64) > 0, sample(code, 1, prob = without_disability_18_to_64/sum(without_disability_18_to_64)), warning_message),
                                                          "65 to 74"=, "75 to 84"=, "Over 85" = ifelse(sum(without_disability_over_65) > 0, sample(code, 1, prob = without_disability_over_65/sum(without_disability_over_65)), warning_message)))

  syntheticdataset$health.insurance = health.insurance

  return(syntheticdataset = syntheticdataset)
}
