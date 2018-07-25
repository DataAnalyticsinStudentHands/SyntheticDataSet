#' Simulate number of vehicles for people in group quarters
#'
#' This function uses data from the U.S. Census to sample the number of vehicles for people living in group quarters.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for the number of cars based off of sex and employment.
#' Sex and employment can be simulated using the getindividualcharacteristics function.
#' It then samples using the user inputed seed.
#'
#' @param syntheticdataset The individual simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return number.of.vehicles The number.of.vehicles for the individual.

getnumberofvehiclesforgroupquarters <- function(Census_data, seed, syntheticdataset){
  #Set seed so sampling is random but repeatable
  set.seed(seed)

  male_number_of_vehicles = Census_data[670:675]
  colnames(male_number_of_vehicles) = paste0(0:5)
  female_number_of_vehicles = Census_data[670:675]
  colnames(female_number_of_vehicles) = paste0(0:5)

  if(sum(male_number_of_vehicles) <= 0){
    male_number_of_vehicles = data.frame(number_of_cars_available_by_gender_not_available_for_this_Census_Tract = 1)
  }
  if(sum(female_number_of_vehicles) <= 0){
    female_number_of_vehicles = data.frame(number_of_cars_available_by_gender_not_available_for_this_Census_Tract = 1)
  }

  number.of.vehicles = ifelse(syntheticdataset$sex == "Male" & syntheticdataset$employment == "Employed", sample(colnames(male_number_of_vehicles), 1, prob = male_number_of_vehicles/sum(male_number_of_vehicles)),
                              ifelse(syntheticdataset$sex == "Female" & syntheticdataset$employment == "Employed", sample(colnames(female_number_of_vehicles), 1, prob = female_number_of_vehicles/sum(female_number_of_vehicles)), NA))

  return(number.of.vehicles)
}

#' Simulate income for people in group quarters
#'
#' This function uses data from the U.S. Census to sample the income for people living in group quarters.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for income.
#' It then samples using the user inputed seed.
#'
#' @param syntheticdataset The individual simulated.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return household.income The income for the individual.

getincomeforgroupquarters <- function(Census_data, seed, syntheticdataset){
  #Set seed so sampling is random but repeatable
  set.seed(seed)

  income = Census_data[startsWith(names(Census_data), "individual")]

  if(sum(income) <= 0){
    income = data.frame(income_not_available_for_this_Census_Tract = 1)
  }
  
  # bracket.hosuehold.income is a string stating the income range (Ex: "individual.10000.14999"). 
  bracket.household.income = sample(colnames(income), 1, prob = income/sum(income))

  twonumbers <- strsplit(gsub("individual.", "", bracket.household.income), "\\.")
  income_range <- as.numeric(twonumbers[[1]])

  # household.income is a number between the range stated in bracket.household.income
  household.income = sample(c(income_range[1]:income_range[2]),1)

  return(household.income)
}

#' Simulate health insurance for people in group quarters
#'
#' This function uses data from the U.S. Census to sample the health insurance status for people living in group quarters.
#'
#' The function uses the data from the U.S. Census on the tract level to build a probability vector for health insurance status based off of age and disability status.
#' Age can be simulated with the getsexraceandage function. Disability status cen be simulated using the getdisability function.
#' It then samples using the user inputed seed.
#'
#' @param syntheticdataset The individual simulated must include age and disability status.
#' @param seed The seed to use for sampling.
#' @param Census_data Census data to use for the simulation. Can be mined from the function census_data_API
#' @return health.insurance The health insurance for the individual.

gethealthinsuranceforgroupquarters <- function(Census_data, seed, syntheticdataset){
  #Set seed so sampling is random but repeatable
  set.seed(seed)

  #divide out appropriately
  disability_group = c("with.disability.under18", "without.disability.under18", "with.disability.18.64", "without.disability.18.64", "with.disability.over65", "without.disability.over65")
  sapply(disability_group, function(group) assign(group, Census_data[startsWith(names(Census_data), group)], envir = parent.frame(3)))

  code = c("private insurance","public insurance","no insurance")
  warning_message = "health_insurance_not_available_by_age_and_disability_for_this_Census_Tract"

  health.insurance = switch(syntheticdataset$disability,
                            "With One Type of Disability"=, "With Two or More Types of Disabilities" = switch(syntheticdataset$bracket.age,
                                                                                                              "0.to.5"=, "5.to.9"=, "10.to.14"=, "15.to.17" = ifelse(sum(with.disability.under18) > 0, sample(code, 1, prob = with.disability.under18/sum(with.disability.under18)), warning_message),
                                                                                                              "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34"=, "35.to.44"=, "45.to.54"=, "55.to.64" = ifelse(sum(with.disability.18.64) > 0, sample(code, 1, prob = with.disability.18.64/sum(with.disability.18.64)), warning_message),
                                                                                                              "65.to.74"=, "75.to.84"=, "85.to.100" = ifelse(sum(with.disability.over65) > 0, sample(code, 1, prob = with.disability.over65/sum(with.disability.over65)), warning_message)),
                            "No Disabilities" = switch(syntheticdataset$bracket.age,
                                                       "0.to.5"=, "5.to.9"=, "10.to.14"=, "15.to.17" = ifelse(sum(without.disability.under18) > 0, sample(code, 1, prob = without.disability.under18/sum(without.disability.under18)), warning_message),
                                                       "18.to.19"=, "20.to.24"=, "25.to.29"=, "30.to.34"=, "35.to.44"=, "45.to.54"=, "55.to.64" = ifelse(sum(without.disability.18.64) > 0, sample(code, 1, prob = without.disability.18.64/sum(without.disability.18.64)), warning_message),
                                                       "65.to.74"=, "75.to.84"=, "85.to.100" = ifelse(sum(without.disability.over65) > 0, sample(code, 1, prob = without.disability.over65/sum(without.disability.over65)), warning_message)))

  return(health.insurance)
}
