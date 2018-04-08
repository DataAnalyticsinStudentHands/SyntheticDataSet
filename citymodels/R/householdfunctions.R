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

getnumberofvehiclesforhouseholds <- function(state, county, tract,syntheticdataset,seed,Census_data){
  #Generates number of vehicles per household

  #Set seed so sampling is random but repeatable
  set.seed(seed)

  #Read in data for sampling distribution
  Census_data=Census_data[(Census_data$tract==tract)&(Census_data$county==county)&(Census_data$state==state),]


  #Sample for one person household
  if (nrow(syntheticdataset)==1){
    #Use appropriate Census Subheadings
    houseofone=Census_data[c("households.of.1.no.vehicle","households.of.1.1.vehicle","households.of.1.2.vehicles","households.of.1.3.vehicles","households.of.1.4.vehicles")]
    #Create probability Distribution
    total=sum(houseofone[1,1:5])
    prob1=(houseofone[1,1:5])/total
    colnames(prob1)=c(0,1,2,3,4)
    if(sum(total)<=0){
      prob1=data.frame(number_of_vehicles_available_for_households_of_1_not_available_for_this_Census_Tract=1)
    }
    #Samples
    a=sample(colnames(prob1),size=1,prob=prob1)
    #add number of cars to each person in household in data frame
    number.of.vehicles <- rep(a,nrow(syntheticdataset))
    syntheticdataset$number.of.vehicles=number.of.vehicles
  }
  #Sample for 2 person households
  if (nrow(syntheticdataset)==2){
    #Use apppropriate Census subheadings for 2 people households
    houseoftwo=Census_data[c("households.of.2.no.vehicle","households.of.2.1.vehicle","households.of.2.2.vehicles","households.of.2.3.vehicles","households.of.2.4.vehicles")]
    #Create probability distribution
    total=sum(houseoftwo[1,1:5])
    prob2=(houseoftwo[1,1:5])/total
    colnames(prob2)=c(0,1,2,3,4)
    if(sum(total)<=0){
      prob1=data.frame(number_of_vehicles_available_for_households_of_2_not_available_for_this_Census_Tract=1)
    }
    #Sample for number of cars
    a=sample(colnames(prob2),size=1,prob=prob2)
    #add number of cars to each person in household in data frame
    number.of.vehicles <- rep(a,nrow(syntheticdataset))
    syntheticdataset$number.of.vehicles=number.of.vehicles
  }
  #Sample for 3 person households
  if (nrow(syntheticdataset)==3){
    #Use apppropriate Census subheadings for 2 people households
    houseofthree=Census_data[c("households.of.3.no.vehicle","households.of.3.1.vehicle","households.of.3.2.vehicles","households.of.3.3.vehicles","households.of.3.4.vehicles")]
    #Create probability distribution
    total=sum(houseofthree[1,1:5])
    prob2=(houseofthree[1,1:5])/total
    colnames(prob2)=c(0,1,2,3,4)
    if(sum(total)<=0){
      prob1=data.frame(number_of_vehicles_available_for_households_of_3_not_available_for_this_Census_Tract=1)
    }
    #Sample for number of cars
    a=sample(colnames(prob2),size=1,prob=prob2)
    #add number of cars to each person in household in data frame
    number.of.vehicles <- rep(a,nrow(syntheticdataset))
    syntheticdataset$number.of.vehicles=number.of.vehicles
  }
  #Sample for 4 person households
  if (nrow(syntheticdataset)>=4){
    #Use apppropriate Census subheadings for 2 people households
    houseof4=Census_data[c("households.of.4.no.vehicle","households.of.4.1.vehicle","households.of.4.2.vehicles","households.of.4.3.vehicles","households.of.4.4.vehicles")]
    #Create probability distribution
    total=sum(houseof4[1,1:5])
    prob2=(houseof4[1,1:5])/total
    colnames(prob2)=c(0,1,2,3,4)
    if(sum(total)<=0){
      prob1=data.frame(number_of_vehicles_available_for_households_of_4_or_more_not_available_for_this_Census_Tract=1)
    }
    #Sample for number of cars
    a=sample(colnames(prob2),size=1,prob=prob2)
    #add number of cars to each person in household in data frame
    number.of.vehicles <- rep(a,nrow(syntheticdataset))
    syntheticdataset$number.of.vehicles=number.of.vehicles
  }

  return(syntheticdataset=syntheticdataset)
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

gethouseholdincome <- function(state,county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) & (Census_data$county==county) & (Census_data$tract==tract),]

  income=Census_data[c("income.less.10000","income.10000.14999","income.15000.19999","income.20000.24999","income.25000.29999",
                       "income.30000.34999","income.35000.39999","income.40000.44999","income.45000.49999","income.50000.59999",
                       "income.60000.74999","income.75000.99999","income.100000.124999","income.125000.149999","income.150000.199999","income.over.200000")]

  code=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")

  if(sum(income)<=0){
    income=1
    code="household income not available in this Census Tract"
  }

  household.income=sample(code,size=1,prob=c(income/sum(income)))
  household.income=rep(household.income,nrow(syntheticdataset))
  syntheticdataset$household.income=household.income


  return(syntheticdataset)#=finalsyntheticdataset)

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

gethouseholdhealthinsurance <- function(state, county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)

  Census_data=Census_data[(Census_data$state==state) &(Census_data$county==county) & (Census_data$tract==tract),]

  #Organize Data Set by row
  under25000=Census_data[c("private.insurance.under25000","public.insurance.under25000","no.insurance.under25000")]
  between25to49=Census_data[c("private.insurance.25.49","public.insurance.25.49","no.insurance.25.49")]
  between50to75=Census_data[c("private.insurance.50.75","public.insurance.50.75","no.insurance.50.75")]
  between75to100=Census_data[c("private.insurance.75.100","public.insurance.75.100","no.insurance.75.100")]
  over100=Census_data[c("private.insurance.over100","public.insurance.over100","no.insurance.over100")]


  code=c("private insurance","public insurance","no insurance")

  health.insurance=ifelse(sum(under25000)>0&syntheticdataset$household.income=="less than 10,000"|syntheticdataset$household.income=="10,000 to 14,999"|syntheticdataset$household.income=="15,000 to 19,999"|syntheticdataset$household.income=="20,000 to 24,999",sample(code,1,prob=under25000/sum(under25000)),
                          ifelse(sum(under25000)<=0&syntheticdataset$household.income=="less than 10,000"|syntheticdataset$household.income=="10,000 to 14,999"|syntheticdataset$household.income=="15,000 to 19,999"|syntheticdataset$household.income=="20,000 to 24,999","Health Insurance Status By Income Not Available for this Census Tract",
                          ifelse(sum(between25to49)>0&syntheticdataset$household.income=="20,000 to 24,999"|syntheticdataset$household.income=="25,000 to 29,999"|syntheticdataset$household.income=="30,000 to 34,999"|syntheticdataset$household.income=="35,000 to 39,999"|syntheticdataset$household.income=="40,000 to 44,999"|syntheticdataset$household.income=="45,000 to 49,999",sample(code,1,prob=between25to49/sum(between25to49)),
                                 ifelse(sum(between25to49)<=0&syntheticdataset$household.income=="20,000 to 24,999"|syntheticdataset$household.income=="25,000 to 29,999"|syntheticdataset$household.income=="30,000 to 34,999"|syntheticdataset$household.income=="35,000 to 39,999"|syntheticdataset$household.income=="40,000 to 44,999"|syntheticdataset$household.income=="45,000 to 49,999","Health Insurance by Income Not Available for This Census Tract",
                                 ifelse(sum(between50to75)>0&syntheticdataset$household.income=="50,000 to 59,999"|syntheticdataset$household.income=="60,000 to 74,999",sample(code,1,prob=between50to75/sum(between50to75)),
                                        ifelse(sum(between50to75)<=0&syntheticdataset$household.income=="50,000 to 59,999"|syntheticdataset$household.income=="60,000 to 74,999","Health Insurance by Income not Available for This Census Tract",
                                        ifelse(sum(between75to100)>0&syntheticdataset$household.income=="75,000 to 99,999",sample(code,1,prob=between75to100/sum(between75to100)),
                                               ifelse(sum(between75to100)<=0&syntheticdataset$household.income=="75,000 to 99,999","Health Insurance by Income not Available for this Census Tract",
                                               ifelse(sum(over100)>0&syntheticdataset$household.income=="100,000 to 124,999"|syntheticdataset$household.income=="125,000 to 149,999"|syntheticdataset$household.income=="150,000 to 199,999"|syntheticdataset$household.income=="200,000 or more",sample(code,1,prob=over100/sum(over100)),
                                                      ifelse(sum(over100)<=0&syntheticdataset$household.income=="100,000 to 124,999"|syntheticdataset$household.income=="125,000 to 149,999"|syntheticdataset$household.income=="150,000 to 199,999"|syntheticdataset$household.income=="200,000 or more","Health Insurance Status By Income Not Available for this Census Tract",
                                                      NA))))))))))
  syntheticdataset$health.insurance=health.insurance
  return(syntheticdataset)
}
