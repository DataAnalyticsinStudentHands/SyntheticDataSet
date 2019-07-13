# The following package must be installed and loaded for this function to work
library(broman)

# Function to simulate life expectancy based on age
  # Inputs: tract number, Sam City (must at least have everyone's age already simulated), life expectancy data
  # Output: Sam City with life.expectancy column
  
# The data is downloaded from https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html
life_expectancy = read.csv("TX_B.csv")

getLifeExpectancy <- function(tract, syntheticdataset, life_expectancy_data){
  # Subset life expectancy data for the relevant tract
  life_expectancy_data = life_expectancy_data[life_expectancy_data$TRACT2KX == tract,]
  
  # The remaining number of years a person is expected to live is assigned based on their age
  syntheticdataset$life.expectancy = switchv(syntheticdataset$bracket.age,
                                            "0.to.4" = ifelse(syntheticdataset$age == 0, life_expectancy_data[1,]$e.x., life_expectancy_data[2,]$e.x.),
                                            "5.to.9"=, "10.to.14" = life_expectancy_data[3,]$e.x.,
                                            "15.to.17"=, "18.to.19"=, "20.to.24" = life_expectancy_data[4,]$e.x.,
                                            "25.to.29"=, "30.to.34" = life_expectancy_data[5,]$e.x.,
                                            "35.to.44" = life_expectancy_data[6,]$e.x.,
                                            "45.to.54" = life_expectancy_data[7,]$e.x.,
                                            "55.to.64" = life_expectancy_data[8,]$e.x.,
                                            "65.to.74" = life_expectancy_data[9,]$e.x.,
                                            "75.to.84" = life_expectancy_data[10,]$e.x.,
                                            "85.to.100" = life_expectancy_data[11,]$e.x.)
  
  # Make the life.expectancy column numeric and then add the person's current age to get thheir full life expectancy
  syntheticdataset$life.expectancy = as.numeric(syntheticdataset$life.expectancy)
  syntheticdataset$life.expectancy = syntheticdataset$life.expectancy + syntheticdataset$age
  
  return(syntheticdataset)
}
