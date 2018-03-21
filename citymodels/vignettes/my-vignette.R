## ------------------------------------------------------------------------
library(citymodels)

## ------------------------------------------------------------------------
households_in_421300=household_generator(48,201,421300,seed=1,inputdir = "../Inputs/",census_data)
View(households_in_421300)

## ------------------------------------------------------------------------
group_quarters_in_421300=group_quarters_simulater(48,201,421300,seed=1,inputdir = "../Inputs/",census_data)
View(group_quarters_in_421300)

## ------------------------------------------------------------------------
#library(citymodels)
#Census_data=readRDS("Census_data.RDS")

#tracts=subset(Census_data,Census_data$county==201)
#tracts=tracts$tract

#Set Up to run in Parallel
#library(doParallel)
#library(foreach)
#no_cores <- detectCores() - 1
#cl<-makeCluster(no_cores)
#registerDoParallel(cl)

#Simulate Households
#sample.set=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
 # library(citymodels)
  #sample=household_generator(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)
  #return(sample)
#}

#stopCluster(cl)


#cl<-makeCluster(no_cores)
#registerDoParallel(cl)

#Simulate Group Quarters
#sample.set2=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
 # library(citymodels)
#  sample=group_quarters_simulater(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)
 # return(sample)
#}

#stopCluster(cl)

#sample.set=rbind(sample.set,sample.set2)



