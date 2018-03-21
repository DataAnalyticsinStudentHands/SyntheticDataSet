library(citymodels)
Census_data=readRDS("Census_data.RDS")

tracts=subset(Census_data,Census_data$county==201)
tracts=tracts$tract

#Set Up to run in Parallel
library(doParallel)
library(foreach)
cl<-makeCluster(1)
registerDoParallel(cl)

Census_data=readRDS("Census_data.RDS")

#Simulate Households
sample.set=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
  library(citymodels)
  sample=household_generator(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)
  return(sample)
}

stopCluster(cl)


cl<-makeCluster(10)
registerDoParallel(cl)

#Simulate Group Quarters
source("group_quarters_generator.R")
sample.set2=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
  library(citymodels)
  sample=group_quarters_simulater(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data_List)
  return(sample)
}

stopCluster(cl)

sample.set=rbind(sample.set,sample.set2)
