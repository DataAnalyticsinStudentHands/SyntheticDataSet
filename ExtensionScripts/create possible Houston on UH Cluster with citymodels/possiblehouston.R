library(citymodels)
Census_data=readRDS("Census_data.RDS")

tracts=subset(Census_data,Census_data$county==201)
tracts=tracts$tract

#Set Up to run in Parallel
library(doParallel)
library(foreach)
cl<-makeCluster(10)
registerDoParallel(cl)

Census_data=readRDS("Census_data.RDS")

#Simulate Households
sample.set=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
  library(citymodels)
  Census_data=readRDS("Census_data.RDS")	
  sample=household_generator(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)
  sample2=group_quarters_simulater(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)
  fullsample=rbind(sample,sample2)
  return(fullsample)
}


saveRDS(sample.set,"citymodels.RDS")
stopCluster(cl)


