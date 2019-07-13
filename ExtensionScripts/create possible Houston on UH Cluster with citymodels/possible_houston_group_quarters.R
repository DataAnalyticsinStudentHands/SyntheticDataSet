library(doParallel)
library(foreach)
cl<-makeCluster(10)
registerDoParallel(cl)

Census_data=readRDS("Census_data.RDS")
Census_data=Census_data[(Census_data$county==201),]
tracts=Census_data$tract

#Simulate Households
sample.set=foreach (index=1:length(tracts),.combine='rbind')%dopar%{
  library(citymodels)
  Census_data=readRDS("Census_data.RDS")	
  sample=group_quarters_simulater(48,201,tracts[index],seed=1,inputdir = "../Inputs/",Census_data)
  return(sample)
}


saveRDS(sample.set,"citymodels_houston_group_quarters.RDS")
stopCluster(cl)

#Why isn't my code working????
options(error = recover)

for(tract in tracts){
  sample=group_quarters_simulater(48,201,tract,seed=1,inputdir = "../Inputs/",Census_data)
}