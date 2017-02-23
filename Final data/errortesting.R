#Error testing
source('masterfunction.R')
library("plyr", lib.loc="~/R/win-library/3.3")

sampleset=read.csv('sample_set5.csv')
sample.set.error=read.csv('error_for_sample_set5.csv')

for (seed in 1:10){
  #source.of.tracts=read.csv("veteran_status.csv") #choice of file was arbitrary all files have counties and regions
  #a=subset(source.of.tracts,source.of.tracts$county==201)
  #tracts=unique(a$tract)
#  sample.set=master(201,tracts,100,seed)
  filename <- paste("Harris_sample_set",seed, ".csv", sep="") 
  sample.set=read.csv(filename)
  
  error.for.sample.set=run.me.for.errors(sample.set)
  filename2 <- paste("error_for_Harris.sample_set",seed,".csv",sep="")
  write.csv(error.for.sample.set,file=filename2)
  
}


#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
run.me.for.errors=function(sampleset){
  
  source.of.tracts=read.csv("veteran_status.csv") #choice of file was arbitrary all files have counties and regions
  
  sampleseterror=data.frame()
  for (county in 1:length(countycodesinhouston)){
    b=countycodesinhouston[county]
    a=subset(source.of.tracts,source.of.tracts$county==b)
    tracts=unique(a$tract)
    
    for (tract in tracts){
      subsample <- sampleset[(sampleset$tract==tract) & (sampleset$county==b),]
      tractsampleseterror=data.frame(tract)
      for (col in c(21:23,27:43)){
        variables=unique(subsample[,col])
        
        for (var in variables){
          percentage=data.frame(sum(subsample[,col]==var)/length(subsample[,col]))
          q=colnames(subsample)
          colnames(percentage)=paste(q[col],var,sep=".")
          tractsampleseterror=cbind(percentage,tractsampleseterror)
          
        }
      }
      
      sampleseterror=rbind.fill(sampleseterror,tractsampleseterror)
    }

  }
  return(sampleseterror)
}