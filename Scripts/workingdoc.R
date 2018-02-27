
#Get number of vehicles available

getnumberofvehicles <- function(county, tract,syntheticdataset,seed,Census_data){
  #Generates number of vehicles per household
  
  #Set seed so sampling is random but repeatable
  set.seed(seed)
  
  #Read in data for sampling distribution
  Census_data=Census_data[(Census_data$tract==tract)&(Census_data$county==county)]
  
    
      #Sample for one person household
      if (nrow(syntheticdataset)==1){
        #Use appropriate Census Subheadings
        houseofone=Census_data[c("households.of.1.no.vehicle","households.of.1.1.vehicle","households.of.1.2.vehicles","households.of.1.3.vehicles","households.of.1.4.vehicles")]
        #Create probability Distribution
        total=sum(houseofone[1,1:5])
        prob1=(houseofone[1,1:5])/total
        colnames(prob1)=c(0,1,2,3,4)
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
    #Sample for number of cars
    a=sample(colnames(prob2),size=1,prob=prob2)
    #add number of cars to each person in household in data frame
    number.of.vehicles <- rep(a,nrow(syntheticdataset))
    syntheticdataset$number.of.vehicles=number.of.vehicles
  }
  
      

      
      
    #}
  return(syntheticdataset=syntheticdataset)
}



