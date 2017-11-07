# Define server logic required to draw a histogram ----

prediabetes_simulation_data=readRDS("prediabetes_simulation_for_app.RDS")
library(sf)
library(dplyr)
miles_from_location=1
#we will assume a program cost of 417 but will allow the user to change that

cost_per_participant=417

#Medical Costs
#refer to medical costs from impact methodology
#will use simpler model for now

cost_for_year_of_diagnosis=6424
cost_every_year_after=3900


validparcels=readRDS("validparcels.RDS")

get_houses_close_enough_to_class=function(address,validparcels){
  address=filter(validparcels,validparcels$LocAddr==address)
  #If not a vadil address return not a valid address
  if(nrow(address)==0){
    return(paste("Not a valid address"))
  }
  else
    #Calculate distance
    validparcels$close_enough_to_participate=st_distance(validparcels,address)
  #Convert to miles
  validparcels$close_enough_to_participate=validparcels$close_enough_to_participate/5280
  
  validparcels$close_enough_to_participate=as.numeric(as.character(validparcels$close_enough_to_participate))
  #If it's less than one it's good
  validparcels$close_enough_to_participate[validparcels$close_enough_to_participate<=miles_from_location]="yes"
  validparcels=subset(validparcels,validparcels$close_enough_to_participate=="yes")
  
  for_merge_with_synthetic_data_set=data.frame(ACCOUNT=validparcels$HCAD_NUM,close_enough_to_participate=validparcels$close_enough_to_participate)
  prediabetes_simulation_data=merge(prediabetes_simulation_data,for_merge_with_synthetic_data_set,by="ACCOUNT",all.x=TRUE)
  
  #We're only interested in populations close enough to the class
  part_of_set_of_interest=subset(prediabetes_simulation_data,prediabetes_simulation_data$close_enough_to_participate=="yes")
  return(part_of_set_of_interest)
}

get_number_of_cases_and_costs=function(develop_diabetes,cost_for_year_of_diagnosis=6424,cost_every_year_after=3900){
  cases_that_year=sum(develop_diabetes=="yes"|develop_diabetes=="already developed")
  costs_that_year=cost_for_year_of_diagnosis*sum(develop_diabetes=="yes")+cost_every_year_after*sum(develop_diabetes=="already developed")
  return(c(cases_that_year,costs_that_year))
}

get_number_of_cases_and_costs_for_10_years=function(part_of_set_of_interest,cost_for_year_of_diagnosis=6424,cost_every_year_after=3900,cost_per_participant=417){
  #without intervention
  #first find correct column numbers
  correct_column_number=grep("without_intervention_develop_diabetes_year10", colnames(part_of_set_of_interest))
  without_intervention_cases_costs=sapply(part_of_set_of_interest[(correct_column_number-9):correct_column_number],get_number_of_cases_and_costs,cost_for_year_of_diagnosis,cost_every_year_after)
  row.names(without_intervention_cases_costs)<-c("cumulative_cases_of_diabetes_without_intervention","cumlative_costs_without_intervention")
  without_intervention_cases_costs=as.data.frame(without_intervention_cases_costs)
  colnames(without_intervention_cases_costs)=paste0("year",1:10)
  #And with intervention
  correct_column_number=grep("with_intervention_develop_diabetes_year10", colnames(part_of_set_of_interest))
  with_intervention_cases_costs=sapply(part_of_set_of_interest[(correct_column_number-9):correct_column_number],get_number_of_cases_and_costs,cost_for_year_of_diagnosis,cost_every_year_after)
  row.names(with_intervention_cases_costs)<-c("cumulative_cases_of_diabetes_with_intervention","cumlative_costs_with_intervention")
  with_intervention_cases_costs=as.data.frame(with_intervention_cases_costs)
  colnames(with_intervention_cases_costs)=paste0("year",1:10)
  with_intervention_cases_costs["cumulative_costs_with_intervention","year1"]<-with_intervention_cases_costs["cumulative_costs_with_intervention","year1"]+sum(part_of_set_of_interest$would_participate=="yes")*cost_per_participant
  
  cases_costs=rbind(without_intervention_cases_costs,with_intervention_cases_costs)
  net_costs=with_intervention_cases_costs[2,]-without_intervention_cases_costs[2,]
  
  return(list(cases_costs=cases_costs,net_costs=net_costs))
}
server <- function(input, output) {

    part_of_set_of_interest<-reactive({get_houses_close_enough_to_class(toupper(input$text),validparcels)})
    
    projected_data_for_10_years<-reactive({get_number_of_cases_and_costs_for_10_years(part_of_set_of_interest())})
    

    output$projected_particpants <- renderText({
      projected_particpants=sum(part_of_set_of_interest()$would_participate=="yes")
      return(paste0("projected participants ",projected_particpants))
    })
    
    output$cumulative_cases<-renderPlot({
      year=1:10
      
      plot(year,projected_data_for_10_years()$cases_costs["cumulative_cases_of_diabetes_without_intervention",],type="o",col="red",ann=FALSE)
      lines(year,projected_data_for_10_years()$cases_costs["cumulative_cases_of_diabetes_with_intervention",],type="o",col="blue")
      title(ylab="Cumaltive Cases of Diabetes")
      title(xlab="Year")
      legend("bottomright",c("With Intervention","Without Intervention"),cex=0.8,
             col=c("blue","red"),pch=21:22,lty = 1:2)
    })
    
    output$cumulative_costs<-renderPlot({
      year=1:10
      
      plot(year,projected_data_for_10_years()$cases_costs["cumlative_costs_without_intervention",],type="o",col="red",ann=FALSE)
      lines(year,projected_data_for_10_years()$cases_costs["cumlative_costs_with_intervention",],type="o",col="blue")
      title(ylab="Cumaltive Costs of Diabetes")
      title(xlab="Year")
      legend("bottomright",c("With Intervention","Without Intervention"),cex=0.8,
             col=c("blue","red"),pch=21:22,lty = 1:2)
    })
    
    output$net_costs<-renderPlot({
      year=1:10
      
      barplot(as.vector(unlist(projected_data_for_10_years()$net_costs)),xlab="Year",names.arg = paste(1:10),ylab = "Costs")
      
      title(ylab="Net Costs")
      title(xlab="Year")
      
    })
    
    getData<- reactive({
      inFile <- input$file1
      
      if(is.null(input$file1))
        return(NULL)
      
      addresses_data=read.csv(inFile$datapath)
      
      colnames(addresses_data)="LocAddr"
      
      get_eligibility<-function(LocAddr){
        LocAddr=toupper(LocAddr)
        eligibility=ifelse(LocAddr %in% eligible$LocAddr,"house is eligible",
                           ifelse(LocAddr %in% not_in_houston$LocAddr,"house is not within Houston bounds",
                                  ifelse(LocAddr %in% too_young$LocAddr,"house is too young to be eligible",
                                         ifelse(LocAddr %in% floodplain_addresses$LocAddr,"house is in a floodplain",
                                                ifelse(LocAddr=="Enter Address in All Capital Letters","Please Enter Address",
                                                       ifelse(length(agrep(LocAddr,allhouses$LocAddr))>0,
                                                              paste("did you mean",allhouses$LocAddr[agrep(LocAddr,allhouses$LocAddr)],"?"),"address is not in system"))))))
        return(eligibility)
      }
      addresses_data$eligibility=sapply(addresses_data$LocAddr,get_eligibility)
      return(addresses_data)
    })
    
    output$for_csv=renderTable({

      
      getData()
      
    })
    
    output$downloadData=downloadHandler(
      
      filename = function(){
        paste0("data-",Sys.Date(),".csv")
      },
      
      content = function(file){
        
        write.csv(getData(),file)
        
      })
      
}