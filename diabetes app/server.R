# Define server logic required to draw a histogram ----

prediabetes_simulation_data=readRDS("prediabetes_simulation_for_app.RDS")
library(sf)
library(dplyr)

#we will assume a program cost of 417 but will allow the user to change that

cost_per_participant=417

#Medical Costs
#refer to medical costs from impact methodology
#will use simpler model for now

cost_for_year_of_diagnosis=6424
cost_every_year_after=3900


validparcels=readRDS("validparcels.RDS")

get_houses_close_enough_to_class=function(address,validparcels,miles_from_location){
  validparcels$LocAddr=as.character(validparcels$LocAddr)
  address=filter(validparcels,validparcels$LocAddr==as.character(address))
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
  QALYgained=0.05*(without_intervention_cases_costs[1,]-with_intervention_cases_costs[1,])
  for(year in 2:10){
    QALYgained[year]=QALYgained[year]+QALYgained[year-1]
  }
  
  ICERS=net_costs/QALYgained
  
  return(list(cases_costs=cases_costs,net_costs=net_costs,QALYgained=QALYgained,ICERS=ICERS))
}
server <- function(input, output) {

    part_of_set_of_interest<-reactive({get_houses_close_enough_to_class(toupper(input$text),validparcels,input$miles_from_location)})
    
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
      
    })
    
    output$QALY_gained<-renderPlot(({
      barplot(as.vector(unlist(projected_data_for_10_years()$QALYgained)),xlab="Year",names.arg = paste(1:10),ylab="Quality of Life Years Gained")
    }))
    
    output$ICERS<-renderPlot(({
      barplot(as.vector(unlist(projected_data_for_10_years()$ICERS)),xlab="Year",names.arg = paste(1:10),ylab="Incremental Cost Effective Ratios")
    }))
    #get_houses_close_enough_to_several_classes<- reactive({
     # inFile <- input$file1sssss
      
      #if(is.null(input$file1))
       # return(NULL)
      
      #addresses_data=read.csv(inFile$datapath)
      
      #colnames(addresses_data)="LocAddr"
      
      #part_of_part_of_set_of_interest=get_houses_close_enough_to_class(addresses_data[1,],validparcels,input$miles_from_location)
      #for(address_number in 2:nrow(addresses_data)){
       # another_part_of_part_of_set_of_interest=get_houses_close_enough_to_class(addresses_data[address_number,],validparcels,input$miles_from_location)
        #part_of_part_of_set_of_interest=rbind(part_of_part_of_set_of_interest,another_part_of_part_of_set_of_interest)
      #}
      
      #return(part_of_part_of_set_of_interest)
    #})
    
    #output$report=downloadHandler(
     # 
      #filename = "report.html",
      
      #content=function(file){
       # tempReport <- file.path(tempdir(), "DPP_report_generator.Rmd")
        #file.copy("DPP_report_generator.Rmd", tempReport, overwrite = TRUE)
        
       
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        #rmarkdown::render(tempReport, output_file = file)
      #}
      
      
    #)
    
}