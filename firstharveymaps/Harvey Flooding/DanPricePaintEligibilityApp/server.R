# Define server logic required to draw a histogram ----
server <- function(input, output) {
  too_young=read.csv("houses_too_young.csv")
  not_in_houston=read.csv("not_in_houston.csv")
  floodplain_addresses=read.csv("floodplain_addresses.csv")
  eligible=read.csv("eligible_addresses.csv")
  allhouses=rbind(too_young,not_in_houston,floodplain_addresses,eligible)
  

    output$eligibility_status <- renderText({
      
      ifelse(toupper(input$text) %in% eligible$LocAddr,"house is eligible",
             ifelse(toupper(input$text) %in% not_in_houston$LocAddr,"house is not within Houston bounds",
                    ifelse(toupper(input$text) %in% too_young$LocAddr,"house is too young to be eligible",
                           ifelse(toupper(input$text) %in% floodplain_addresses$LocAddr,"house is in a floodplain",
                                  ifelse(toupper(input$text)=="Enter Address in All Capital Letters","Please Enter Address",
                                  ifelse(length(agrep(toupper(input$text),allhouses$LocAddr))>0,
                                         paste("did you mean",allhouses$LocAddr[agrep(toupper(input$text),allhouses$LocAddr)],"?"),"address is not in system"))))))
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