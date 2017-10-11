# Define server logic required to draw a histogram ----
server <- function(input, output) {
  too_young=read.csv("houses_too_young.csv")
  not_in_houston=read.csv("not_in_houston.csv")
  floodplain_addresses=read.csv("floodplain_addresses.csv")
  eligible=read.csv("eligible_addresses.csv")
  allhouses=rbind(too_young,not_in_houston,floodplain_addresses,eligible)
  

    output$eligibility_status <- renderText({ 
      ifelse(input$text %in% eligible$LocAddr,"house is eligible",
             ifelse(input$text %in% not_in_houston$LocAddr,"house is not within Houston bounds",
                    ifelse(input$text %in% too_young$LocAddr,"house is too young to be eligible",
                           ifelse(input$text %in% floodplain_addresses$LocAddr,"house is in a floodplain",
                                  ifelse(input$text=="Enter Address in All Capital Letters","Please Enter Address",
                                  ifelse(length(agrep(input$text,allhouses$LocAddr))>0,
                                         paste("did you mean",allhouses$LocAddr[agrep(input$text,allhouses$LocAddr)],"?"),"address is not in system"))))))
    })
  
  
}