# Define server logic required to draw a histogram ----

library(leaflet)

server <- function(input, output) {
  
  output$Elementary_School_Severity<-renderLeaflet({
    Elementary_School_Severity=readRDS("Elementary_School_Severity_Map.rds")
  })
  
  output$Elementary_School_Hazards<-renderLeaflet({
    Elementary_School_Severity=readRDS("Elementary_School_Hazards_Map.rds")
  })
  
  output$High_School_Severity<-renderLeaflet({
    High_School_Severity=readRDS("High_School_Severity_Map.rds")
  })
  
  output$High_School_Hazards<-renderLeaflet({
    High_School_Hazards=readRDS("High_School_Hazards_Map.rds")
  })
  
  output$Middle_School_Severity<-renderLeaflet({
    Middle_School_Severity=readRDS("Middle_School_Severity_Map.rds")
  })
  
  output$Middle_School_Hazards<-renderLeaflet({
    Middle_School_Severity=readRDS("Middle_School_Hazards_Map.rds")
  })

   
}