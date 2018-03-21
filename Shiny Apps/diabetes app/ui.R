library(shiny)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Project Effects of a Diabetes Prevention Program around Houston"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Slider for the number of bins ----
      textInput("text", ("Enter Address"), 
                value  = "14060 Dublin St"),
      
      numericInput("miles_from_location","Miles from location you expect to reach",1),
      
      submitButton("Query Address")),
      
      #helpText("Or upload csv files of addresses"),
      
      #fileInput("file1", "Choose CSV File",
      #          accept = c(
      #            "text/csv",
      #            "text/comma-separated-values,text/plain",
      #            ".csv")
      #),
      #downloadButton("report", "Generate report")),

    
      
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      textOutput("projected_particpants"),
      plotOutput("cumulative_cases"),
      plotOutput("cumulative_costs"),
      plotOutput("net_costs"),
      plotOutput("QALY_gained"),
      plotOutput("ICERS"),
      h4("Methodology and Definitions"),
      p("A synthetic data set was created using Census tract information to simulate the number of households and people with characteristics such as race, gender, educational attainment, and age. Health behvaiors and conditions were then simulated by matching with the CDC's Behavioral Risk Factor Surveillance system by sex, race, education, and income. Prediabetes and diabetes were originally simulated in this way. These simulated people were then randomly put into houses within a census tract using data from the Harris County Appraisal District."),
      p("The projected effectiveness and participation in the Diabetes Prevention Program was modeled using information from the technical report of the CDC's Impact app located at https://www.cdc.gov/diabetes/prevention/pdf/Impact_Toolkit_TechnicalReport.pdf. Each simulated person with prediabetes was given a 35% likelihood of participating in an offered Diabetes Prevention Program similarly to the Impact App default. The likelihood of developing diabetes in a year was set at 3%, the incidence rate of diabetes for the prediabetic population. The risk reduction for developing diabetes for simulated persons who participated in the program was 35.4% for the first year, 19.3% for the second year, 15.3% for the third year and there was nor risk reduction for year four or later based on table 14 of the technical report for the CDC's Diabetes Impact App. Based on these numbers participation and whether or not a simulated prediabetic person would develop or have diabetes with or without the program in any given year for the next 10 years was simulated. The cost of diabetic care for that simulated person in a particular year was set at $6,424 for the year of diagnosis and $3,900 for every year afterwards similar to section 2.5.6 of the Impact App technical report. For each year with diabetes the assumed reduction in quality of life was 5%."),
      p("The application subsets the part of the synthetic data set living within the user specified distance from the user specified address. Projected particpants refers to the number of simulated persons in that area that would participate. Cumulative cases of diabetes refers to the number of diabetic simulated patients that year that were originally prediabetic, both with and without the offered program. Cumulative costs refers to the cumulated diabetes related medical costs for the prediabetic population, also with and without the offered program. Costs refer to net costs or the cost of the offered program minus the saved medical costs. Cumulative quality of life years gained refers to the number of quality of life years gained based off of not losing the 5% in quality of life from developing diabetes. ICERS, Incremental Cost Effective Ratios are the cumluated quality of life years gained divided by the net costs of the program.")
    )
  )
)
