library(shiny)
library(leaflet)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Air Pollution and Diabetes:Modeling a Possible Intervention Through Stove Top Replacement"),
  h4("By: Tiffany Lim, Dr. Dan Price Ph.D, University of Houston Honors College"),
  h5("Presented at Undergraduate Research Day, University of Houston Fall 2017"),

             fluidRow(
               column(3,
                      h4("Background: New Findings"),
                      p("Researchers typically target diet and exercise for type 2 diabetes prevention"),
                      p(" New discoveries have found that air pollution is a risk factor as well"),
                      p("-1 A study showed that an average level of 18.8 ug/m^3 of NO2 caused significant percent increases in: 
                          HOMA-IR, which measures insulin resistance, (73.1%), 
                        insulin (67.2%) 
                        hs-CRP, measuring blood inflammation, (33.9%)") ,
                      p("Other studies report increased hs-CRP and metabolic syndrome (factor also related to T2D), plus interrupted insulin signaling pathways 2,3,4"),
                      p("Objective: Model effectiveness of possible approach for diabetes prevention by reducing exposure to air pollutants by replacing gas stoves with electric ones."),
                      br(),
                      h4("Biomedical Pathway"),
                      p("Chart mapping the association between NO2 air pollution and type 2 diabetes through the development of insulin 
                        resistance"),
                      img(src="BiomedicalPathway.png", height = 600, width = 400)
                        
                      ),
               column(6,
                      h4("Gas Versus Electric Stoves"),
                      p("Chose stove replacement as method of intervention due to completed experiments showing lower [NO2] in homes with electric stoves.
"),
                      p("-5 Gas stoves emit [NO2] higher than the average 18.8 ug/m^3 (as high as 31.9 ug/m^3) from the pollution study, making it likely for increases in the noted biomarkers leading to type 2 diabetes"),
                      p("-5 Electric stoves produces NO2 levels much lower,typically only around 10.6 ug/m^3"),
                      p("Another study showed a 51% decrease in [NO2] after switching from a gas stove to an electric6"),
                      h4("Hypothesis"),
                      p("Hypothesis: If lower income families (those in the lower two quartiles) have gas stoves, then they will have a higher incidence of diabetes.
"),
                      p("We decided on this because this group spends a little more time per day than those of the higher two income quartiles (.48 hr/day versus .40 and .44 hr/day), thus getting higher exposures to NO27
"),
                      h4("Methodology"),
                      p("Using Rstudio and data bases such as 500 Cities Project, Houston State of Health, SimplyAnalytics, American Time Use Survey, and the United States Census, we were able to construct maps to illustrate a plausible Houston with the variables significant to our hypothesis, such as income and gas stoves. Maps were compared to determine a correlation.
"),
                      h4("Maps"),
                      p("Map displaying the percent of gas stoves in each census tract
"),
                      leafletOutput("Percent_Gas_Stoves_Map"),
                      p("Map showing the census tracts that meet the higher NO2 exposure levels (gas stove and lower two income quartiles)
"),
                      leafletOutput("Number_Adults_Meeting_Threshold"),
                      p("Number of people with diabetes."),
                      leafletOutput("People_with_diabetes")
                      
                      
                      ),
               column(3,
                      h4("Results"),
                      p("After making the maps, we did not find the correlations we were initially hoping for"),
                      p("There was little matching in the census tracts between those who had more exposure due to lower income and 
                        owning gas stoves and those who had diabetes; only a few census tracts matched"),
                      h4("Conclusions"),
                      p("Though the results were not what we expected, it showed that stove replacement may not be the best intervention to invest money into and get significant results.
"),
                      p("Reasons for the unexpected results could be that the data is not visible on the census tract level, it's not significant for Houston, or air pollution is not as significant of a factor to diabetes as other factors. 
"),
                      p("After finding that this intervention is not as helpful as we hoped, maybe we can approach air pollution from a different angle, such as targeting sources that produce very large amounts of pollution.
"),
                      p("We can also focus on more primary causes of diabetes such as diet and exercise to produce more effective interventions.  
"),
                      h4("References and Thanks"),
                      p("1. Wolf, K., Popp, A., Schneider, A., Breitner, S., Hampel, R., Rathmann, W., . . . Peters, A. (2016). Association Between Long-term Exposure to Air Pollution and Biomarkers Related to Insulin Resistance, Subclinical Inflammation, and Adipokines. Diabetes, 65(11), 3314-3326. doi:10.2337/db15-1567"),
                      p("2. Ruckerl, R., Hampel, R., Breitner, S., Cyrys, J., Kraus, U., Carter, J., . . . Schneider, A. (2014). Associations between ambient air pollution and blood markers of inflammation and coagulation/fibrinolysis in susceptible populations. Environment International, 70, 32-49. doi:10.1016/j.envint.2014.05.013"),
                      p("3. Eze, I. C., Schaffner, E., Foraster, M., Imboden, M., Eckardstein, A. V., Gerbase, M. W., . . . Probst-Hensch, N. (2015). Long-Term Exposure to Ambient Air Pollution and Metabolic Syndrome in Adults. Plos One, 10(6). doi:10.1371/journal.pone.0130337"),
                      p("4. Yan, W., Ku, T., Yue, H., Li, G., & Sang, N. (2016). NO2 inhalation causes tauopathy by disturbing the insulin signaling pathway. Chemosphere, 165, 248-256.doi:10.1016/j.chemosphere.2016.09.063"),
                      p("5. Dedele, A., & Miskinyte, A. (2016). Seasonal variation of indoor and outdoor air quality of nitrogen dioxide in homes with gas and electric stoves. Environmental Science and Pollution Research, 23(17), 17784-17792. doi:10.1007/s11356-016-6978-5"),
                      p("6. Paulin, L. M., Diette, G. B., Scott, M., Mccormack, M. C., Matsui, E. C., Curtin-Brosnan, J., . . . Hansel, N. N. (2014). Home interventions are effective at decreasing indoor nitrogen dioxide concentrations. Indoor Air, 24(4), 416-424. doi:10.1111/ina.12085"),
                      p("7. About ATUS Data. (n.d.). Retrieved September 06, 2017, from https://www.bls.gov/tus/datafiles_2015.htm"),
                      
                      p("Thank you to Carol Upchurch for guiding me through Rstudio and data analysis.")
                      

             )
             )

)
