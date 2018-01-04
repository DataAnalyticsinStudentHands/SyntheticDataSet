library(shiny)
library(leaflet)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Modeling Air Pollution's Effect on Residents Living Near Major Roads"),
  h4("BAuthors: Nicholas Nguyen, Dr. Dan Price Ph.D, University of Houston Honors College"),
  h5("Presented at Undergraduate Research Day, University of Houston Fall 2017"),

             fluidRow(
               column(4,
                      h4("Background"),
                      p("Mobile and fuel combustion sources are the predominant contributors to national air pollution, which includes carbon monoxide (CO), nitrogen oxides (NOx), and particulate matter (PM) Ref 1"),
                      p("It is estimated that 200,000 early deaths occur in the U.S. each year due to U.S. combustion emissions Ref 2"),
                      p("Volatile organic compounds (VOCs) and PM 2.5 or smaller can remain airborne for long periods and can enter the alveoli, diffusing into the blood and result in inflammatory responses.
") ,
                      p("Approximately 11.3 million persons (or 3.7% of the 308.7 million U.S. population taken in 2010) live within 150 meters of a major highway and 5.2% of the population in Harris County reside within 150 meters of a major road Ref 3"),
                      p("A scientific statement in 2010 concluded that short term exposure to a 10-ug/m^3 increase in PM 2.5 concentration increases the relative risk for daily cardiovascular mortality by approximately 0.4% to 1.0%. Long term exposure of the same degree increases the relative risk to 1.06% and 1.76%. This elevated risk poses a treat to more susceptible populations Ref 7"),
                      br(),
                      h4("Approach"),
                      p("Data from CDC's 500 Cities Project was coded into RStudio to visualize Harris County census tracts with prevalence of stroke and cardiovascular disease cases near major roadways   (Figure 3).
"),
                      p("To conclude the relative risk of short term and long term exposures, linear regressions taken from other studies on the decrease of PM concentrations as distance increases from roads were extrapolated.
"),
                      p("Census tracts rather than households were evaluated due to the difficulty in coding a 500 meter buffer zone from major roads to individual households, and assuming uniform distribution of households within tracts would be fallible. 
"),
                      p("Differences in data values for PM in different studies may result in a higher margin of error when converted.
"),
                      p("Other sets of data taken from Houston State of Health were applied to the census tracts to ensure that roadside pollution indeed had an effect on cardiovascular health.
"),
                      p("Inspiration for a prevention plan came from the Via Verde pillars in Mexico City and The Great Wall of Mulch in Long Beach, California. 
"),
                      img(src="fig1.png", height = 250, width = 500),
                      p("Figure 1. Concentration of pollutants versus distance from edge of road Ref 6")
                        
                      ),
               column(4,
                      img(src="fig2.png", height = 250, width = 500),
                      p("Figure 2. Potential mechanism for pathophysiological pathways of Particulate Matter (2.5, 10) Ref 4"),
                      leafletOutput("NicksMap"),
                      p("Figure 3. Prevalence of disease overlaid with major roads (primary & secondary) in red. a) shows stroke cases, b) shows coronary heart disease. Lower elevation of residential areas in comparison with nearby roads and higher and longer exposure to air pollution prompts the question if traffic is linked to increase heart disease. Data taken from CDC's 500 Cities Project, maps compiled in RStudio"),
                      img(src="fig4.png", height = 250, width = 500),
                      p("Figure 4. Reduction of NO2 and PM10 with increased vegetation cover on green walls Ref 5. 
")
                      ),
               column(4,
                      h4("Results"),
                      p("NOx and CO concentrations decreased significantly by 90% and 52%, respectively, within less than 200 meters. Benzene, NO2, and PM 2.5 concentrations decreased minimally by 45%, 42%, and 22% at around 300, 500, and 900 meters away from road edge, respectively. PM 10 concentrations showed no trend (Figure 1) Ref 6"),
                      p("Neighborhoods such as Pleasantville (circled in Figure 3) located near major roadways have higher cases of asthma, COPD, stroke and cardiovascular disease. Despite the construction of an 800 meter roadside block alongside Pleasantville's border and the E Loop Fwy, its current height and lack of vegetation may not be effective in decreasing PM and other pollutant concentrations.
"),
                      p("Street canyons with vegetation taken from an ACS study projects a potential 40% reduction in NO??? and 60% PM concentrations via dry deposition on leaf surfaces.
"),
                      p("The residence times of pollutants increases as the aspect ratio ((height of wall block)/(width of street)) increases and wind speeds decrease Ref 5.
",align="center"),
                      p("The study attributes these results to green walls and street trees as wel",align="center"),
                      p("Adopting the construction of the green wall along major roadways can possibly reduce relative risk of cardiovascular mortality within 24 hours (Figure 4). This could benefit the 3,425 residents Ref 9 living in Pleasantville and similar structures could help approximately 225,000 residents living in Harris County.
"),
                      p("Near road air pollution-related coronary heart disease mortality in 2008 ranged from approximately $3.8 billion to $11.5 billion Ref 11 and that number is projected to increase by 2035. 
"),
                      p("Trees that are arbitrarily placed in bad locations can trap NO2 emissions and increasing ground levels of the pollutant by 21%. Lack of wind fails to disperse the pollutants and the tree begins to act as a sponge, accumulating high levels of pollutants and poses a threat to pedestrians and bikers Ref 8"),
                      p("Increased hair and wax density on leaves of certain plant species were shown to have increased PM accumulation via deposition. 
"),
                      p("Roadside blocks can act as noise muffling barriers and temperature moderators as two separate studies have shown green walls to reduce noise Ref 10 by 15 db and cool ambient temperatures by 10-20 degrees Celsius"),
                      h4("Conclusions"),
                      p("Looking at large data in different ways allows for intervention methods to be more accurate and effective in targeting systemic problems
"),
                      p("Neighborhoods near roadways like Pleasantville have higher rates of diseases relating to the pulmonary system.
"),
                      p("Though confounding variables may drive the prevalence of cardiovascular disease, countless studies have concluded the detrimental effects of roadside pollutants."),
                      p("As a result, city planning may be able to use the following findings to aid in reducing diseases relating to air pollution.
"),
                      p("Construction of green roadside blocks could benefit approximately 225,000 residents in the short term
",align="center"),
                      p("Long-term policies that can decrease traffic through superblocking, toll roads, public transportation, parking, education
",align="center"),
                      p("Air pollution is an nondiscriminatory hazard that puts the elderly, children, and pregnant women more at risk for cardiovascular mortality
"),
                      p("Calling for the lowering of EPA's PM standard levels and other pollutants can help regulate pollution from other sources.
"),
                      h4("Acknowledgements"),
                      p("I am incredibly grateful to Houston Scholars for providing the connections and funded opportunity to make this research project possible, Carol Upchurch for assisting in coding maps and visualizing data, and the following studies and platforms for references:
"),
                      p("1. EPA Air Emission Sources
"),
                      p("2. Caiazzo, Fabio et al., 2013
"),
                      p("3. Boehmer, Tegan et al., 2010
"),
                      p("4. Du, Yixing et al., 2016
"),
                      p("5. Pugh, Thomas et al., 2012
"),
                      p("6. Karner, Alex et al., 2010
"),
                      p("7. AHA Scientific Statement, Circulation: June 1, 2010, Volume 121, Issue 21
"),
                      p("8. Yli-Pelkonen, Vesa et al., 2017
"),
                      p("9. Weichert, Pleasantville Houston
"),
                      p("10. Z. Azkorra et al., 2014
"),
                      p("11. Brandt, Sylvia et al., 2017
")
                      

             )
             )

)
