library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
#library(rio) #may not need here - playing with it
#library(FactoMineR)
#library(doParallel)
#library(foreach)
#for distance calculation ? may do as own calculation instead
#library(stats)
#census does a process of modeling, based on weighting of responses. Hard to find exactly where they're doing the projections.
#cf. Appendix B, starting at esp. around 103: https://www2.census.gov/programs-surveys/decennial/2020/program-management/census-research/predictive-models-audience-segmentation-report.pdf

#' exp_census
#'
#' This function creates the expanded census data and runs tests on them
#'
#' @return a list of data.tables with expanded census.
exp_census_hh <- function() {
  #need to break into smaller pieces - not sure best approach for actually programming - need folder structure right
  exp_census_data_file <- paste0(censusdir, vintage,"/exp_census.RDS") 
  #Create or read in individual sam residents
  if(file.exists(exp_census)) {
    # import saved sam residents from RDS file
    exp_census <- readRDS(exp_census_data_file)
    print(sprintf("Done reading exp_sam RDS from %s", exp_census_data_file ))
  } else {
    
    #get the census key
    censuskey <- readLines(paste0(censusdir, vintage, "/key"))
    
    #American community survey 1yr variables: https://api.census.gov/data/2018/acs/acs1/variables.html
    #American community survey 5yr variables: https://api.census.gov/data/2018/acs/acs5/variables.html
    #definitions: https://www2.census.gov/programs-surveys/acs/tech_docs/subject_definitions/2018_ACSSubjectDefinitions.pdf
    
    #setup race codes https://www.census.gov/programs-surveys/acs/guidance/which-data-tool/table-ids-explained.html
    acs_race_codes <- c("A","B","C","D","E","F","G") #could collect all - add them up, without H and I, and you get the total! H is white alone, not hispanic and I is hispanic and if you add them up they don't equal white alone
    #acs_ethnicity <- c("H","I") #H is White Alone, not Hispanic or Latino; I is Hispanic or Latino #usually just use !acs_race_codes
    
        
    #concept: TYPES OF COMPUTERS IN HOUSEHOLD - assign multiples to underlying hh b/c computer_alone dups some of computer_type when multiple
    computers_hh_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28001")
    computers_hh_data <- computers_hh_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(computers_hh_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("computer","computer_alone","computer_type"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(computer_type=case_when(
        computer=="No Computer" ~ computer,
        computer_alone=="Desktop or laptop" & is.na(computer_type) ~ 'Multiple computing devices',
        TRUE ~ computer_type
      )) %>%
      filter(!is.na(computer_alone) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "computer_id",.remove = TRUE)
    computers_hh <- as.data.table(computers_hh_data)
    
    #concept: PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
    internet_hh_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28002")
    internet_hh_data <- internet_hh_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(internet_hh_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("internet","extra"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(is.na(extra) &!is.na(internet) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "internet_id",.remove = TRUE)
    internet_hh <- as.data.table(internet_hh_data)
    
    #concept: PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
    computer_internet_hh_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28003")
    computer_internet_hh_data <- computer_internet_hh_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(computer_internet_hh_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("computer","internet"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(internet=if_else(computer=="No computer",computer,internet))%>%
      filter(!is.na(internet) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "internet_id",.remove = TRUE)
    computer_internet_hh <- as.data.table(computer_internet_hh_data)
    
    #concept: HOUSEHOLD INCOME IN THE LAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS) BY PRESENCE AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
    income_internet_hh_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28004")
    income_internet_hh_data <- income_internet_hh_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(income_internet_hh_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("hh_income","internet"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(internet) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "internet_income_id",.remove = TRUE)
    income_internet_hh <- as.data.table(income_internet_hh_data)
    
    #concept: AGE BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD - looks like families, not households
    age_internet_hh_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28005")
    age_internet_hh_data <- age_internet_hh_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(age_internet_hh_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("age","computer","internet"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(internet=if_else(computer=="No computer",computer,internet))%>%
      filter(!is.na(internet) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "internet_age_id",.remove = TRUE)
    age_internet_hh <- as.data.table(age_internet_hh_data)
    
    ##EDUCATIONAL ATTAINMENT BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD - 2840381 ? check with others
    educ_internet_hh_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28006")
    educ_internet_hh_data <- educ_internet_hh_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(educ_internet_hh_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("hh_educ","computer","internet"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(internet=if_else(computer=="No computer",computer,internet))%>%
      filter(!is.na(internet) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "internet_educ_id",.remove = TRUE)
    educ_internet_hh <- as.data.table(educ_internet_hh_data)
    
    ##LABOR FORCE STATUS BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD B28007
    #job_internet_hh_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28007")
    #job_internet_hh_data <- job_internet_hh_census %>%
    #  mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
    #  filter(label != "Estimate!!Total") %>%
    #  pivot_longer(4:ncol(job_internet_hh_census),names_to = "tract", values_to = "number_sams") %>%
    #  separate(label, c("job","computer","internet"), sep = "!!", remove = F, convert = FALSE) %>%
    #  mutate(internet=if_else(computer=="No computer",computer,internet))%>%
    #  filter(!is.na(internet) & number_sams > 0) %>%
    #  uncount(as.numeric(number_sams),.id = "internet_job_id",.remove = TRUE)
    #job_internet_hh <- as.data.table(job_internet_hh_data)
    
    ##PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD acs_race_codes - seems to match in households 4484299
    internet_race_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B28009") 
    internet_race_data <- internet_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(internet_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("computer","internet"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(race = substr(name,7,7),
             internet=if_else(computer=="No computer",computer,internet)) %>%
      filter(race %in% acs_race_codes & is.na(internet)) %>%
      uncount(as.numeric(number_sams),.id = "internet_race_id",.remove = TRUE)
    internet_race_dt <- as.data.table(internet_race_data)
    
    internet_eth_data <- internet_race_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(internet_race_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("computer","internet"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(ethnicity = substr(name,7,7),
             internet=if_else(computer=="No computer",computer,internet)) %>%
      filter(is.na(internet)) %>%
      uncount(as.numeric(number_sams),.id = "internet_eth_id",.remove = TRUE)
    internet_eth_dt <- as.data.table(internet_eth_data)
    internet_eth_dt[ethnicity %in% acs_race_codes,("ethnicity"):="_"]
    internet_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity=="_"]),by=.(tract,computer,internet)]
    internet_eth_dt[order(match(ethnicity,c("H","I","_"))),c("cnt_ethn"):=list(1:.N),by=.(tract,computer,internet)]
    internet_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,computer,internet)]
    internet_eth_dt <- internet_eth_dt[(tokeep)]
    rm(internet_eth_dt)
    rm(internet_race_dt)
    
    #all NAs
    #concept: AGGREGATE GROSS RENT (DOLLARS) BY YEAR HOUSEHOLDER MOVED INTO UNIT
    #gross_rent_yr_moved_in_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25114")
    
    #equals "Renter occupied" on own_rent in Sam
    #gross rent is contract plus estimate for utilities, etc.
    #concept: GROSS RENT
    gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25063")
    gross_rent_data <- gross_rent_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(gross_rent_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("rent_cash","gross_rent"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(gross_rent=if_else(rent_cash=="No cash rent", "Less than $100", gross_rent)) %>%
      filter(!is.na(gross_rent) & number_sams > 0) %>%
      mutate(gross_rent_low=case_when(
        gross_rent == "Less than $100" ~ as.integer(1),
        gross_rent == "$100 to $149" ~ as.integer(100),
        gross_rent == "$150 to $199" ~ as.integer(150),
        gross_rent == "$200 to $249" ~ as.integer(200),
        gross_rent == "$250 to $299" ~ as.integer(250),
        gross_rent == "$300 to $349" ~ as.integer(300),
        gross_rent == "$350 to $399" ~ as.integer(350),
        gross_rent == "$400 to $449" ~ as.integer(400),
        gross_rent == "$450 to $499" ~ as.integer(450),
        gross_rent == "$500 to $549" ~ as.integer(500),
        gross_rent == "$550 to $599" ~ as.integer(550),
        gross_rent == "$600 to $649" ~ as.integer(600),
        gross_rent == "$650 to $699" ~ as.integer(650),
        gross_rent == "$700 to $749" ~ as.integer(700),
        gross_rent == "$750 to $799" ~ as.integer(750),
        gross_rent == "$800 to $899" ~ as.integer(800),
        gross_rent == "$900 to $999" ~ as.integer(900),
        gross_rent == "$1 000 to $1 249" ~ as.integer(1000),
        gross_rent == "$1 250 to $1 499" ~ as.integer(1250),
        gross_rent == "$1 500 to $1 999" ~ as.integer(1500),
        gross_rent == "$2 000 to $2 499" ~ as.integer(2000),
        gross_rent == "$2 500 to $2 999" ~ as.integer(2500),
        gross_rent == "$3 000 to $3 499" ~ as.integer(3000),
        gross_rent == "$3 500 or more" ~ as.integer(3500)),
        gross_rent_high=case_when(
          gross_rent == "Less than $100" ~ as.integer(99),
          gross_rent == "$100 to $149" ~ as.integer(149),
          gross_rent == "$150 to $199" ~ as.integer(199),
          gross_rent == "$200 to $249" ~ as.integer(249),
          gross_rent == "$250 to $299" ~ as.integer(299),
          gross_rent == "$300 to $349" ~ as.integer(349),
          gross_rent == "$350 to $399" ~ as.integer(399),
          gross_rent == "$400 to $449" ~ as.integer(449),
          gross_rent == "$450 to $499" ~ as.integer(499),
          gross_rent == "$500 to $549" ~ as.integer(549),
          gross_rent == "$550 to $599" ~ as.integer(599),
          gross_rent == "$600 to $649" ~ as.integer(649),
          gross_rent == "$650 to $699" ~ as.integer(699),
          gross_rent == "$700 to $749" ~ as.integer(749),
          gross_rent == "$750 to $799" ~ as.integer(799),
          gross_rent == "$800 to $899" ~ as.integer(899),
          gross_rent == "$900 to $999" ~ as.integer(999),
          gross_rent == "$1 000 to $1 249" ~ as.integer(1249),
          gross_rent == "$1 250 to $1 499" ~ as.integer(1499),
          gross_rent == "$1 500 to $1 999" ~ as.integer(1999),
          gross_rent == "$2 000 to $2 499" ~ as.integer(2499),
          gross_rent == "$2 500 to $2 999" ~ as.integer(2999),
          gross_rent == "$3 000 to $3 499" ~ as.integer(3449),
          gross_rent == "$3 500 or more" ~ as.integer(6000)), #have to think about skew on this one
        gross_rent_high2=case_when(
          gross_rent == "Less than $100" ~ as.integer(99),
          gross_rent == "$100 to $149" ~ as.integer(199),
          gross_rent == "$150 to $199" ~ as.integer(199),
          gross_rent == "$200 to $249" ~ as.integer(299),
          gross_rent == "$250 to $299" ~ as.integer(299),
          gross_rent == "$300 to $349" ~ as.integer(399),
          gross_rent == "$350 to $399" ~ as.integer(399),
          gross_rent == "$400 to $449" ~ as.integer(499),
          gross_rent == "$450 to $499" ~ as.integer(499),
          gross_rent == "$500 to $549" ~ as.integer(599),
          gross_rent == "$550 to $599" ~ as.integer(599),
          gross_rent == "$600 to $649" ~ as.integer(699),
          gross_rent == "$650 to $699" ~ as.integer(699),
          gross_rent == "$700 to $749" ~ as.integer(799),
          gross_rent == "$750 to $799" ~ as.integer(799),
          gross_rent == "$800 to $899" ~ as.integer(899),
          gross_rent == "$900 to $999" ~ as.integer(999),
          gross_rent == "$1 000 to $1 249" ~ as.integer(1249),
          gross_rent == "$1 250 to $1 499" ~ as.integer(1499),
          gross_rent == "$1 500 to $1 999" ~ as.integer(1999),
          gross_rent == "$2 000 to $2 499" ~ as.integer(6000),
          gross_rent == "$2 500 to $2 999" ~ as.integer(6000),
          gross_rent == "$3 000 to $3 499" ~ as.integer(6000),
          gross_rent == "$3 500 or more" ~ as.integer(6000))
      ) %>%
      uncount(as.numeric(number_sams),.id = "gross_rent_id",.remove = TRUE)
    gross_rent_hh <- as.data.table(gross_rent_data)
    
    #matches gross_rent for with cash rent only... - add it before gross_rent
    income_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25122")
    income_gross_rent_data <- income_gross_rent_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(income_gross_rent_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("hh_income_title","hh_income_renters","gross_rent_title","gross_rent"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(gross_rent) & number_sams > 0) %>%
      mutate(
        gross_rent_high2=case_when(
          gross_rent == "Less than $100" ~ as.integer(99),
          gross_rent == "$100 to $199" ~ as.integer(199),
          gross_rent == "$200 to $299" ~ as.integer(299),
          gross_rent == "$300 to $399" ~ as.integer(399),
          gross_rent == "$400 to $499" ~ as.integer(499),
          gross_rent == "$500 to $599" ~ as.integer(599),
          gross_rent == "$600 to $699" ~ as.integer(699),
          gross_rent == "$700 to $799" ~ as.integer(799),
          gross_rent == "$800 to $899" ~ as.integer(899),
          gross_rent == "$900 to $999" ~ as.integer(999),
          gross_rent == "$1 000 to $1 249" ~ as.integer(1249),
          gross_rent == "$1 250 to $1 499" ~ as.integer(1499),
          gross_rent == "$1 500 to $1 999" ~ as.integer(1999),
          gross_rent == "$2 000 or more" ~ as.integer(6000)) #have to think about skew on this one
      ) %>%
      uncount(as.numeric(number_sams),.id = "income_gross_rent_id",.remove = TRUE)
    income_gross_rent_hh <- as.data.table(income_gross_rent_data)
    
    bedrooms_gross_rent_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25068")
    bedrooms_gross_rent_data <- bedrooms_gross_rent_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(bedrooms_gross_rent_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("num_bedrooms","cash_rent","gross_rent_title","gross_rent"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(gross_rent_title) & number_sams > 0) %>%
      mutate(
        num_bedrooms = if_else(num_bedrooms=="No bedroom","0 bedrooms",num_bedrooms),
        gross_rent_high2=case_when(
          gross_rent == "Less than $300" ~ as.integer(299),
          gross_rent == "$300 to $499" ~ as.integer(499),
          gross_rent == "$500 to $749" ~ as.integer(749),
          gross_rent == "$750 to $999" ~ as.integer(999),
          gross_rent == "$1 000 to $1 499" ~ as.integer(1499),
          gross_rent == "$1 500 or more" ~ as.integer(1501))
      ) %>%
      uncount(as.numeric(number_sams),.id = "bedrooms_gross_rent_id",.remove = TRUE)
    bedrooms_gross_rent_hh <- as.data.table(bedrooms_gross_rent_data)
    
    #MORTGAGE STATUS BY AGE OF HOUSEHOLDER
    #shows for 855629 - with different age groups from housing_per_room_age_data (which also has 1562813 / hh) / = to "Owner occupied" in own_rent
    mortgage_age_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25027") 
    mortgage_age_data <- mortgage_age_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_age_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("mortgage","householder_age"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(householder_age) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_age_id",.remove = TRUE)
    mortgage_age_hh <- as.data.table(mortgage_age_data)
    
    #concept: MORTGAGE STATUS
    mortgage_status_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25081") 
    mortgage_status_data <- mortgage_status_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_status_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("first_mortgage","second","second_mortgage"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(second_mortgage=case_when(
        second=="With either a second mortgage or home equity loan but not both" ~ second_mortgage,
        first_mortgage=="Housing units without a mortgage" ~ first_mortgage,
        TRUE ~ second
      )) %>%
      filter(!is.na(second_mortgage) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_status_id",.remove = TRUE)
    mortgage_status_hh <- as.data.table(mortgage_status_data)
    
    #concept: MORTGAGE STATUS BY VALUE - I believe it's value of house, not of outstanding balance on mortgage
    mortgage_value_status_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25096") 
    mortgage_value_status_data <- mortgage_value_status_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_value_status_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("with_mortgage","value"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(value) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_status_id",.remove = TRUE)
    mortgage_value_status_hh <- as.data.table(mortgage_value_status_data)
    
    #concept: AGGREGATE VALUE (DOLLARS) BY YEAR HOUSEHOLDER MOVED INTO UNIT - NAs
    #value_yr_moved_in_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25110") 
    
    #concept: MORTGAGE STATUS BY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)
    mortgage_status_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25098") 
    mortgage_status_income_data <- mortgage_status_income_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_status_income_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("with_mortgage","hh_income"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_income) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_status_id",.remove = TRUE)
    mortgage_status_income_hh <- as.data.table(mortgage_status_income_data)
    
    #concept: MORTGAGE STATUS BY MONTHLY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
    mortgage_status_hh_costs_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25101") 
    mortgage_status_hh_costs_data <- mortgage_status_hh_costs_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_status_hh_costs_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("with_mortgage","hh_income","hh_costs_income"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(hh_costs_income=if_else(
        hh_income=="Zero or negative income",hh_income,hh_costs_income
      )) %>%
      filter(!is.na(hh_costs_income) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_costs_status_id",.remove = TRUE)
    mortgage_status_hh_costs_hh <- as.data.table(mortgage_status_hh_costs_data)
    
    #concept: MORTGAGE STATUS AND SELECTED MONTHLY OWNER COSTS - https://www.census.gov/quickfacts/fact/note/US/HSG651218 and p. 34 of https://www2.census.gov/programs-surveys/acs/tech_docs/subject_definitions/2018_ACSSubjectDefinitions.pdf
    #Selected monthly owner costs are the sum of payments
    #for mortgages, deeds of trust, contracts to purchase, or similar debts on the property
    #(including payments for the first mortgage, second mortgages, home equity loans, and other
    #  junior mortgages); real estate taxes; fire, hazard, and flood insurance on the property; utilities
    #(electricity, gas, and water and sewer); and fuels (oil, coal, kerosene, wood, etc.). It also
    #includes, where appropriate, the monthly condominium fee for condominiums (Question 15)
    #and mobile home costs (Question 23) (personal property taxes, site rent, registration fees, and
    #                                     license fees).
    mortgage_monthly_owner_costs_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25087")
    mortgage_monthly_owner_costs_data <- mortgage_status_income_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_status_income_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("with_mortgage","hh_monthly_costs"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_monthly_costs) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_costs_status_id",.remove = TRUE)
    mortgage_monthly_owner_costs_hh <- as.data.table(mortgage_monthly_owner_costs_data)
    
    #concept: MORTGAGE STATUS BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
    mortgage_monthly_costs_income_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25091")
    mortgage_monthly_costs_income_data <- mortgage_monthly_costs_income_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(mortgage_monthly_costs_income_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("with_mortgage","hh_income_costs"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_income_costs) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "mortgage_costs_income_id",.remove = TRUE)
    mortgage_monthly_costs_income_hh <- as.data.table(mortgage_monthly_costs_income_data)
    
    #concept: AGE OF HOUSEHOLDER BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS - seems to be only owners not renters
    age_hh_monthly_costs_income_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25093")
    age_hh_monthly_costs_income_data <- age_hh_monthly_costs_income_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(age_hh_monthly_costs_income_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("with_mortgage","age_income_costs"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(age_income_costs) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "age_costs_income_id",.remove = TRUE)
    age_hh_monthly_costs_income_hh <- as.data.table(age_hh_monthly_costs_income_data)
    
    #concept: HOUSEHOLD INCOME BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
    percent_income_monthly_costs_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25095")
    percent_income_monthly_costs_data <- percent_income_monthly_costs_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(percent_income_monthly_costs_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("hh_income","hh_income_costs"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_income_costs) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "percent_costs_income_id",.remove = TRUE)
    percent_income_monthly_costs_hh <- as.data.table(percent_income_monthly_costs_data)
    
    #concept: TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
    tenure_income_monthly_costs_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25106")
    tenure_income_monthly_costs_data <- tenure_income_monthly_costs_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(tenure_income_monthly_costs_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_income","hh_costs_income"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(hh_costs_income=if_else(
        hh_income=="Zero or negative income" | hh_income=="No cash rent",hh_income,hh_costs_income
      )) %>%
      filter(!is.na(hh_costs_income) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "tenure_costs_income_id",.remove = TRUE)
    tenure_income_monthly_costs_hh <- as.data.table(tenure_income_monthly_costs_data)
    
    #saveRDS(sam_hh,file = paste0(housingdir, vintage, "/sam_hh_",Sys.Date(),".RDS")) #"2020-04-06"
    #foodstamps B22005 race of HH
    #join with nat'l SNAP data? https://host76.mathematica-mpr.com/fns/Download.aspx?, but looks like a pain
    #FSBEN Unit SNAP benefit
    #FSUSIZE Unit size
    #FSGRINC Unit gross countable income
    #FSNETINC Unit net countable income
    #FSERNDED Unit earned income deduction
    #TPOV Unit gross income as a percentage of poverty 
    food_stamps_data_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B22005")
    food_stamps_data <- food_stamps_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(food_stamps_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(race = substr(name,7,7)) %>%
      rename(food_stamps = label) %>%
      filter(race %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "food_stamps_id",.remove = TRUE)
    food_stamps_race_dt <- as.data.table(food_stamps_data)
    
    food_stamps_eth_data <- food_stamps_data_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(food_stamps_data_from_census),names_to = "tract", values_to = "number_sams") %>%
      mutate(ethnicity = substr(name,7,7)) %>%
      rename(food_stamps = label) %>%
      #filter(!ethnicity %in% acs_race_codes) %>%
      uncount(as.numeric(number_sams),.id = "food_stamps_id",.remove = TRUE)
    food_stamps_eth_dt <- as.data.table(food_stamps_eth_data) 
    food_stamps_eth_dt[,c("cnt_total"):=nrow(.SD[ethnicity %in% acs_race_codes]),by=.(tract,food_stamps)]
    food_stamps_eth_dt[order(match(ethnicity,c("H","I",ethnicity %in% acs_race_codes))),c("cnt_ethn"):=list(1:.N),by=.(tract,food_stamps)]
    food_stamps_eth_dt[ethnicity %in% acs_race_codes,("ethnicity"):="_"]
    food_stamps_eth_dt[,("tokeep"):=if_else(cnt_ethn <= cnt_total,TRUE,FALSE),by=.(tract,ethnicity,food_stamps)]
    food_stamps_eth_dt <- food_stamps_eth_dt[(tokeep)]
    
    #match income on own_rent, sorted by Foodstamps
    #concept: TENURE BY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) - 1562813hh
    housing_occup_income_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25118") #of occup, own or rent by income
    housing_occup_income_data <- housing_occup_income_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_income_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_income_level"), sep = "!!", remove = F, convert = FALSE) %>%
      mutate(income_low=case_when(
        hh_income_level == "Less than $5 000" ~ as.integer(1),
        hh_income_level == "$5 000 to $9 999" ~ as.integer(5000),
        hh_income_level == "$10 000 to $14 999" ~ as.integer(10000),
        hh_income_level == "$15 000 to $19 999" ~ as.integer(15000),
        hh_income_level == "$20 000 to $24 999" ~ as.integer(20000),
        hh_income_level == "$25 000 to $34 999" ~ as.integer(25000),
        hh_income_level == "$35 000 to $49 999" ~ as.integer(35000),
        hh_income_level == "$50 000 to $74 999" ~ as.integer(50000),
        hh_income_level == "$75 000 to $99 999" ~ as.integer(75000),
        hh_income_level == "$100 000 to $149 999" ~ as.integer(100000),
        hh_income_level == "$150 000 or more" ~ as.integer(150000)),
        income_high=case_when(
          hh_income_level == "Less than $5 000" ~ as.integer(4999),
          hh_income_level == "$5 000 to $9 999" ~ as.integer(9999),
          hh_income_level == "$10 000 to $14 999" ~ as.integer(14999),
          hh_income_level == "$15 000 to $19 999" ~ as.integer(19999),
          hh_income_level == "$20 000 to $24 999" ~ as.integer(24999),
          hh_income_level == "$25 000 to $34 999" ~ as.integer(34999),
          hh_income_level == "$35 000 to $49 999" ~ as.integer(49999),
          hh_income_level == "$50 000 to $74 999" ~ as.integer(74999),
          hh_income_level == "$75 000 to $99 999" ~ as.integer(99999),
          hh_income_level == "$100 000 to $149 999" ~ as.integer(149999),
          hh_income_level == "$150 000 or more" ~ as.integer(500000)) #have to think about skew on this one... ~.006 make more than 500000
      ) %>%
      filter(!is.na(hh_income_level) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hhincome_id",.remove = TRUE)
    hh_income_dt <- as.data.table(housing_occup_income_data)
    
    #concept: TENURE BY EDUCATIONAL ATTAINMENT OF HOUSEHOLDER 1562813hh
    housing_occup_educ_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B25013") #of occup, own or rent by educ attainment
    housing_occup_educ_data <- housing_occup_educ_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(housing_occup_educ_from_census),names_to = "tract", values_to = "number_sams") %>%
      separate(label, c("own_rent","hh_education_level"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(hh_education_level) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "own_rent_hheduc_id",.remove = TRUE)
    hh_educ_dt <- as.data.table(housing_occup_educ_data)
    
    
    #concept is:"RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS BY NATIVITY OF CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY LIVING ARRANGEMENTS AND NATIVITY OF PARENTS"
    #tells whether living with one parent or two
    pov_ratio_kids_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B05010") #count of kids with family income less than pov.
    pov_ratio_kids_data <- pov_ratio_kids_from_census %>%
      mutate(label = str_remove_all(label,"Estimate!!Total!!")) %>%
      filter(label != "Estimate!!Total") %>%
      pivot_longer(4:ncol(pov_ratio_kids_from_census),names_to = "tract", values_to = "number_sams") %>% 
      separate(label, c("poverty_ratio","parent_type","parent_nativity"), sep = "!!", remove = F, convert = FALSE) %>%
      filter(!is.na(parent_nativity) & number_sams > 0) %>%
      uncount(as.numeric(number_sams),.id = "poverty_kids_id",.remove = TRUE)
    
    
}}
    



