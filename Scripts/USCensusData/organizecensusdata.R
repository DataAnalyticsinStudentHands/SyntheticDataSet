#Organize all R data that wil be used

inputdir = "../Inputs/"

#housetype and race data
householdtypeandrace=read.csv(paste0(inputdir,"householdtypeandrace.csv"))

#Assuming previously sampled type sample size
#load size data
householdsizebytype=read.csv(paste0(inputdir,"household_size.csv"))

#separate types by family and non family, as their probability for sizes is different
family=householdsizebytype[c("tract","B11016_003E","B11016_004E","B11016_005E","B11016_006E","B11016_007E","B11016_008E")]
nonfamily=householdsizebytype[c("tract","B11016_011E","B11016_012E","B11016_013E","B11016_014E","B11016_015E","B11016_016E")]

#Read in data for sampling distribution for number of vehicles by number of people in household
householdsizebyvehicles=read.csv(paste0(inputdir,"household_size_by_vehicles_available.csv"))

#Read in Census data and subset by tract and county
#Data for sex age and race code
sexbyagebyrace1=read.csv(paste0(inputdir,"sex_by_age_by_race.csv"))

Census_data_List=list(householdtypeandrace=householdtypeandrace,family=family,nonfamily=nonfamily,householdsizebyvehicles=householdsizebyvehicles,sexbyagebyrace1=sexbyagebyrace1)
rm(householdsizebytype,householdtypeandrace,family,nonfamily)

#read in school enrollment data
Census_data_List$enrollschool1=read.csv(paste0(inputdir,"school_enrollment_by_sex_by_age.csv"))

Census_data_List$eduattain1=read.csv(paste0(inputdir,"education_attainment_by_sex_by_age.csv"))

Census_data_List$employment1=read.csv(paste0(inputdir,"employment.csv"))

Census_data_List$disability1=read.csv(paste0(inputdir,"disability_status.csv"))

Census_data_List$language=read.csv(paste0(inputdir,"nativity_language.csv"))

Census_data_List$citizen=read.csv(paste0(inputdir,"citizenship_language.csv"))

Census_data_List$veterans=read.csv(paste0(inputdir,"veteran_status.csv"))

Census_data_List$transport=read.csv(paste0(inputdir,"work_transportation.csv"))

Census_data_List$traveltime=read.csv(paste0(inputdir,"travel_time.csv"))

Census_data_List$income=read.csv(paste0(inputdir,"household_income.csv"))

Census_data_List$insurance=read.csv(paste0(inputdir,"health_insurance.csv"))

#source("workingdoc.R")
#source("workingdoc2.R")
#source("workingdoc3.R")
#source("workingdoc4.R")
#source("household_generator.R")
#will_this_work=household_generator(201,100000,1,1,Census_data_List=Census_data_List)
#will_this_work=getnumberofvehicles(201,100000,will_this_work,1,Census_data_List)
