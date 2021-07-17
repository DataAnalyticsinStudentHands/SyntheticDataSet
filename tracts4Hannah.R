#assumes you have Census_Data.R sourced and loaded variable names from workflow; censuskey from expand_from_census.R



sex_by_age_race_data_from_census_tx <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B01001",tr_bg = "tr", est_err = "est")
sex_by_age_race_data <- sex_by_age_race_data_from_census %>%
  mutate(label = str_remove_all(label,"Estimate!!Total!!"),
         race = substr(name,7,7)) %>%
  filter(label != "Estimate!!Total") %>%
  pivot_longer(5:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams")%>%
  separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE)

sex_by_age_race_total <- sex_by_age_race_data_from_census %>%
  mutate(label = str_remove_all(label,"Estimate!!Total!!"),
         race = substr(name,7,7)) %>%
  filter(label == "Estimate!!Total") %>%
  pivot_longer(5:ncol(sex_by_age_race_data_from_census),names_to = "tract", values_to = "number_sams")%>%
  separate(label, c("sex","age_range"), sep = "!!", remove = F, convert = FALSE)


pop_sample_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B00001",tr_bg = "tr")
households_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B00002",tr_bg = "tr")
gini_from_census <- censusDataFromAPI_byGroupName(censusdir, vintage, state, county, tract, censuskey, groupname = "B19083",tr_bg = "tr",est_err = "est")
