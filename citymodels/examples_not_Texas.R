library(citymodels)

#get data for louisiana
louisiana_census_data=census_data_API(state=22)

lsample=household_generator(22,"087","030400",seed=1,inputdir = "../Inputs/",louisiana_census_data)
lsample2=group_quarters_simulater(22,"087","030400",seed=1,inputdir = "../Inputs/",louisiana_census_data)

lsample.set=rbind(lsample,lsample2)

pennsylvania_census_data=census_data_API(state=42)

psample=household_generator(42,"101","000500",seed=1,inputdir = "../Inputs/",pennsylvania_census_data)
psample2=group_quarters_simulater(42,"101","000500",seed=1,inputdir = "../Inputs/",pennsylvania_census_data)

psample.set=rbind(psample,psample2)

