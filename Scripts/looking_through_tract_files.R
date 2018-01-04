firsttractfile=readRDS("households1e+05.rds")
homeless_households=subset(firsttractfile,is.na(firsttractfile$householdID))
#Yay! no homeless households
lonely_houses=subset(firsttractfile,is.na(firsttractfile$householdID))
unique(lonelyhouses$BUILDING_STYLE_CODE)
#there are empty condos

second_tract_file=readRDS("households210100.rds")
#not many householdIDs but everything is merged
sample.set=read.csv("sample_set_with_account.csv")
tract210100houses=subset(sample.set,sample.set$tract==210100)
#but apparently the right number of houses