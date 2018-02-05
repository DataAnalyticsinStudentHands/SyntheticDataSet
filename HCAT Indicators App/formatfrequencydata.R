#This code was previously written for the general_map_app, but reformatting for HCAD Indicators is simpler from the frequency tables
#so we will just comment out some of the lines and work further with the HCAT Indicators
syntheticdataset=readRDS("complete_sample_set.RDS")
syntheticdataset$GEOID=paste0(syntheticdataset$county,syntheticdataset$tract)

#Make first frequency table
example=ftable(table(syntheticdataset$GEOID,syntheticdataset$household.type))
ex=as.data.frame.matrix(example)

colnames(ex)=paste("household.type",unlist(attr(example, "col.vars")),sep="_")
ex$GEOID=unlist(attr(example, "row.vars"))

#Make Second frequency table
example2=ftable(table(syntheticdataset$GEOID,syntheticdataset$size))
ex2=as.data.frame.matrix(example2)

colnames(ex2)=paste("size",unlist(attr(example2, "col.vars")),sep="_")
ex2$GEOID=unlist(attr(example2, "row.vars"))

#Merge frequency tables
example=merge(ex,ex2,by.x="GEOID",by.y="GEOID",all=TRUE)

syntheticdataset$X.1=NULL

#apparently there is no data on colonscopy in Houston
syntheticdataset$colonoscopy=NULL
syntheticdataset$mammography.use=NULL
syntheticdataset$pap.smear=NULL

#loop through rest of variables
varnames=colnames(syntheticdataset)
for (var in 10:26){
  newpart=ftable(table(syntheticdataset[ ,97],syntheticdataset[ ,var]))
  newp=as.data.frame.matrix(newpart)
  
  colnames(newp)=paste(varnames[var],unlist(attr(newpart, "col.vars")),sep="_")
  newp$GEOID=unlist(attr(newpart, "row.vars"))
  
  example=merge(example,newp,by.x="GEOID",by.y="GEOID",all=TRUE)
}



#Next 13 lines table data gotten from the 500 Cities Project by number but not percentage
#I thought the percentage would be better so I commented these out and the next chunk of lines do it by percentage
#for (var in 27:49){
 # newpart=ftable(table(syntheticdataset[ ,9],syntheticdataset[ ,var]))
  #newp=as.data.frame.matrix(newpart)
  
  #colnames(newp)=paste(unlist(attr(newpart, "col.vars")),sep="_")
  #newp$no=NULL
  #newp$`Not Available for this Census Tract`=NULL
  
  #colnames(newp)=paste(varnames[var],sep="_")
  
  #newp$tract=unlist(attr(newpart, "row.vars"))
  
  #example=merge(example,newp,by.x="tract",by.y="tract")
#}

adults=example[c("age_18 to 19","age_20 to 24","age_25 to 29","age_30 to 34","age_35 to 44","age_45 to 54","age_55 to 64","age_65 to 74","age_75 to 84","age_Over 85")]
adults$total_adults=rowSums(adults)
adults$GEOID=example$GEOID
adults=adults[c("GEOID","total_adults")]
older_adults=example[c("age_65 to 74","age_75 to 84","age_Over 85")]
older_adults$total_older_adults=rowSums(older_adults)
older_adults$GEOID=example$GEOID
older_adults=older_adults[c("GEOID","total_older_adults")]

for (var in 27:28){
  newpart=ftable(table(syntheticdataset[ ,97],syntheticdataset[ ,var]))
  newp=as.data.frame.matrix(newpart)
  
  colnames(newp)=paste(unlist(attr(newpart, "col.vars")),sep="_")
  newp$no=NULL
  newp$`Not Available for this Census Tract`=NULL
  
  newp$GEOID=unlist(attr(newpart, "row.vars"))
  newp=merge(newp,older_adults,by.x = "GEOID",by.y = "GEOID",all=TRUE)
  newp$yes=(newp$yes/newp$total_older_adults)*100
  newp=newp[c("yes","GEOID")]
  
  colnames(newp)=c(paste(varnames[var],sep="_"),"GEOID")
  
  example=merge(example,newp,by.x="GEOID",by.y="GEOID",all=TRUE)
}
for (var in 29:49){
  newpart=ftable(table(syntheticdataset[ ,97],syntheticdataset[ ,var]))
  newp=as.data.frame.matrix(newpart)
  
  colnames(newp)=paste(unlist(attr(newpart, "col.vars")),sep="_")
  newp$no=NULL
  newp$`Not Available for this Census Tract`=NULL
  
  newp$GEOID=unlist(attr(newpart, "row.vars"))
  newp=merge(newp,adults,by.x = "GEOID",by.y = "GEOID",all=TRUE)
  newp$yes=(newp$yes/newp$total_adults)*100
  newp=newp[c("yes","GEOID")]
  
  colnames(newp)=c(paste(varnames[var],sep="_"),"GEOID")
  
  example=merge(example,newp,by.x="GEOID",by.y="GEOID",all=TRUE)
}

example$"Arthritis among adults aged >=18 Years"=example$diagnosed.arthritis
example$"Binge drinking among adults aged >=18 Years"=example$binge.drinker
example$"High blood pressure among adults aged >=18 Years"=example$high.blood.pressure
example$"Obesity among adults aged >=18 Years"=example$Obesity
example$"Taking medicine for high blood pressure control among adults aged >=18 Years with high blood pressure"=example$blood.pressure.medications
example$"Cancer (excluding skin cancer) among adults aged >=18 Years"=example$cancer.not.including.skin.cancer
example$"Current asthma among adults aged >=18 Years"=example$adult.asthma
example$"Coronary heart disease among adults aged >=18 Years"=example$coronary.heart.disease
example$"Visits to doctor for routine checkup within the past Year among adults aged >=18 Years"=example$routine.checkups
example$"Cholesterol screening among adults aged >=18 Years"=example$cholesterol.screening
example$"Chronic obstructive pulmonary disease among adults aged >=18 Years"=example$chronic.obstructive.pulmonary.disease
example$"Older adult men aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening"=example$older.men.up.to.date.on.preventative.services
example$"Older adult women aged >=65 Years who are up to date on a core set of clinical preventive services: Flu shot past Year, PPV shot ever, Colorectal cancer screening, and Mammogram past 2 Years"=example$older.women.up.to.date.on.preventative.services
example$"Current smoking among adults aged >=18 Years"=example$smoker
example$"Diagnosed diabetes among adults aged >=18 Years"=example$diagnosed.diabetes
example$"High cholesterol among adults aged >=18 Years who have been screened in the past 5 Years"=example$high.cholesterol
example$"Chronic kidney disease among adults aged >=18 Years"=example$chronic.kidney.disease
example$"No leisure-time physical activity among adults aged >=18 Years"=example$no.leisure.time.physical.activity
example$"Mental health not good for >=14 days among adults aged >=18 Years"=example$mental.health.not.good.for.more.than.14.days
example$"Stroke among adults aged >=18 Years"=example$had.a.stroke
example$"Physical health not good for >=14 days among adults aged >=18 Years"=example$physical.health.not.good.for.14.days
example$"Sleeping less than 7 hours among adults aged >=18 Years"=example$sleeping.less.than.7.hours
example$"All teeth lost among adults aged >=65 Years"=example$lost.all.their.teeth


write.csv(example,"syntheticdatasetfrequenciespertract.csv")
saveRDS(example,"syntheticdatasetfrequencypertract.RDS")

#Make frequency table from actual data
inputdir = "../Inputs/"

householdtype=read.csv("../Inputs/household_type_for_error.csv")
colnames(householdtype)=gsub(".", " ", colnames(householdtype),fixed=TRUE)
colnames(householdtype)=paste0("household.type_",colnames(householdtype))
names(householdtype)[1:3] <- c("state","county","tract")

size=read.csv("../Inputs/household_size.csv")

size$size_1=(size$B11016_010E)
size$size_2=(size$B11016_003E+size$B11016_011E)
size$size_3=(size$B11016_004E+size$B11016_012E)
size$size_4=(size$B11016_005E+size$B11016_013E)
size$size_5=(size$B11016_006E+size$B11016_014E)
size$size_6=(size$B11016_007E+size$B11016_015E)
size$size_7=(size$B11016_008E+size$B11016_016E)
size[4:14]=NULL
colnames(size)[4] <- "size_1"

realfrequencypertract=merge(householdtype,size,all=TRUE)

vehicles=read.csv("../Inputs/vehicles_for_error.csv")
names(vehicles)[4:7]=paste("number.of.vehicles",1:4,sep="_")

realfrequencypertract=merge(realfrequencypertract,vehicles,all=TRUE)

racesexandage=read.csv("../Inputs/sex_by_age_by_race.csv")
#B01001B_003E
Male=c("03","04","05","06","07","08","09",as.character(10:16))
Female=18:31
Races=c("B","C","D","E","F","G","H","I")
vars=paste0("B01001B_00",3:8,"E")

realfrequencypertract$sex_Male=rowSums(racesexandage[paste0("B01001",Races,"_","0",Male,"E")])
realfrequencypertract$sex_Female=rowSums(racesexandage[paste0("B01001",Races,"_","0",Female,"E")])

realfrequencypertract$"age_Under 5"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("03","18"),"E")])
realfrequencypertract$"age_5 to 9"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("04","19"),"E")])
realfrequencypertract$"age_10 to 14"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("05","20"),"E")])
realfrequencypertract$"age_15 to 17"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("06","21"),"E")])
realfrequencypertract$"age_18 to 19"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("07","22"),"E")])
realfrequencypertract$"age_20 to 24"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("08","23"),"E")])
realfrequencypertract$"age_25 to 29"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("09","24"),"E")])
realfrequencypertract$"age_30 to 34"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("10","25"),"E")])
realfrequencypertract$"age_35 to 44"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("11","26"),"E")])
realfrequencypertract$"age_45 to 54"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("12","27"),"E")])
realfrequencypertract$"age_55 to 64"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("13","28"),"E")])
realfrequencypertract$"age_65 to 74"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("14","29"),"E")])
realfrequencypertract$"age_75 to 84"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("15","30"),"E")])
realfrequencypertract$"age_Over 85"=rowSums(racesexandage[paste0("B01001",Races,"_","0",c("16","31"),"E")])

realfrequencypertract$"race_Black or African American"=rowSums(racesexandage[paste0("B01001","B","_","0",c(Male,Female),"E")])
realfrequencypertract$"race_American Indian or Alaskan Native" =rowSums(racesexandage[paste0("B01001","C","_","0",c(Male,Female),"E")])
realfrequencypertract$"race_Asian" =rowSums(racesexandage[paste0("B01001","D","_","0",c(Male,Female),"E")])
realfrequencypertract$"race_Native Hawaiian or Other Pacific Islander"=rowSums(racesexandage[paste0("B01001","E","_","0",c(Male,Female),"E")])
realfrequencypertract$"race_Some Other Race"=rowSums(racesexandage[paste0("B01001","F","_","0",c(Male, Female),"E")])
realfrequencypertract$"race_Two or More Races" =rowSums(racesexandage[paste0("B01001","G","_","0",c(Male, Female),"E")])
realfrequencypertract$"race_White" =rowSums(racesexandage[paste0("B01001","H","_","0",c(Male, Female),"E")])
realfrequencypertract$"race_Hispanic or Latino" =rowSums(racesexandage[paste0("B01001","I","_","0",c(Male, Female),"E")])

enrollschool1=read.csv(paste0(inputdir,"school_enrollment_by_sex_by_age.csv"))

public.school=c("05","06","07","08","09","10","11",as.character(33:39))
private.school=c(42:48,14:20)
no.school=c(23:29,51:57)

#"B14003_033E"
realfrequencypertract$"school.enrollment_Public School"=rowSums(enrollschool1[paste0("B14003_0",public.school,"E")])
realfrequencypertract$"school.enrollment_Private School" =rowSums(enrollschool1[paste0("B14003_0",private.school,"E")])
realfrequencypertract$"school.enrollment_Not Enrolled in School" =rowSums(enrollschool1[paste0("B14003_0",no.school,"E")])

eduattain1=read.csv(paste0(inputdir,"education_attainment_by_sex_by_age.csv"))

less.than.9=c("04",as.character(c(seq(12,36,by=8),seq(45,77,by=8))))
hs.no.degree=c("05",as.character(c(seq(13,37,by=8),seq(46,78,by=8))))
hs=c("06",as.character(c(seq(14,38,by=8),seq(47,79,by=8))))
some.college=c("07",as.character(c(seq(15,39,by=8),seq(48,80,by=8))))
assoc=c("08",as.character(c(seq(16,40,by=8),seq(49,81,by=8))))
bach=c("09",as.character(c(seq(17,41,by=8),seq(50,82,by=8))))
grad=c("10",as.character(c(seq(18,42,by=8),seq(51,83,by=8))))

realfrequencypertract$"education.attainment_Less than 9th grade"=rowSums(eduattain1[paste0("B15001_0",less.than.9,"E")])
realfrequencypertract$"education.attainment_9th to 12th grade, no diploma" =rowSums(eduattain1[paste0("B15001_0",hs.no.degree,"E")])
realfrequencypertract$"education.attainment_High School Graduate" =rowSums(eduattain1[paste0("B15001_0",hs,"E")]) 
realfrequencypertract$"education.attainment_Some College, no degree" =rowSums(eduattain1[paste0("B15001_0",some.college,"E")]) 
realfrequencypertract$"education.attainment_Associate's degree"  =rowSums(eduattain1[paste0("B15001_0",assoc,"E")]) 
realfrequencypertract$"education.attainment_Bachelor's Degree" =rowSums(eduattain1[paste0("B15001_0",bach,"E")]) 
realfrequencypertract$"education.attainment_Graduate or Professional Degree"  =rowSums(eduattain1[paste0("B15001_0",grad,"E")]) 

disability1=read.csv(paste0(inputdir,"disability_status.csv"))

realfrequencypertract$"disability_With One Type of Disability"=rowSums(disability1[c("C18108_003E","C18108_007E","C18108_011E")])
realfrequencypertract$"disability_With Two or More Types of Disabilities"=rowSums(disability1[c("C18108_004E","C18108_008E","C18108_012E")])
realfrequencypertract$"disability_No Disabilities" =rowSums(disability1[c("C18108_005E","C18108_009E","C18108_013E")])

citizenship=read.csv("../Inputs/citizenship_for_error.csv")

realfrequencypertract$"citizenship_Citizen"=rowSums(citizenship[c("B05001_002E","B05001_003E","B05001_004E")])
realfrequencypertract$"citizenship_Naturalized Citizen" =citizenship$"B05001_005E"
realfrequencypertract$"citizenship_Not a U.S. Citizen" =citizenship$"B05001_006E"

transport=read.csv(paste0(inputdir,"work_transportation.csv"))

realfrequencypertract$"means.of.transportation.to.work_drove alone"=rowSums(transport[c("B08006_020E","B08006_037E")])
realfrequencypertract$"means.of.transportation.to.work_carpooled"=rowSums(transport[c("B08006_021E","B08006_038E")])
realfrequencypertract$"means.of.transportation.to.work_public transportation"=rowSums(transport[c("B08006_025E","B08006_042E")])
realfrequencypertract$"means.of.transportation.to.work_bicycle"=rowSums(transport[c("B08006_031E","B08006_048E")])
realfrequencypertract$"means.of.transportation.to.work_walked"=rowSums(transport[c("B08006_032E","B08006_049E")])
realfrequencypertract$"means.of.transportation.to.work_motorcycle taxicab or other"=rowSums(transport[c("B08006_033E","B08006_050E")])
realfrequencypertract$"means.of.transportation.to.work_worked at home"=rowSums(transport[c("B08006_034E","B08006_051E")])

income=read.csv(paste0(inputdir,"household_income.csv"))
code=c("less than 10,000","10,000 to 14,999","15,000 to 19,999","20,000 to 24,999","25,000 to 29,999","30,000 to 34,999","35,000 to 39,999","40,000 to 44,999","45,000 to 49,999","50,000 to 59,999","60,000 to 74,999","75,000 to 99,999","100,000 to 124,999","125,000 to 149,999","150,000 to 199,999","200,000 or more")

col=c("state","county","tract",paste0("household.income_",code))
colnames(income)=col

realfrequencypertract=merge(realfrequencypertract,income,all=TRUE)

insurance=read.csv(paste0(inputdir,"health_insurance.csv"))

private=c("04","09",as.character(seq(11,24,by=5)))
public=c("05",as.character(seq(10,25,by=5)))
none=c("06",as.character(seq(11,26,by=5)))

realfrequencypertract$"health.insurance_no insurance"=rowSums(insurance[paste0("B27015_0",none,"E")])
realfrequencypertract$"health.insurance_private insurance"=rowSums(insurance[paste0("B27015_0",private,"E")])
realfrequencypertract$"health.insurance_public insurance"=rowSums(insurance[paste0("B27015_0",public,"E")])

travel.time.for.work=read.csv("../Inputs/travel_time_for_error.csv")
realfrequencypertract=merge(realfrequencypertract,travel.time.for.work,all=TRUE)

other.vars=read.csv("../Inputs/other_vars_for_error.csv")
realfrequencypertract=merge(realfrequencypertract,other.vars,all=TRUE)

inputdir = "../Inputs/"
houstondata=read.csv(paste0(inputdir,'houstondata.csv'))
#This provides us with 28 variables all in percentages

#formatting
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

houstondata$UniqueID=as.character(houstondata$UniqueID)
houstondata$tract=substrRight(houstondata$UniqueID,6)
houstondata$county=substr(houstondata$TractFIPS, 3, 5)
houstondata$Measure=as.character(houstondata$Measure)


variables=unique(houstondata$Measure)

for (var in 1:28){
  partofhoustondata=subset(houstondata,houstondata$Measure==variables[var])
  partofhoustondata=data.frame(county=partofhoustondata$county,tract=partofhoustondata$tract,value=partofhoustondata$Data_Value)
  
  cnames=c("county","tract",paste(variables[var]))
  colnames(partofhoustondata)=cnames
  
  realfrequencypertract=merge(partofhoustondata,realfrequencypertract,all=TRUE)
}


write.csv(realfrequencypertract,"realfrequenciespertract.csv")
saveRDS(realfrequencypertract,"realfrequencypertract.RDS")



##
#These next 17 lines really only need to be run once so they will and just have the resulting spacial frame read in using RDS
#special code for Carol can't figure out how to install tigris on the server
#Load maps of tracts
#Harris_tract_data <- readRDS("Harris_tract_data.rds")
#Harris_tract_data=Harris_tract_data[,-(6:1779)]
#Harris_tract_data=Harris_tract_data[,-(6:105)]
#Load data
#realfrequencypertract=read.csv("realfrequenciespertract.csv")
#syntheticfrequencypertract=read.csv("syntheticdatasetfrequenciespertract.csv")

#My numbers seem really off so I want to look at the number of households to compare
#households=read.csv("../Inputs/household_type_for_error.csv")
#get total number of households per tract
#households$Households=rowSums(households[4:8])
#households$GEOID=paste0(households$county,households$tract)
#households=households[c("GEOID","Households")]

#householdIDsbytract=syntheticdataset[c("householdID","GEOID")]
#rm(syntheticdataset)
#householdIDsbytract=householdIDsbytract[!duplicated(householdIDsbytract$householdID),]
#householdsbytract=table(householdIDsbytract$GEOID)
#householdsbytract=as.data.frame.table(householdsbytract)
#colnames(householdsbytract)=c("GEOID","Simulated_Households")

#And the number
#number_of_people=read.csv("../Inputs/sex_by_age_by_race.csv")
#number_of_people$total_people_recorded_in_Census=rowSums(number_of_people[4:227])
#number_of_people$GEOID=paste0(number_of_people$county,number_of_people$tract)
#number_of_people=number_of_people[c("GEOID","total_people_recorded_in_Census")]

#number_synthetic_people=data.frame(GEOID=syntheticfrequencypertract$GEOID,number_of_simulated_people=rowSums(syntheticfrequencypertract[22:35]))

#number_of_people=merge(number_of_people,number_synthetic_people,by="GEOID")
#households=merge(households,householdsbytract,all=TRUE)
#number_of_people=merge(number_of_people,households,by="GEOID",all=TRUE)
#saveRDS(number_of_people,"number_of_people.RDS")


#to add right length location field to Harris_tract_data
#Harris_tract_data$GEOID <- paste0(Harris_tract_data$COUNTYFP,Harris_tract_data$TRACTCE)
#realfrequencypertract$GEOID <- paste0(realfrequencypertract$county,realfrequencypertract$tract)
#library(tigris)
#Houston_real <- geo_join(Harris_tract_data, realfrequencypertract, 'GEOID', 'GEOID', by = NULL, how = "left")
#Houston_real <- geo_join(Houston_real,number_of_people,'GEOID','GEOID',how="left")
#syntheticfrequencypertract$TRACTCE <- syntheticfrequencypertract$tract
#Harris_tract_data$GEOID<-paste0(Harris_tract_data$COUNTYFP,Harris_tract_data$TRACTCE)
#Houston_prime <- geo_join(Harris_tract_data,syntheticfrequencypertract, 'GEOID', 'GEOID', by = NULL, how = "left")
#Houston_prime <- geo_join(Houston_prime,number_of_people,'GEOID','GEOID',how="left")
#saveRDS(Houston_prime,"Houston_prime.RDS")
#saveRDS(Houston_real,"Houston_real.RDS")





#Reformating for HCAT Indicators
realfrequencypertract=read.csv("realfrequenciespertract.csv")
syntheticfrequencypertract=read.csv("syntheticdatasetfrequenciespertract.csv")

HCATindicator_real=data.frame(GEOID=paste0(realfrequencypertract$county,realfrequencypertract$tract),Adult_Education_Attainment=(rowSums(realfrequencypertract[,78:82])/rowSums(realfrequencypertract[,76:82]))*100)
HCATindicator_prime=data.frame(GEOID=syntheticfrequencypertract$GEOID,Adult_Education_Attainment=(rowSums(syntheticfrequencypertract[,c(48:51,53)])/rowSums(syntheticfrequencypertract[,47:53]))*100)

HCATindicator_real$employment_rate=(rowSums(realfrequencypertract[,c("employment_Employed","employment_In.Armed.Forces")])/rowSums(realfrequencypertract[c("employment_Employed","employment_In.Armed.Forces","employment_Unemployed","employment_Not.in.labor.force")]))*100
HCATindicator_prime$employment_rate=(rowSums(syntheticfrequencypertract[,c("employment_Employed","employment_In.Armed.Forces")])/rowSums(syntheticfrequencypertract[c("employment_Employed","employment_In.Armed.Forces","employment_Unemployed","employment_Not.in.labor.force")])) *100                                                                

HCATindicator_real$unemployment_rate=(realfrequencypertract[,c("employment_Unemployed")])/rowSums(realfrequencypertract[c("employment_Employed","employment_In.Armed.Forces","employment_Unemployed","employment_Not.in.labor.force")])*100
HCATindicator_prime$unemployment_rate=(syntheticfrequencypertract[,c("employment_Unemployed")])/rowSums(syntheticfrequencypertract[c("employment_Employed","employment_In.Armed.Forces","employment_Unemployed","employment_Not.in.labor.force")])*100                                                                 


#Merge for maps
Harris_tract_data <- readRDS("Harris_tract_data.rds")
Harris_tract_data=Harris_tract_data[,-(6:1779)]
Harris_tract_data=Harris_tract_data[,-(6:105)]

Harris_tract_data$GEOID <- paste0(Harris_tract_data$COUNTYFP,Harris_tract_data$TRACTCE)

library(tigris)
HCAT_real <- geo_join(Harris_tract_data, HCATindicator_real, 'GEOID', 'GEOID', by = NULL, how = "left")
HCAT_prime <- geo_join(Harris_tract_data,HCATindicator_prime,'GEOID','GEOID',how="left")

saveRDS(HCAT_real,"HCAT_real.RDS")
saveRDS(HCAT_prime,"HCAT_prime.RDS")