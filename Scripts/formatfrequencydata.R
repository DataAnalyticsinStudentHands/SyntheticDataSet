syntheticdataset=read.csv('sample_set1.csv')


#Make first frequency table
example=ftable(table(syntheticdataset$tract,syntheticdataset$household.type))
ex=as.data.frame.matrix(example)

colnames(ex)=paste("household.type",unlist(attr(example, "col.vars")),sep="_")
ex$tract=unlist(attr(example, "row.vars"))

#Make Second frequency table
example2=ftable(table(syntheticdataset$tract,syntheticdataset$size))
ex2=as.data.frame.matrix(example2)

colnames(ex2)=paste("size",unlist(attr(example2, "col.vars")),sep="_")
ex2$tract=unlist(attr(example2, "row.vars"))

example=merge(ex,ex2,by.x="tract",by.y="tract")

#loop through rest of variables
varnames=colnames(syntheticdataset)
for (var in 8:24){
  newpart=ftable(table(syntheticdataset[ ,7],syntheticdataset[ ,var]))
  newp=as.data.frame.matrix(newpart)
  
  colnames(newp)=paste(varnames[var],unlist(attr(newpart, "col.vars")),sep="_")
  newp$tract=unlist(attr(newpart, "row.vars"))
  
  example=merge(example,newp,by.x="tract",by.y="tract")
}

write.csv(example,"syntheticdatasetfrequenciespertract.csv")


#Make frequency table from actual data
inputdir = "../Inputs/"

householdtype=read.csv("../Inputs/household_type_for_error.csv")
colnames(householdtype)=gsub(".", " ", colnames(householdtype),fixed=TRUE)
colnames(householdtype)=paste("householdtype_",colnames(householdtype))
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

realfrequencypertract=merge(householdtype,size)

vehicles=read.csv("../Inputs/vehicles_for_error.csv")
names(vehicles)[4:7]=paste("number.of.vehicles",1:4,sep="_")

realfrequencypertract=merge(realfrequencypertract,vehicles)

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

citizenship=read.csv("citizenship_for_error.csv")

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

realfrequencypertract=merge(realfrequencypertract,income)

insurance=read.csv(paste0(inputdir,"health_insurance.csv"))

private=c("04","09",as.character(seq(11,24,by=5)))
public=c("05",as.character(seq(10,25,by=5)))
none=c("06",as.character(seq(11,26,by=5)))

realfrequencypertract$"health.insurance_no insurance"=rowSums(insurance[paste0("B27015_0",none,"E")])
realfrequencypertract$"health.insurance_private insurance"=rowSums(insurance[paste0("B27015_0",private,"E")])
realfrequencypertract$"health.insurance_public insurance"=rowSums(insurance[paste0("B27015_0",public,"E")])

travel.time.for.work=read.csv("travel_time_for_error.csv")
realfrequencypertract=merge(realfrequencypertract,travel.time.for.work)

other.vars=read.csv("other_vars_for_error.csv")
realfrequencypertract=merge(realfrequencypertract,other.vars)

write.csv(realfrequencypertract,"realfrequenciespertract.csv")
#