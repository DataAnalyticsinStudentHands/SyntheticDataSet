#prioritizing Census data on age race and gender

make_easier=function(sex,race,age){
  race.for.code=ifelse(race=="Black or African American","black",
                       ifelse(race=="American Indian or Alaskan Native","amer.indian.alaskan",
                              ifelse(race=="Asian","asian",
                                     ifelse(race=="Native Hawaiian or Other Pacific Islander","islander",
                                            ifelse(race=="Some Other Race","other",
                                                   ifelse(race=="Two or More Races","multiracial",
                                                          ifelse(race=="White","white",
                                                                 ifelse(race=="Hispanic or Latino","hispanic",NA
                       ))))))))
  age.for.code=ifelse(age<5,"under.5",
                      ifelse(age>=5&age<=9,"5.to.9",
                             ifelse(age>=10&age<=14,"10.to.14",
                                    ifelse(age>=15&age<=17,"15.to.17",
                                           ifelse(age>=18&age<=19,"18.to.19",
                                                  ifelse(age>=20&age<=24,"20.to.24",
                                                         ifelse(age>=25&age<=29,"25.to.29",
                                                                ifelse(age>=30&age<=34,"30.to.34",
                                                                       ifelse(age>=35&age<=44,"35.to.44",
                                                                              ifelse(age>=45&age<=54,"45.to.54",
                                                                                     ifelse(age>=55&age<=64,"55.to.64",
                                                                                            ifelse(age>=65&age<=74,"65.to.74",
                                                                                                   ifelse(age>=75&age<=84,"75.to.84",
                                                                                                          ifelse(age>=75,"over.75",
                                                                                                                 NA))))))))))))))
  sex.for.code=ifelse(sex=="Female"&age<18,"girls",
                      ifelse(sex=="Female"&age>=18,"women",
                             ifelse(sex=="Male"&age<18,"boys",
                                    ifelse(sex=="Male"&age>=18,"men"))))

  final_code_for_table_reference=paste(race.for.code,sex.for.code,age.for.code,sep = ".")
  return(final_code_for_table_reference)
}