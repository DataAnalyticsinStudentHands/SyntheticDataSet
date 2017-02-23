####COME BACK TO EMPLOYMENT AND LOOK FOR WORK AROUND ON AGE 15!!!!!!!!!!!!!!!!!!!!


getschoolenrollment <- function(county,tract,syntheticdataset,seed){
  
  set.seed(seed)
  enrollschool=read.csv("school_enrollment_by_sex_by_age.csv")
  enrollschool=enrollschool[(enrollschool$tract==tract)&(enrollschool$county==county),]
  
  Women5.9=enrollschool[c("B14003_033E","B14003_042E","B14003_051E")]
  Women10.14=enrollschool[c("B14003_034E","B14003_043E","B14003_052E")]
  Women15.17=enrollschool[c("B14003_035E","B14003_044E","B14003_053E")]
  Women18.19=enrollschool[c("B14003_036E","B14003_045E","B14003_054E")]
  Women20.24=enrollschool[c("B14003_037E","B14003_046E","B14003_055E")]
  Women25.34=enrollschool[c("B14003_038E","B14003_047E","B14003_056E")]
  Women35=enrollschool[c("B14003_039E","B14003_048E","B14003_057E")]
  
  Men5.9=enrollschool[c("B14003_005E","B14003_014E","B14003_023E")]
  Men10.14=enrollschool[c("B14003_006E","B14003_015E","B14003_024E")]
  Men15.17=enrollschool[c("B14003_007E","B14003_016E","B14003_025E")]
  Men18.19=enrollschool[c("B14003_008E","B14003_017E","B14003_026E")]
  Men20.24=enrollschool[c("B14003_009E","B14003_018E","B14003_027E")]
  Men25.34=enrollschool[c("B14003_010E","B14003_019E","B14003_028E")]
  Men35=enrollschool[c("B14003_011E","B14003_020E","B14003_029E")]
  #"Public School","Private School","Not Enrolled in School"

  school.enrollment=ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="5 to 9",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women5.9/sum(Women5.9)),
         ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="10 to 14",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women10.14/sum(Women10.14)),
                ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="15 to 17",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women15.17/sum(Women15.17)),
                       ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="18 to 19",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women18.19/sum(Women18.19)),
                              ifelse(syntheticdataset$sex=="Female"&syntheticdataset$age=="20 to 24",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women20.24/sum(Women20.24)),
                                     ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women25.34/sum(Women25.34)),
                                            ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"|syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Women35/sum(Women35)),
                                                   ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="5 to 9",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men5.9/sum(Men5.9)),
                                                          ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="10 to 14",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men10.14/sum(Men10.14)),
                                                                 ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="15 to 17",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men15.17/sum(Men15.17)),
                                                                        ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="18 to 19",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men18.19/sum(Men18.19)),
                                                                               ifelse(syntheticdataset$sex=="Male"&syntheticdataset$age=="20 to 24",sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men20.24/sum(Men20.24)),
                                                                                      ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men25.34/sum(Men25.34)),
                                                                                             ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"|syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(c("Public School","Private School","Not Enrolled in School"),1,prob=Men35/sum(Men35))
                                                                                                    ,NA))))))))))))))
  syntheticdataset$school.enrollment=school.enrollment
  return(syntheticdataset)
}



geteducationattainment=function(county,tract,syntheticdataset,seed){
  
  set.seed(seed)
  eduattain=read.csv("education_attainment_by_sex_by_age.csv")
  eduattain=eduattain[(eduattain$tract==tract)&(eduattain$county==county),]
  
  
  men18.24=eduattain[c("B15001_004E","B15001_005E","B15001_006E","B15001_007E","B15001_008E","B15001_009E","B15001_010E")]
  men25.34=eduattain[c("B15001_012E","B15001_013E","B15001_014E","B15001_015E","B15001_016E","B15001_017E","B15001_018E")]
  men35.44=eduattain[c("B15001_020E","B15001_021E","B15001_022E","B15001_023E","B15001_024E","B15001_025E","B15001_026E")]
  men45.64=eduattain[c("B15001_028E","B15001_029E","B15001_030E","B15001_031E","B15001_032E","B15001_033E","B15001_034E")]
  men65=eduattain[c("B15001_036E","B15001_037E","B15001_038E","B15001_039E","B15001_040E","B15001_041E","B15001_042E")]
  
  women18.24=eduattain[c("B15001_045E","B15001_046E","B15001_047E","B15001_048E","B15001_049E","B15001_050E","B15001_051E")]
  women25.34=eduattain[c("B15001_053E","B15001_054E","B15001_055E","B15001_056E","B15001_057E","B15001_058E","B15001_059E")]
  women35.44=eduattain[c("B15001_061E","B15001_062E","B15001_063E","B15001_064E","B15001_065E","B15001_066E","B15001_067E")]
  women45.64=eduattain[c("B15001_069E","B15001_070E","B15001_071E","B15001_072E","B15001_073E","B15001_074E","B15001_075E")]
  women65=eduattain[c("B15001_077E","B15001_078E","B15001_079E","B15001_080E","B15001_081E","B15001_082E","B15001_083E")]
  
  code=c("Less than 9th grade","9th to 12th grade, no diploma","High School Graduate","Some College, no degree","Associate's degree","Bachelor's Degree","Graduate or Professional Degree")
  
  educational.attainment=ifelse((syntheticdataset$sex=="Male"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24")),sample(code,1,prob=men18.24/sum(men18.24)),
                                ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(code,1,prob=men25.34/sum(men25.34)),
                                       ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="35 to 44"),sample(code,1,prob=men35.44/sum(men35.44)),
                                              ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"),sample(code,1,prob=men45.64/sum(men45.64)),
                                                     ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code,1,prob=men65/sum(men65)),
                                                            ifelse((syntheticdataset$sex=="Female"&(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24")),sample(code,1,prob=women18.24/sum(women18.24)),
                                                                   ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"),sample(code,1,prob=women25.34/sum(women25.34)),
                                                                          ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="35 to 44"),sample(code,1,prob=women35.44/sum(women35.44)),
                                                                                 ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64"),sample(code,1,prob=women45.64/sum(women45.64)),
                                                                                        ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code,1,prob=women65/sum(women65)),NA))))))))))
  syntheticdataset$education.attainment=educational.attainment
  return(syntheticdataset)
}



getemployment=function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  employment=read.csv("employment.csv")
  employment=employment[(employment$tract==tract)&(employment$county==county),]
  
  Women16.19=employment[c("B23001_091E","B23001_093E","B23001_094E","B23001_095E")]
  Women20.21=employment[c("B23001_098E","B23001_100E","B23001_101E","B23001_102E")]
  Women22.24=employment[c("B23001_105E","B23001_107E","B23001_108E","B23001_109E")]
  Women20.24=c(Women20.21$B23001_098E+Women22.24$B23001_105E,Women20.21$B23001_100E+Women22.24$B23001_107E,Women20.21$B23001_101E+Women22.24$B23001_108E,Women20.21$B23001_102E+Women22.24$B23001_109E)
  Women25.29=employment[c("B23001_112E","B23001_114E","B23001_115E","B23001_116E")]
  Women30.34=employment[c("B23001_119E","B23001_121E","B23001_122E","B23001_123E")]
  Women35.44=employment[c("B23001_126E","B23001_128E","B23001_129E","B23001_130E")]
  Women45.54=employment[c("B23001_133E","B23001_135E","B23001_136E","B23001_137E")]
  Women55.59=employment[c("B23001_140E","B23001_142E","B23001_143E","B23001_144E")]
  Women60.61=employment[c("B23001_147E","B23001_149E","B23001_150E","B23001_151E")]
  Women62.64=employment[c("B23001_154E","B23001_156E","B23001_157E","B23001_158E")]
  Women55.64=c(Women55.59$B23001_140E+Women60.61$B23001_147E+Women62.64$B23001_154E,Women55.59$B23001_142E+Women60.61$B23001_149E+Women62.64$B23001_156E,Women55.59$B23001_143E+Women60.61$B23001_150E+Women62.64$B23001_157E,Women55.59$B23001_144E+Women60.61$B23001_151E+Women62.64$B23001_158E)
  Women65.69=employment[c("B23001_161E","B23001_162E","B23001_163E")]
  Women70.74=employment[c("B23001_166E","B23001_167E","B23001_168E")]
  Women65.74=c(Women65.69$B23001_161E+Women70.74$B23001_166E,Women65.69$B23001_162E+Women70.74$B23001_167E,Women65.69$B23001_163E+Women70.74$B23001_168E)
  Women75=employment[c("B23001_171E","B23001_172E","B23001_173E")]
  Men16.19=employment[c("B23001_005E","B23001_007E","B23001_008E","B23001_009E")]
  Men20.21=employment[c("B23001_012E","B23001_014E","B23001_015E","B23001_016E")]
  Men22.24=employment[c("B23001_019E","B23001_021E","B23001_022E","B23001_023E")]
  Men20.24=c(Men20.21$B23001_012E+Men22.24$B23001_019E,Men20.21$B23001_014E+Men22.24$B23001_021E,Men20.21$B23001_015E+Men22.24$B23001_022E,Men20.21$B23001_016E+Men22.24$B23001_023E)
  Men25.29=employment[c("B23001_026E","B23001_028E","B23001_029E","B23001_030E")]
  Men30.34=employment[c("B23001_033E","B23001_035E","B23001_036E","B23001_037E")]
  Men35.44=employment[c("B23001_040E","B23001_042E","B23001_043E","B23001_044E")]
  Men45.54=employment[c("B23001_047E","B23001_049E","B23001_050E","B23001_051E")]
  Men55.59=employment[c("B23001_054E","B23001_056E","B23001_057E","B23001_058E")]
  Men60.61=employment[c("B23001_061E","B23001_063E","B23001_064E","B23001_065E")]
  Men62.64=employment[c("B23001_068E","B23001_070E","B23001_071E","B23001_072E")]
  Men55.64=c(Men55.59$B23001_054E+Men60.61$B23001_061E+Men62.64$B23001_068E,Men55.59$B23001_056E+Men60.61$B23001_063E+Men62.64$B23001_070E,Men55.59$B23001_057E+Men60.61$B23001_064E+Men62.64$B23001_071E,Men55.59$B23001_058E+Men60.61$B23001_065E+Men62.64$B23001_072E)
  Men65.69=employment[c("B23001_075E","B23001_076E","B23001_077E")]
  Men70.74=employment[c("B23001_080E","B23001_081E","B23001_082E")]
  Men65.74=c(Men65.69$B23001_075E+Men70.74$B23001_080E,Men65.69$B23001_076E+Men70.74$B23001_081E,Men65.69$B23001_077E+Men70.74$B23001_082E)
  Men75=employment[c("B23001_085E","B23001_086E","B23001_087E")]
  
  code1=c("In Armed Forces","Employed","Unemployed","Not in labor force")
  code2=c("Employed","Unemployed","Not in labor force")
  employment=ifelse((syntheticdataset$sex=="Male"&(syntheticdataset$age=="18 to 19")),sample(code1,1,prob=Men16.19/sum(Men16.19)), #syntheticdataset$age=="15 to 17"| is left out because of age problems :(
                    ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="20 to 24"),sample(code1,1,prob=Men20.24/sum(Men20.24)),
                                ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="25 to 29"),sample(code1,1,prob=Men25.29/sum(Men25.29)),
                                       ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="30 to 34"),sample(code1,1,prob=Men30.34/sum(Men30.34)),
                                              ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="35 to 44"),sample(code1,1,prob=Men35.44/sum(Men35.44)),
                                                     ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="45 to 54"),sample(code1,1,prob=Men45.54/sum(Men45.54)),
                                                     ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="55 to 64"),sample(code1,1,prob=Men55.64/sum(Men55.64)),
                                                            ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="65 to 74"),sample(code2,1,prob=Men65.74/sum(Men65.74)),
                                                     ifelse(syntheticdataset$sex=="Male"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code2,1,prob=Men75/sum(Men75)),
                                                            ifelse((syntheticdataset$sex=="Female"&(syntheticdataset$age=="18 to 19")),sample(code1,1,prob=Women16.19/sum(Women16.19)), #syntheticdataset$age=="15 to 17"| we've got age problems :(
                                                                   ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="20 to 24"),sample(code1,1,prob=Women20.24/sum(Women20.24)),
                                                                          ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="25 to 29"),sample(code1,1,prob=Women25.29/sum(Women25.29)),
                                                                                 ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="30 to 34"),sample(code1,1,prob=Women30.34/sum(Women30.34)),
                                                                                        ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="35 to 44"),sample(code1,1,prob=Women35.44/sum(Women35.44)),
                                                                                               ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="45 to 54"),sample(code1,1,prob=Women45.54/sum(Women45.54)),
                                                                                               ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="55 to 64"),sample(code1,1,prob=Women55.64/sum(Women55.64)),
                                                                                                      ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="65 to 74"),sample(code2,1,prob=Women65.74/sum(Women65.74)),
                                                                                                             ifelse(syntheticdataset$sex=="Female"&(syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85"),sample(code2,1,prob=Women75/sum(Women75)),NA))))))))))))))))))
  syntheticdataset$employment=employment
  return(syntheticdataset)
}


getdisability <- function(county,tract,syntheticdataset,seed){
  set.seed(seed)
  
  disability=read.csv("disability_status.csv")
  disability=disability[(disability$tract==tract)&(disability$county==county),]
  
  Under18=disability[c("C18108_003E","C18108_004E","C18108_005E")]
  From18.64=disability[c("C18108_007E","C18108_008E","C18108_009E")]
  Over65=disability[c("C18108_011E","C18108_012E","C18108_013E")]
  code=c("With One Type of Disability","With Two or More Types of Disabilities","No Disabilities")
  
  disability=ifelse(syntheticdataset$age=="Under 5"|syntheticdataset$age=="5 to 9"|syntheticdataset$age=="10 to 14"|syntheticdataset$age=="15 to 17",sample(code,1,prob=Under18/sum(Under18)),
         ifelse(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24"|syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"|syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64",sample(code,1,prob=From18.64/sum(From18.64)),
                ifelse(syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85",sample(code,1,prob=Over65/sum(Over65)),NA)))
  
  syntheticdataset$disability=disability
  return(syntheticdataset)
}

