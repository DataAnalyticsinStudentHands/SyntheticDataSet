####COME BACK TO Census_data AND LOOK FOR WORK AROUND ON AGE 15!!!!!!!!!!!!!!!!!!!!


getschoolenrollment <- function(county,tract,syntheticdataset,seed,Census_data){
  
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  Women5.9=Census_data[c("public.school.women.5.to.9","private.school.women.5.to.9","no.school.women.5.to.9")]
  Women10.14=Census_data[c("public.school.women.10.to.14","private.school.women.10.to.14","no.school.women.10.to.14")]
  Women15.17=Census_data[c("public.school.women.15.to.17","private.school.women.15.to.17","no.school.women.15.to.17")]
  Women18.19=Census_data[c("public.school.women.18.to.19","private.school.women.18.to.19","no.school.women.18.to.19")]
  Women20.24=Census_data[c("public.school.women.20.to.24","private.school.women.20.to.24","no.school.women.20.to.24")]
  Women25.34=Census_data[c("public.school.women.25.to.34","private.school.women.25.to.34","no.school.women.25.to.34")]
  Women35=Census_data[c("public.school.women.over.35","private.school.women.over.35","no.school.women.over.35")]
  
  Men5.9=Census_data[c("public.school.men.5.to.9","private.school.men.5.to.9","no.school.men.5.to.9")]
  Men10.14=Census_data[c("public.school.men.10.to.14","private.school.men.10.to.14","no.school.men.10.to.14")]
  Men15.17=Census_data[c("public.school.men.15.to.17","private.school.men.15.to.17","no.school.men.15.to.17")]
  Men18.19=Census_data[c("public.school.men.18.to.19","private.school.men.18.to.19","no.school.men.18.to.19")]
  Men20.24=Census_data[c("public.school.men.20.to.24","private.school.men.20.to.24","no.school.men.20.to.24")]
  Men25.34=Census_data[c("public.school.men.25.to.34","private.school.men.25.to.34","no.school.men.25.to.34")]
  Men35=Census_data[c("public.school.men.over.35","private.school.men.over.35","no.school.men.over.35")]
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



geteducationattainment=function(county,tract,syntheticdataset,seed,Census_data){
  
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  men18.24=Census_data[c("less.than.9.grade.men.18.24","btwn.9.to.12.grade.men.18.24","high.school.men.18.24","some.college.men.18.24","associates.men.18.24","bachelors.men.18.24","post.grad.men.18.24")]
  men25.34=Census_data[c("less.than.9.grade.men.25.34","btwn.9.to.12.grade.men.25.34","high.school.men.25.34","some.college.men.25.34","associates.men.25.34","bachelors.men.25.34","post.grad.men.25.34")]
  men35.44=Census_data[c("less.than.9.grade.men.35.44","btwn.9.to.12.grade.men.35.44","high.school.men.35.44","some.college.men.35.44","associates.men.35.44","bachelors.men.35.44","post.grad.men.35.44")]
  men45.64=Census_data[c("less.than.9.grade.men.45.64","btwn.9.to.12.grade.men.45.64","high.school.men.45.64","some.college.men.45.64","associates.men.45.64","bachelors.men.45.64","post.grad.men.45.64")]
  men65=Census_data[c("less.than.9.grade.men.over.65","btwn.9.to.12.grade.men.over.65","high.school.men.over.65","some.college.men.over.65","associates.men.over.65","bachelors.men.over.65","post.grad.men.over.65")]
  
  women18.24=Census_data[c("less.than.9.grade.women.18.24","btwn.9.to.12.grade.women.18.24","high.school.women.18.24","some.college.women.18.24","associates.women.18.24","bachelors.women.18.24","post.grad.women.18.24")]
  women25.34=Census_data[c("less.than.9.grade.women.25.34","btwn.9.to.12.grade.women.25.34","high.school.women.25.34","some.college.women.25.34","associates.women.25.34","bachelors.women.25.34","post.grad.women.25.34")]
  women35.44=Census_data[c("less.than.9.grade.women.35.44","btwn.9.to.12.grade.women.35.44","high.school.women.35.44","some.college.women.35.44","associates.women.35.44","bachelors.women.35.44","post.grad.women.35.44")]
  women45.64=Census_data[c("less.than.9.grade.women.45.64","btwn.9.to.12.grade.women.45.64","high.school.women.45.64","some.college.women.45.64","associates.women.45.64","bachelors.women.45.64","post.grad.women.45.64")]
  women65=Census_data[c("less.than.9.grade.women.over.65","btwn.9.to.12.grade.women.over.65","high.school.women.over.65","some.college.women.over.65","associates.women.over.65","bachelors.women.over.65","post.grad.women.over.65")]
  
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



getemployment=function(county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  Women16.19=Census_data[c("in.armed.forces.women.16.19","employed.women.16.19","unemployed.women.16.19","not.in.labor.forces.women.16.19")]
  Women20.21=Census_data[c("in.armed.forces.women.20.21","employed.women.20.21","unemployed.women.20.21","not.in.labor.forces.women.20.21")]
  Women22.24=Census_data[c("in.armed.forces.women.22.24","employed.women.22.24","unemployed.women.22.24","not.in.labor.forces.women.22.24")]
  Women20.24=c(Women20.21$in.armed.forces.women.20.21+Women22.24$in.armed.forces.22.24,Women20.21$employed.women.20.21+Women22.24$employed.women.22.24,Women20.21$unemployed.women.20.21+Women22.24$unemployed.women.22.24,Women20.21$not.in.labor.forces.women.20.21+Women22.24$not.in.labor.forces.women.22.24)
  Women25.29=Census_data[c("in.armed.forces.women.25.29","employed.women.25.29","unemployed.women.25.29","not.in.labor.forces.women.25.29")]
  Women30.34=Census_data[c("in.armed.forces.women.30.34","employed.women.30.34","unemployed.women.30.34","not.in.labor.forces.women.30.34")]
  Women35.44=Census_data[c("in.armed.forces.women.35.44","employed.women.35.44","unemployed.women.35.44","not.in.labor.forces.women.35.44")]
  Women45.54=Census_data[c("in.armed.forces.women.45.54","employed.women.45.54","unemployed.women.45.54","not.in.labor.forces.women.45.54")]
  Women55.59=Census_data[c("in.armed.forces.women.55.59","employed.women.55.59","unemployed.women.55.59","not.in.labor.forces.women.55.59")]
  Women60.61=Census_data[c("in.armed.forces.women.60.61","employed.women.60.61","unemployed.women.60.61","not.in.labor.forces.women.60.61")]
  Women62.64=Census_data[c("in.armed.forces.women.62.64","employed.women.62.64","unemployed.women.62.64","not.in.labor.forces.women.62.64")]
  Women55.64=c(Women55.59$in.armed.forces.women.55.59+Women60.61$in.armed.forces.women.60.61+Women62.64$in.armed.forces.women.62.64,Women55.59$employed.women.55.59+Women60.61$employed.women.60.61+Women62.64$employed.women.62.64,Women55.59$unemployed.women.55.59+Women60.61$unemployed.women.60.61+Women62.64$unemployed.women.62.64,Women55.59$not.in.labor.forces.women.55.59+Women60.61$not.in.labor.forces.women.60.61+Women62.64$not.in.labor.forces.women.62.64)
  Women65.69=Census_data[c("employed.women.65.69","unemployed.women.65.69","not.in.labor.forces.women.65.69")]
  Women70.74=Census_data[c("employed.women.70.74","unemployed.women.70.74","not.in.labor.forces.women.70.74")]
  Women65.74=c(Women65.69$employed.women.65.69+Women70.74$employed.women.70.74,Women65.69$unemployed.65.69+Women70.74$unemployed.70.74,Women65.69$not.in.labor.forces.women.65.69+Women70.74$not.in.labor.forces.women.70.74)
  Women75=Census_data[c("employed.women.over.75","unemployed.women.over.75","not.in.labor.forces.women.over.75")]
  
  Men16.19=Census_data[c("in.armed.forces.men.16.19","employed.men.16.19","unemployed.men.16.19","not.in.labor.forces.men.16.19")]
  Men20.21=Census_data[c("in.armed.forces.men.20.21","employed.men.20.21","unemployed.men.20.21","not.in.labor.forces.men.20.21")]
  Men22.24=Census_data[c("in.armed.forces.men.22.24","employed.men.22.24","unemployed.men.22.24","not.in.labor.forces.men.22.24")]
  Men20.24=c(Men20.21$in.armed.forces.men.20.21+Men22.24$in.armed.forces.22.24,Men20.21$employed.men.20.21+Men22.24$employed.men.22.24,Men20.21$unemployed.men.20.21+Men22.24$unemployed.men.22.24,Men20.21$not.in.labor.forces.men.20.21+Men22.24$not.in.labor.forces.men.22.24)
  Men25.29=Census_data[c("in.armed.forces.men.25.29","employed.men.25.29","unemployed.men.25.29","not.in.labor.forces.men.25.29")]
  Men30.34=Census_data[c("in.armed.forces.men.30.34","employed.men.30.34","unemployed.men.30.34","not.in.labor.forces.men.30.34")]
  Men35.44=Census_data[c("in.armed.forces.men.35.44","employed.men.35.44","unemployed.men.35.44","not.in.labor.forces.men.35.44")]
  Men45.54=Census_data[c("in.armed.forces.men.45.54","employed.men.45.54","unemployed.men.45.54","not.in.labor.forces.men.45.54")]
  Men55.59=Census_data[c("in.armed.forces.men.55.59","employed.men.55.59","unemployed.men.55.59","not.in.labor.forces.men.55.59")]
  Men60.61=Census_data[c("in.armed.forces.men.60.61","employed.men.60.61","unemployed.men.60.61","not.in.labor.forces.men.60.61")]
  Men62.64=Census_data[c("in.armed.forces.men.62.64","employed.men.62.64","unemployed.men.62.64","not.in.labor.forces.men.62.64")]
  Men55.64=c(Men55.59$in.armed.forces.men.55.59+Men60.61$in.armed.forces.men.60.61+Men62.64$in.armed.forces.men.62.64,Men55.59$employed.men.55.59+Men60.61$employed.men.60.61+Men62.64$employed.men.62.64,Men55.59$unemployed.men.55.59+Men60.61$unemployed.men.60.61+Men62.64$unemployed.men.62.64,Men55.59$not.in.labor.forces.men.55.59+Men60.61$not.in.labor.forces.men.60.61+Men62.64$not.in.labor.forces.men.62.64)
  Men65.69=Census_data[c("employed.men.65.69","unemployed.men.65.69","not.in.labor.forces.men.65.69")]
  Men70.74=Census_data[c("employed.men.70.74","unemployed.men.70.74","not.in.labor.forces.men.70.74")]
  Men65.74=c(Men65.69$employed.men.65.69+Men70.74$employed.men.70.74,Men65.69$unemployed.65.69+Men70.74$unemployed.70.74,Men65.69$not.in.labor.forces.men.65.69+Men70.74$not.in.labor.forces.men.70.74)
  Men75=Census_data[c("employed.men.over.75","unemployed.men.over.75","not.in.labor.forces.men.over.75")]
  
  code1=c("In Armed Forces","Employed","Unemployed","Not in labor force")
  code2=c("Employed","Unemployed","Not in labor force")
  Census_data=ifelse((syntheticdataset$sex=="Male"&(syntheticdataset$age=="18 to 19")),sample(code1,1,prob=Men16.19/sum(Men16.19)), #syntheticdataset$age=="15 to 17"| is left out because of age problems :(
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
  syntheticdataset$Census_data=Census_data
  return(syntheticdataset)
}


getdisability <- function(county,tract,syntheticdataset,seed,Census_data){
  set.seed(seed)
  
  Census_data=Census_data[(Census_data$county==county) & (Census_data$tract==tract),]
  
  Under18=Census_data[c("under18.1.disability","under18.2.disability","under18.no.disability")]
  From18.64=Census_data[c("from18.64.1.disability","from18.64.2.disability","from18.64.no.disability")]
  Over65=Census_data[c("over65.1.disability","over65.2.disability","over65.no.disability")]
  code=c("With One Type of Disability","With Two or More Types of Disabilities","No Disabilities")
  
  disability=ifelse(syntheticdataset$age=="Under 5"|syntheticdataset$age=="5 to 9"|syntheticdataset$age=="10 to 14"|syntheticdataset$age=="15 to 17",sample(code,1,prob=Under18/sum(Under18)),
         ifelse(syntheticdataset$age=="18 to 19"|syntheticdataset$age=="20 to 24"|syntheticdataset$age=="25 to 29"|syntheticdataset$age=="30 to 34"|syntheticdataset$age=="35 to 44"|syntheticdataset$age=="45 to 54"|syntheticdataset$age=="55 to 64",sample(code,1,prob=From18.64/sum(From18.64)),
                ifelse(syntheticdataset$age=="65 to 74"|syntheticdataset$age=="75 to 84"|syntheticdataset$age=="Over 85",sample(code,1,prob=Over65/sum(Over65)),NA)))
  
  syntheticdataset$disability=disability
  return(syntheticdataset)
}

