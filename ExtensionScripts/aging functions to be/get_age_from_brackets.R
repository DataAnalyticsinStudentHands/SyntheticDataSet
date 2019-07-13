#As ages were created in brackets from the Census Data if the model is new from citymodels and hasn't been aged
#an age for each individual may need to be sampled from the brackets

get_age_from_brackets<-function(seed,age_bracket){
  set.seed(seed)
  age=ifelse(age_bracket=="Under 5",sample(c(0:4),1),
             ifelse(age_bracket=="5 to 9",sample(c(5:9),1),
                    ifelse(age_bracket=="10 to 14",sample(c(10:14),1),
                           ifelse(age_bracket=="15 to 17",sample(c(15:17),1),
                                  ifelse(age_bracket=="18 to 19",sample(c(18:19),1),
                                         ifelse(age_bracket=="20 to 24",sample(c(20:24),1),
                                                ifelse(age_bracket=="25 to 29",sample(c(25:29),1),
                                                       ifelse(age_bracket=="30 to 34",sample(c(30:34),1),
                                                              ifelse(age_bracket=="35 to 44",sample(c(35:44),1),
                                                                     ifelse(age_bracket=="45 to 54",sample(c(45:54),1),
                                                                            ifelse(age_bracket=="55 to 64",sample(c(55:64),1),
                                                                                   ifelse(age_bracket=="65 to 74",sample(c(65:74),1),
                                                                                          ifelse(age_bracket=="75 to 84",sample(c(75:84),1),
                                                                                                 ifelse(age_bracket=="Over 85",sample(c(85:100),1),
                                                                                                        NA))))))))))))))
  return(age)
}