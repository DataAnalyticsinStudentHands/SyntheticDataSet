# This function tests if there are too many NAs in the Sam City model than there are suppossed to be

naTest <- function(sam){
  naCheck=data.frame()
  
  #T These are the indices that will be tested
  index = c(2:30)

  naCheck = as.data.frame(do.call(rbind, lapply(index, function(x){
    column = colnames(sam[x])
    
    # Find how many NAs are actually in this column from the model
    actual.NA = sum(is.na(sam[x]))
    
    # Find how many NAs there should be in the column
    expected.NA = switch(column,
                         "household.type"=, "householder"=, "size"=, "member"=, "sex"=, "bracket.age"=, "age"=, "race"=, "disability"=, "veteran.status"=, "bracket.household.income"=, "household.income"=, "health.insurance"=, "state"=, "county"=, "tract"=, "householdID"=, "individualID"= 0,
                         "number.of.vehicles" = nrow(sam[sam$household.type=="Group Quarters" & sam$employment != "Employed",]),
                         "school.enrollment"=, "nativity"=, "English.speaking.skills"=, "citizenship"=, "Language.at.home"= nrow(sam[sam$bracket.age == "0.to.4",]),
                         "educational.attainment"= nrow(sam[sam$bracket.age %in% c("0.to.4", "5.to.9", "10.to.14", "15.to.17"),]),
                         "employment"= nrow(sam[sam$age < 16,]),
                         "means.of.transportation.to.work"= nrow(sam[sam$employment != "Employed",]),
                         "bracket.travel.time.to.work"=, "travel.time.to.work" = nrow(sam[sam$means.of.transportation.to.work == "worked at home",]))

    # Compare the actual and expected number of NAs. If they are equal, return 0, otherwise return 1.
    if(actual.NA != expected.NA){
      flag = 1
    }else{
      flag = 0
    }

    # Store all this info into a data frame
    newRow= data.frame(variable=column, Expected_NAs=expected.NA, Actual_NAs=actual.NA, Flag=flag)
    return(newRow)
  })))
  
  return(naCheck)
}



