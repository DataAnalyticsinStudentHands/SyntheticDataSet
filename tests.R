library(data.table)

#' suite of tests for data tables to test symmetries by comparing outputs to each other or other expected
#'
#' census_table_check looks for basic errors in the census tables while still wide format
#' symmetry_test compares outputs of table calls
#'
#' @param dt data.table in wide census format with rows of labels and columns of geographic units (for example, tract or block group).
#' @param total_concept character string that matches census concept for row with overall totals.
#' @param term_concept character string that indicates individual granularity (for example, individual or household)
#' @param mult integer for how many times to multiply the total row to equal the rest of the rows (usually 2).
#' @param dt1 data.table with long data structure (for example, by households or individuals).
#' @param dt2 - rows need to be same granularity as dt1; column names don't need to be same, but expected outcomes should be.
#' @param vars1 - vector (or list?) that should contain the column names in correct order
#' @param vars2 - vector (or list?) that should contain the column names in correct order
#' @param limits_dt1 - character string with i as limitiation (for example, "race="A"")
#' @param limits_dt2 - character string with i as limitiation (for example, "race="A"")
#' Prints to log - should it also return?
#' @return ??

census_table_check <- function(dt, total_concept, term_concept, total_label="Total", mult=2){
  percent_na <- dt[,sum(is.na(.SD))] / 
    (dt[,sum(!is.na(.SD))]+dt[,sum(is.na(.SD))])
  #multiplying the side labeled only "Total" by mult:
  test1 <- colSums(dt[label==total_label,4:ncol(dt)])*mult ==
    colSums(dt[label!=total_label,4:ncol(dt)])
  #see if any of the tests don't match; if false, need to go back and check on what happened
  totals <- dt[label==total_label&concept==total_concept,4:ncol(dt)]
  if(nrow(totals)==0){totals <- dt[label==total_label,4:ncol(dt)]}
  check_file <- paste0("The file has ",as.integer(100*percent_na),"% of NAs")
  check_totals <- paste0("The internal totals sum incorrectly in ",as.integer(length(test1[test1==F])/length(test1)*100), "% of cases")
  total_num <- paste0("There are ",sum(totals[,])," ",term_concept)
  check_summary <- paste(check_file,check_totals,total_num,sep="\n")
  return(check_summary) #have to think about mechanism for tests that fail
}

# not doing something basic correctly...
#> vars1 <- c("geoid")
#> dt1[,vars1]
#Error in `[.data.table`(dt1, , vars1) : 
#  j (the 2nd argument inside [...]) is a single symbol but column name 'vars1' is not found. Perhaps you intended DT[, ..vars1]. This difference to data.frame is deliberate and explained in FAQ 1.1.
#> dt1[,eval(vars1)]
#[1] "geoid"
symmetry_test <- function(dt1,dt2,vars1,vars2,limits_dt1='',limits_dt2=''){
  #sapply to append to a character string?
  test <- table(
    dt1[limits_dt1,vars1] #some way of indexing them??
  )==table(
    dt2[limits_dt2,vars2]
  )
  test <- table1==table2
  paste0("Test failed ", as.integer(length(test[test==F])/length(test)*100), "% of table comparison tests")
  return(test)
}