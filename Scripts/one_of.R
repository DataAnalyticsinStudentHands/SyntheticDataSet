# The dplyr package must be installed and loaded for the function to work
library(dplyr)

# This function numbers the rows in the model by powers of ten
one_of <- function(sam){
  sam <-  mutate(sam
      one_of = case_when(
          1:n() %% 10000 == 0 ~ 10000,
          1:n() %% 1000 == 0 ~ 1000,
          1:n() %% 100 == 0 ~ 100,
          1:n() %% 10 == 0 ~ 10,
          TRUE ~ 1)
  )
  
  return(sam)
}
