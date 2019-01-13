# This function converts the columns of Sam City to their appropriate class, which is either character or numeric

typeCheck <- function(sam){
  # These columns should be characters
  sam[c(1:3, 5:7, 9:20, 23, 25, 33:34, 36:38, 41, 44, 47, 51:52, 56:57, 63:64, 80:81)] = sapply(sam[c(1:3, 5:7, 9:20, 23, 25, 33:34, 36:38, 41, 44, 47, 51:52, 56:57, 63:64, 80:81)], as.character)
  
  # These columns ahould be numeric
  sam[c(4, 8, 21:22, 24, 26:32, 35, 39:40, 42:43, 45:46, 48:50, 53:55, 58:62, 65:79, 82:87)] = sapply(sam[c(4, 8, 21:22, 24, 26:32, 35, 39:40, 42:43, 45:46, 48:50, 53:55, 58:62, 65:79, 82:87)], as.numeric)

  # Return the updated model
  return(sam)
}
