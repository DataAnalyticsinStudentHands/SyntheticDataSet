roll8avg <- function (x) {
  y=(1:(length(x)))
  for (i in 1:(length(x)-2)){
    missing=0
    total=0
    validhours=0
    for (j in 1:8){
      a=x[i+j-1]
      if (is.na(a)==TRUE){
        missing=missing+1
      }
      else {
        total=total+a
        validhours=validhours+1
      }
    }
    if (missing<=2){
      y[i]=(total+(0.1*missing))/8
    }
    else {
      y[i]=NA
    }
  }
  y[length(x)]=NA
  y[length(x)-1]=NA
  y[length(x)-2]=NA
  y[length(x)-3]=NA
  y[length(x)-4]=NA
  y[length(x)-5]=NA
  return(y)
}
