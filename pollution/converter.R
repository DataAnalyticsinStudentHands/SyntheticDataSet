converter <- function (data) {
  data$time=as.POSIXct(data$epoch,origin="1970-01-01",tz="UTC")
  data$value=as.numeric(as.character(data$value))
  data=data[complete.cases(data),]
  data=subset(data, data$flag=="")
  return(data)
}