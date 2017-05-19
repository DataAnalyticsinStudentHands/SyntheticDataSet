hourlyaverage <- function (data) {
  #Hourlyaverages
  hourlyaverages=aggregate(data$value,list(hourofday=cut(data$time,breaks="hour"),site=data$site,region=data$region,param_id=data$param_id),mean)
  hourlyaverages$hourofday=as.POSIXct(hourlyaverages$hourofday,tz="UTC")
  hourlyaverages$value=as.numeric(as.character(hourlyaverages$x))
  #Hourly Valid
  hourlyvalid=aggregate(data$value,list(hourofday=cut(data$time,breaks="hour"),site=data$site,region=data$region,param_id=data$param_id),length)
  hourlyvalid$hourofday=as.POSIXct(hourlyaverages$hourofday,tz="UTC")
  hourlyaverages$numberofmeasurements=hourlyvalid$x
  hourlyaverages$value[which(hourlyaverages$numberofmeasurements<=8)]<-NA
  return(hourlyaverages)
}