maxhr <- function (hourlyavg) {
  #Max Hour
  maxhour=aggregate(hourlyavg$value,list(day=cut(hourlyavg$hourofday,breaks="day"),site=hourlyavg$site,region=hourlyavg$region,param_id=hourlyavg$param_id),max)
  maxhour$hour=hourlyavg$hourofday[match(maxhour$x,hourlyavg$value)]
  maxhourvalid=aggregate(hourlyavg$value,list(day=cut(hourlyavg$hourofday,breaks="day"),site=hourlyavg$site,region=hourlyavg$region,param_id=hourlyavg$param_id),length)
  maxhour$numberofmeasurements=maxhourvalid$x
  maxhour=subset(maxhour,maxhour$numberofmeasurements>=18)
  return(maxhour)
}
  