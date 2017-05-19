airpollutionstats <- function (data,region=NA,pollutants=NA,AQS.Code=NA,stats=NA) {
  source('converter.R')
  source('hourlyaverage.R')
  source('maxhour.R')
  source('roll8avg.R')
  data=converter(data)
  if (is.na(AQS.Code)==TRUE) {
    if (is.na(region)==TRUE){
      AQS.Code=unique(data$AQS.Code)
      if(!"AQS.Code"%in% colnames(data)){
        AQS.Code=unique(data$site)
      }
    }
    if (is.na(region)==FALSE){
      sites <- vector(mode="numeric", length=0)
      if (is.element(10,region)==TRUE){
        AQS.Code=c(AQS.Code,480055013,482450009,482450011,482450014,482450017,482450018,482450019,482450021,482450022,482450101,482450102,482450628,482451035,483611001,483611100)
      }
      if (is.element(11,region)==TRUE){
        AQS.Code=c(AQS.Code,480210684,480535009,480551604,481490001,482090614,482091675,484530014,484530020,484530021,484530326,484531068,484531603,484531605,484535001,484535003,484910690,484916602)
      }
      if (is.element(9,region)==TRUE){
        AQS.Code=c(AQS.Code,480271045,480271047,480415011,483091037,483095010)
      }
      if (is.element(13,region)==TRUE){
        AQS.Code=c(AQS.Code,480290032,480290051,480290052,480290053,480290055,480290059,480290060,480290501,480290502,480290622,480290623,480290625,480290626,480290676,480290677,480291069,480910503,480910505,481870504,481870506,481875004,482551070,484931038)
      }
      if (is.element(5,region)==TRUE){
        AQS.Code=c(AQS.Code,480370004,480371031,481830001,482030002,484230007)
      }
      if (is.element(12,region)==TRUE){
        AQS.Code=c(AQS.Code,480390618,480391003,480391004,480391012,480391016,480710013,480711606,481570696,481670004,481670005,481670056,481670571,481670615,481670616,481670621,481670683,481670697,481671034,481675005,482010024,482010026,482010029,482010036,482010046,482010047,482010051,482010055,482010057,482010058,482010060,482010061,482010062,482010066,482010069,482010071,482010075,482010307,482010416,482010551,482010552,482010553,482010554,482010556,482010557,482010558,482010559,482010560,482010561,482010562,482010563,482010570,482010572,482010617,482010669,482010670,482010671,482010673,482010695,482010803,482011015,482011017,482011034,482011035,482011039,482011042,482011043,482011049,482011050,48201105,482011066,48201600,48291069,483390078,483390698,483395006,484715012)
      }
      if (is.element(6,region)==TRUE){
        AQS.Code=c(AQS.Code,480430101,481095018,481410029,481410037,481410038,481410044,481410047,481410054,481410055,481410057,481410058,481410693,481411021)
      }
      if (is.element(15,region)==TRUE){
        AQS.Code=c(AQS.Code,480610006,480611023,480612004,482150043,482151046)
      }
      if (is.element(1,region)==TRUE){
        AQS.Code=c(AQS.Code,480650004,480650005,480650007,483750024,483750320,483751025)
      }
      if (is.element(4,region)==TRUE){
        AQS.Code=c(AQS.Code,480850003,480850005,480850007,480850009,480850029,480971504,481130018,481130050,481130061,481130069,481130075,481130087,481131067,481131500,481131505,481210034,481211007,481211013,481211032,481215008,481390016,481391044,482210001,482311006,482510003,482511008,482511063,482511501,482570005,482570020,483491051,483495014,483631502,483670081,483671506,483970001,484390075,484391002,484391006,484391009,484391018,484391053,484391062,484391065,484391503,484392003,484393009,484393010,484393011,484395007,484970088,484971064)
      }
      if (is.element(14,region)==TRUE){
        AQS.Code=c(AQS.Code,481231602,481750624,482730314,483550025,483550026,483550029,483550032,483550034,483550041,483550083,483550660,483550664,483551024,484090659,484090685,484090686,484090687,484690003,484690609)
      }
      if (is.element(7,region)==TRUE){
        AQS.Code=c(AQS.Code,481350003,481351014)
      }
      if (is.element(16,region)==TRUE){
        AQS.Code=c(AQS.Code,483230004,484655017,484790016,484790017,484790313)
      }
      if (is.element(3,region)==TRUE){
        AQS.Code=c(AQS.Code,483371507,484411509,484415015,484851508)
      }
      if (is.element(8,region)==TRUE){
        AQS.Code=c(AQS.Code,484515016)
      }
      
    }
    }
  if (is.na(pollutants)==FALSE){
    poll<-vector(mode="numeric",length=0)
    if (is.element("CO", pollutants)==TRUE){
      poll=c(poll,42101)}
    if (is.element("SO2",pollutants)==TRUE){
      poll=c(poll,42401)}
    if (is.element("NO",pollutants)==TRUE){
      poll=c(poll,42601)
    }
    if (is.element("NO2",pollutants)==TRUE){
      poll=c(poll,42602)
    }
    if (is.element("NOx",pollutants)==TRUE){
      poll=c(poll,42603)
    }
    if (is.element("O3",pollutants)==TRUE){
      poll=c(poll,44291)
    }
    if (is.element("PM",pollutants)==TRUE){
      poll=c(poll,88502,88101)
    }
    if (is.element("Temperature",pollutants)==TRUE){
      poll=c(poll,62101)
    }
    if (is.element("Wind Speed",pollutants)==TRUE){
      poll=c(poll,61103)
    }
    if (is.element("Wind Direction",pollutants)==TRUE){
      poll=c(poll,61104)
    }
    if (is.element("Humidity",pollutants)==TRUE){
      poll=c(poll,62201)
    }
    if (is.element("Solar Radiation",pollutants)==TRUE){
      poll=c(poll,63301)
    }
    pollutants=poll
    } 
  if (is.na(pollutants)==TRUE){
    pollutants=unique(data$param_id)
  }
  if (is.na(stats==TRUE)){
    stats=c('maxhr','eighthrrolling','eighthrmax')
  }
  if(!"AQS.Code"%in% colnames(data)){
    AQS.Code[AQS.Code==481490001]=1
    AQS.Code[AQS.Code==800060011]=10
    AQS.Code[AQS.Code==484970088]=100
    AQS.Code[AQS.Code==480290626]=1001
    AQS.Code[AQS.Code==481410055]=1015
    AQS.Code[AQS.Code==482010060]=1016
    AQS.Code[AQS.Code==480290059]=1020
    AQS.Code[AQS.Code==482010553]=1022
    AQS.Code[AQS.Code==483550660]=1029
    AQS.Code[AQS.Code==800060007]=1034
    AQS.Code[AQS.Code==800060007]=1036
    AQS.Code[AQS.Code==483550660]=1049
    AQS.Code[AQS.Code==481211013]=108
    AQS.Code[AQS.Code==481231602]=109
    AQS.Code[AQS.Code==481231602]=110
    AQS.Code[AQS.Code==484415015]=114
    AQS.Code[AQS.Code==800060007]=139
    AQS.Code[AQS.Code==800060011]=145
    AQS.Code[AQS.Code==800060007]=146
    AQS.Code[AQS.Code==800060011]=147
    AQS.Code[AQS.Code==480535009]=15
    AQS.Code[AQS.Code==800060007]=150
    AQS.Code[AQS.Code==481231602]=152
    AQS.Code[AQS.Code==800060011]=154
    AQS.Code[AQS.Code==800060011]=1615
    AQS.Code[AQS.Code==800060011]=1616
    AQS.Code[AQS.Code==800060011]=1621
    AQS.Code[AQS.Code==484970088]=165
    AQS.Code[AQS.Code==484971064]=166
    AQS.Code[AQS.Code==482010058]=167
    AQS.Code[AQS.Code==484655017]=169
    AQS.Code[AQS.Code==481410055]=18
    AQS.Code[AQS.Code==481231602]=181
    AQS.Code[AQS.Code==481490001]=2003
    AQS.Code[AQS.Code==484655017]=22
    AQS.Code[AQS.Code==482010058]=235
    AQS.Code[AQS.Code==800060011]=240
    AQS.Code[AQS.Code==800060007]=243
    AQS.Code[AQS.Code==800060011]=245
    AQS.Code[AQS.Code==484393009]=26
    AQS.Code[AQS.Code==481490001]=309
    AQS.Code[AQS.Code==481131067]=34
    AQS.Code[AQS.Code==481211013]=35
    AQS.Code[AQS.Code==484655017]=404
    AQS.Code[AQS.Code==800060007]=405
    AQS.Code[AQS.Code==800060007]=406
    AQS.Code[AQS.Code==482450014]=407
    AQS.Code[AQS.Code==800060007]=408
    AQS.Code[AQS.Code==483550660]=409
    AQS.Code[AQS.Code==484531603]=410
    AQS.Code[AQS.Code==800060007]=411
    AQS.Code[AQS.Code==800060011]=416
    AQS.Code[AQS.Code==800060011]=45
    AQS.Code[AQS.Code==800060007]=48
    
    
  }
  hourlyaverages=data.frame()
  maxhr=data.frame()
  eighthrrolling=data.frame()
  eighthrmax=data.frame()
    for (a in (1:length(AQS.Code))){
      for (b in (1:length(pollutants))){
        if("AQS.Code"%in% colnames(data)){
          c=subset(data,data$AQS.Code==AQS.Code[a])
        }
        if(!"AQS.Code"%in% colnames(data)){
          c=subset(data,data$site==AQS.Code[a])
        }
        c=subset(data,data$param_id==pollutants[b])
        if (nrow(c)>0) {
          hrlyaverages=hourlyaverage(c)
          hourlyaverages=rbind(hourlyaverages,hrlyaverages)
          if (is.element('maxhr',stats)==TRUE){
            maxhr=rbind(maxhr,maxhr(hrlyaverages))
          }
          if (is.element('eighthrrolling',stats)==TRUE){
            hrlyaverages$value=as.numeric(as.character(hrlyaverages$value))
            hrlyaverages$roll8avg=roll8avg(hrlyaverages$value)
            hrlyaverages$value=NULL
            hrlyaverages$x=NULL
            hrlyaverages$numberofmeasurements=NULL
            eighthrrolling=rbind(eighthrrolling,hrlyaverages)
          }
          if (is.element('eighthrmax',stats)==TRUE){
            hrlyaverages$hourofday=as.POSIXct(hrlyaverages$hourofday,tz="UTC")
            hrlyaverages$hour=as.numeric(format(hrlyaverages$hourofday, format="%H"))
            a=subset(hrlyaverages,hrlyaverages$hour<=16)
            moving8hourmax=aggregate(a$roll8avg,list(day=cut(a$hourofday,breaks="day"),site=a$site,param_id=a$param_id,region=a$region),max)
            moving8hourmax$hour=a$hourofday[match(moving8hourmax$x,a$roll8avg)]
            eighthrmax=rbind(eighthrmax,moving8hourmax)
          }
        }
        
      }
    }
  return(list(hourlyaverages,maxhr,eighthrrolling,eighthrmax))
}
