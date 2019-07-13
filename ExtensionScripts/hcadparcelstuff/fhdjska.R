sites$siteID=paste(substr(sites$AQS.Code,1,2),"_",substr(sites$AQS.Code,3,5),"_",substr(sites$AQS.Code,6,9),sep="")
forDrLindner=data.frame(sites$siteID,sites$Site.Name,sites$Longitude,sites$Latitude,sites$Reg.,sites$Activation.Date,sites$Deactivation.Date)
colnames(forDrLindner)=c("Site ID","Site Name","Longitude","Latitude","Region","Activation Date","Deactivation Date")
write.csv(forDrLindner,"sites.csv")