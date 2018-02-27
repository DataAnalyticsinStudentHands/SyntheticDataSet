#Census Mapping Variables

##The U.S. Census provides html tables with the name and header for each variable
##These can be converted to csv tables with a tool like Chrome extension Table Capture

##This script loads this csv file and uses it to map variables to their table names
options(stringsAsFactors = FALSE)
Table=read.csv("US_Census_VARS_2014.csv",strip.white = TRUE)
Table=Table[,1:2]
colnames(Table)=c("code","variable")

Table=Table[!grepl("Margin Of Error For!!", Table$variable),]

Table$variable=sub(":!!","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table$variable=sub(" ","_",Table$variable)
Table=Table[1:31788,]

Table2=as.data.frame(t(Table$code))
colnames(Table2)=paste0("var_",Table$variable)

attach(Table2)

