data=read.csv('2007_small.csv')

source('airpollutionstats.R')

measurements=airpollutionstats(data)

hourlyaverages2011=measurements[1]
maxhr2011=measurements[2]
eighthrrolling2011=measurements[3]
eighthrmax2011=measurements[4]

write.csv(hourlyaverages2011,"hourlyaverages2007.csv")
write.csv(maxhr2011,"maxhr2007.csv")
write.csv(eighthrrolling2011,"eighthrrolling2007.csv")
write.csv(eighthrmax2011,"eighthrmax2007.csv")

sitelist=read.table("sitelist.txt", sep="\t")
sitelist=read.csv("sitelist.csv")
