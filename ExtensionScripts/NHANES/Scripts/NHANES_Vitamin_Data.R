forboxplot <- data.frame(Vitamin_B6 = NHANESDiabetesFinal3$`Vitamin B6`, Vitamin_B1 = NHANESDiabetesFinal3$`Vitamin B1`, Vitamin_B2 = NHANESDiabetesFinal3$`Vitamin B2`,Vitamin_C = NHANESDiabetesFinal3$`Vitamin C`, Vitamin_D = NHANESDiabetesFinal3$`Vitamin D`, sex = NHANESDiabetesFinal3$sex, household_income = NHANESDiabetesFinal3$household.income, race = NHANESDiabetesFinal3$race)
ggplot(forboxplot, aes(x = race, y=Vitamin_D)) + geom_boxplot()
ggplot(forboxplot, aes(x = race, y=Vitamin_B1)) + geom_boxplot()
ggplot(forboxplot, aes(x = race, y=Vitamin_B2)) + geom_boxplot()
ggplot(forboxplot, aes(x = race, y=VitaminB6)) + geom_boxplot()
ggplot(forboxplot, aes(x = race, y=Vitamin_C)) + geom_boxplot()

ggplot(forboxplot, aes(x = sex, y=Vitamin_D)) + geom_boxplot()
ggplot(forboxplot, aes(x = sex, y=Vitamin_B1)) + geom_boxplot()
ggplot(forboxplot, aes(x = sex, y=Vitamin_B2)) + geom_boxplot()
ggplot(forboxplot, aes(x = sex, y=VitaminB6)) + geom_boxplot()
ggplot(forboxplot, aes(x = sex, y=Vitamin_C)) + geom_boxplot()

ggplot(forboxplot, aes(x = household_income, y=Vitamin_D)) + geom_boxplot()
ggplot(forboxplot, aes(x = household_income, y=Vitamin_B1)) + geom_boxplot()
ggplot(forboxplot, aes(x = household_income, y=Vitamin_B2)) + geom_boxplot()
ggplot(forboxplot, aes(x = household_income, y=VitaminB6)) + geom_boxplot()
ggplot(forboxplot, aes(x = household_income, y=Vitamin_C)) + geom_boxplot()



colnames(NHANESDiabetesFinal3)[colnames(NHANESDiabetesFinal3) == "DR2TVB1"] <- "Vitamin B1"
colnames(NHANESDiabetesFinal3)[colnames(NHANESDiabetesFinal3) == "DR2TVB2"] <- "Vitamin B2"


forboxplot$sex <- as.numeric(as.factor(forboxplot$sex))
forboxplot$household_income <- as.numeric((as.factor(forboxplot$household_income)))
forboxplot$race <- as.numeric(as.factor(forboxplot$race))



#attempt 2 at creating pca
##log transform
forboxplot<- na.omit(forboxplot)
row_sub = apply(forboxplot, 1, function(row) all(row !=0 ))
forboxplot<-forboxplot[row_sub,]
log.vitamins <- log(forboxplot[, (1:5)])
log.vitamins <- na.omit(log.vitamins)


vitamins.socialfactor1 <- forboxplot[, 8]
vitamins.socialfactor2 <- forboxplot[, 7]
vitamins.socialfactor3 <- forboxplot[, 6]

vitamins <- forboxplot[, (1:5)]
log.vitamins <- log(vitamins)
log.vitamins <- log.vitamins[!is.infinite(rowSums(log.vitamins)),]

Vitamins_PCA<-prcomp(vitamins, scale=TRUE)
plot(Vitamins_PCA, type = "l")


install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
graphPCA <- ggbiplot(Vitamins_PCA, scale = 1, obs.pca = 1, var.scale = 1, groups = vitamins.socialfactor1, ellipse = T, circle = T)
graphPCA<- graphPCA + scale_color_manual(values =  c("orange", "yellow", "blue", "firebrick3", "black", "sky blue", "dodgerblue3"))
graphPCA <- graphPCA + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(graphPCA)
 