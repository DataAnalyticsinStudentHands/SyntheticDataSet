library(ggplot2)
library(ggalluvial)

#for Monique/ S/LI Disproportionality
S_LI_disprop <- data.table(
  TX_student=as.character(1:5372806)
)
S_LI_disprop[,("race"):=if_else(as.numeric(TX_student)<681746,"black","white")]
S_LI_disprop[race=="white",("race"):=if_else(as.numeric(TX_student)<(681746+254197),"asian","white")]
S_LI_disprop[race=="white",("race"):=if_else(as.numeric(TX_student)<(681746+254197+1424600),"white","other")]
S_LI_disprop[,("S_LI"):=if_else(as.numeric(TX_student)<29085,"SL_I","not_SL_I")]
S_LI_disprop[as.numeric(TX_student)>681745,("S_LI"):=sample(
                                c(rep(c("SL_I"),(190804-29084)),rep(c("not_SL_I"),(5372806-190804-681745+29084))))]

ggplot(S_LI_disprop,
       aes(x=race,stratum=race,alluvium=S_LI,
           axis1 = race, axis2 = S_LI)) + 
  geom_alluvium(aes(fill = S_LI),
                width = 0, knot.pos = 0, reverse = TRUE) +
  #guides(fill = FALSE) +  
  geom_stratum(width = 1/6, alpha = .5, reverse = TRUE) +  #na.rm=TRUE includes the na as gray
  geom_text(stat = "stratum", aes(label=after_stat(stratum)),color="black") +
  scale_x_discrete(expand = c(.1, 0)) +
  #scale_x_continuous(breaks = 1:5, labels = c("role", "social", "chronic", "kids", "seniors")) +
  #coord_flip() +
  ggtitle("S_LI in Texas")
#this plot showed us that everyone who was not a CHW didn't list a community they worked with



#for CHW-frontline survey
CHW_survey <- read.csv2("~/Downloads/UH_OneDrive/OneDrive\ -\ University\ Of\ Houston/CHW-Frontline_Survey_ Data_combined.csv",header = TRUE, sep = ",")
CHW_survey_dt <- as.data.table(CHW_survey)
CHW_survey_dt <- CHW_survey_dt[UserLanguage!=""]
#for purposes of the plot, made na on role into CHWs
CHW_survey_dt[is.na(role),("role"):=1]
setorder(CHW_survey_dt,UserLanguage,role)
ggplot(CHW_survey_dt,
       aes(x=role,stratum=role,alluvium=group_social,
           axis1 = role, axis2 = group_social, axis3 = group_chronic, axis4 = group_children, axis5 = group_seniors)) + 
  geom_alluvium(aes(fill = UserLanguage),
                width = 0, knot.pos = 0, reverse = TRUE) +
  #guides(fill = FALSE) +  
  geom_stratum(width = 1/6, alpha = .5, reverse = TRUE) +  #na.rm=TRUE includes the na as gray
  geom_text(stat = "stratum", aes(label=after_stat(stratum)),color="black") +
  scale_x_discrete(expand = c(.1, 0)) +
  #scale_x_continuous(breaks = 1:5, labels = c("role", "social", "chronic", "kids", "seniors")) +
  #coord_flip() +
  ggtitle("CHW-frontline Survey - Respondent Role in Work and Community Served")
#this plot showed us that everyone who was not a CHW didn't list a community they worked with

ggplot(CHW_survey_dt,
       aes(
         axis1 = group_social, axis2 = group_children, axis3 = group_seniors, axis4 = group_chronic)) + 
  geom_alluvium(aes(fill = UserLanguage),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) + 
  geom_stratum(width = 1/8, reverse = TRUE) +
  geom_text(stat = "stratum", aes(label=after_stat(stratum)),color="black") +
  scale_x_continuous(breaks = 1:4, labels = c("social", "kids", "seniors", "chronic")) +
  #coord_flip() +
  ggtitle("CHW-frontline Survey - Community Served")

#then do ones about how they feel utilized or not??

ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject, y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, 0)) +
  geom_flow(width = 1/4) +
  geom_stratum(alpha = .5, width = 1/4) +
  geom_text(stat = "stratum", size = 4) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses", "labeled using `geom_text()`")