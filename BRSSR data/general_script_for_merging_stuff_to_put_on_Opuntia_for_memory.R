#General Script for things that don't take long but won't run on my computers memory

#Load Data
Children_BRSSR=readRDS("Children_BRSSR.rds")
ChildASTHMA=readRDS("ChildASTHMA.rds")

#Merge
Children_BRSSR=merge(Children_BRSSR,ChildASTHMA,all.x=TRUE)

#Save to retrieve
saveRDS(Children_BRSSR,"Final_Children_BRSSR_with_Asthma_included.rds")