#Bandaid so we have a file now
phbbth=list.files(getwd())
phbbth=subset(phbbth,substr(phbbth,1,5)=="tract")

#phbbth=phbbth[15:454]
#Load needed data for merge
validparceldataframe2=readRDS("valid_parcels_for_simulation.RDS")
validparceldataframe2$ACCOUNT=paste0(validparceldataframe2$ACCOUNT,"_",validparceldataframe2$BUILDING_NUMBER)

#tract_sample_set=readRDS("tract_that_has_ACCOUNT_numbers_100000")

#tract_sample_set=subset(tract_sample_set,tract_sample_set$tract==100000)
#Merge
#tract_sample_set=merge(tract_sample_set,validparceldataframe2,by="ACCOUNT",all.x=TRUE)

complete_sample_set=data.frame()

for(why in phbbth){
  
  tract_sample_set=readRDS(paste0("C:/Users/Carol/Desktop/SyntheticDataSet/Potential Expansions/merging with HCAD functions/tract_files_May/",why))
  
  tract_sample_set=subset(tract_sample_set,tract_sample_set$tract==substr(why,32,38))
  #Merge
  tract_sample_set=merge(tract_sample_set,validparceldataframe2,by="ACCOUNT",all.x=TRUE)
  
  complete_sample_set=rbind(complete_sample_set,tract_sample_set)
}
saveRDS(complete_sample_set,"complete_sample_set_5_4_18_did_you_merge.RDS")
why=complete_sample_set[!duplicated(complete_sample_set), ]
saveRDS(why,"merged_set_5_4_18.RDS")

#mergedsampleset=readRDS("merged_set_5_3_18.RDS")
