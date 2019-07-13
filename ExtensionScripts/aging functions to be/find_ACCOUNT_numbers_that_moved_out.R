
find_ACCOUNT_numbers_that_moved_out<-function(old_parcels,new_parcels){
  old_parcels$ACCOUNT=paste0(old_parcels$ACCOUNT,"_",old_parcels$BUILDING_NUMBER)
  new_parcels$ACCOUNT=paste0(new_parcels$ACCOUNT,"_",new_parcels$BUILDING_NUMBER)
  
  buildings_no_longer_available=old_parcels$ACCOUNT[which(!old_parcels$ACCOUNT %in% new_parcels$ACCOUNT)]
  new_buildings=new_parcels$ACCOUNT[which(!new_parcels$ACCOUNT %in% old_parcels$ACCOUNT)]
  
  #Can I figure out changes in ownership?
  old_owners=old_parcels[c("ACCOUNT","CurrOwner")]
  new_owners=new_parcels[c("ACCOUNT","CurrOwner","BUILDING_STYLE_CODE")]
  owners=merge(old_owners,new_owners,by="ACCOUNT")
  owners$change_in_ownership=ifelse(as.character(owners$CurrOwner.x)==as.character(owners$CurrOwner.y),"Same Owner","Change in Ownership")
  owners_changed=subset(owners,owners$change_in_ownership=="Change in Ownership")
  #Really only care about residential places
  owners_changed_residential=subset(owners_changed,owners_changed$BUILDING_STYLE_CODE %in% c("101","102","103","104","107","108","109","125","8177","8178","8179","8351","8354","8401","8548","8549","8550","8986","8988"))
  
  #Really only need account numbers and there's too much in memory
  to_move_people_out=c(buildings_no_longer_available,owners_changed_residential$ACCOUNT)
  return(to_move_people_out)
}