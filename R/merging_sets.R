
 merged <- merge(contact_dat,dat_full, by="BOTTOM_CONTACT_HEADER_ID")
  
  bottom_contact_dat1 <- merged[which(merged$DATE_TIME >= merged$DATE_TIME.x & merged$DATE_TIME <= merged$DATE_TIME.y),]
  
  bottom_contact_full <- rbind(bottom_contact_full, bottom_contact_dat1)
  

  
  
  saveRDS(object = bottom_contact_full, file = here::here("data", "bcFULL.rds"))

  