bottom_contacts <- function() {
  
  dir.create(here::here("data"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  bc23 <- RODBC::sqlQuery(channel = channel,
                               query = "SELECT b.DATE_TIME, h.BOTTOM_CONTACT_HEADER_ID, b.X_AXIS, b.Y_AXIS, 
b.Z_AXIS
from
RACE_DATA.BOTTOM_CONTACTS b, RACE_DATA.BOTTOM_CONTACT_HEADERS h
where
h.BOTTOM_CONTACT_HEADER_ID = b.BOTTOM_CONTACT_HEADER_ID
and ( b.DATUM_CODE = 0 OR b.DATUM_CODE= 1 OR b.DATUM_CODE = 11)
and b.DATE_TIME >= '01-MAY-2023' and b.DATE_TIME <= '31-DEC-2023'
and X_AXIS IS NOT NULL
ORDER BY b.BOTTOM_CONTACT_HEADER_ID, b.DATE_TIME ASC
")
  
  saveRDS(object = bc23, file = here::here("data", "bc23.rds"))


  
  output <- bc23

  
  return(output)
  
}

contact_dat <- bottom_contacts()
lubridate::force_tz(contact_dat$DATE_TIME, tz = "America/Anchorage")
merged <- merge(contact_dat,dat_full, by="BOTTOM_CONTACT_HEADER_ID")
bottom_contact_dat1 <- merged[which(merged$DATE_TIME >= merged$DATE_TIME.x & merged$DATE_TIME <= merged$DATE_TIME.y),]
bottom_contact_full <- rbind(bottom_contact_full, bottom_contact_dat1)

saveRDS(object = bottom_contact_full, file = here::here("data", "bcFULL.rds"))
