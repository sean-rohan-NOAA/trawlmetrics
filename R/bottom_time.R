library(trawlmetrics)
library(lubridate)


bottomtime <- function() {
  
  dir.create(here::here("data"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  onbottom <- RODBC::sqlQuery(channel = channel,
                              query = "SELECT 
e.HAUL_ID, e.DATE_TIME
from
RACE_DATA.EVENTS e
where
e.EVENT_TYPE_ID = 3
AND e.HAUL_ID > 4673
ORDER BY e.HAUL_ID, EVENT_TYPE_ID")
  
  offbottom <- RODBC::sqlQuery(channel = channel,
                               query = "SELECT 
e.HAUL_ID, e.DATE_TIME
from
RACE_DATA.EVENTS e
where
e.EVENT_TYPE_ID = 7
AND e.HAUL_ID > 4673

ORDER BY e.HAUL_ID, EVENT_TYPE_ID")
  
  saveRDS(object = onbottom, file = here::here("data", "onbottom.rds"))
  
  saveRDS(object = offbottom, file = here::here("data", "offbottom.rds"))
  
  output <- merge(onbottom,offbottom,by="HAUL_ID")
  
  
  return(output)
  
}

dat <- bottomtime()


lubridate::force_tz(dat_full$DATE_TIME.x, tz = "America/Anchorage")

