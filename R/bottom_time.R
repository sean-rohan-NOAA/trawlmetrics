library(trawlmetrics)
library(lubridate)
library(dplyr)

bottomtime <- function() {
  
  dir.create(here::here("data"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  onbottom <- RODBC::sqlQuery(channel = channel,
                              query = "SELECT 
e.HAUL_ID, e.DATE_TIME 
from
RACE_DATA.EVENTS e, RACE_DATA.HAULS h, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
where
s.SURVEY_ID = c.SURVEY_ID
AND c.CRUISE_ID = h.CRUISE_ID
AND h.HAUL_ID = e.HAUL_ID
AND e.EVENT_TYPE_ID = 3
AND s.survey_definition_ID in (98, 143)
AND s.YEAR >= 2010
ORDER BY e.HAUL_ID, e.EVENT_TYPE_ID")
  
  offbottom <- RODBC::sqlQuery(channel = channel,
                               query = "SELECT 
e.HAUL_ID, e.DATE_TIME
from
RACE_DATA.EVENTS e, RACE_DATA.HAULS h, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
where
s.SURVEY_ID = c.SURVEY_ID
AND c.CRUISE_ID = h.CRUISE_ID
AND h.HAUL_ID = e.HAUL_ID
AND e.EVENT_TYPE_ID = 7
AND s.survey_definition_ID in (98, 143)
AND s.YEAR >= 2010
ORDER BY e.HAUL_ID, e.EVENT_TYPE_ID")
  
  
  saveRDS(object = onbottom, file = here::here("data", "onbottom.rds"))
  
  saveRDS(object = offbottom, file = here::here("data", "offbottom.rds"))
  
  output <- merge(onbottom,offbottom, by ="HAUL_ID")

  
  
  return(output)
  
}

headerlabel <- function() {
  
  dir.create(here::here("data"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  headers <- RODBC::sqlQuery(channel = channel,
                             query = "SELECT
b.BOTTOM_CONTACT_HEADER_ID, b.HAUL_ID
from
RACE_DATA.BOTTOM_CONTACT_HEADERS b
WHERE b.HAUL_ID > 6119
ORDER BY BOTTOM_CONTACT_HEADER_ID ASC")
  
  
  
  saveRDS(object = headers, file = here::here("data", "headers.rds"))
  
  
  output <- headers
  
  
  return(output)
  
}

header <- headerlabel()
dat <- bottomtime()

dat_full <- merge (header,dat,by="HAUL_ID")

dat_full <- dat_full %>% rename(ONBOTTOM = DATE_TIME.x, 
                    OFFBOTTOM = DATE_TIME.y)

lubridate::force_tz(dat_full$ONBOTTOM, tz = "America/Anchorage")
force_tz(dat_full$OFFBOTTOM, tz = "America/Anchorage")
