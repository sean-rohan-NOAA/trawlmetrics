environ <- function() {
  
  dir.create(here::here("data"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  envdat <- RODBC::sqlQuery(channel = channel,
                             query = "SELECT s.YEAR, u.HAUL_ID, u.NET_NUMBER, u.STRATUM, u.STATION, u.WAVE_HEIGHT, u.SWELL_HEIGHT, u.SWELL_DIRECTION, 
u.CURRENT_SPEED, u.CURRENT_DIRECTION, u.BOTTOM_TYPE, u.STEEPNESS
from
RACE_DATA.HAULS u, RACE_DATA.CRUISES c, RACE_DATA.SURVEYS s
where c.CRUISE_ID = u.CRUISE_ID
AND s.SURVEY_ID = c.SURVEY_ID
AND s.survey_definition_ID in (98, 143)
AND s.YEAR >= 2010")
  
  
  
  saveRDS(object = envdat, file = here::here("data", "env_dat.rds"))
  
  
  output <- envdat
  
  
  return(output)
  
}
envdat <- environ()

full <- merge(envdat, stats, by="HAUL_ID")