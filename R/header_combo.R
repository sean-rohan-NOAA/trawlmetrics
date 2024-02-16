headerlabel <- function() {
  
  dir.create(here::here("data"))
  
  channel <- trawlmetrics:::get_connected(schema = "AFSC")
  
  headers <- RODBC::sqlQuery(channel = channel,
                              query = "SELECT
b.BOTTOM_CONTACT_HEADER_ID, b.HAUL_ID
from
RACE_DATA.BOTTOM_CONTACT_HEADERS b
WHERE b.HAUL_ID > 4673
ORDER BY BOTTOM_CONTACT_HEADER_ID ASC")
  
  
  saveRDS(object = headers, file = here::here("data", "headers.rds"))
  
  
  output <- headers
  
  
  return(output)
  
}

header_dat <- headerlabel()

dat_full <- merge (header_dat,dat,by="HAUL_ID")

