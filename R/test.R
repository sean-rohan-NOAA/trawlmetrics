#' Retrieve net mensuration data
#' 
#' 
#' 

# Retrieve number of hauls

# Compare net mensuration metrics to historical data



"select a.net_number,
count(a.net_number) n_hauls
from race_data.hauls a, 
race_data.cruises b
where a.cruise_id in (755, 756, 757, 758)
and b.cruise_id = a.cruise_id
group by a.net_number
order by a.net_number"

"select a.hauljoin, a.vessel, a.cruise, a.haul, a.net_measured, a.net_height, a.net_width, a.wire_length, a.bottom_depth, a.performance, a.gear, a.accessories,
a.stationid, d.net_number, d.footrope_number, d.autotrawl_method, d.starboard_door_number, d.port_door_number, d.haul_type, e.description
from
racebase.haul a, race_data.cruises b, race_data.surveys c, race_data.hauls d, race_data.gear_codes e
where c.survey_definition_id in (98, 143)
and b.survey_id = c.survey_id
and a.cruisejoin = b.racebase_cruisejoin
and d.cruise_id = b.cruise_id
and a.haul = d.haul
and e.gear_code = a.gear"

