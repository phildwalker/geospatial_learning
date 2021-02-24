# write to sandbox


library(glue)
library(DBI)
library(odbc)

load(file = here::here("data", "cdc_places.rda"))

# class(cdc_places)

cdc_places <-
  cdc_places %>% 
  as_data_frame() %>% 
  select(-geolocation.type, -geolocation.coordinates)




# Write to ds_team in "demographics" schema

con <- dbConnect(odbc(), 'Shared_MCCBISOLDBDEV1') #replace Sandbox with your odbc connection name


dbWriteTable(con, SQL('demographics.cdc_places_US_tracts'), 
             cdc_places, 
             overwrite = TRUE, #If the table does not exist
             #append = TRUE if I need to break up different tables (slice to get id rows to grab)
             row.names = FALSE)

dbDisconnect(con)
