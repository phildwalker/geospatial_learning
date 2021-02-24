# Let's read the jeoJson file that is stored on the web with the geojsonio library:

# install.packages("geojsonio")

library(geojsonio)
spdf <- geojson_read("https://chronicdata.cdc.gov/resource/yjkw-uj5s.geojson?stateabbr=NC&countyname=Guilford",  what = "sp")


library(sf)
library(tidyverse)
cdc_sf <-
  spdf %>% 
  st_as_sf()


unique(cdc_sf$statedesc)
unique(cdc_sf$countyname)



cdc_sf %>% 
  mutate(highchol_crudeprev = as.numeric(highchol_crudeprev)) %>% 
  ggplot() +
  geom_sf(aes(fill=highchol_crudeprev))



cdc_sf %>% 
  st_drop_geometry() %>% 
  count(countyname)

cdc_sf %>% 
  st_drop_geometry() %>%
  filter(countyname == "Guilford") %>% 
  select(tractfips)


NC_tracts <- tigris::tracts("NC", county = "Guilford")

NC_sf <-
  NC_tracts %>% 
  st_as_sf()



cdc_nc <-
  cdc_sf %>%
  mutate(highchol_crudeprev = as.numeric(highchol_crudeprev),
         ) %>% 
  select(highchol_crudeprev, tractfips) %>% 
  st_drop_geometry() %>% 
  full_join(., NC_sf, by = c("tractfips" = "GEOID")) %>% 
  st_as_sf()
  

class(cdc_nc)
st_crs(cdc_nc)

cdc_nc %>% 
  ggplot() +
  geom_sf(aes(fill=highchol_crudeprev))











