## Install the required package with:
## install.packages("RSocrata")
# install.packages("RSocrata")
library("RSocrata")

df <- read.socrata(
  "https://chronicdata.cdc.gov/resource/yjkw-uj5s.json",
  app_token = "TpJAM6WSYX9QaxfkG3fa4jCmP",
  email     = "philip.walker@conehealth.com",
  password  = RSOCRATA_PAT
)

cdc_places <- df


save(cdc_places, file = here::here("data", "cdc_places.rda"))



cdc_places %>% 
  filter(countyname == "Guilford") %>% 
  count()




states <- tigris::states() %>% 
  st_as_sf() %>% 
  select(STUSPS) %>% 
  pull(STUSPS)

# all_tracts <- tigris::tracts(states)

combined <- tigris::rbind_tigris(
  lapply(states, function(x) {
    tigris::tracts(x, cb = TRUE)
  })
)


all_sf <-
  combined %>% 
  st_as_sf()

save(all_sf, file = here::here("data", "all_sf.rda"))



cdc_nc <-
  cdc_places %>%
  mutate(highchol_crudeprev = as.numeric(highchol_crudeprev),
  ) %>% 
  select(highchol_crudeprev, tractfips) %>% 
  # st_drop_geometry() %>% 
  right_join(., all_sf, by = c("tractfips" = "GEOID")) %>% 
  st_as_sf()


cdc_nc %>% 
  ggplot() +
  geom_sf(aes(fill=highchol_crudeprev))

# st_crs(cdc_nc)
# class(cdc_nc)
# 
# 
# library(eageo)
# 
# test <- cdc_nc %>% 
#   filter(!is.na(highchol_crudeprev)) %>% 
#   st_as_sf() %>% 
#   st_transform("+proj=longlat +datum=WGS84") 
# 
# 
# test %>%
#   prettyLeaflet() %>%
#     addPrettyPolygons()
#   




