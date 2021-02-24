# flag if polygons overlap --------------

library(sf)
library(dpylr)

iso_location$pt_count <- lengths(st_intersects(iso_location, ED_Within))

iso_location <-
  iso_location %>% 
  arrange(loc_id, steps) %>% 
  group_by(loc_id) %>% 
  mutate(TotalPatCov = cumsum(pt_count))

# --------------------------------------
# reading in the manually developed polygons
# http://apps.headwallphotonics.com/

NEIGH <-
  readxl::read_excel(here::here("data-raw","Neighborhood_Lookup.xlsx"), sheet = "Neighborhoods") %>% 
  separate(Coordinates, into = c("y", "x"), sep = ",") %>% 
  st_as_sf(coords = c("x", "y"), crs = 4269) %>% 
  cbind(., st_coordinates(.))

NEIGH_poly <-
  NEIGH %>% 
  group_by(Name) %>% 
  # filter(Area == "Northeast Greensboro") %>% 
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
  st_transform(29902) %>% 
  st_buffer(60) %>% 
  st_transform(4269) %>% 
  st_difference() %>%
  ungroup()

mapview::mapview(NEIGH_poly)

# --------------------------------------














