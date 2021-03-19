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


# save(AOI_poly, file = here::here("data-raw", "AOI_poly.rda"))
# st_write(AOI_poly,  "HealthCampus_AreaOfInterest.shp")

#-----------

# distance from points...

dist_to_testing <- st_distance(x = centroids_sf, y = testing_sites_sf)
ClosestTestSite <- testing_sites$Name[apply(dist_to_testing,1,which.min)] 










# --------------------------------------

# library(tidyverse)
# library(eastyle)
# library(ggspatial)
# library(sf)
# library(eageo)
# library(ggsflabel)
# library(ggnewscale) # for two fill scales


# Greensboro and Areas of Interest-------
ggplot() +
  geom_sf(data = GuilTracts, color="grey", fill="white") +  
  geom_sf(data = gso, color="grey", fill="grey50", alpha=0.6) +  
  geom_sf(data = AOI_poly, aes(fill=name),alpha=0.7) +
  geom_sf(data = road_filt, size = 0.9, color = "black") +
  geom_sf_label_repel(data=AOI_poly, aes(label = name, fill=name), size = 4, alpha=0.9)+
  # scale_fill_manual(values = c("#63ccff","#f1bd51","#b7d866","#00a2b2","#7750a9"), labels = scales::percent)+ #palette_cone_main[c(10,1,6,2,4)]
  # geom_sf_text(data = gso, label = "Greensboro", size =5, nudge_y = .08)+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Guilford and Greensboro",
       subtitle= "Greensboro City Limits Overlaying Census Tracts",
       # caption= glue::glue("Primary Care Pomona indentified as black dot // New location in red
       #                       12 and 24 minute drive times from the current location in blue"),
       y = NULL, x=NULL, color=NULL)+
  theme(plot.title = element_text(20),
        plot.subtitle = element_text(size= 16),
        plot.caption = element_text(size= 16),
        axis.text = element_blank(),
        legend.position = "none")+
  # guides(colour = guide_legend(override.aes = list(size=5))) +
  NULL


# SVI -------

ggplot() + 
  geom_sf(data = gso, color="grey", fill="grey50", alpha=0.6) +  
  geom_sf(data =SVI_LifeExp, aes(fill=RPL_THEMES), alpha=0.5)+
  scale_fill_gradientn(colors = palette_cone_main[c(2,6,1,10,5)],na.value = "transparent",
                       breaks=c(0,0.1,0.5, 0.9,1),labels=c("","Low Vulnerability","","Highly Vulnerable", ""),
                       limits=c(0,1)) +
  # geom_sf(data = RTG_poly, color="black",alpha=0.4, fill=palette_cone_main[4], show.legend = F) +
  geom_sf(data = AOI_poly, color="tomato",alpha=0.1, size =1.1 , fill=palette_cone_main[4], show.legend = F) +
  geom_sf(data = road_filt, size = 0.75, color = "black") +
  # geom_sf_label_repel(data=RTG_poly, aes(label = Area), size = 3, alpha=0.9)+
  scale_y_continuous(expand= c(0,0))+
  scale_x_continuous(expand= c(0,0))+
  labs(fill = NULL,
       title = "Social Vulnerability by Tract",caption = "Data Source: CDC 2018")+
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        # plot.title = element_text(hjust = 0.4),
        legend.key.width = unit(2.5, "cm"))



# Patient Density -----------------

GUILPats <- PCP_ED_Pats %>% 
  distinct(PatientID, X, Y, RE=SixRaceEthnicity, PCP) %>% 
  filter(X != 0) %>% 
  st_as_sf(coords = c("X", "Y"),crs = 4326) %>% 
  st_transform(crs = 4326) %>% 
  cbind(., st_coordinates(.)) %>% 
  mutate(WithNC = lengths(st_intersects(., GuilTracts))) %>% 
  filter(WithNC == 1)


GUILPats %>% 
  ggplot() +
  geom_sf(data = GuilTracts, color="grey", fill="white") +  
  geom_sf(data = road_filt, size = 0.9, color = "black") +
  
  geom_point(aes(x=X, y=Y), size = 0.4, alpha=0.4) + #, color = "black"
  stat_density_2d(data = GUILPats, aes(X,Y, fill = (..level..)), geom = "polygon", alpha=0.25) +
  
  geom_sf(data = RTG_poly, color="black",alpha=0.3, fill=palette_cone_main[5], show.legend = F) +
  
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Guilford Resident Patient listed home address",
       subtitle= "For patients with an encounter at the ED from 2019 - Jan 2021",
       y = NULL, x=NULL, color=NULL)+
  theme(plot.title = element_text(20),
        plot.subtitle = element_text(size= 16),
        plot.caption = element_text(size= 16),
        axis.text = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")+
  # guides(colour = guide_legend(override.aes = list(size=5))) +
  facet_wrap(~RE, ncol = 3)




# Places data for greensboro -------------------------
NCplaces <- tigris::places(state = "NC")

gso <-
  NCplaces %>% 
  filter(NAME == "Greensboro")

mapview::mapview(NCplaces, zcol = "LSAD") #3 categories: city, town, village

# save(gso, file = here::here("data-raw", "gso.rda"))

