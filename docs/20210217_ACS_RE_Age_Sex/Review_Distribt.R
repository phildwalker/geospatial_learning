# Review ACS data
# Wed Feb 17 08:53:39 2021 ------------------------------

# Goal: Understand how different racial communities are distributed throughout the Cone service area.

library(tidyverse)
library(sf)
library(eastyle)

load(file = here::here("data-raw", "SexAgeRE_Names.rda"))


unique(SexAgeRE_Names$Age)
  
SexAgeRE_Names <-
  SexAgeRE_Names %>% 
  mutate(Age = factor(Age, levels = c("Under 5 years","5 to 9 years","10 to 14 years","15 to 17 years","18 and 19 years","20 to 24 years",
                                      "25 to 29 years","30 to 34 years","35 to 44 years","45 to 54 years","55 to 64 years","65 to 74 years",
                                      "75 to 84 years","85 years and over")))

unique(SexAgeRE_Names$Race)

# AgeRE <- 
Perc <-
  SexAgeRE_Names %>% 
  st_drop_geometry() %>% 
  group_by(County, Race, Age) %>% 
  summarise(EstPop = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(County, Race) %>% 
  mutate(Perc = EstPop / sum(EstPop)) %>% 
  ungroup() %>% 
  filter(Race %in% c("WHITE ALONE, NOT HISPANIC OR LATINO", "BLACK OR AFRICAN AMERICAN ALONE", "HISPANIC OR LATINO" )) %>% 
  ggplot(aes(Age, Perc, fill=Race))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_y_continuous(labels = scales::percent)+
    theme(axis.text.x = element_text(angle = 90))+
    labs(x = NULL, y=NULL, fill = NULL,
         subtitle = "Percent by Age Group")+
    facet_wrap(County ~ . , ncol= 1)


Pop <- 
  SexAgeRE_Names %>% 
  st_drop_geometry() %>% 
  group_by(County, Race, Age) %>% 
  summarise(EstPop = sum(estimate)) %>% 
  ungroup() %>% 
  group_by(County, Race) %>% 
  mutate(Perc = EstPop / sum(EstPop)) %>% 
  ungroup() %>% 
  filter(Race %in% c("WHITE ALONE, NOT HISPANIC OR LATINO", "BLACK OR AFRICAN AMERICAN ALONE", "HISPANIC OR LATINO" )) %>% 
  ggplot(aes(Age, EstPop, fill=Race))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = NULL, y=NULL,fill = NULL,
       subtitle = "Total Population")+
  facet_wrap(County ~ . , ncol= 1, scales = "free_y")


library(patchwork)

Pop + Perc +
  plot_layout(guides = "collect")+
  plot_annotation(title = "Population Distribution by County")




