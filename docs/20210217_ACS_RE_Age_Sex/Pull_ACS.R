# ACS data for Age/Sex/Race
# Wed Feb 17 08:52:52 2021 ------------------------------

library(tidycensus)
library(tidyverse)

v19 <- load_variables(2019, "acs5", cache = TRUE)
#---------- Sex, Age, RE -----

vars <- sort(paste0("B01001", rep(LETTERS[1:9] , times = 31) ,"_", stringr::str_pad(c(1:31),3,pad = "0")))

SexAgeRE <- get_acs(geography = "tract", variables = vars,
                    state = "NC", 
                    county = c("Guilford", "Alamance", "Caswell", "Rockingham"), geometry = TRUE)


SexAgeRE_Names <-
  SexAgeRE %>% 
  left_join(., v19, by = c("variable" = "name")) %>% 
  mutate(labelRaw = label) %>% 
  separate(label, into = c("Var", "Total", "Sex", "Age"), sep = "!!") %>% 
  separate(NAME, into = c("Tract", "County", "State"), sep = ",") %>% 
  mutate(Race = str_remove_all(concept, "SEX BY AGE"),
         Race = trimws(str_remove_all(Race, "\\(|\\)")),
         Sex = str_remove_all(Sex, ":")) %>% 
  mutate(DataGroup = case_when(is.na(Age) ~ "TotalGrouping",
                               is.na(Sex) & is.na(Age) ~ "TotalOverall",
                               TRUE ~ "Counts")) %>%   
  filter(!DataGroup %in% c("TotalGrouping", "TotalOverall")) %>% 
  select(-Total, -DataGroup, -concept) %>% 
  ungroup()


save(SexAgeRE_Names, file = here::here("data-raw", "SexAgeRE_Names.rda"))


unique(SexAgeRE_Names$Race)



