
library(dplyr)
library(tidyverse)
library(tidycensus)


poverty <- get_acs(geography = "tract", table = c("B17001"), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                   state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE) %>% 
  filter(variable == "B17001_001" | variable == "B17001_002")

GEOIDs <- poverty[1:110,1] %>% unique()
ID_list <- GEOIDs$GEOID

poverty_final <- poverty %>% 
  filter(GEOID %in% ID_list) %>%
  dplyr::select(1:4) %>%
  spread(variable, estimate) %>%
  mutate(Poverty_Rate = round(B17001_002/B17001_001,3))

Poverty_rates_by_tract <- poverty_final %>% dplyr::select(1,5)
