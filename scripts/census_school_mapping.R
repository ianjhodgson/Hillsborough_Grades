rm(list = ls())

library(sf)
library(tidyverse)

elem_zones <- st_read("data/shape files/Elementary")
school_grades <- read_csv('data/schoolgrades23_linked.csv') %>% 
  filter(charter_school == "NO", 
         alternative_ese_center_school == "N",
         district_name == "HILLSBOROUGH")
## poverty data
poverty <- st_read('data/census/acs2022_5yr_B17020_14000US12057011514 2/acs2022_5yr_B17020_14000US12057011514.geojson')
poverty_u18 <- poverty %>% 
  select(geoid, name,
         pov_6_11 = B17020004, 
         pov_6_11_error = B17020004..Error,
         pov_12_17 = B17020005,
         pov_12_17_error = B17020005..Error,
         npov_6_11 = B17020012, 
         npov_6_11_error = B17020012..Error,
         npov_12_17 = B17020013, 
         npov_12_17_error = B17020013..Error,
         geometry)

## join census and school area data
joined <- st_covers(elem_zones, poverty_u18)
